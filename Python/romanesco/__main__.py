import sys
import os
import time
import re
from typing import List, Set, Dict

from . import lexing, parser, undeterminable, runtime, constraints
from .runtime import evaluator as runtime_evaluator

class TokenizerConfig:
    def __init__(self, keywords: Set[str], operators: Dict[str, lexing.Token], delimiters: Set[str]):
        self.keywords = keywords
        self.operators = operators
        self.delimiters = delimiters

    @staticmethod
    def default() -> 'TokenizerConfig':
        return TokenizerConfig(
            keywords={"syntax", "import"},
            operators={
                "and": lexing.Op("and"),
                "or": lexing.Op("or"),
                "==": lexing.Op("=="),
                "!=": lexing.Op("!="),
                ">=": lexing.Op(">="),
                "<=": lexing.Op("<="),
                "+": lexing.Op("+"),
                "-": lexing.Op("-"),
                "*": lexing.Op("*"),
                "/": lexing.Op("/"),
                ">": lexing.Op(">"),
                "<": lexing.Op("<"),
                "=": lexing.Op("="),
                "->": lexing.Op("->"),
            },
            delimiters={"(", ")", "{", "}", "[", "]", ",", ":", "\\"}
        )

def build_tokenizers(config: TokenizerConfig) -> List[lexing.Tokenizer]:
    ident_regex = r"[a-zA-Z_][a-zA-Z_0-9]*"
    ws_regex = r"([ \t\r\n;]|//.*)+"
    number_regex = r"[0-9]+"
    string_regex = r'"([^"\\]|\\.)*"'
    
    sorted_ops = sorted(config.operators.keys(), key=len, reverse=True)
    op_regex = "|".join(re.escape(k) for k in sorted_ops)
    
    return [
        lexing.lex_regex_longest(ws_regex, lambda s: lexing.WS(s)),
        lexing.lex_regex_longest(number_regex, lambda s: lexing.Number(s)),
        lexing.lex_regex_longest(string_regex, lambda s: lexing.String(s[1:-1])),
        lexing.lex_regex_longest(op_regex, lambda s: config.operators[s]),
        lexing.lex_regex_longest(ident_regex, lambda s: 
            lexing.Keyword(s) if s in config.keywords else lexing.Ident(s)
        ),
        lexing.lex_delim(config.delimiters)
    ]

def print_lexing_results(token_tree: undeterminable.Tree[lexing.Token], time_ms: int):
    print("== Lexing (final) ==")
    token_paths = token_tree.flatten_paths()
    display_count = 5
    for i, tokens in enumerate(token_paths[:display_count]):
        filtered = [t.lexeme for t in tokens if not isinstance(t, lexing.WS)]
        print(f"  Path {i}: {' '.join(filtered)}")
    if len(token_paths) > display_count:
        print(f"  ... and {len(token_paths) - display_count} more paths")
    print(f"  Time: {time_ms}ms")
    print(f"  Found {len(token_paths)} token sequences")
    print()

def print_parsing_results(ast_tree: undeterminable.Tree[List], time_ms: int):
    # Note: Type hint List is loose, actually List[ast.Expr]
    from .syntax import ast
    print("== Parsing ==")
    ast_paths = ast_tree.flatten_paths()
    for i, exprs in enumerate(ast_paths):
        print(f"  Parse {i}:")
        # In current parser implementation, parse_all returns List[Expr] (treated as Stmt list)
        for expr in exprs:
            # Simple repr for now
            print(f"    {expr}")
    if not ast_paths:
        print("  No valid parse trees")
    print(f"  Time: {time_ms}ms")
    print(f"  Found {len(ast_paths)} parse trees")
    print()

def parse_mode(arg: str) -> lexing.LexMode | None:
    if arg == "best": return lexing.BestOnly()
    if arg == "all": return lexing.All()
    if arg.startswith("topN:"):
        try:
            n = int(arg[5:])
            return lexing.TopN(n)
        except: return None
    return None

def main():
    args = sys.argv[1:]
    if len(args) < 1:
        print("Usage: python -m romanesco <input-file> [options...]")
        return

    input_path = os.path.abspath(args[0])
    options = set(args)
    use_prune = "prune" in options
    use_debug = "debug" in options
    use_eval = "eval" in options
    
    if use_debug:
        runtime_evaluator.DEBUG_EVAL = True

    mode_arg = next((a for a in args if a not in ["prune", "debug", "eval", input_path] and not a.endswith(".romanesco")), "all")
    mode = parse_mode(mode_arg) or lexing.All()

    # Tokenizer Config
    config = TokenizerConfig.default()
    tokenizers = build_tokenizers(config)

    if use_debug:
        print("=== Romanesco Compiler (Python Port) ===")
        print(f"Input: {input_path}")
        print(f"Mode: {mode}")
        print(f"Z3 Pruning: {'enabled' if use_prune else 'disabled'}")
        print(f"Debug mode: {'enabled' if use_debug else 'disabled'}")
        print(f"Evaluation: {'enabled' if use_eval else 'disabled'}")
        print()

    # 1. Lexing
    try:
        with open(input_path, 'r', encoding='utf-8') as f:
            input_str = f.read()
    except Exception as e:
        print(f"Failed to read file: {input_path}")
        print(f"Error: {e}")
        return

    lex_start = time.time() * 1000
    raw_token_tree = lexing.lex_all(input_str, tokenizers, mode, use_debug)
    lex_end = time.time() * 1000

    if use_debug:
        print(raw_token_tree.draw_tree())
    
    if use_debug:
        print("== Lexing (before pruning) ==")
        raw_paths = raw_token_tree.flatten_paths()
        print(f"  Found {len(raw_paths)} token sequences")
        print(f"  Time: {int(lex_end - lex_start)}ms")
        print()

    # 2. Pruning (Solver)
    if use_prune:
        prune_start = time.time() * 1000
        token_tree = constraints.prune_token_tree(raw_token_tree)
        prune_end = time.time() * 1000
        
        if use_debug:
            # Re-calculate raw paths for display
            raw_paths = raw_token_tree.flatten_paths()
            pruned_paths = token_tree.flatten_paths()
            print("== Z3 Constraint Pruning ==")
            print(f"  Before: {len(raw_paths)} sequences")
            print(f"  After: {len(pruned_paths)} sequences")
            print(f"  Eliminated: {len(raw_paths) - len(pruned_paths)} invalid sequences")
            print(f"  Time: {int(prune_end - prune_start)}ms")
            print()
    else:
        token_tree = raw_token_tree

    if use_debug:
        print_lexing_results(token_tree, int(lex_end - lex_start))

    # 3. Parsing
    parse_start = time.time() * 1000
    ast_tree = parser.parse_all(token_tree.flatten_paths()[0], debug=use_debug) if token_tree.flatten_paths() else undeterminable.DeadEnd()
    parse_end = time.time() * 1000

    if use_prune:
        # Prune AST tree based on constraints
        ast_paths = ast_tree.flatten_paths()
        valid_ast_paths = [p for p in ast_paths if constraints.check_ast_constraints(p)]
        if len(valid_ast_paths) < len(ast_paths):
            if use_debug:
                print(f"== AST Constraint Pruning ==")
                print(f"  Before: {len(ast_paths)} parse trees")
                print(f"  After: {len(valid_ast_paths)} parse trees")
                print(f"  Eliminated: {len(ast_paths) - len(valid_ast_paths)} invalid parse trees")
                print()
            
            if not valid_ast_paths:
                ast_tree = undeterminable.DeadEnd()
            else:
                ast_tree = undeterminable.fork([undeterminable.leaf(p) for p in valid_ast_paths])

    if use_debug:
        print_parsing_results(ast_tree, int(parse_end - parse_start))

    # 4. Evaluation
    if use_eval:
        ast_paths = ast_tree.flatten_paths()
        
        if ast_paths:
            # Evaluate all possible interpretations (paths)
            for i, path in enumerate(ast_paths):
                if use_debug: print(f"== Evaluation (Path {i}) ==")
                
                # Unwrap path: path is List[List[Expr]], we want List[Expr]
                exprs = []
                for node_val in path:
                    exprs.extend(node_val)

                eval_start = time.time() * 1000
                
                try:
                    all_results = runtime.eval_program(exprs)
                    eval_end = time.time() * 1000
                    
                    if not all_results:
                        print("No solutions found.")
                    else:
                        if use_debug: print(f"  Found {len(all_results)} possible solutions:")
                        for j, (env, result) in enumerate(all_results):
                            if use_debug: print(f"  --- Solution {j} ---")
                            
                            # Show result if exists
                            if result is not None:
                                print(f"Result: {result}")
                            elif not env.subs:
                                print("Success (no result)")
                                
                            # Show environment bindings (filtered)
                            # Assuming env.bindings contains Expr objects
                            # We might want to resolve them to values if possible
                            # For now, just print non-prelude ones
                            pass 

                    if use_debug:
                        print(f"  Time: {int(eval_end - eval_start)}ms")
                        print()
                except Exception as e:
                    eval_end = time.time() * 1000
                    print(f"Evaluation error: {e}")
                    if use_debug:
                        import traceback
                        traceback.print_exc()

    if use_debug:
        print()
        print(f"Total time: {int(time.time() * 1000 - lex_start)}ms")

if __name__ == "__main__":
    main()
