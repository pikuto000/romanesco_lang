import sys
import os
import time
import re
from typing import List, Set, Dict, Optional

# Ensure we can import romanesco package
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from romanesco import lexing, parsing, undeterminable, core, solver

def main():
    args = sys.argv[1:]
    
    if len(args) < 1:
        print_usage()
        return

    input_path = args[0]
    
    options = set(args)
    use_prune = "prune" in options
    use_debug = "debug" in options
    use_eval = "eval" in options
    
    mode_arg = next((a for a in args if a not in ["prune", "debug", "eval", input_path]), "all")
    mode = parse_mode(mode_arg) or lexing.All()

    try:
        with open(input_path, 'r', encoding='utf-8') as f:
            input_str = f.read()
    except Exception as e:
        print(f"Failed to read file: {input_path}")
        print(f"Error: {e}")
        return

    # Tokenizer Config
    config = TokenizerConfig.default()
    tokenizers = build_tokenizers(config)

    print("=== Romanesco Compiler (Python Port) ===")
    print(f"Input: {input_path}")
    print(f"Mode: {mode}")
    print(f"Z3 Pruning: {'enabled' if use_prune else 'disabled'}")
    print(f"Debug mode: {'enabled' if use_debug else 'disabled'}")
    print(f"Evaluation: {'enabled' if use_eval else 'disabled'}")
    print(f"Input size: {len(input_str)} chars")
    print()

    # Lexing
    lex_start = time.time() * 1000
    raw_token_tree = lexing.lex_all(input_str, tokenizers, mode, use_debug)
    lex_end = time.time() * 1000

    if use_debug:
        print(raw_token_tree.draw_tree())
    
    print("== Lexing (before pruning) ==")
    raw_paths = raw_token_tree.flatten_paths()
    print(f"  Found {len(raw_paths)} token sequences")
    print(f"  Time: {int(lex_end - lex_start)}ms")
    print()

    # Pruning
    if use_prune:
        prune_start = time.time() * 1000
        token_tree = solver.prune_token_tree(raw_token_tree)
        prune_end = time.time() * 1000
        
        pruned_paths = token_tree.flatten_paths()
        print("== Z3 Constraint Pruning ==")
        print(f"  Before: {len(raw_paths)} sequences")
        print(f"  After: {len(pruned_paths)} sequences")
        print(f"  Eliminated: {len(raw_paths) - len(pruned_paths)} invalid sequences")
        print(f"  Time: {int(prune_end - prune_start)}ms")
        print()
    else:
        token_tree = raw_token_tree

    print_lexing_results(token_tree, int(lex_end - lex_start)) # using lex time for now

    # Parsing
    parse_start = time.time() * 1000
    ast_tree = parsing.parse_all(token_tree)
    parse_end = time.time() * 1000

    if use_prune:
        # Prune AST tree based on constraints
        ast_paths = ast_tree.flatten_paths()
        valid_ast_paths = [p for p in ast_paths if solver.check_ast_constraints(p[0])]
        if len(valid_ast_paths) < len(ast_paths):
            print(f"== AST Constraint Pruning ==")
            print(f"  Before: {len(ast_paths)} parse trees")
            print(f"  After: {len(valid_ast_paths)} parse trees")
            print(f"  Eliminated: {len(ast_paths) - len(valid_ast_paths)} invalid parse trees")
            print()
            
            if not valid_ast_paths:
                ast_tree = undeterminable.DeadEnd()
            else:
                ast_tree = undeterminable.fork([undeterminable.leaf(p[0]) for p in valid_ast_paths])

    print_parsing_results(ast_tree, int(parse_end - parse_start))

    # Evaluation
    if use_eval:
        ast_paths = ast_tree.flatten_paths()
        
        if ast_paths and ast_paths[0]:
            # ast_tree is Tree[List[Expr]]
            exprs = ast_paths[0][0]
            
            print("== Evaluation ==")
            eval_start = time.time() * 1000
            
            try:
                all_results = core.eval_program(exprs)
                eval_end = time.time() * 1000
                
                print(f"  Found {len(all_results)} possible solutions:")
                for i, (env, result) in enumerate(all_results):
                    print(f"  --- Solution {i} ---")
                    print("    Environment bindings:")
                    # Filter out prelude
                    for name, expr in env.bindings.items():
                        if name not in core.prelude_bindings:
                            print(f"      {name} = {core.show_core_expr(expr)}")
                    
                    if result is not None:
                        print(f"    Result: {result}")
                    else:
                        print("    No result")
                    print()
                    
                print(f"  Time: {int(eval_end - eval_start)}ms")
                print()
            except Exception as e:
                eval_end = time.time() * 1000
                print(f"  Evaluation error: {e}")
                print(f"  Time: {int(eval_end - eval_start)}ms")
                print()
                if use_debug:
                    import traceback
                    traceback.print_exc()

    print()
    print(f"Total time: {int(time.time() * 1000 - lex_start)}ms")


class TokenizerConfig:
    def __init__(self, keywords: Set[str], operators: Dict[str, lexing.Token], delimiters: Set[str]):
        self.keywords = keywords
        self.operators = operators
        self.delimiters = delimiters

    @staticmethod
    def default() -> 'TokenizerConfig':
        return TokenizerConfig(
            keywords={"syntax"},
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
    
    # Sort operators by length desc to match longest first in regex alternation
    sorted_ops = sorted(config.operators.keys(), key=len, reverse=True)
    op_regex = "|".join(re.escape(k) for k in sorted_ops)
    
    return [
        lexing.lex_regex_longest(ws_regex, lambda s: lexing.WS(s)),
        lexing.lex_regex_longest(number_regex, lambda s: lexing.Number(s)),
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
        print(f"  Path {i}: {" ".join(filtered)}")
        
    if len(token_paths) > display_count:
        print(f"  ... and {len(token_paths) - display_count} more paths")
        
    print(f"  Time: {time_ms}ms")
    print(f"  Found {len(token_paths)} token sequences")
    print()

def print_parsing_results(ast_tree: undeterminable.Tree[List[parsing.Expr]], time_ms: int):
    print("== Parsing ==")
    
    ast_paths = ast_tree.flatten_paths()
    
    for i, exprs in enumerate(ast_paths):
        print(f"  Parse {i}:")
        for expr in exprs:
            print(f"    {parsing.show_expr(expr)}")
            
    if not ast_paths:
        print("  No valid parse trees")
        
    print(f"  Time: {time_ms}ms")
    print(f"  Found {len(ast_paths)} parse trees")
    print()

def parse_mode(arg: str) -> Optional[lexing.LexMode]:
    if arg == "best": return lexing.BestOnly()
    if arg == "all": return lexing.All()
    if arg.startswith("topN:"):
        try:
            n = int(arg[5:])
            return lexing.TopN(n)
        except:
            return None
    return None

def print_usage():
    print("""Usage:
  python main.py <input-file> [options...]\n
Options:
  all      - All possible tokenizations (default)
  best     - Only the longest tokenization
  topN:<n> - Top N tokenizations
  prune    - Enable constraint pruning
  debug    - Enable debug output
  eval     - Run evaluation phase

Examples:
  python main.py test.romanesco eval
  python main.py test.romanesco debug eval
  python main.py test.romanesco prune eval
""")

if __name__ == "__main__":
    main()
