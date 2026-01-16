import sys
import os
import time
import re
from typing import List, Set, Dict

from . import lexing, parsing, core

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

def main():
    args = sys.argv[1:]
    if len(args) < 1:
        print("Usage: python -m romanesco <input-file> [options...]")
        return

    input_path = os.path.abspath(args[0])
    options = set(args)
    use_debug = "debug" in options
    use_eval = "eval" in options
    
    if use_debug: core.DEBUG_EVAL = True

    config = TokenizerConfig.default()
    tokenizers = build_tokenizers(config)
    processed_files = {} # path -> List[List[Expr]]

    def import_handler(rel_path: str) -> List[List[parsing.Expr]]:
        # This callback is called during parsing
        # Find path relative to the file currently being parsed?
        # For simplicity, we'll use CWD or relative to the main file for now.
        target_path = os.path.abspath(os.path.join(os.path.dirname(input_path), rel_path))
        return process_file(target_path)

    parsing._import_handler = import_handler

    def process_file(path: str) -> List[List[parsing.Expr]]:
        if path in processed_files:
            return processed_files[path]
        
        if use_debug: print(f"Processing: {path}")
        # To handle circular imports, we put a temporary empty interpretation
        processed_files[path] = [[]]
        
        try:
            with open(path, 'r', encoding='utf-8') as f:
                content = f.read()
        except Exception as e:
            if use_debug: print(f"Failed to read {path}: {e}")
            return [[]]

        lex_tree = lexing.lex_all(content, tokenizers, lexing.All(), False)
        if use_debug:
            paths = lex_tree.flatten_paths()
            print(f"  Lexed into {len(paths)} paths")
            
        ast_tree = parsing.parse_all(lex_tree.flatten_paths()[0], debug=use_debug) if lex_tree.flatten_paths() else parsing.undeterminable.DeadEnd()
        interpretations = ast_tree.flatten_paths()
        if use_debug: print(f"  Parsed into {len(interpretations)} interpretations")
        
        final_list = []
        for path_seqs in interpretations:
            final_list.append(path_seqs)
            
        processed_files[path] = final_list
        return final_list

    # Kick off processing
    all_interpretations = process_file(input_path)

    if use_debug:
        print(f"=== Romanesco Compiler (Python Port) ===")
        print(f"Input: {input_path}")
        print(f"Found {len(all_interpretations)} possible interpretations.")
        print()

    # Evaluation
    if use_eval:
        eval_start = time.time()
        solution_found = False
        
        for p_idx, exprs in enumerate(all_interpretations):
            # If exprs is nested (due to how undeterminable.Tree was constructed), unwrap it
            if exprs and isinstance(exprs[0], list):
                exprs = exprs[0]
            
            try:
                results = core.eval_program(exprs)
                if not results: continue
                
                for s_idx, (env, val) in enumerate(results):
                    solution_found = True
                    if use_debug: print(f"--- Path {p_idx}, Solution {s_idx} ---")
                    
                    # Output result
                    if val is not None:
                        print(f"Result: {val}")
                    
                    # Output resolved free variables
                    resolved = {}
                    for name in env.bindings:
                        if name in core.prelude_bindings: continue
                        vid = hash(("free", name, 0))
                        if vid in env.subs:
                            resolved[name] = env.resolve(env.subs[vid])
                    
                    for name, v in resolved.items():
                        print(f"{name} = {v}")
            except Exception as e:
                if use_debug: print(f"Path {p_idx} failed: {e}")

        if not solution_found:
            print("No solutions found.")
        
        if use_debug:
            print(f"Eval time: {int((time.time() - eval_start)*1000)}ms")

if __name__ == "__main__":
    main()