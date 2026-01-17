from typing import List, Dict, Optional
from ..syntax import ast
from ..inference.solver import WidthInference
from ..lexing import Token, WS
from .. import undeterminable

def prune_token_tree(tree: undeterminable.Tree[Token]) -> undeterminable.Tree[Token]:
    """
    Use Z3 to eliminate tokenization paths that will never satisfy bit-width constraints.
    """
    paths = tree.flatten_paths()
    valid_paths = []
    
    # We need a way to quickly check if a token path is 'plausible'
    # For now, we'll try to parse and infer. If it fails Unsat, it's an invalid path.
    from ..parser.main import parse_all
    
    for tokens in paths:
        try:
            # Filter WS for parsing
            filtered = [t for t in tokens if not isinstance(t, WS)]
            ast_tree = parse_all(filtered)
            ast_paths = ast_tree.flatten_paths()
            
            is_plausible = False
            for exprs in ast_paths:
                # exprs is List[ast.Expr]
                inf = WidthInference()
                # If Z3 finds it SAT, this token path is plausible
                if inf.infer(exprs):
                    is_plausible = True
                    break
            
            if is_plausible:
                valid_paths.append(tokens)
        except:
            # If it doesn't even parse, it's not a valid path
            continue
            
    if not valid_paths:
        return undeterminable.DeadEnd()
        
    return _rebuild_tree(valid_paths)

def check_ast_constraints(exprs: List[ast.Expr]) -> bool:
    """
    Check if a given AST path satisfies bit-width constraints.
    """
    inf = WidthInference()
    res = inf.infer(exprs)
    return len(res) > 0

def _rebuild_tree(paths: List[List[Token]]) -> undeterminable.Tree[Token]:
    if not paths: return undeterminable.DeadEnd()
    def build_path(tokens: List[Token]) -> undeterminable.Tree[Token]:
        if not tokens: return undeterminable.Node([], [])
        return undeterminable.single(tokens[0], build_path(tokens[1:]))
    return undeterminable.fork([build_path(p) for p in paths])