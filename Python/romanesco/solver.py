import z3
from typing import List, Set
from romanesco import undeterminable
from romanesco.lexing import Token, Delim, Ident, Number
from romanesco import parsing
from romanesco.parsing import Expr

# ======================================
# Solver (Z3 / Constraints)
# ======================================

delimiter_pairs = {
    "(": ")",
    "{": "}",
    "[": "]"
}

def check_delimiter_balance(tokens: List[Token]) -> bool:
    # 1. Nesting order check (Z3 can do this, but simple stack is efficient)
    if not check_nesting_order(tokens):
        return False

    # 2. Count check using Z3
    ctx = z3.Context()
    solver = z3.Solver(ctx=ctx)
    
    for open_char, close_char in delimiter_pairs.items():
        open_count = sum(1 for t in tokens if isinstance(t, Delim) and t.lexeme == open_char)
        close_count = sum(1 for t in tokens if isinstance(t, Delim) and t.lexeme == close_char)
        
        # Define symbolic constants
        open_var = z3.Int(f"open_{open_char}", ctx=ctx)
        close_var = z3.Int(f"close_{close_char}", ctx=ctx)
        
        # Add constraints
        solver.add(open_var == open_count)
        solver.add(close_var == close_count)
        solver.add(open_var == close_var)
        
    return solver.check() == z3.sat

def check_nesting_order(tokens: List[Token]) -> bool:
    stack: List[str] = []
    
    close_chars = set(delimiter_pairs.values())
    
    for t in tokens:
        if isinstance(t, Delim):
            d = t.lexeme
            if d in delimiter_pairs:
                # Open
                stack.append(d)
            elif d in close_chars:
                # Close
                if not stack:
                    return False
                open_char = stack.pop()
                if delimiter_pairs.get(open_char) != d:
                    return False
    
    return len(stack) == 0

def check_identifier_rules(tokens: List[Token]) -> bool:
    for t in tokens:
        if isinstance(t, Ident):
            if not t.lexeme: return False
            first = t.lexeme[0]
            if not (first.isalpha() or first == '_'):
                return False
        elif isinstance(t, Number):
            if not t.lexeme.isdigit():
                return False
    return True

def check_token_constraints(tokens: List[Token]) -> bool:
    return check_delimiter_balance(tokens) and check_identifier_rules(tokens)

def check_ast_constraints(exprs: List[Expr]) -> bool:
    # 1. Simple Scope Validation
    defined_vars = {"true", "false", "and", "or", "seq", "=", "+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=", "lambda"}
    
    def check_expr(expr: Expr, local_vars: Set[str]) -> bool:
        if isinstance(expr, parsing.Num):
            return True
        elif isinstance(expr, parsing.Var):
            # In logic programming context, unknown variables are allowed
            # as they become Logic Variables at runtime.
            return True
        elif isinstance(expr, parsing.Call):
            # '=' in expression
            if isinstance(expr.f, parsing.Var) and expr.f.name == "=":
                def collect_vars(e: Expr):
                    if isinstance(e, parsing.Var): local_vars.add(e.name)
                    elif isinstance(e, parsing.Call):
                        collect_vars(e.f)
                        for a in e.args: collect_vars(a)
                for arg in expr.args: collect_vars(arg)
                return True
            # 'lambda' in expression
            if isinstance(expr.f, parsing.Var) and expr.f.name == "lambda" and len(expr.args) == 2:
                param = expr.args[0]
                if isinstance(param, parsing.Var):
                    new_locals = local_vars.copy()
                    new_locals.add(param.name)
                    return check_expr(expr.args[1], new_locals)
            
            return check_expr(expr.f, local_vars) and all(check_expr(arg, local_vars) for arg in expr.args)
        elif isinstance(expr, parsing.Block):
            current_locals = local_vars.copy()
            for e in expr.exprs:
                if not check_expr(e, current_locals): return False
                # If it's a top-level assignment inside block, add to locals
                if isinstance(e, parsing.Call) and isinstance(e.f, parsing.Var) and e.f.name == "=":
                    if e.args and isinstance(e.args[0], parsing.Var):
                        current_locals.add(e.args[0].name)
            return True
        return False

    # Top-level pass to collect definitions
    for expr in exprs:
        if isinstance(expr, parsing.Call) and isinstance(expr.f, parsing.Var) and expr.f.name == "=":
            if expr.args and isinstance(expr.args[0], parsing.Var):
                defined_vars.add(expr.args[0].name)
            
    # Second pass to check usages
    for expr in exprs:
        if not check_expr(expr, set()):
            return False
            
    return True


def prune_token_tree(token_tree: undeterminable.Tree[Token]) -> undeterminable.Tree[Token]:
    paths = token_tree.flatten_paths()
    
    valid_paths = [p for p in paths if check_token_constraints(p)]
    
    if not valid_paths:
        return undeterminable.DeadEnd()
    
    return rebuild_tree(valid_paths)

def rebuild_tree(paths: List[List[Token]]) -> undeterminable.Tree[Token]:
    if not paths:
        return undeterminable.DeadEnd()
    
    branches = [build_path(p) for p in paths]
    return undeterminable.fork(branches)

def build_path(tokens: List[Token]) -> undeterminable.Tree[Token]:
    if not tokens:
        return undeterminable.Node([], [])
    
    return undeterminable.single(tokens[0], build_path(tokens[1:]))
