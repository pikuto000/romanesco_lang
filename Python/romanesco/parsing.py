from typing import List, Optional, TypeVar, Generic, Dict
from dataclasses import dataclass
from romanesco.lexing import Token, Op, Ident, Keyword, Number, Delim, WS
from romanesco import undeterminable

# ======================================
# AST & Macro Registry
# ======================================

class Expr:
    @property
    def result_arity(self) -> int: return 0

@dataclass
class Num(Expr):
    value: str
    @property
    def result_arity(self) -> int: return 0

@dataclass
class Var(Expr):
    name: str
    @property
    def result_arity(self) -> int:
        return get_global_arity(self.name)

@dataclass
class Call(Expr):
    f: Expr
    args: List[Expr]
    @property
    def result_arity(self) -> int:
        if isinstance(self.f, Var) and self.f.name == "lambda":
            return 0 if len(self.args) >= 2 else 2 - len(self.args)
        return max(0, self.f.result_arity - len(self.args))

@dataclass
class Block(Expr):
    exprs: List[Expr]
    @property
    def result_arity(self) -> int: return 0

@dataclass
class MacroDef(Expr):
    name: str
    patterns: List['Pattern']
    body: Expr
    @property
    def result_arity(self) -> int: return 0

@dataclass
class Pattern: pass
@dataclass
class VarPattern(Pattern): name: str
@dataclass
class WordPattern(Pattern): word: str
@dataclass
class NumPattern(Pattern): value: str

# Global State
_global_arities: Dict[str, int] = {
    "+": 2, "-": 2, "*": 2, "/": 2,
    "==": 2, "!=": 2, "<": 2, ">": 2, "<=": 2, ">=": 2,
    "and": 2, "or": 2, "=": 2, "seq": 2,
    "syntax": 2, "lambda": 2,
    "true": 0, "false": 0
}

# Registered Mixfix Macros
_registered_macros: List[MacroDef] = []

def get_global_arity(name: str) -> int:
    return _global_arities.get(name, 0)

def set_global_arity(name: str, arity: int):
    _global_arities[name] = arity

def register_macro(m: MacroDef):
    _registered_macros.append(m)

# Pretty Printer
def show_expr(expr: Expr) -> str:
    if isinstance(expr, Num): return expr.value
    if isinstance(expr, Var): return expr.name
    if isinstance(expr, Call):
        args_str = " ".join(map(show_expr, expr.args))
        return f"({show_expr(expr.f)} {args_str})"
    if isinstance(expr, Block):
        exprs_str = " ".join(map(show_expr, expr.exprs))
        return f"{{ {exprs_str} }}"
    if isinstance(expr, MacroDef):
        pats = " ".join(map(show_pattern, expr.patterns))
        return f"syntax {expr.name} {pats} = ..."
    return str(expr)

def show_pattern(p: Pattern) -> str:
    if isinstance(p, VarPattern): return f"[{p.name}]"
    if isinstance(p, WordPattern): return p.word
    if isinstance(p, NumPattern): return p.value
    return str(p)

# ======================================
# Parser
# ======================================

T = TypeVar('T')
@dataclass
class ParseResult(Generic[T]):
    value: T
    remaining: List[Token]

def parse_all(token_tree: undeterminable.Tree[Token]) -> undeterminable.Tree[List[Expr]]:
    paths = token_tree.flatten_paths()
    if not paths: return undeterminable.DeadEnd()
    results = []
    for tokens in paths:
        filtered = [t for t in tokens if not isinstance(t, WS)]
        
        # Reset local state for this path
        global _global_arities, _registered_macros
        old_arities = _global_arities.copy()
        old_macros = list(_registered_macros)
        
        exprs = []
        current = filtered
        while current:
            # Try to parse an expression
            res = parse_expr(current)
            if res:
                expr = res.value
                exprs.append(expr)
                current = res.remaining
                
                # Update arity if it's an assignment
                if isinstance(expr, Call) and isinstance(expr.f, Var) and expr.f.name == "=":
                    if len(expr.args) >= 2 and isinstance(expr.args[0], Var):
                        def get_lambda_arity(e: Expr) -> int:
                            if isinstance(e, Call) and isinstance(e.f, Var) and e.f.name == "lambda":
                                return 1 + get_lambda_arity(e.args[1])
                            return 0
                        set_global_arity(expr.args[0].name, get_lambda_arity(expr.args[1]))
                # If it's a macro definition, register it
                if isinstance(expr, MacroDef):
                    register_macro(expr)
            else: break
        
        if exprs: results.append(exprs)
        _global_arities = old_arities
        _registered_macros = old_macros
            
    if not results: return undeterminable.DeadEnd()
    return undeterminable.fork([undeterminable.leaf(r) for r in results])

def parse_expr(tokens: List[Token]) -> Optional[ParseResult[Expr]]:
    if not tokens: return None
    
    # --- 1. Try Mixfix Macros First ---
    for macro in _registered_macros:
        res = match_macro(tokens, macro)
        if res: return res
        
    # --- 2. Macro Definition (syntax) ---
    if tokens[0].lexeme == "syntax":
        return parse_macro_def(tokens)
    
    # --- 3. Default Prefix Application ---
    res_primary = parse_primary(tokens)
    if not res_primary: return None
    
    f = res_primary.value
    rest = res_primary.remaining
    
    needed = f.result_arity
    args = []
    
    while len(args) < needed and rest:
        if isinstance(rest[0], Delim) and rest[0].lexeme in [")", "}", "]"]:
            break
        if isinstance(f, Var) and f.name == "lambda" and len(args) == 0:
            arg_res = parse_primary(rest)
        else:
            arg_res = parse_expr(rest)
            
        if arg_res:
            args.append(arg_res.value)
            rest = arg_res.remaining
        else: break
            
    if args: return ParseResult(Call(f, args), rest)
    return ParseResult(f, rest)

def match_macro(tokens: List[Token], macro: MacroDef) -> Optional[ParseResult[Expr]]:
    """マクロのパターンにトークン列が一致するか試行する"""
    current = tokens
    bindings: Dict[str, Expr] = {}
    
    for p in macro.patterns:
        if not current: return None
        
        if isinstance(p, WordPattern):
            if current[0].lexeme == p.word:
                current = current[1:]
            else: return None
        elif isinstance(p, NumPattern):
            if isinstance(current[0], Number) and current[0].lexeme == p.value:
                current = current[1:]
            else: return None
        elif isinstance(p, VarPattern):
            # 穴を埋めるために式をパース
            res = parse_expr(current)
            if res:
                bindings[p.name] = res.value
                current = res.remaining
            else: return None
            
    # マッチ成功！body内の変数を置換して展開（簡易実装）
    expanded = expand_macro(macro.body, bindings)
    return ParseResult(expanded, current)

def expand_macro(body: Expr, bindings: Dict[str, Expr]) -> Expr:
    """マクロ本体のプレースホルダーを実際の式で置換する"""
    if isinstance(body, Var):
        return bindings.get(body.name, body)
    if isinstance(body, Call):
        return Call(expand_macro(body.f, bindings), [expand_macro(a, bindings) for a in body.args])
    if isinstance(body, Block):
        return Block([expand_macro(e, bindings) for e in body.exprs])
    return body

def parse_primary(tokens: List[Token]) -> Optional[ParseResult[Expr]]:
    if not tokens: return None
    t = tokens[0]
    if isinstance(t, Number): return ParseResult(Num(t.lexeme), tokens[1:])
    if isinstance(t, (Ident, Op, Keyword)): return ParseResult(Var(t.lexeme), tokens[1:])
    if isinstance(t, Delim) and t.lexeme == "(":
        # Parse the first expression inside (maybe a function)
        res = parse_expr(tokens[1:])
        if not res: return None
        
        current_expr = res.value
        current_tokens = res.remaining
        
        # Continue applying any following expressions within the same parenthesis
        while current_tokens and not (isinstance(current_tokens[0], Delim) and current_tokens[0].lexeme == ")"):
            arg_res = parse_expr(current_tokens)
            if arg_res:
                if isinstance(current_expr, Call):
                    current_expr.args.append(arg_res.value)
                else:
                    current_expr = Call(current_expr, [arg_res.value])
                current_tokens = arg_res.remaining
            else: break
            
        if current_tokens and current_tokens[0].lexeme == ")":
            # Parenthesized expression acts as a single saturated value (arity 0)
            # relative to the outer context, unless it's a function.
            # For pure Lisp style, it just returns the call.
            return ParseResult(current_expr, current_tokens[1:])
    if isinstance(t, Delim) and t.lexeme == "{":
        res_list = []
        current = tokens[1:]
        while current and not (isinstance(current[0], Delim) and current[0].lexeme == "}"):
            res = parse_expr(current)
            if res:
                res_list.append(res.value)
                current = res.remaining
            else: break
        if current and current[0].lexeme == "}":
            return ParseResult(Block(res_list), current[1:])
    return None

def parse_macro_def(tokens: List[Token]) -> Optional[ParseResult[Expr]]:
    # syntax name word1 [hole1] ... = body
    if len(tokens) < 2: return None
    name = tokens[1].lexeme
    rest = tokens[2:]
    
    patterns = []
    current = rest
    while current and not (isinstance(current[0], Op) and current[0].lexeme == "="):
        p_res = parse_pattern(current)
        if p_res:
            patterns.append(p_res.value)
            current = p_res.remaining
        else: break
        
    if current and isinstance(current[0], Op) and current[0].lexeme == "=":
        body_res = parse_expr(current[1:])
        if body_res:
            return ParseResult(MacroDef(name, patterns, body_res.value), body_res.remaining)
    return None

def parse_pattern(tokens: List[Token]) -> Optional[ParseResult[Pattern]]:
    if len(tokens) >= 3 and tokens[0].lexeme == "[" and tokens[2].lexeme == "]":
        return ParseResult(VarPattern(tokens[1].lexeme), tokens[3:])
    if isinstance(tokens[0], (Ident, Op, Keyword)):
        return ParseResult(WordPattern(tokens[0].lexeme), tokens[1:])
    if isinstance(tokens[0], Number):
        return ParseResult(NumPattern(tokens[0].lexeme), tokens[1:])
    return None