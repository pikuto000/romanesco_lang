from typing import List, Dict, Optional, Tuple, Set, Any, Union
from dataclasses import dataclass
from romanesco import parsing
from romanesco.parsing import Expr as ASTExpr

# ======================================
# Core Expr
# ======================================

class Expr: pass

@dataclass
class Atom(Expr):
    name: str
    def __repr__(self): return f"Atom({self.name})"

@dataclass
class Apply(Expr):
    f: Expr
    arg: Expr
    def __repr__(self): return f"Apply({self.f}, {self.arg})"

def show_core_expr(expr: Expr) -> str:
    if isinstance(expr, Atom): return expr.name
    elif isinstance(expr, Apply): return f"({show_core_expr(expr.f)} {show_core_expr(expr.arg)})"
    return str(expr)

# ======================================
# Value & Env
# ======================================

class Value: pass

@dataclass
class NumVal(Value):
    n: int
    def __repr__(self): return str(self.n)
    def __str__(self): return str(self.n)

@dataclass
class BoolVal(Value):
    b: bool
    def __repr__(self): return str(self.b).lower()
    def __str__(self): return str(self.b).lower()

@dataclass
class PairVal(Value):
    left: Value
    right: Value
    def __repr__(self): return f"({self.left}, {self.right})"
    def __str__(self): return f"({self.left}, {self.right})"

@dataclass
class Closure(Value):
    param: str
    body: Expr
    env: 'Env'
    def __str__(self): return f"<lambda {self.param}>"

@dataclass
class PrimOp(Value):
    name: str
    def __str__(self): return f"<{self.name}>"

@dataclass
class AtomVal(Value):
    name: str
    def __str__(self): return self.name

@dataclass
class PartialBinOp(Value):
    op: str
    left_expr: Optional[Expr] = None
    left_val: Optional[Value] = None

class Env:
    def __init__(self, bindings: Dict[str, Expr]):
        self.bindings = bindings
    def lookup(self, name: str) -> Optional[Expr]: return self.bindings.get(name)
    def extend(self, name: str, value: Expr) -> 'Env':
        nb = self.bindings.copy(); nb[name] = value; return Env(nb)
    def extend_all(self, new_bindings: Dict[str, Expr]) -> 'Env':
        nb = self.bindings.copy(); nb.update(new_bindings); return Env(nb)
    def contains(self, name: str) -> bool: return name in self.bindings
    @staticmethod
    def empty() -> 'Env': return Env({})
    @staticmethod
    def initial() -> 'Env': return Env(prelude_bindings.copy())

class Contradiction(Exception): pass
class MatchFailure(Exception): pass

# ======================================
# Prelude
# ======================================

prelude_bindings: Dict[str, Expr] = {
    "and": Atom("and"), "or": Atom("or"), "seq": Atom("seq"), "=": Atom("="),
    "+": Atom("+"), "-": Atom("-"), "*": Atom("*"), "/": Atom("/"),
    ">": Atom(">"), "<": Atom("<"), ">=": Atom(">="), "<=": Atom("<="),
    "==": Atom("=="), "!=": Atom("!="), "lambda": Atom("lambda"),
    "true": Atom("true"), "false": Atom("false")
}

def eval_primitive(op: str, args: List[Value]) -> Value:
    if len(args) != 2: raise RuntimeError(f"Cannot apply {op} to {args}")
    a, b = args[0], args[1]
    if op in ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!="]:
        if not (isinstance(a, NumVal) and isinstance(b, NumVal)):
            if op == "==": return BoolVal(a == b)
            if op == "!=": return BoolVal(a != b)
            raise Contradiction(f"Type error: {op} expected numbers, got {a}, {b}")
    if op == "+": return NumVal(a.n + b.n)
    if op == "-": return NumVal(a.n - b.n)
    if op == "*": return NumVal(a.n * b.n)
    if op == "/": return NumVal(a.n // b.n)
    if op == ">": return BoolVal(a.n > b.n)
    if op == "<": return BoolVal(a.n < b.n)
    if op == ">=": return BoolVal(a.n >= b.n)
    if op == "<=": return BoolVal(a.n <= b.n)
    if op == "==": return BoolVal(a.n == b.n)
    if op == "!=": return BoolVal(a.n != b.n)
    raise RuntimeError(f"Cannot apply {op} to {args}")

# ======================================
# Translator
# ======================================

def translate_expr(expr: ASTExpr) -> Expr:
    if isinstance(expr, parsing.Num): return Atom(expr.value)
    elif isinstance(expr, parsing.Var): return Atom(expr.name)
    elif isinstance(expr, parsing.Call):
        if isinstance(expr.f, parsing.Var) and expr.f.name == "lambda" and len(expr.args) == 2:
            param = expr.args[0]
            if not isinstance(param, parsing.Var): raise ValueError("Lambda param must be identifier")
            return Apply(Apply(Atom("lambda"), Atom(param.name)), translate_expr(expr.args[1]))
        f = translate_expr(expr.f)
        for arg in expr.args: f = Apply(f, translate_expr(arg))
        return f
    elif isinstance(expr, parsing.Block): return translate_program_to_expr(expr.exprs)
    elif isinstance(expr, parsing.MacroDef): return translate_expr(expr.body)
    else: raise ValueError(f"Unknown expr type: {type(expr)}")

def translate_program_to_expr(exprs: List[ASTExpr]) -> Expr:
    if not exprs: return Atom("true")
    if len(exprs) == 1: return translate_expr(exprs[0])
    return Apply(Apply(Atom("seq"), translate_expr(exprs[0])), translate_program_to_expr(exprs[1:]))

def translate_program(exprs: List[ASTExpr]) -> Tuple[Dict[str, Expr], Optional[Expr]]:
    bindings = {}
    last_expr = None
    for expr in exprs:
        if isinstance(expr, parsing.Call) and isinstance(expr.f, parsing.Var) and expr.f.name == "=":
            if len(expr.args) >= 2 and isinstance(expr.args[0], parsing.Var):
                bindings[expr.args[0].name] = translate_expr(expr.args[1])
            else: last_expr = translate_expr(expr)
        elif isinstance(expr, parsing.MacroDef): bindings[expr.name] = translate_expr(expr.body)
        else: last_expr = translate_expr(expr)
    return bindings, last_expr

# ======================================
# Evaluator (Non-deterministic Search)
# ======================================

def eval_expr(expr: Expr, env: Env) -> List[Tuple[Value, Env]]:
    """Returns a list of all possible (ResultValue, UpdatedEnv) pairs."""
    try:
        if isinstance(expr, Atom): return eval_atom(expr.name, env)
        elif isinstance(expr, Apply): return eval_apply(expr.f, expr.arg, env)
    except (Contradiction, MatchFailure):
        return [] # No solutions in this branch
    raise ValueError(f"Unknown core expr: {expr}")

def eval_atom(name: str, env: Env) -> List[Tuple[Value, Env]]:
    looked_up = env.lookup(name)
    if looked_up:
        if isinstance(looked_up, Atom) and looked_up.name == name:
            val = PrimOp(name) if name in prelude_bindings else AtomVal(name)
            return [(val, env)]
        return eval_expr(looked_up, env)
    
    if name in prelude_bindings: return [(PrimOp(name), env)]
    try: return [(NumVal(int(name)), env)]
    except ValueError: pass
    if name == "true": return [(BoolVal(True), env)]
    if name == "false": return [(BoolVal(False), env)]
    return [(AtomVal(name), env)]

def eval_apply(f: Expr, arg: Expr, env: Env) -> List[Tuple[Value, Env]]:
    results = []
    # 1. Evaluate function part
    for f_val, env1 in eval_expr(f, env):
        if isinstance(f_val, PrimOp):
            op = f_val.name
            if op in ["and", "or", "seq", "="]:
                results.append((PartialBinOp(op, left_expr=arg), env1))
            elif op == "lambda":
                if isinstance(arg, Atom): 
                    results.append((PartialBinOp("lambda", left_val=AtomVal(arg.name)), env1))
                else: raise RuntimeError("Lambda param must be atom")
            else:
                # Eager evaluation for others
                for arg_val, env2 in eval_expr(arg, env1):
                    results.append((PartialBinOp(op, left_val=arg_val), env2))
                    
        elif isinstance(f_val, PartialBinOp):
            op = f_val.op
            if op == "and":
                # Flatmap over left, then right
                for left_val, env2 in eval_expr(f_val.left_expr, env1):
                    for res_val, env3 in eval_and(left_val, arg, env2):
                        results.append((res_val, env3))
            elif op == "or":
                # Combine results from both branches (Search!)
                # Branch 1
                branch1 = eval_expr(f_val.left_expr, env1)
                results.extend(branch1)
                # Branch 2 (using original env from this branch)
                branch2 = eval_expr(arg, env1)
                results.extend(branch2)
            elif op == "seq":
                for _, env2 in eval_expr(f_val.left_expr, env1):
                    results.extend(eval_expr(arg, env2))
            elif op == "=":
                for left_val, env2 in eval_expr(f_val.left_expr, env1):
                    results.extend(eval_eq(left_val, arg, env2))
            elif op == "lambda":
                if isinstance(f_val.left_val, AtomVal):
                    results.append((Closure(f_val.left_val.name, arg, env1), env1))
            else:
                # Arithmetic / Comparison
                for right_val, env2 in eval_expr(arg, env1):
                    results.append((eval_primitive(op, [f_val.left_val, right_val]), env2))
                    
        elif isinstance(f_val, Closure):
            for arg_val, env2 in eval_expr(arg, env1):
                # Expand closure body in captured env + parameter binding
                results.extend(eval_expr(f_val.body, f_val.env.extend(f_val.param, value_to_expr(arg_val))))
                
    return results

def eval_and(left_val: Value, right_expr: Expr, env: Env) -> List[Tuple[Value, Env]]:
    if isinstance(left_val, BoolVal):
        if not left_val.b: return [(BoolVal(False), env)]
        return eval_expr(right_expr, env)
    
    # Pair creation
    results = []
    for right_val, env2 in eval_expr(right_expr, env):
        results.append((PairVal(left_val, right_val), env2))
    return results

def eval_eq(left_val: Value, right_expr: Expr, env: Env) -> List[Tuple[Value, Env]]:
    try:
        if isinstance(right_expr, Atom):
            if not env.contains(right_expr.name):
                return [(left_val, env.extend(right_expr.name, value_to_expr(left_val)))]
            # Match
            for looked_up, env2 in eval_atom(right_expr.name, env):
                if values_equal(left_val, looked_up): return [(left_val, env2)]
            return []
            
        if isinstance(left_val, PairVal) and isinstance(right_expr, Apply) and isinstance(right_expr.f, Apply):
            p1 = right_expr.f.arg
            p2 = right_expr.arg
            res = []
            for _, env2 in eval_eq(left_val.left, p1, env):
                for _, env3 in eval_eq(left_val.right, p2, env2):
                    res.append((left_val, env3))
            return res
                
        # General case
        res = []
        for rhs_val, env2 in eval_expr(right_expr, env):
            if values_equal(left_val, rhs_val): res.append((left_val, env2))
        return res
    except (Contradiction, MatchFailure):
        return []

def values_equal(a: Value, b: Value) -> bool:
    if isinstance(a, NumVal) and isinstance(b, NumVal): return a.n == b.n
    if isinstance(a, BoolVal) and isinstance(b, BoolVal): return a.b == b.b
    if isinstance(a, AtomVal) and isinstance(b, AtomVal): return a.name == b.name
    if isinstance(a, PairVal) and isinstance(b, PairVal):
        return values_equal(a.left, b.left) and values_equal(a.right, b.right)
    return False

def value_to_expr(val: Value) -> Expr:
    if isinstance(val, NumVal): return Atom(str(val.n))
    if isinstance(val, BoolVal): return Atom(str(val.b).lower())
    if isinstance(val, AtomVal): return Atom(val.name)
    if isinstance(val, PrimOp): return Atom(val.name)
    if isinstance(val, PairVal): return Apply(Apply(Atom("and"), value_to_expr(val.left)), value_to_expr(val.right))
    if isinstance(val, Closure): return Apply(Apply(Atom("lambda"), Atom(val.param)), val.body)
    return Atom("null")

def eval_program(exprs: List[ASTExpr]) -> List[Tuple[Env, Optional[Value]]]:
    bindings, last_expr = translate_program(exprs)
    
    # Initialize with one empty solution
    current_solutions: List[Env] = [Env.initial().extend_all(bindings)]
    
    # 1. Evaluate top-level bindings (simplify: assume deterministic for now or flatmap)
    for name, expr in bindings.items():
        new_solutions = []
        for env in current_solutions:
            results = eval_expr(expr, env)
            if not results:
                # If a mandatory binding fails, that solution is gone
                continue
            for val, env2 in results:
                new_solutions.append(env2.extend(name, value_to_expr(val)))
        current_solutions = new_solutions
            
    final_results = []
    if last_expr:
        for env in current_solutions:
            results = eval_expr(last_expr, env)
            if not results:
                final_results.append((env, BoolVal(False)))
            for val, env2 in results:
                final_results.append((env2, val))
    else:
        for env in current_solutions:
            final_results.append((env, None))
            
    return final_results
