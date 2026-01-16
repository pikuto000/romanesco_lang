from typing import List, Dict, Optional, Tuple, Any, cast
from dataclasses import dataclass
import time
from romanesco import parsing
from romanesco.parsing import Expr as ASTExpr

# Global debug flag
DEBUG_EVAL = False

def log(msg: str):
    if DEBUG_EVAL:
        print(f"[EVAL] {msg}")

# ======================================
# AST & Values
# ======================================

class Expr: pass
@dataclass
class Atom(Expr):
    name: str
    def __repr__(self): return f"Atom({self.name})"
@dataclass
class Apply(Expr):
    f: Expr; arg: Expr
    def __repr__(self): return f"Apply({self.f}, {self.arg})"

def show_core_expr(expr: Expr) -> str:
    if isinstance(expr, Atom): return expr.name
    elif isinstance(expr, Apply): return f"({show_core_expr(expr.f)} {show_core_expr(expr.arg)})"
    return str(expr)

class Value: pass
@dataclass
class NumVal(Value):
    n: int
    def __str__(self): return str(self.n)
@dataclass
class BoolVal(Value):
    b: bool
    def __str__(self): return str(self.b).lower()
@dataclass
class PairVal(Value):
    left: Value; right: Value
    def __str__(self): return f"({self.left}, {self.right})"
@dataclass
class Closure(Value):
    param: str; body: Expr; env: 'Env'
    def __str__(self): return f"<lambda {self.param}>"
    def __hash__(self): return hash((self.param, id(self.body)))
    def __eq__(self, other: Any):
        return isinstance(other, Closure) and self.param == other.param and self.body == other.body
@dataclass
class PrimOp(Value):
    name: str
    def __str__(self): return f"<{self.name}>"
    def __hash__(self): return hash(self.name)
@dataclass
class LogicVar(Value):
    vid: int; name: str
    def __str__(self): return self.name
    def __repr__(self): return f"LogicVar({self.name}, {hex(self.vid)[-4:]})"
    def __hash__(self): return hash(self.vid)
    def __eq__(self, other: Any): return isinstance(other, LogicVar) and self.vid == other.vid
@dataclass
class AtomVal(Value):
    name: str
    def __str__(self): return self.name
    def __hash__(self): return hash(self.name)
    def __eq__(self, other: Any): return isinstance(other, AtomVal) and self.name == other.name
@dataclass
class PartialBinOp(Value):
    op: str; left_expr: Optional[Expr] = None; left_val: Optional[Value] = None
    def __hash__(self): return hash((self.op, id(self.left_expr), id(self.left_val)))

# --- Constants ---
prelude_bindings: Dict[str, Expr] = {
    "and": Atom("and"), "or": Atom("or"), "seq": Atom("seq"), "=": Atom("="),
    "+": Atom("+"), "-": Atom("-"), "*": Atom("*"), "/": Atom("/"),
    ">": Atom(">"), "<": Atom("<"), ">=": Atom(">="), "<=": Atom("<="),
    "==": Atom("=="), "!=": Atom("!="), "lambda": Atom("lambda"),
    "true": Atom("true"), "false": Atom("false")
}

class Env:
    def __init__(self, bindings: Dict[str, Expr], values: Optional[Dict[str, Value]] = None, subs: Optional[Dict[int, Value]] = None, nonce: int = 0):
        self.bindings = bindings
        self.values = values if values is not None else {}
        self.subs = subs if subs is not None else {}
        self.nonce = nonce

    def lookup_expr(self, name: str) -> Optional[Expr]: return self.bindings.get(name)
    def lookup_value(self, name: str) -> Optional[Value]: return self.values.get(name)

    def resolve(self, val: Value) -> Value:
        if isinstance(val, LogicVar) and val.vid in self.subs:
            return self.resolve(self.subs[val.vid])
        if isinstance(val, AtomVal):
            # Resolve name from cache
            cached = self.lookup_value(val.name)
            if cached is not None and cached != val: return self.resolve(cached)
            # Try to resolve name as a LogicVar from top-level
            vid = hash(("free", val.name, 0))
            if vid in self.subs: return self.resolve(self.subs[vid])
        if isinstance(val, PairVal):
            return PairVal(self.resolve(val.left), self.resolve(val.right))
        return val

    def bind_name(self, name: str, val: Value) -> 'Env':
        nv = self.values.copy(); nv[name] = val
        return Env(self.bindings, nv, self.subs.copy(), self.nonce)

    def bind_var(self, vid: int, val: Value) -> 'Env':
        ns = self.subs.copy(); ns[vid] = val
        return Env(self.bindings, self.values.copy(), ns, self.nonce)

    def with_nonce(self, nonce: int) -> 'Env':
        return Env(self.bindings, self.values.copy(), self.subs.copy(), nonce)

    def extend_all(self, new_bindings: Dict[str, Expr]) -> 'Env':
        nb = self.bindings.copy(); nb.update(new_bindings)
        return Env(nb, self.values.copy(), self.subs.copy(), self.nonce)

    def contains(self, name: str) -> bool: return name in self.bindings or name in self.values

    @staticmethod
    def initial() -> 'Env':
        return Env(prelude_bindings.copy(), nonce=hash("root"))

class Contradiction(Exception): pass
class MatchFailure(Exception): pass

# ======================================
# Functions
# ======================================

def eval_primitive(op: str, args: List[Value]) -> Value:
    if len(args) != 2: raise RuntimeError(f"Cannot apply {op} to {args}")
    a, b = args[0], args[1]
    if not (isinstance(a, NumVal) and isinstance(b, NumVal)):
        if op == "==": return BoolVal(values_equal(a, b))
        if op == "!=": return BoolVal(not values_equal(a, b))
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
    raise RuntimeError(f"Primitive {op} failed")

def translate_expr(expr: ASTExpr) -> Expr:
    if isinstance(expr, parsing.Num): return Atom(expr.value)
    elif isinstance(expr, parsing.Var): return Atom(expr.name)
    elif isinstance(expr, parsing.Call):
        if isinstance(expr.f, parsing.Var) and expr.f.name == "lambda" and len(expr.args) == 2:
            return Apply(Apply(Atom("lambda"), Atom(expr.args[0].name)), translate_expr(expr.args[1]))
        f = translate_expr(expr.f)
        for arg in expr.args: f = Apply(f, translate_expr(arg))
        return f
    elif isinstance(expr, parsing.Block): return translate_program_to_expr(expr.exprs)
    elif isinstance(expr, parsing.MacroDef): return translate_expr(expr.body)
    return Atom("null")

def translate_program_to_expr(exprs: List[ASTExpr]) -> Expr:
    if not exprs: return Atom("true")
    if len(exprs) == 1: return translate_expr(exprs[0])
    return Apply(Apply(Atom("seq"), translate_expr(exprs[0])), translate_program_to_expr(exprs[1:]))

def eval_expr(expr: Expr, env: Env) -> List[Tuple[Value, Env]]:
    try:
        if isinstance(expr, Atom): return eval_atom(expr.name, env)
        if isinstance(expr, Apply): return eval_apply(expr.f, expr.arg, env)
    except (Contradiction, MatchFailure): return []
    return []

def eval_atom(name: str, env: Env) -> List[Tuple[Value, Env]]:
    if name in prelude_bindings: return [(PrimOp(name), env)]
    val = env.lookup_value(name)
    if val is not None: return [(env.resolve(val), env)]
    expr = env.lookup_expr(name)
    if expr:
        if isinstance(expr, Atom) and expr.name == name:
            # Free logic variable
            vid = hash(("free", name, 0))
            return [(env.resolve(LogicVar(vid, name)), env)]
        return eval_expr(expr, env)
    try: return [(NumVal(int(name)), env)]
    except ValueError: pass
    if name == "true": return [(BoolVal(True), env)]
    if name == "false": return [(BoolVal(False), env)]
    # Implicit free variable
    vid = hash(("free", name, 0))
    return [(env.resolve(LogicVar(vid, name)), env)]

def eval_apply(f: Expr, arg: Expr, env: Env) -> List[Tuple[Value, Env]]:
    results = []
    for f_val, env1 in eval_expr(f, env):
        f_val = env1.resolve(f_val)
        if isinstance(f_val, PrimOp):
            op = f_val.name
            if op in ["and", "or", "seq", "="]: results.append((PartialBinOp(op, left_expr=arg), env1))
            elif op == "lambda":
                if isinstance(arg, Atom): results.append((PartialBinOp("lambda", left_val=LogicVar(hash((arg.name, env1.nonce)), arg.name)), env1))
                else: raise RuntimeError("Lambda param must be atom")
            else:
                for arg_val, env2 in eval_expr(arg, env1): results.append((PartialBinOp(op, left_val=env2.resolve(arg_val)), env2))
        elif isinstance(f_val, PartialBinOp):
            op = f_val.op
            if op == "and":
                for left_val, env2 in eval_expr(cast(Expr, f_val.left_expr), env1):
                    results.extend(eval_and(env2.resolve(left_val), arg, env2))
            elif op == "or":
                results.extend(eval_expr(cast(Expr, f_val.left_expr), env1))
                results.extend(eval_expr(arg, env1))
            elif op == "seq":
                for _, env2 in eval_expr(cast(Expr, f_val.left_expr), env1):
                    results.extend(eval_expr(arg, env2))
            elif op == "=":
                for left_val, env2 in eval_expr(cast(Expr, f_val.left_expr), env1):
                    results.extend(unify(env2.resolve(left_val), arg, env2))
            elif op == "lambda":
                lv = cast(LogicVar, f_val.left_val)
                results.append((Closure(lv.name, arg, env1), env1))
            else:
                for right_val, env2 in eval_expr(arg, env1):
                    results.extend([(eval_primitive(op, [cast(Value, f_val.left_val), env2.resolve(right_val)]), env2)])
        elif isinstance(f_val, Closure):
            for arg_val, env2 in eval_expr(arg, env1):
                new_nonce = hash((f_val, env2.nonce, time.time_ns()))
                body_env = f_val.env.with_nonce(new_nonce).bind_name(f_val.param, env2.resolve(arg_val))
                for res_val, _ in eval_expr(f_val.body, body_env):
                    results.append((env2.resolve(res_val), env2))
    return results

def eval_and(left_val: Value, right_expr: Expr, env: Env) -> List[Tuple[Value, Env]]:
    if isinstance(left_val, BoolVal):
        if not left_val.b: return [(BoolVal(False), env)]
        return eval_expr(right_expr, env)
    res = []
    for right_val, env2 in eval_expr(right_expr, env):
        res.append((PairVal(left_val, env2.resolve(right_val)), env2))
    return res

def unify(v1: Value, expr2: Expr, env: Env) -> List[Tuple[Value, Env]]:
    results = []
    for v2, env2 in eval_expr(expr2, env):
        val1 = env2.resolve(v1); val2 = env2.resolve(v2)
        try:
            new_env = _unify_values(val1, val2, env2)
            results.append((new_env.resolve(val1), new_env))
        except Contradiction: continue
    return results

def _unify_values(v1: Value, v2: Value, env: Env) -> Env:
    v1 = env.resolve(v1); v2 = env.resolve(v2)
    if v1 == v2: return env
    if isinstance(v1, LogicVar): return env.bind_var(v1.vid, v2)
    if isinstance(v2, LogicVar): return env.bind_var(v2.vid, v1)
    if isinstance(v1, AtomVal): return env.bind_name(v1.name, v2)
    if isinstance(v2, AtomVal): return env.bind_name(v2.name, v1)
    if isinstance(v1, PairVal) and isinstance(v2, PairVal):
        env = _unify_values(v1.left, v2.left, env)
        return _unify_values(v1.right, v2.right, env)
    if values_equal(v1, v2): return env
    raise Contradiction(f"Cannot unify {v1} and {v2}")

def values_equal(a: Value, b: Value) -> bool:
    if isinstance(a, NumVal) and isinstance(b, NumVal): return a.n == b.n
    if isinstance(a, BoolVal) and isinstance(b, BoolVal): return a.b == b.b
    if isinstance(a, LogicVar) and isinstance(b, LogicVar): return a.vid == b.vid
    if isinstance(a, AtomVal) and isinstance(b, AtomVal): return a.name == b.name
    if isinstance(a, PairVal) and isinstance(b, PairVal):
        return values_equal(a.left, b.left) and values_equal(a.right, b.right)
    if isinstance(a, Closure) and isinstance(b, Closure):
        return a.param == b.param and _exprs_equal(a.body, b.body)
    return False

def _exprs_equal(e1: Expr, e2: Expr) -> bool:
    if type(e1) != type(e2): return False
    if isinstance(e1, Atom): return e1.name == cast(Atom, e2).name
    if isinstance(e1, Apply): return _exprs_equal(e1.f, cast(Apply, e2).f) and _exprs_equal(e1.arg, cast(Apply, e2).arg)
    return False

def value_to_expr(val: Value) -> Expr:
    if isinstance(val, NumVal): return Atom(str(val.n))
    if isinstance(val, BoolVal): return Atom(str(val.b).lower())
    if isinstance(val, LogicVar): return Atom(val.name)
    if isinstance(val, AtomVal): return Atom(val.name)
    if isinstance(val, PrimOp): return Atom(val.name)
    if isinstance(val, PairVal): return Apply(Apply(Atom("and"), value_to_expr(val.left)), value_to_expr(val.right))
    if isinstance(val, Closure): return Apply(Apply(Atom("lambda"), Atom(val.param)), val.body)
    return Atom("null")

def eval_program(exprs: List[ASTExpr]) -> List[Tuple[Env, Optional[Value]]]:
    bindings = {}
    executable = []
    for expr in exprs:
        if isinstance(expr, parsing.Call) and isinstance(expr.f, parsing.Var) and expr.f.name == "=":
            if len(expr.args) >= 2 and isinstance(expr.args[0], parsing.Var):
                bindings[expr.args[0].name] = translate_expr(expr.args[1])
            else: executable.append(expr)
        else: executable.append(expr)
    
    env = Env.initial().extend_all(bindings)
    paths = [(None, env)]
    for expr in executable:
        new_paths = []
        ast = translate_expr(expr)
        for _, current_env in paths:
            results = eval_expr(ast, current_env)
            for val, env_after in results:
                new_paths.append((env_after.resolve(val), env_after))
        if new_paths: paths = new_paths
        else: return []
    return [(e, v) for v, e in paths]