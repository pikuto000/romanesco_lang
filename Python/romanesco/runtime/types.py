from dataclasses import dataclass
from typing import Dict, Optional, Any, List
from ..syntax import ast

# ======================================
# Values & Environment
# ======================================

class Value: pass

@dataclass
class IntVal(Value):
    val: int
    width: Optional[int] = None # None means infinite precision integer (Z3 Int)
    
    def __str__(self):
        return str(self.val)
    
    def __hash__(self): return hash((self.val, self.width))
    def __eq__(self, other: Any):
        return isinstance(other, IntVal) and self.val == other.val and self.width == other.width

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
    param: str; body: ast.Expr; env: 'Env'
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
    op: str; left_val: Value; right_expr: Optional[ast.Expr] = None
    def __hash__(self): return hash((self.op, id(self.left_val)))

class Env:
    def __init__(self, bindings: Dict[str, ast.Expr], values: Optional[Dict[str, Value]] = None, subs: Optional[Dict[int, Value]] = None, widths: Optional[Dict[str, int]] = None, nonce: int = 0):
        self.bindings = bindings
        self.values = values if values is not None else {}
        self.subs = subs if subs is not None else {}
        self.widths = widths if widths is not None else {}
        self.nonce = nonce

    def lookup_expr(self, name: str) -> Optional[ast.Expr]: return self.bindings.get(name)
    def lookup_value(self, name: str) -> Optional[Value]: return self.values.get(name)
    def lookup_width(self, name: str) -> Optional[int]: return self.widths.get(name)

    def resolve(self, val: Value) -> Value:
        if isinstance(val, LogicVar) and val.vid in self.subs:
            return self.resolve(self.subs[val.vid])
        if isinstance(val, AtomVal):
            cached = self.lookup_value(val.name)
            if cached is not None and cached != val: return self.resolve(cached)
            vid = hash(("free", val.name, 0))
            if vid in self.subs: return self.resolve(self.subs[vid])
        if isinstance(val, PairVal):
            return PairVal(self.resolve(val.left), self.resolve(val.right))
        return val

    def bind_name(self, name: str, val: Value) -> 'Env':
        nv = self.values.copy(); nv[name] = val
        return Env(self.bindings, nv, self.subs.copy(), self.widths.copy(), self.nonce)

    def bind_var(self, vid: int, val: Value) -> 'Env':
        ns = self.subs.copy(); ns[vid] = val
        return Env(self.bindings, self.values.copy(), ns, self.widths.copy(), self.nonce)

    def bind_width(self, name: str, width: int) -> 'Env':
        nw = self.widths.copy(); nw[name] = width
        return Env(self.bindings, self.values.copy(), self.subs.copy(), nw, self.nonce)

    def with_nonce(self, nonce: int) -> 'Env':
        return Env(self.bindings, self.values.copy(), self.subs.copy(), self.widths.copy(), nonce)

    def extend_all(self, new_bindings: Dict[str, ast.Expr]) -> 'Env':
        nb = self.bindings.copy(); nb.update(new_bindings)
        return Env(nb, self.values.copy(), self.subs.copy(), self.widths.copy(), self.nonce)

    def contains(self, name: str) -> bool: return name in self.bindings or name in self.values

    @staticmethod
    def initial(bindings: Dict[str, ast.Expr]) -> 'Env':
        return Env(bindings.copy(), nonce=hash("root"))

class Contradiction(Exception): pass
class MatchFailure(Exception): pass