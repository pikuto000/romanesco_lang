from typing import List
from ..runtime.types import Value, IntVal, BoolVal, AtomVal, PairVal, Closure, LogicVar
from . import arithmetic
from ..z3_ops import bitvec

def eval_primitive(op: str, args: List[Value]) -> Value:
    if op in ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!="]:
        return arithmetic.eval_arithmetic(op, args)
    if op in ["bv", "bvadd", "bvsub", "bvand", "bvor"]:
        return bitvec.eval_bitvec(op, args)
    raise RuntimeError(f"Unknown primitive: {op}")

def values_equal(a: Value, b: Value) -> bool:
    if isinstance(a, IntVal) and isinstance(b, IntVal): 
        return a.val == b.val and a.width == b.width
    if isinstance(a, BoolVal) and isinstance(b, BoolVal): return a.b == b.b
    if isinstance(a, LogicVar) and isinstance(b, LogicVar): return a.vid == b.vid
    if isinstance(a, AtomVal) and isinstance(b, AtomVal): return a.name == b.name
    if isinstance(a, PairVal) and isinstance(b, PairVal):
        return values_equal(a.left, b.left) and values_equal(a.right, b.right)
    if isinstance(a, Closure) and isinstance(b, Closure):
        return a.param == b.param
    return False