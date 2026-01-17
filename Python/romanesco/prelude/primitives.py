from typing import List, Optional
from ..runtime.types import Value, IntVal, BoolVal, AtomVal, PairVal, Closure, LogicVar
from . import arithmetic
from ..z3_ops import bitvec

def eval_primitive(op: str, args: List[Value]) -> Optional[Value]:
    if op in ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!="]:
        return arithmetic.eval_arithmetic(op, args)
    if op in ["bv", "bvadd", "bvsub", "bvand", "bvor"]:
        return bitvec.eval_bitvec(op, args)
    if op == "and":
        # logic 'and' for pairs or values
        # In Romanesco, (and a b) can be used to create a Pair
        if len(args) == 2:
            return PairVal(args[0], args[1])
    if op == "or":
        # 'or' is handled as non-determinism in evaluator.py
        # but if it reaches here, we might just return None to indicate path-branching should have happened
        return None
    raise RuntimeError(f"Unknown primitive: {op}")

def values_equal(a: Value, b: Value) -> bool:
    if isinstance(a, IntVal) and isinstance(b, IntVal): 
        # Optional: should we compare width? 
        return a.val == b.val
    if isinstance(a, BoolVal) and isinstance(b, BoolVal): return a.b == b.b
    if isinstance(a, LogicVar) and isinstance(b, LogicVar): return a.vid == b.vid
    if isinstance(a, AtomVal) and isinstance(b, AtomVal): return a.name == b.name
    if isinstance(a, PairVal) and isinstance(b, PairVal):
        return values_equal(a.left, b.left) and values_equal(a.right, b.right)
    if isinstance(a, Closure) and isinstance(b, Closure):
        return a.param == b.param and a.body == b.body
    return False
