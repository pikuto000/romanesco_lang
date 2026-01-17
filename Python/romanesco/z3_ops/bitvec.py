from typing import List
from ..runtime.types import Value, IntVal, Contradiction
import z3

def eval_bitvec(op: str, args: List[Value]) -> Value:
    # Constructor: bv val width
    if op == "bv":
        if len(args) != 2: raise RuntimeError(f"bv takes 2 args (val, width), got {len(args)}")
        val, width = args[0], args[1]
        
        # val can be IntVal(unbound) or IntVal(bound). width must be IntVal.
        if not (isinstance(val, IntVal) and isinstance(width, IntVal)):
            raise Contradiction(f"Type error: bv expected numbers")
        
        # width.val is the bit width
        target_width = width.val
        
        z_val = z3.BitVecVal(val.val, target_width)
        return IntVal(z_val.as_long(), target_width)
        
    # Binary Ops (bvadd, etc.)
    if len(args) != 2: raise RuntimeError(f"Cannot apply {op} to {args}")
    a, b = args[0], args[1]
    
    if not (isinstance(a, IntVal) and isinstance(b, IntVal)):
        raise Contradiction(f"Type error: {op} expected BitVecs")
        
    width = None
    if a.width is not None and b.width is not None:
        if a.width != b.width: raise Contradiction(f"Width mismatch: {a.width} vs {b.width}")
        width = a.width
    elif a.width is not None: width = a.width
    elif b.width is not None: width = b.width
    else:
        raise Contradiction(f"Ambiguous width for {op}: both operands are unbound integers")
    
    z_a = z3.BitVecVal(a.val, width)
    z_b = z3.BitVecVal(b.val, width)
    
    res = None
    if op == "bvadd": res = z_a + z_b
    elif op == "bvsub": res = z_a - z_b
    elif op == "bvand": res = z_a & z_b
    elif op == "bvor": res = z_a | z_b
    else: raise RuntimeError(f"Unknown bitvec op: {op}")
        
    simple_res = z3.simplify(res)
    if not isinstance(simple_res, z3.BitVecNumRef):
        raise RuntimeError(f"Z3 result not concrete: {simple_res}")
        
    return IntVal(simple_res.as_long(), width)