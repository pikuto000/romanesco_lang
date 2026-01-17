from typing import List, Optional
from ..runtime.types import Value, IntVal, BoolVal, Contradiction
import z3

def eval_arithmetic(op: str, args: List[Value]) -> Value:
    if len(args) != 2: raise RuntimeError(f"Cannot apply {op} to {args}")
    a, b = args[0], args[1]
    
    if not (isinstance(a, IntVal) and isinstance(b, IntVal)):
        # BoolVal handling...
        return BoolVal(False) 
        
    wa = a.width; wb = b.width
    
    # If any operand has width, we perform bitvec operation
    if wa is not None or wb is not None:
        # Default to a safe width if one is missing
        wa = wa if wa is not None else max(1, a.val.bit_length())
        wb = wb if wb is not None else max(1, b.val.bit_length())
        
        # Calculate result width for safety
        res_width = max(wa, wb)
        if op == "+": res_width += 1
        elif op == "*": res_width = wa + wb
        
        return eval_bitvec_op(op, a.val, b.val, wa, wb, res_width)
    else:
        return eval_int_op(op, a.val, b.val)

def eval_bitvec_op(op: str, va: int, vb: int, wa: int, wb: int, res_w: int) -> Value:
    # Use Z3 to perform calculation in the resulting width
    za = z3.BitVecVal(va, res_w)
    zb = z3.BitVecVal(vb, res_w)
    
    if op == "+": return from_z3(za + zb, res_w)
    if op == "-": return from_z3(za - zb, res_w)
    if op == "*": return from_z3(za * zb, res_w)
    if op == "/": return from_z3(z3.UDiv(za, zb), res_w)
    
    # Comparisons return BoolVal (no width)
    if op == "==": return BoolVal(z3.is_true(z3.simplify(za == zb)))
    if op == "!=": return BoolVal(z3.is_false(z3.simplify(za == zb)))
    if op == ">": return BoolVal(z3.is_true(z3.simplify(z3.UGT(za, zb))))
    if op == "<": return BoolVal(z3.is_true(z3.simplify(z3.ULT(za, zb))))
    if op == ">=": return BoolVal(z3.is_true(z3.simplify(z3.UGE(za, zb))))
    if op == "<=": return BoolVal(z3.is_true(z3.simplify(z3.ULE(za, zb))))
    
    raise RuntimeError(f"Unknown bitvec op: {op}")

def eval_int_op(op: str, va: int, vb: int) -> Value:
    if op == "+": return IntVal(va + vb)
    if op == "-": return IntVal(va - vb)
    if op == "*": return IntVal(va * vb)
    if op == "/": return IntVal(va // vb)
    if op == ">": return BoolVal(va > vb)
    if op == "<": return BoolVal(va < vb)
    if op == ">=": return BoolVal(va >= vb)
    if op == "<=": return BoolVal(va <= vb)
    if op == "==": return BoolVal(va == vb)
    if op == "!=": return BoolVal(va != vb)
    raise RuntimeError(f"Unknown int op: {op}")

def from_z3(ref, width):
    simp = z3.simplify(ref)
    if isinstance(simp, z3.BitVecNumRef):
        return IntVal(simp.as_long(), width)
    raise RuntimeError(f"Z3 result not concrete: {simp}")
