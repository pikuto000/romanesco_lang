from ..syntax.ast import Atom
from . import arithmetic, logic, primitives
from ..z3_ops import bitvec

# Merge all bindings
initial_bindings = {}
initial_bindings.update(logic.bindings)

# Register arithmetic ops
for op in ["+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!="]:
    initial_bindings[op] = Atom(op)


eval_primitive = primitives.eval_primitive
values_equal = primitives.values_equal
