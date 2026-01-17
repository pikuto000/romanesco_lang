import z3
import uuid
from typing import List, Dict, Optional, Tuple, Any
from ..syntax import ast

class WidthInference:
    def __init__(self, debug: bool = False):
        self.optimizer = z3.Optimize()
        self.w_vars: Dict[str, z3.ArithRef] = {}
        self.const_values: Dict[str, int] = {} # Variable to Constant Value
        self.debug = debug

    def get_w(self, name: str) -> z3.ArithRef:
        if name not in self.w_vars:
            w = z3.Int(f"w_{name}")
            self.optimizer.add(w >= 0)
            self.w_vars[name] = w
            self.optimizer.minimize(w)
        return self.w_vars[name]

    def infer(self, exprs: List[ast.Expr]) -> Dict[str, int]:
        # Reset for each run
        self.const_values = {}
        
        # 1. First pass: Unification of widths for consistency in hardware
        for expr in exprs:
            self._unify(expr)
            
        # 2. Second pass: Detailed analysis with constant propagation
        for expr in exprs:
            self._analyze(expr)
            
        if self.optimizer.check() == z3.sat:
            model = self.optimizer.model()
            return {name: model[w].as_long() for name, w in self.w_vars.items() if model[w] is not None}
        return {}

    def _unify(self, expr: ast.Expr):
        """Standard width unification logic."""
        if isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return
            op, args = struct
            if op in ["+", "-", "*", "/", "==", ">", "<", ">=", "<="]:
                vars = [self.get_w(arg.name) for arg in args if isinstance(arg, ast.Atom) and not self._is_num(arg.name)]
                for i in range(len(vars) - 1):
                    self.optimizer.add(vars[i] == vars[i+1])
            for arg in args: self._unify(arg)

    def _analyze(self, expr: ast.Expr) -> z3.ArithRef:
        """Analyze expression and return a Z3 expression for its bit-width."""
        if isinstance(expr, ast.Atom):
            if self._is_num(expr.name):
                val = int(expr.name)
                return z3.IntVal(max(1, val.bit_length()))
            return self.get_w(expr.name)
        
        elif isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return z3.IntVal(0)
            op, args = struct
            
            if op == "=":
                if len(args) == 2 and isinstance(args[0], ast.Atom):
                    target = args[0].name
                    
                    # Try to evaluate the source expression to a constant
                    val = self._try_eval(args[1])
                    if val is not None:
                        self.const_values[target] = val
                        w_val = z3.IntVal(max(1, val.bit_length()))
                    else:
                        w_val = self._analyze(args[1])
                    
                    w_tgt = self.get_w(target)
                    self.optimizer.add(w_tgt >= w_val)
                    return w_tgt
            
            if op == "+":
                # Value-Aware Optimization: Result width based on actual sum
                val = self._try_eval(expr)
                if val is not None: return z3.IntVal(max(1, val.bit_length()))
                
                # Nature optimization: x + 0 -> width of x
                if len(args) == 2:
                    v0 = self._try_eval(args[0])
                    v1 = self._try_eval(args[1])
                    if v1 == 0: return self._analyze(args[0])
                    if v0 == 0: return self._analyze(args[1])

                # Fallback: Safety growth
                w_args = [self._analyze(arg) for arg in args]
                return self._max_z3(w_args) + 1

            if op == "*":
                # Value-Aware Optimization
                val = self._try_eval(expr)
                if val is not None: return z3.IntVal(max(1, val.bit_length()))
                
                # Nature: x * 0 -> 1 bit, x * 1 -> width of x
                if len(args) == 2:
                    v0, v1 = self._try_eval(args[0]), self._try_eval(args[1])
                    if v0 == 0 or v1 == 0: return z3.IntVal(1)
                    if v0 == 1: return self._analyze(args[1])
                    if v1 == 1: return self._analyze(args[0])

                # Fallback: sum of widths
                w_args = [self._analyze(arg) for arg in args]
                return z3.Sum(w_args)

            if op == "or":
                # Non-determinism: width is the max of all branches
                w_args = [self._analyze(arg) for arg in args]
                return self._max_z3(w_args)

            # Default propagation
            w_args = [self._analyze(arg) for arg in args]
            return self._max_z3(w_args) if w_args else z3.IntVal(0)
            
        return z3.IntVal(0)

    def _try_eval(self, expr: ast.Expr) -> Optional[int]:
        """Attempt to evaluate an expression using known constants."""
        if isinstance(expr, ast.Atom):
            if self._is_num(expr.name): return int(expr.name)
            return self.const_values.get(expr.name)
        
        if isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return None
            op, args = struct
            
            vals = [self._try_eval(arg) for arg in args]
            if any(v is None for v in vals): return None
            
            if op == "+": return sum(vals)
            if op == "-": return vals[0] - vals[1] if len(vals) == 2 else None
            if op == "*":
                p = 1
                for v in vals: p *= v
                return p
            if op == "/": return vals[0] // vals[1] if len(vals) == 2 and vals[1] != 0 else None
            
        return None

    def _is_num(self, s: str) -> bool:
        try: int(s); return True
        except ValueError: return False

    def _max_z3(self, vars: List[Any]) -> Any:
        if not vars: return z3.IntVal(0)
        m = vars[0]
        for v in vars[1:]: m = z3.If(v > m, v, m)
        return m

    def _unroll_apply(self, expr: ast.Apply) -> Optional[Tuple[str, List[ast.Expr]]]:
        curr = expr
        args = []
        while isinstance(curr, ast.Apply):
            args.append(curr.arg)
            curr = curr.f
        if isinstance(curr, ast.Atom):
            return curr.name, list(reversed(args))
        return None