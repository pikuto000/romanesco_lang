import z3
import uuid
from typing import List, Dict, Optional, Set, Tuple, Any
from ..syntax import ast

class WidthInference:
    def __init__(self, debug: bool = False):
        self.optimizer = z3.Optimize()
        self.var_widths: Dict[str, z3.ArithRef] = {}
        self.debug = debug

    def get_width_var(self, name: str) -> z3.ArithRef:
        if name not in self.var_widths:
            v = z3.Int(f"w_{name}")
            self.optimizer.add(v >= 0)
            self.var_widths[name] = v
            self.optimizer.minimize(v)
        return self.var_widths[name]

    def infer(self, exprs: List[ast.Expr]) -> Dict[str, int]:
        # 1. Unification Pass: Operands of same op should have same width
        for expr in exprs:
            self._unify(expr)
            
        # 2. Safety Pass: Result must not overflow
        for expr in exprs:
            self._analyze(expr)
            
        if self.optimizer.check() == z3.sat:
            model = self.optimizer.model()
            return {name: model[w].as_long() for name, w in self.var_widths.items()}
        return {}

    def _unify(self, expr: ast.Expr):
        if isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return
            op, args = struct
            if op in ["+", "-", "*", "/", "==", ">", "<", ">=", "<="]:
                vars = [self.get_width_var(arg.name) for arg in args if isinstance(arg, ast.Atom) and not self._is_num(arg.name)]
                for i in range(len(vars) - 1):
                    self.optimizer.add(vars[i] == vars[i+1])
            for arg in args: self._unify(arg)

    def _analyze(self, expr: ast.Expr) -> z3.ArithRef:
        if isinstance(expr, ast.Atom):
            try:
                val = int(expr.name)
                return z3.IntVal(max(1, val.bit_length()))
            except ValueError:
                return self.get_width_var(expr.name)
        
        elif isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return z3.IntVal(0)
            op, args = struct
            
            if op == "=":
                if len(args) == 2 and isinstance(args[0], ast.Atom):
                    target_name = args[0].name
                    source_bits = self._analyze(args[1])
                    w_target = self.get_width_var(target_name)
                    self.optimizer.add(w_target >= source_bits)
                    return w_target
            
            if op == "+":
                bits = [self._analyze(arg) for arg in args]
                # The result of (+) needs 1 more bit than operands
                # Since operands are unified, max(bits) is their width
                return self._max_z3(bits) + 1
            
            if op == "*":
                bits = [self._analyze(arg) for arg in args]
                return z3.Sum(bits)
            
            bits = [self._analyze(arg) for arg in args]
            return self._max_z3(bits) if bits else z3.IntVal(0)
            
        return z3.IntVal(0)

    def _max_z3(self, vars: List[Any]) -> Any:
        if not vars: return z3.IntVal(0)
        m = vars[0]
        for v in vars[1:]: m = z3.If(v > m, v, m)
        return m

    def _is_num(self, s: str) -> bool:
        try: int(s); return True
        except ValueError: return False

    def _unroll_apply(self, expr: ast.Apply) -> Optional[Tuple[str, List[ast.Expr]]]:
        curr = expr
        args = []
        while isinstance(curr, ast.Apply):
            args.append(curr.arg)
            curr = curr.f
        if isinstance(curr, ast.Atom):
            return curr.name, list(reversed(args))
        return None
