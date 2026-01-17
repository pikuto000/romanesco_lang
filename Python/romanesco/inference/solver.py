import z3
from typing import List, Dict, Optional, Tuple, Any, cast
from ..syntax import ast

class WidthInference:
    def __init__(self, debug: bool = False):
        self.optimizer = z3.Optimize()
        self.w_vars: Dict[str, z3.ArithRef] = {}
        self.debug = debug

    def log(self, msg: str):
        if self.debug: print(f"[INFER] {msg}")

    def get_w(self, name: str) -> z3.ArithRef:
        if name not in self.w_vars:
            w = cast(z3.ArithRef, z3.Int(f"w_{name}"))
            self.optimizer.add(w >= 0)
            self.w_vars[name] = w
            self.optimizer.minimize(w)
        return self.w_vars[name]

    def infer(self, exprs: List[ast.Expr]) -> Dict[str, int]:
        self.log("Starting Bit-Width Inference with Optimization")
        try:
            for expr in exprs: self._unify(expr)
            for expr in exprs: self._analyze(expr)
            if self.optimizer.check() == z3.sat:
                model = self.optimizer.model()
                res = {name: model[w].as_long() for name, w in self.w_vars.items() if model[w] is not None}
                self.log(f"Inference Successful: {res}")
                return res
            self.log("Inference Failed (Unsat)")
        except Exception as e:
            self.log(f"Inference Error: {e}")
            import traceback
            if self.debug: traceback.print_exc()
        return {}

    def _unify(self, expr: ast.Expr):
        if isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return
            op, args = struct
            if op in ["+", "-", "*", "/", "==", ">", "<", ">=", "<="]:
                v_list = [self.get_w(arg.name) for arg in args if isinstance(arg, ast.Atom) and not self._is_num(arg.name)]
                for i in range(len(v_list) - 1):
                    self.optimizer.add(v_list[i] == v_list[i+1])
            for arg in args: self._unify(arg)

    def _analyze(self, expr: ast.Expr) -> z3.ArithRef:
        if isinstance(expr, ast.Atom):
            if self._is_num(expr.name): 
                bits = max(1, int(expr.name).bit_length())
                return cast(z3.ArithRef, z3.IntVal(bits))
            return self.get_w(expr.name)
        if isinstance(expr, ast.Apply):
            struct = self._unroll_apply(expr)
            if not struct: return cast(z3.ArithRef, z3.IntVal(0))
            op, args = struct
            
            if op == "=" and len(args) == 2 and isinstance(args[0], ast.Atom):
                w_val = self._analyze(args[1])
                w_tgt = self.get_w(args[0].name)
                self.optimizer.add(w_tgt >= w_val)
                return w_tgt
            
            if op == "+":
                bits = [self._analyze(arg) for arg in args]
                # Optimization: x + 0 -> no growth
                is_zero = [isinstance(arg, ast.Atom) and arg.name == "0" for arg in args]
                if any(is_zero):
                    self.log("  Optimization: +0 detected, maintaining width")
                    return self._max_z3(bits)
                return cast(z3.ArithRef, self._max_z3(bits) + 1)
                
            if op == "*":
                bits = [self._analyze(arg) for arg in args]
                # Optimization: x * 1 -> no growth
                is_one = [isinstance(arg, ast.Atom) and arg.name == "1" for arg in args]
                if any(is_one):
                    self.log("  Optimization: *1 detected, maintaining width")
                    return self._max_z3(bits)
                return cast(z3.ArithRef, z3.Sum(*bits))
            
            bits = [self._analyze(arg) for arg in args]
            return self._max_z3(bits) if bits else cast(z3.ArithRef, z3.IntVal(0))
        return cast(z3.ArithRef, z3.IntVal(0))

    def _is_num(self, s: str) -> bool:
        try: int(s); return True
        except ValueError: return False

    def _max_z3(self, vars: List[z3.ArithRef]) -> z3.ArithRef:
        if not vars: return cast(z3.ArithRef, z3.IntVal(0))
        m = vars[0]
        for v in vars[1:]: m = cast(z3.ArithRef, z3.If(v > m, v, m))
        return m

    def _unroll_apply(self, expr: ast.Apply) -> Optional[Tuple[str, List[ast.Expr]]]:
        curr: ast.Expr = expr; args: List[ast.Expr] = []
        while isinstance(curr, ast.Apply):
            args.append(curr.arg); curr = curr.f
        if isinstance(curr, ast.Atom): return curr.name, list(reversed(args))
        return None
