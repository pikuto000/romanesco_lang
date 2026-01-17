from typing import List, Tuple, Any, Optional, cast
import time
from ..syntax import ast
from .types import Value, IntVal, BoolVal, PairVal, Closure, PrimOp, LogicVar, AtomVal, PartialBinOp, Env, Contradiction, MatchFailure
from ..prelude import initial_bindings, eval_primitive, values_equal
from ..inference import WidthInference

DEBUG_EVAL = False

def log(msg: str):
    if DEBUG_EVAL:
        print(f"[EVAL] {msg}")

def translate_to_core(expr: ast.Expr) -> ast.Expr:
    if isinstance(expr, ast.Call):
        f = translate_to_core(expr.f)
        for arg in expr.args:
            f = ast.Apply(f, translate_to_core(arg))
        return f
    elif isinstance(expr, ast.Lambda):
        return ast.Apply(ast.Apply(ast.Atom("lambda"), ast.Atom(expr.param)), translate_to_core(expr.body))
    elif isinstance(expr, ast.Block):
        if not expr.exprs: return ast.Atom("false")
        res = translate_to_core(expr.exprs[0])
        for e in expr.exprs[1:]:
            res = ast.Apply(ast.Apply(ast.Atom("seq"), res), translate_to_core(e))
        return res
    elif isinstance(expr, ast.Num):
        return ast.Atom(expr.value)
    elif isinstance(expr, ast.Var):
        return ast.Atom(expr.name)
    return expr

def eval_expr(expr: ast.Expr, env: Env) -> List[Tuple[Value, Env]]:
    try:
        if isinstance(expr, ast.Atom): return eval_atom(expr.name, env)
        if isinstance(expr, ast.Apply): return eval_apply(expr.f, expr.arg, env)
    except (Contradiction, MatchFailure, RecursionError): return []
    except Exception as e:
        log(f"Error evaluating {expr}: {e}")
        if DEBUG_EVAL:
            import traceback
            traceback.print_exc()
        return []
    return []

def eval_atom(name: str, env: Env) -> List[Tuple[Value, Env]]:
    log(f"eval_atom: {name}")
    if name in initial_bindings: 
        log(f"  -> PrimOp({name})")
        return [(PrimOp(name), env)]
    val = env.lookup_value(name)
    if val is not None:
        val = env.resolve(val)
        # Apply implicit width logic
        if isinstance(val, IntVal) and val.width is None:
            w = env.lookup_width(name)
            if w is not None and w > 0: # 0 means infinite
                val = IntVal(val.val, w)
        return [(val, env)]
    
    vid_free = hash(("free", name, 0))
    if vid_free in env.subs:
        return [(env.resolve(env.subs[vid_free]), env)]

    expr = env.lookup_expr(name)
    if expr:
        if isinstance(expr, ast.Atom) and expr.name == name:
            vid = hash(("free", name, 0))
            return [(env.resolve(LogicVar(vid, name)), env)]
        return eval_expr(expr, env)
    
    if name == "true": return [(BoolVal(True), env)]
    if name == "false": return [(BoolVal(False), env)]
    try: return [(IntVal(int(name)), env)] # Default to infinite precision Int
    except ValueError: pass
    
    vid = hash(("free", name, 0))
    return [(env.resolve(LogicVar(vid, name)), env)]

def eval_apply(f: ast.Expr, arg: ast.Expr, env: Env) -> List[Tuple[Value, Env]]:
    log(f"eval_apply: f={f}, arg={arg}")
    results = []
    for f_val, env1 in eval_expr(f, env):
        f_val = env1.resolve(f_val)
        log(f"  f evaluated to: {f_val}")
        
        if isinstance(f_val, PrimOp):
            op = f_val.name
            if op in ["and", "or", "seq", "="]:
                results.append((PartialBinOp(op, AtomVal("dummy"), arg), env1))
            elif op == "lambda":
                if isinstance(arg, ast.Atom):
                    results.append((PartialBinOp("lambda", AtomVal(arg.name), None), env1))
            else:
                for arg_val, env2 in eval_expr(arg, env1):
                    results.append((PartialBinOp(op, env2.resolve(arg_val), None), env2))
        
        elif isinstance(f_val, PartialBinOp):
            op = f_val.op
            if op == "and":
                if f_val.right_expr:
                    for left_val, env2 in eval_expr(f_val.right_expr, env1):
                        results.extend(eval_and(env2.resolve(left_val), arg, env2))
            elif op == "or":
                if f_val.right_expr:
                    results.extend(eval_expr(f_val.right_expr, env1))
                    results.extend(eval_expr(arg, env1))
            elif op == "seq":
                if f_val.right_expr:
                    for _, env2 in eval_expr(f_val.right_expr, env1):
                        results.extend(eval_expr(arg, env2))
            elif op == "=":
                if f_val.right_expr:
                    for left_val, env2 in eval_expr(f_val.right_expr, env1):
                        results.extend(unify(env2.resolve(left_val), arg, env2))
            elif op == "lambda":
                if isinstance(f_val.left_val, AtomVal):
                    results.append((Closure(f_val.left_val.name, arg, env1), env1))
            else:
                for right_val, env2 in eval_expr(arg, env1):
                    results.append((eval_primitive(op, [f_val.left_val, env2.resolve(right_val)]), env2))
                    
        elif isinstance(f_val, Closure):
            for arg_val, env2 in eval_expr(arg, env1):
                new_nonce = hash((f_val, env2.nonce, time.time_ns()))
                body_env = f_val.env.with_nonce(new_nonce).bind_name(f_val.param, env2.resolve(arg_val))
                for res_val, _ in eval_expr(f_val.body, body_env):
                    results.append((env2.resolve(res_val), env2))
                    
    return results

def eval_and(left_val: Value, right_expr: ast.Expr, env: Env) -> List[Tuple[Value, Env]]:
    if isinstance(left_val, BoolVal):
        if not left_val.b: return [(BoolVal(False), env)]
        return eval_expr(right_expr, env)
    res = []
    for right_val, env2 in eval_expr(right_expr, env):
        res.append((PairVal(left_val, env2.resolve(right_val)), env2))
    return res

def unify(v1: Value, expr2: ast.Expr, env: Env) -> List[Tuple[Value, Env]]:
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
    
    # Try unifying IntVals with/without width
    if isinstance(v1, IntVal) and isinstance(v2, IntVal):
        # Value mismatch is contradiction
        if v1.val != v2.val: raise Contradiction(f"Value mismatch: {v1.val} != {v2.val}")
        # Width mismatch: can unify if one is None
        if v1.width is None and v2.width is not None:
            return env
        if v1.width is not None and v2.width is None:
            return env
        if v1.width == v2.width: return env
        
    raise Contradiction(f"Cannot unify {v1} and {v2}")

def value_to_expr(val: Value) -> ast.Expr:
    if isinstance(val, IntVal):
        if val.width is None: return ast.Atom(str(val.val))
        return ast.Apply(ast.Apply(ast.Atom("bv"), ast.Atom(str(val.val))), ast.Atom(str(val.width)))
    if isinstance(val, BoolVal): return ast.Atom(str(val.b).lower())
    if isinstance(val, LogicVar): return ast.Atom(val.name)
    if isinstance(val, AtomVal): return ast.Atom(val.name)
    if isinstance(val, PrimOp): return ast.Atom(val.name)
    if isinstance(val, PairVal): return ast.Apply(ast.Apply(ast.Atom("and"), value_to_expr(val.left)), value_to_expr(val.right))
    if isinstance(val, Closure): return ast.Apply(ast.Apply(ast.Atom("lambda"), ast.Atom(val.param)), val.body)
    return ast.Atom("null")

def eval_program(exprs: List[ast.Expr]) -> List[Tuple[Env, Optional[Value]]]:
    core_exprs = [translate_to_core(e) for e in exprs]
    
    # --- Width Inference ---
    inference = WidthInference(debug=DEBUG_EVAL)
    inferred_widths = inference.infer(core_exprs)
    if DEBUG_EVAL and inferred_widths:
        print(f"[INFER] Inferred widths: {inferred_widths}")
    # -----------------------

    env = Env.initial(initial_bindings)
    # Apply inferred widths to initial environment
    for name, width in inferred_widths.items():
        env = env.bind_width(name, width)

    paths = [(None, env)]
    for i, expr in enumerate(core_exprs):
        log(f"Evaluating stmt {i}: {expr}")
        new_paths = []
        is_assignment = False
        if isinstance(expr, ast.Apply) and isinstance(expr.f, ast.Apply) and \
           isinstance(expr.f.f, ast.Atom) and expr.f.f.name == "=" and \
           isinstance(expr.f.arg, ast.Atom):
            name = expr.f.arg.name
            value_expr = expr.arg
            for _, current_env in paths:
                for val, env_after in eval_expr(value_expr, current_env):
                    val = env_after.resolve(val)
                    # Implicit casting on assignment
                    w = env_after.lookup_width(name)
                    if w is not None and w > 0 and isinstance(val, IntVal) and val.width is None:
                        val = IntVal(val.val, w)
                    
                    new_env = env_after.bind_name(name, val)
                    new_env = new_env.extend_all({name: value_to_expr(val)})
                    new_paths.append((val, new_env))
            is_assignment = True
        
        if not is_assignment:
            for _, current_env in paths:
                results = eval_expr(expr, current_env)
                for val, env_after in results:
                    new_paths.append((env_after.resolve(val), env_after))
        
        if new_paths: paths = new_paths
        else: return []
        
    return [(e, v) for v, e in paths]
