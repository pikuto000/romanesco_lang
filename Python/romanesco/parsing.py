import uuid
from dataclasses import dataclass
from typing import List, Tuple, Dict, Any, Optional, Iterable, Callable
from romanesco.lexing import Token, WS, Number
from romanesco import undeterminable

# ======================================
# AST Nodes
# ======================================

class Expr: pass

@dataclass
class Num(Expr):
    value: str
    def __repr__(self): return f"Num({self.value})"

@dataclass
class Var(Expr):
    name: str
    def __repr__(self): return f"Var({self.name})"

@dataclass
class Call(Expr):
    f: Expr
    args: List[Expr]
    def __repr__(self): return f"Call({self.f}, {self.args})"

@dataclass
class Block(Expr):
    exprs: List[Expr]
    def __repr__(self): return f"Block({self.exprs})"

# Internal engine nodes
@dataclass
class Atom(Expr):
    name: str
    def __repr__(self): return f"Atom({self.name})"

@dataclass
class Apply(Expr):
    f: Any
    arg: Any
    def __repr__(self): return f"Apply({self.f}, {self.arg})"

# Global handler for imports (set by __main__.py)
_import_handler: Optional[Callable[[str], List[List[Expr]]]] = None

# ======================================
# Rewriting Rules (Mixfix)
# ======================================

@dataclass
class Rule:
    id: str
    pattern: List[Tuple[str, str]]
    body: List[Token]
    def __hash__(self):
        return hash(self.id)

# ======================================
# Structural Mixfix Rewriting Engine
# ======================================

class RewritingEngine:
    def __init__(self, tokens: List[Token], debug: bool = False):
        self.tokens = tokens
        self.debug = debug
        self.arities = {
            "+": 2, "-": 2, "*": 2, "/": 2, "=": 2, "seq": 2, "lambda": 2,
            "true": 0, "false": 0, "and": 2, "or": 2, 
            "==": 2, "!=": 2, ">": 2, "<": 2, ">=": 2, "<=": 2
        }
        self.keywords = {"syntax", "->", "import"} | set(self.arities.keys())
        self.memo: Dict[Any, List[List[Token]]] = {}

    def log(self, msg: str):
        if self.debug:
            print(f"[REWRITE] {msg}")

    def run(self) -> undeterminable.Tree[List[Expr]]:
        final_sequences = self.rewrite(self.tokens, tuple())
        results = []
        seen = set()
        for seq in final_sequences:
            filtered = [t for t in seq if not isinstance(t, WS)]
            key = tuple(t.lexeme for t in filtered)
            if key not in seen:
                seen.add(key)
                if self.debug: self.log(f"Stable state: {' '.join(key)}")
                ast_list = self.to_ast(filtered)
                if ast_list: results.append(ast_list)
        
        if not results: return undeterminable.DeadEnd()
        return undeterminable.fork([undeterminable.leaf(r) for r in results])

    def rewrite(self, tokens: List[Token], rules: Tuple[Rule, ...]) -> List[List[Token]]:
        if not tokens: return [[]]
        
        # 1. Macro Registration
        first_non_ws = 0
        while first_non_ws < len(tokens) and isinstance(tokens[first_non_ws], WS):
            first_non_ws += 1
        
        if first_non_ws < len(tokens) and tokens[first_non_ws].lexeme == "syntax":
            res = []
            for rule, rest in self.extract_syntax_rule(tokens[first_non_ws:]):
                for p_type, p_val in rule.pattern:
                    if p_type == "word": self.keywords.add(p_val)
                self.log(f"Registered macro: {' '.join(p[1] for p in rule.pattern if p[0]=='word')}")
                res.extend(self.rewrite(tokens[:first_non_ws] + rest, rules + (rule,)))
            return res

        # 2. Structural Rewriting
        # Partition tokens into "statements" by newlines
        statements = self.split_statements(tokens)
        rewritten_statements = []
        for stmt in statements:
            rewritten_statements.append(self.rewrite_statement(stmt, rules))
        
        # Combine back
        res = []
        # Since rewrite_statement is currently deterministic for simplicity, just combine
        current = []
        for s in rewritten_statements:
            current.extend(s)
        res.append(current)
        return res

    def split_statements(self, tokens: List[Token]) -> List[List[Token]]:
        stmts = []
        curr = []
        depth = 0
        for t in tokens:
            if t.lexeme in ["(", "{", "["]:
                depth += 1
            elif t.lexeme in [")", "}", "]"]:
                depth -= 1
            
            curr.append(t)
            if depth == 0 and isinstance(t, WS) and "\n" in t.lexeme:
                stmts.append(curr)
                curr = []
        if curr: stmts.append(curr)
        return stmts

    def rewrite_statement(self, tokens: List[Token], rules: Tuple[Rule, ...]) -> List[Token]:
        # Recursive rewrite until fixed point
        curr = tokens
        for _ in range(20): # Safety limit
            changed = False
            # Break into "units" (expressions or keywords)
            units = self.get_units(curr)
            
            # Try applying rules to the sequence of units
            for rule in rules:
                match_res = self.find_match(rule, units)
                if match_res:
                    u_idx, u_len, binds = match_res
                    before_units = units[:u_idx]
                    after_units = units[u_idx + u_len:]
                    expanded_tokens = self.apply_rule(rule, binds)
                    
                    # Reconstruct tokens
                    new_curr = []
                    for u in before_units: new_curr.extend(u)
                    new_curr.extend(expanded_tokens)
                    for u in after_units: new_curr.extend(u)
                    
                    self.log(f"Applied {rule.id[:4]}: {' '.join(t.lexeme for t in curr if not isinstance(t, WS))} -> {' '.join(t.lexeme for t in new_curr if not isinstance(t, WS))}")
                    curr = new_curr
                    changed = True
                    break # Restart with new tokens
            if not changed: break
        return curr

    def get_units(self, tokens: List[Token]) -> List[List[Token]]:
        units = []
        i = 0
        while i < len(tokens):
            if isinstance(tokens[i], WS):
                # Attach WS to next unit or keep as separate
                units.append([tokens[i]])
                i += 1
                continue
            
            # Match one expression
            expr_len = self.match_one_expression(tokens[i:])
            if expr_len > 0:
                units.append(tokens[i:i+expr_len])
                i += expr_len
            else:
                # Must be a keyword or something we can't match as a full expr yet
                units.append([tokens[i]])
                i += 1
        return units

    def find_match(self, rule: Rule, units: List[List[Token]]):
        # units is a list of token-lists
        for i in range(len(units)):
            binds = {}
            u_ptr = i
            p_ptr = 0
            possible = True
            
            # Special case for infix: trigger word must not be at start of statement
            if rule.pattern[0][0] == "param":
                # Find first non-WS unit
                first_non_ws = 0
                while first_non_ws < len(units) and all(isinstance(t, WS) for t in units[first_non_ws]):
                    first_non_ws += 1
                if i == first_non_ws: continue

            while p_ptr < len(rule.pattern):
                # Skip WS units in units stream when looking for pattern elements
                while u_ptr < len(units) and all(isinstance(t, WS) for t in units[u_ptr]):
                    u_ptr += 1
                
                if u_ptr >= len(units):
                    possible = False
                    break
                
                p_type, p_val = rule.pattern[p_ptr]
                if p_type == "param":
                    # Unit must be a complete expression (not just a keyword)
                    # and not a structural delimiter
                    first_tok = units[u_ptr][0]
                    if first_tok.lexeme in self.keywords or first_tok.lexeme in ["(", "{", ")", "}", "syntax", "->"]:
                        # Exception: if it's bracketed, it's fine (match_one_expression handles this)
                        if first_tok.lexeme not in ["(", "{"]:
                            possible = False
                            break
                    
                    binds[p_val] = units[u_ptr]
                    u_ptr += 1
                else:
                    # Word match
                    if units[u_ptr][0].lexeme != p_val:
                        possible = False
                        break
                    u_ptr += 1
                p_ptr += 1
            
            if possible:
                return i, u_ptr - i, binds
        return None

    def match_one_expression(self, tokens: List[Token]) -> int:
        if not tokens: return 0
        start = 0
        while start < len(tokens) and isinstance(tokens[start], WS):
            if "\n" in tokens[start].lexeme: return 0
            start += 1
        if start >= len(tokens): return 0
        first = tokens[start]
        if first.lexeme in ["(", "{"]:
            depth = 0; idx = start
            open_delim = first.lexeme; close_delim = ")" if open_delim == "(" else "}"
            while idx < len(tokens):
                t = tokens[idx]
                if t.lexeme == open_delim: depth += 1
                elif t.lexeme == close_delim: depth -= 1
                idx += 1
                if depth == 0: break
            return idx if depth == 0 else 0
        if first.lexeme in self.arities:
            arity = self.arities[first.lexeme]
            consumed = start + 1
            for _ in range(arity):
                arg_len = self.match_one_expression(tokens[consumed:])
                if arg_len == 0: return 0
                consumed += arg_len
            return consumed
        if first.lexeme in self.keywords or first.lexeme in ["syntax", "->", "import", ")", "}", "]"]:
            return 0
        return start + 1

    def extract_syntax_rule(self, tokens: List[Token]) -> Iterable[Tuple[Rule, List[Token]]]:
        try:
            arrow_idx = next(i for i, t in enumerate(tokens) if t.lexeme == "->")
        except StopIteration: return
        pattern_tokens = [t for t in tokens[1:arrow_idx] if not isinstance(t, WS)]
        pattern = self.parse_pattern(pattern_tokens)
        if not pattern: return
        body_start = arrow_idx + 1
        body_end = body_start
        has_content = False
        while body_end < len(tokens):
            t = tokens[body_end]
            if t.lexeme == "syntax": break
            if isinstance(t, WS) and "\n" in t.lexeme and has_content: break
            if not isinstance(t, WS): has_content = True
            body_end += 1
        yield Rule(uuid.uuid4().hex, pattern, tokens[body_start:body_end]), tokens[body_end:]

    def parse_pattern(self, toks: List[Token]) -> Optional[List[Tuple[str, str]]]:
        pattern = []
        i = 0
        while i < len(toks):
            if toks[i].lexeme == "[":
                if i + 2 >= len(toks) or toks[i+2].lexeme != "]": return None
                pattern.append(("param", toks[i+1].lexeme)); i += 3
            else:
                pattern.append(("word", toks[i].lexeme)); i += 1
        return pattern if pattern else None

    def apply_rule(self, rule: Rule, binds: Dict[str, List[Token]]) -> List[Token]:
        res = []
        for t in rule.body:
            if t.lexeme in binds: res.extend(binds[t.lexeme])
            else: res.append(t)
        return res

    def to_ast(self, tokens: List[Token]) -> List[Expr]:
        ptr = 0
        def parse_one() -> Expr:
            nonlocal ptr
            if ptr >= len(tokens): return Var("null")
            t = tokens[ptr]; lexeme = t.lexeme; ptr += 1
            if lexeme == "(":
                inner = []; depth = 1
                while ptr < len(tokens) and depth > 0:
                    if tokens[ptr].lexeme == "(": depth += 1
                    elif tokens[ptr].lexeme == ")": depth -= 1
                    if depth > 0: inner.append(tokens[ptr])
                    ptr += 1
                if not inner: return Var("null")
                temp_engine = RewritingEngine(inner, False)
                paths = temp_engine.run().flatten_paths()
                if paths and paths[0] and paths[0][0]:
                    exprs_inside = paths[0][0]
                    return exprs_inside[0] if len(exprs_inside) == 1 else Call(exprs_inside[0], exprs_inside[1:])
                return Var("null")
            if lexeme == "{":
                inner = []; depth = 1
                while ptr < len(tokens) and depth > 0:
                    if tokens[ptr].lexeme == "{": depth += 1
                    elif tokens[ptr].lexeme == "}": depth -= 1
                    if depth > 0: inner.append(tokens[ptr])
                    ptr += 1
                temp_engine = RewritingEngine(inner, False)
                paths = temp_engine.run().flatten_paths()
                if paths and paths[0]: return Block(paths[0][0])
                return Block([])
            if isinstance(t, Number): return Num(lexeme)
            arity = self.arities.get(lexeme, 0)
            if arity == 0: return Var(lexeme)
            args = []
            for _ in range(arity): args.append(parse_one())
            return Call(Var(lexeme), args)
        exprs = []
        while ptr < len(tokens):
            e = parse_one()
            exprs.append(e)
        return exprs

def parse_all(tokens: List[Token], initial_macros=None, debug: bool = False) -> undeterminable.Tree[List[Expr]]:
    engine = RewritingEngine(tokens, debug=debug)
    return engine.run()

def show_expr(expr: Expr) -> str:
    if isinstance(expr, Num): return expr.value
    if isinstance(expr, Var): return expr.name
    if isinstance(expr, Call):
        args_str = " ".join(map(show_expr, expr.args))
        return f"({show_expr(expr.f)} {args_str})"
    if isinstance(expr, Block):
        exprs_str = "; ".join(map(show_expr, expr.exprs))
        return f"{{ {exprs_str} }}"
    return str(expr)