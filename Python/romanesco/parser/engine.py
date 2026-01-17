import uuid
from dataclasses import dataclass
from typing import List, Tuple, Dict, Any, Optional, Iterable, Set
from ..lexing import Token, WS, Number
from ..syntax import ast
from .. import undeterminable

@dataclass
class Rule:
    id: str
    pattern: List[Tuple[str, str]]
    body: List[Token]
    def __hash__(self):
        return hash(self.id)

class RewritingEngine:
    def __init__(self, tokens: List[Token], debug: bool = False, inherited_rules: Tuple[Rule, ...] = tuple()):
        self.tokens = tokens
        self.debug = debug
        self.arities = {
            "+": 2, "-": 2, "*": 2, "/": 2, "=": 2, "seq": 2, "lambda": 2,
            "true": 0, "false": 0, "and": 2, "or": 2, 
            "==": 2, "!=": 2, ">": 2, "<": 2, ">=": 2, "<=": 2
        }
        self.keywords = {"syntax", "->", "import"} | set(self.arities.keys())
        self.initial_rules = inherited_rules
        
        for r in inherited_rules:
            for p_type, p_val in r.pattern:
                if p_type == "word": self.keywords.add(p_val)
                
        self.memo: Dict[Any, List[Tuple[List[Token], Tuple[Rule, ...]]]] = {}

    def log(self, msg: str):
        if self.debug:
            print(f"[REWRITE] {msg}")

    def run(self) -> undeterminable.Tree[List[ast.Expr]]:
        final_sequences_with_rules = self.rewrite(self.tokens, self.initial_rules)
        results = []
        seen = set()
        for seq, rules in final_sequences_with_rules:
            filtered = [t for t in seq if not isinstance(t, WS)]
            key = tuple(t.lexeme for t in filtered)
            if key not in seen:
                seen.add(key)
                if self.debug: self.log(f"Stable state: {' '.join(key)}")
                ast_list = self.to_ast(filtered, rules)
                if ast_list: results.append(ast_list)
        
        if not results: return undeterminable.DeadEnd()
        return undeterminable.fork([undeterminable.leaf(r) for r in results])

    def rewrite(self, tokens: List[Token], rules: Tuple[Rule, ...], depth: int = 0) -> List[Tuple[List[Token], Tuple[Rule, ...]]]:
        if not tokens: return [([], rules)]
        if depth > 50: return [(tokens, rules)]

        state = (tuple(t.lexeme for t in tokens), rules)
        if state in self.memo: return self.memo[state]

        # 1. Macro Registration
        first_non_ws = 0
        while first_non_ws < len(tokens) and isinstance(tokens[first_non_ws], WS):
            first_non_ws += 1
        
        if first_non_ws < len(tokens) and tokens[first_non_ws].lexeme == "syntax":
            res = []
            for rule, rest in self.extract_syntax_rule(tokens[first_non_ws:]):
                # Update keywords for the current recursion path (clone set if needed, but here we just add)
                # Note: This modifies self.keywords which is shared. 
                # Ideally we should pass keywords down, but for now we assume monotonic growth is okay for valid syntax.
                for p_type, p_val in rule.pattern:
                    if p_type == "word": self.keywords.add(p_val)
                    
                self.log(f"Registered macro: {' '.join(p[1] for p in rule.pattern if p[0]=='word')}")
                res.extend(self.rewrite(tokens[:first_non_ws] + rest, rules + (rule,), depth + 1))
            self.memo[state] = res
            return res

        # 2. Structural Rewriting
        statements = self.split_statements(tokens)
        rewritten_statements = []
        
        # We need to process statements sequentially, accumulating rules? 
        # Actually rules are global to the scope. In Romanesco, macros defined later affect earlier? 
        # No, usually sequential. But here we passed 'rules' which contains all macros defined *before* this point.
        # But 'rewrite' splits all tokens.
        # The macros defined within 'tokens' are processed in step 1.
        # If step 1 didn't find 'syntax', then we proceed to rewrite using 'rules'.
        
        # However, `split_statements` might split `syntax ...` if we are not careful.
        # But step 1 checks for `syntax` at the *beginning* of the block.
        # If `syntax` is in the middle, `split_statements` will separate it.
        # We should probably process statements one by one, allowing `syntax` to update rules for subsequent statements.
        
        # Current logic: If starts with syntax, recurse. If not, rewrite all statements with current rules.
        # This implies all macros must be at the top or we handle them recursively statement by statement.
        # Romanesco usually puts macros at top. If macros are interleaved, the `rewrite` recursion handles it:
        # `rewrite` processes the head. If it's a statement, it rewrites it and appends to result?
        # The current implementation of `rewrite` assumes if it's NOT syntax, it's all code.
        # This might be an issue if there is syntax definition later.
        
        # Let's assume standard structure for now.
        
        for stmt in statements:
            rewritten_statements.append(self.rewrite_statement(stmt, rules))
        
        current = []
        for s in rewritten_statements:
            current.extend(s)
        
        res = [(current, rules)]
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
            elif depth == 0 and t.lexeme == ";": # Semicolon support
                stmts.append(curr)
                curr = []
                
        if curr: stmts.append(curr)
        return stmts

    def rewrite_statement(self, tokens: List[Token], rules: Tuple[Rule, ...]) -> List[Token]:
        curr = tokens
        for _ in range(20):
            changed = False
            units = self.get_units(curr)
            
            for rule in rules:
                match_res = self.find_match(rule, units)
                if match_res:
                    u_idx, u_len, binds = match_res
                    before_units = units[:u_idx]
                    after_units = units[u_idx + u_len:]
                    expanded = self.apply_rule(rule, binds)
                    
                    new_curr = []
                    for u in before_units: new_curr.extend(u)
                    new_curr.extend(expanded)
                    for u in after_units: new_curr.extend(u)
                    
                    self.log(f"Applied {rule.id[:4]}: {' '.join(t.lexeme for t in curr if not isinstance(t, WS))} -> {' '.join(t.lexeme for t in new_curr if not isinstance(t, WS))}")
                    curr = new_curr
                    changed = True
                    break
            if not changed: break
        return curr

    def get_units(self, tokens: List[Token]) -> List[List[Token]]:
        units = []
        i = 0
        while i < len(tokens):
            if isinstance(tokens[i], WS):
                units.append([tokens[i]])
                i += 1
                continue
            
            expr_len = self.match_one_expression(tokens[i:])
            if expr_len > 0:
                units.append(tokens[i:i+expr_len])
                i += expr_len
            else:
                # Fallback: consume one token if no expression matched
                units.append([tokens[i]])
                i += 1
        return units

    def find_match(self, rule: Rule, units: List[List[Token]]):
        for i in range(len(units)):
            binds = {}
            u_ptr = i
            p_ptr = 0
            possible = True
            
            if rule.pattern[0][0] == "param":
                pass

            while p_ptr < len(rule.pattern):
                while u_ptr < len(units) and all(isinstance(t, WS) for t in units[u_ptr]):
                    u_ptr += 1
                
                if u_ptr >= len(units):
                    possible = False
                    break
                
                p_type, p_val = rule.pattern[p_ptr]
                if p_type == "param":
                    first_tok = units[u_ptr][0]
                    if first_tok.lexeme in self.keywords or first_tok.lexeme in ["(", "{", ")", "}", "syntax", "->"]:
                        if first_tok.lexeme not in ["(", "{"]:
                            possible = False
                            break
                    binds[p_val] = units[u_ptr]
                    u_ptr += 1
                else:
                    if units[u_ptr][0].lexeme != p_val:
                        possible = False
                        break
                    u_ptr += 1
                p_ptr += 1
            
            if possible:
                return i, u_ptr - i, binds
        return None

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

    def apply_rule(self, rule: Rule, binds: Dict[str, List[Token]]) -> List[Token]:
        res = []
        for t in rule.body:
            if t.lexeme in binds: res.extend(binds[t.lexeme])
            else: res.append(t)
        return res

    def to_ast(self, tokens: List[Token], rules: Tuple[Rule, ...]) -> List[ast.Expr]:
        ptr = 0
        def parse_one() -> ast.Expr:
            nonlocal ptr
            if ptr >= len(tokens): return ast.Var("null")
            t = tokens[ptr]; lexeme = t.lexeme; ptr += 1
            if lexeme == "(":
                inner = []; depth = 1
                while ptr < len(tokens) and depth > 0:
                    if tokens[ptr].lexeme == "(": depth += 1
                    elif tokens[ptr].lexeme == ")": depth -= 1
                    if depth > 0: inner.append(tokens[ptr])
                    ptr += 1
                if not inner: return ast.Var("null")
                # Pass inherited rules to the sub-engine
                temp_engine = RewritingEngine(inner, False, inherited_rules=rules)
                paths = temp_engine.run().flatten_paths()
                if paths and paths[0] and paths[0][0]:
                    return paths[0][0][0]
                return ast.Var("null")
            if lexeme == "{":
                inner = []; depth = 1
                while ptr < len(tokens) and depth > 0:
                    if tokens[ptr].lexeme == "{": depth += 1
                    elif tokens[ptr].lexeme == "}": depth -= 1
                    if depth > 0: inner.append(tokens[ptr])
                    ptr += 1
                temp_engine = RewritingEngine(inner, False, inherited_rules=rules)
                paths = temp_engine.run().flatten_paths()
                if paths and paths[0]: return ast.Block(paths[0][0])
                return ast.Block([])
            if isinstance(t, Number): return ast.Num(lexeme)
            
            # Lambda (prefix)
            if lexeme == "lambda":
                if ptr < len(tokens):
                    param = tokens[ptr].lexeme; ptr += 1
                    body = parse_one()
                    return ast.Lambda(param, body)
            
            arity = self.arities.get(lexeme, 0)
            if arity == 0: return ast.Var(lexeme)
            args = []
            for _ in range(arity): args.append(parse_one())
            return ast.Call(ast.Var(lexeme), args)
        
        exprs = []
        while ptr < len(tokens):
            e = parse_one()
            exprs.append(e)
        return exprs