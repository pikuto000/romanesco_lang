from typing import List, Tuple, Callable, Set, Dict
import re
from romanesco import undeterminable

# ======================================
# Token Definition
# ======================================

class Token:
    def __init__(self, s: str):
        self.s = s
    
    @property
    def lexeme(self) -> str:
        return self.s
    
    def __repr__(self):
        return f"{self.__class__.__name__}({self.s})"
    
    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.s == other.s

class Op(Token): pass
class Ident(Token): pass
class Keyword(Token): pass
class WS(Token): pass
class Number(Token): pass
class Delim(Token): pass
class String(Token): pass

# ======================================
# Tokenizer Types and Constructors
# ======================================

# Tokenizer: (input_str, chars, pos) -> List[Tuple[Token, next_pos]]
Tokenizer = Callable[[str, List[str], int], List[Tuple[Token, int]]]

def lex_regex(pattern: str, converter: Callable[[str], Token]) -> Tokenizer:
    regex = re.compile(pattern)
    
    def tokenizer(input_str: str, chars: List[str], pos: int) -> List[Tuple[Token, int]]:
        s = input_str[pos:]
        matches = []
        
        # In Python, we can't easily iterate all matches of *different lengths* with one call
        # efficiently unless we manually check substrings.
        # Scala implementation checks substrings of length 1 to maxLen.
        
        max_len = len(s)
        for length in range(1, max_len + 1):
            sub = s[:length]
            # fullmatch checks if the entire string matches the pattern
            if regex.fullmatch(sub):
                matches.append((converter(sub), pos + length))
        
        return matches
    
    return tokenizer

def lex_regex_longest(pattern: str, converter: Callable[[str], Token]) -> Tokenizer:
    regex = re.compile(pattern)
    
    def tokenizer(input_str: str, chars: List[str], pos: int) -> List[Tuple[Token, int]]:
        # match checks from the beginning of the string
        m = regex.match(input_str, pos)
        if m:
            sub = m.group(0)
            return [(converter(sub), pos + len(sub))]
        return []
    
    return tokenizer

def lex_delim(delimiters: Set[str]) -> Tokenizer:
    def tokenizer(input_str: str, chars: List[str], pos: int) -> List[Tuple[Token, int]]:
        matches = []
        for d in delimiters:
            if input_str.startswith(d, pos):
                matches.append((Delim(d), pos + len(d)))
        return matches
    return tokenizer

# ======================================
# Lex Mode
# ======================================

class LexMode:
    pass

class BestOnly(LexMode): pass
class All(LexMode): pass
class TopN(LexMode):
    def __init__(self, n: int):
        self.n = n

# ======================================
# Main Lexer
# ======================================

def lex_to_tree(
    input_str: str,
    tokenizers: List[Tokenizer],
    debug: bool = False
) -> undeterminable.Tree[Token]:
    chars = list(input_str) # List of characters
    length = len(chars)
    
    memo: Dict[int, undeterminable.Tree[Token]] = {}
    
    def build(pos: int) -> undeterminable.Tree[Token]:
        if pos in memo:
            return memo[pos]
        
        if pos == length:
            if debug:
                print(f"[DEBUG] pos={pos}: Reached end of input")
            res = undeterminable.Node([], [])
            memo[pos] = res
            return res
        
        if debug:
            remaining = input_str[pos:min(pos + 10, length)] + "..."
            print(f"[DEBUG] pos={pos}: char='{chars[pos]}' remaining='{remaining}'".replace("\n", "\\n").replace("\r", "\\r"))
        
        branches = []
        for tokenizer in tokenizers:
            matches = tokenizer(input_str, chars, pos)
            if debug and matches:
                match_str = ", ".join([f"{tok.lexeme}@{next_pos}" for tok, next_pos in matches])
                print(f"[DEBUG]   Tokenizer matched: {match_str}".replace("\n", "\\n").replace("\r", "\\r"))
            
            for tok, next_pos in matches:
                next_tree = build(next_pos)
                if isinstance(next_tree, undeterminable.DeadEnd):
                    pass
                else:
                    branches.append(undeterminable.single(tok, next_tree))
        
        if not branches:
            if debug:
                print(f"[DEBUG] pos={pos}: DeadEnd (no tokenizer matched)")
            res = undeterminable.DeadEnd()
        else:
            res = undeterminable.fork(branches)
        
        memo[pos] = res
        return res

    return build(0)

def lex_all(
    input_str: str,
    tokenizers: List[Tokenizer],
    mode: LexMode,
    debug: bool = False
) -> undeterminable.Tree[Token]:
    tree = lex_to_tree(input_str, tokenizers, debug)
    
    if isinstance(mode, All):
        return tree
    
    paths = tree.flatten_paths()
    if not paths:
        return undeterminable.DeadEnd()
    
    # Simple filtering based on path count or length for now
    if isinstance(mode, BestOnly):
        # Longest token sequence (fewest tokens for same input length)
        # or most characters covered? Since lex_to_tree only reaches the end, 
        # all paths cover the same characters.
        # "Best" usually means fewest tokens or specific heuristic.
        # Scala version says "Only the longest tokenization".
        best_path = min(paths, key=len)
        return rebuild_tree([best_path])
    
    if isinstance(mode, TopN):
        top_paths = paths[:mode.n]
        return rebuild_tree(top_paths)
        
    return tree

def rebuild_tree(paths: List[List[Token]]) -> undeterminable.Tree[Token]:
    if not paths:
        return undeterminable.DeadEnd()
    
    def build_path(tokens: List[Token]) -> undeterminable.Tree[Token]:
        if not tokens:
            return undeterminable.Node([], [])
        return undeterminable.single(tokens[0], build_path(tokens[1:]))

    branches = [build_path(p) for p in paths]
    return undeterminable.fork(branches)

# ======================================
# Unlexer
# ======================================

def unlex_token(tok: Token) -> str:
    return tok.lexeme

def unlex(tokens: List[Token]) -> List[str]:
    acc = [""]
    for tok in tokens:
        new_acc = []
        s = unlex_token(tok)
        for prefix in acc:
            new_acc.append(prefix + s)
        acc = new_acc
    return acc

def show_tokens(tokens: List[Token]) -> str:
    res = []
    for t in tokens:
        if isinstance(t, WS):
            res.append(" ")
        else:
            res.append(unlex_token(t))
    return "".join(res)
