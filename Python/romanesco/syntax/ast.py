from dataclasses import dataclass
from typing import List, Any

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

@dataclass
class Lambda(Expr):
    param: str
    body: Expr
    def __repr__(self): return f"Lambda({self.param}, {self.body})"

# For internal engine use or simple core mapping
@dataclass
class Atom(Expr):
    name: str
    def __repr__(self): return f"Atom({self.name})"

@dataclass
class Apply(Expr):
    f: Any
    arg: Any
    def __repr__(self): return f"Apply({self.f}, {self.arg})"

class Stmt: pass

@dataclass
class ExprStmt(Stmt):
    expr: Expr
