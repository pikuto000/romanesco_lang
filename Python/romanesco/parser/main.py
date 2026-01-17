from typing import List, Optional
from ..lexing import Token
from ..syntax import ast
from .. import undeterminable
from .engine import RewritingEngine

def parse_all(tokens: List[Token], debug: bool = False) -> undeterminable.Tree[List[ast.Expr]]:
    engine = RewritingEngine(tokens, debug=debug)
return engine.run()
