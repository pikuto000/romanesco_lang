package romanesco
import scala.util.parsing.input.Positional

enum Token extends Positional:
  case Num(v: BigDecimal); case Var(n: String); case Op(o: String); case Kw(w: String); case Sym(s: String); case WS(c: String); case Comment(c: String)

enum AstExpr:
  case Num(v: BigDecimal)
  case Var(n: String)
  case BinOp(o: String, l: AstExpr, r: AstExpr)
  case If(c: AstExpr, t: AstExpr, e: AstExpr)
  case Lambda(p: String, b: AstExpr)
  case Apply(f: AstExpr, a: List[AstExpr])
  case Ambiguous(o: List[AstExpr])
  case Templated(n: String, i: AstExpr)

enum Stmt:
  case Constraint(l: AstExpr, r: AstExpr)
  case Block(s: List[Stmt])
  case SyntaxDef(p: List[Token], t: List[Token])
  case Branch(o: List[Stmt])