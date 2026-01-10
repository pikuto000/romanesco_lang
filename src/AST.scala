package romanesco
import scala.util.parsing.input.Positional

enum Token extends Positional:
  case Num(v: BigDecimal); case Var(n: String); case Op(o: String); case Kw(w: String); case Sym(s: String); case WS(c: String); case Comment(c: String)
  def raw: String = this match { case Num(v) => v.toString; case Var(n) => n; case Op(o) => o; case Kw(w) => w; case Sym(s) => s; case _ => "" }

enum AstExpr:
  case Num(v: BigDecimal); case Var(n: String)
  case BinOp(o: String, l: AstExpr, r: AstExpr); case UnOp(o: String, e: AstExpr)
  case If(c: AstExpr, t: AstExpr, e: AstExpr); case Lambda(p: String, b: AstExpr); case Apply(f: AstExpr, a: List[AstExpr])
  case Ambiguous(o: List[AstExpr]); case Templated(n: String, i: AstExpr)

enum Stmt:
  case Constraint(l: AstExpr, r: AstExpr); case Verify(e: AstExpr)
  case Block(s: List[Stmt]); case SyntaxDef(p: List[Token], t: List[Token]); case Branch(o: List[Stmt])
