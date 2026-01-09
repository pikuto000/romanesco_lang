package romanesco

import scala.util.parsing.input.Positional

// --- すべてのデータ定義をここに集約 ---

enum Token extends Positional:
  case Num(value: BigDecimal)
  case Var(name: String)
  case Op(op: String)
  case Keyword(word: String)
  case Sym(symbol: String)
  case WS(content: String)
  case Comment(content: String)

enum AstExpr:
  case Num(value: BigDecimal)
  case Var(name: String)
  case BinOp(op: String, left: AstExpr, right: AstExpr)
  case MacroCall(name: String, args: List[AstExpr])
  case Ambiguous(options: List[AstExpr])
  case Templated(name: String, inner: AstExpr)
  case Raw(tokens: List[Token])
  case Lambda(param: String, body: AstExpr)
  case Apply(fn: AstExpr, args: List[AstExpr])

enum Stmt:
  case Constraint(left: AstExpr, right: AstExpr)
  case Block(stmts: List[Stmt])
  case MacroDef(name: String, params: List[String], body: AstExpr)
  case Branch(options: List[Stmt])
  case SyntaxDef(pattern: List[Token], template: List[Token])