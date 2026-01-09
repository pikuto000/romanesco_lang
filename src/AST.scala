package romanesco

// --- データの定義 (AST) ---

enum AstExpr:
  case Num(value: BigDecimal)
  case Var(name: String)
  case BinOp(op: String, left: AstExpr, right: AstExpr)
  case MacroCall(name: String, args: List[AstExpr])
  case Ambiguous(options: List[AstExpr])
  case Templated(name: String, inner: AstExpr)
  case Raw(tokens: List[Token])

enum Stmt:
  case Constraint(left: AstExpr, right: AstExpr)
  case Block(stmts: List[Stmt])
  case MacroDef(name: String, params: List[String], body: AstExpr)
  case Branch(options: List[Stmt])
  // 新しい構文定義: syntax パターン = { テンプレート }
  case SyntaxDef(pattern: List[Token], template: List[Token])