package Parsing

/**
 * romanesco抽象構文木（AST）
 */

/** 式 */
enum Expr:
  case Num(value: String)                           // 数値リテラル
  case Var(name: String)                            // 変数参照
  case Call(f: Expr, args: List[Expr])              // 関数呼び出し
  case Block(exprs: List[Expr])                     // ブロック式
  case Lambda(param: String, body: Expr)            // ラムダ式

/** ステートメント */
enum Stmt:
  case ExprStmt(expr: Expr)                                     // 式文

// ======================================
// Pretty Printer
// ======================================

object Expr:
  /** 式を文字列表現に変換 */
  def show(expr: Expr): String = expr match
    case Num(v) => v
    case Var(n) => n
    case Call(f, args) => s"${show(f)}(${args.map(show).mkString(", ")})"
    case Block(exprs) => s"{ ${exprs.map(show).mkString("; ")} }"
    case Lambda(param, body) => s"\\$param -> ${show(body)}"

object Stmt:
  /** ステートメントを文字列表現に変換 */
  def show(stmt: Stmt): String = stmt match
    case ExprStmt(e) => Expr.show(e)