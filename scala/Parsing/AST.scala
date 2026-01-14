package Parsing

/**
 * romanesco抽象構文木（AST）
 * 
 * 現在は暫定的な実装。将来的に以下を追加予定：
 * - ラムダ式
 * - 制約式
 * - 適用（全ての基本）
 */

/** 式 */
enum Expr:
  case Num(value: String)                           // 数値リテラル
  case Var(name: String)                            // 変数参照
  case BinOp(op: String, left: Expr, right: Expr)  // 二項演算
  case Call(name: String, args: List[Expr])         // 関数呼び出し
  case Block(stmts: List[Stmt])                     // ブロック式

/** ステートメント */
enum Stmt:
  case ExprStmt(expr: Expr)                                     // 式文
  case MacroDef(name: String, patterns: List[Pattern], body: Expr)  // マクロ定義
  case Assignment(name: String, value: Expr)                    // 変数代入

/** パターン（mixfix構文用） */
enum Pattern:
  case VarPattern(name: String)    // プレースホルダー: [X]
  case WordPattern(word: String)   // 固定キーワード: test
  case NumPattern(value: String)   // 数値: 100

// ======================================
// Pretty Printer
// ======================================

object Expr:
  /** 式を文字列表現に変換 */
  def show(expr: Expr): String = expr match
    case Num(v) => v
    case Var(n) => n
    case BinOp(op, l, r) => s"(${show(l)} $op ${show(r)})"
    case Call(n, args) => s"$n(${args.map(show).mkString(", ")})"
    case Block(stmts) => s"{ ${stmts.map(Stmt.show).mkString("; ")} }"

object Stmt:
  /** ステートメントを文字列表現に変換 */
  def show(stmt: Stmt): String = stmt match
    case ExprStmt(e) => 
      Expr.show(e)
    case MacroDef(name, patterns, body) => 
      val pats = patterns.map(Pattern.show).mkString(" ")
      s"syntax $name $pats = ${Expr.show(body)}"
    case Assignment(n, v) => 
      s"$n = ${Expr.show(v)}"

object Pattern:
  /** パターンを文字列表現に変換 */
  def show(p: Pattern): String = p match
    case VarPattern(n) => s"[$n]"
    case WordPattern(w) => w
    case NumPattern(v) => v
