package Parsing

import Lexing.Token
import Undeterminable.tree

// AST定義
enum Expr:
  case Num(value: String)
  case Var(name: String)
  case BinOp(op: String, left: Expr, right: Expr)
  case Call(name: String, args: List[Expr])
  case Block(stmts: List[Stmt])

enum Stmt:
  case ExprStmt(expr: Expr)
  case MacroDef(name: String, patterns: List[Pattern], body: Expr)
  case Assignment(name: String, value: Expr)

enum Pattern:
  case VarPattern(name: String)    // [X]
  case WordPattern(word: String)   // test
  case NumPattern(value: String)   // 100

object Expr:
  def show(expr: Expr): String = expr match
    case Num(v) => v
    case Var(n) => n
    case BinOp(op, l, r) => s"(${show(l)} $op ${show(r)})"
    case Call(n, args) => s"$n(${args.map(show).mkString(", ")})"
    case Block(stmts) => s"{${stmts.map(Stmt.show).mkString("; ")}}"

object Stmt:
  def show(stmt: Stmt): String = stmt match
    case ExprStmt(e) => Expr.show(e)
    case MacroDef(name, patterns, body) => 
      s"syntax $name ${patterns.map(Pattern.show).mkString(" ")} = ${Expr.show(body)}"
    case Assignment(n, v) => s"$n = ${Expr.show(v)}"

object Pattern:
  def show(p: Pattern): String = p match
    case VarPattern(n) => s"[$n]"
    case WordPattern(w) => w
    case NumPattern(v) => v
