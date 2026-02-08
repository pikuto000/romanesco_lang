// ==========================================
// Core.scala (4種類版)
// ==========================================

package romanesco.Solver.core

enum Expr:
  case Var(name: String) // Var
  case Meta(id: Int) // Meta Var
  case Sym(name: String) // Symbol
  case App(head: Expr, args: List[Expr])

  def apply(args: Expr*): Expr = App(this, args.toList)

  override def toString: String = this match
    case Var(n)                         => n
    case Meta(id)                       => s"?$id"
    case Sym(n)                         => n
    case App(Expr.Sym("="), List(l, r)) => s"$l = $r" // 等式の特別表示
    case App(h, args)                   =>
      if args.isEmpty then h.toString
      else s"$h(${args.mkString(",")})"

object Expr:
  def sym(name: String): Expr = Sym(name)
  def v(name: String): Expr = Var(name)
  def meta(id: Int): Expr = Meta(id)

  // 等式判定ヘルパー
  def unapplyEq(e: Expr): Option[(Expr, Expr)] = e match
    case App(Sym("="), List(l, r)) => Some((l, r))
    case _                         => None

enum ProofStep:
  case Apply(rule: CatRule, subst: Map[Int, Expr])

  override def toString: String = this match
    case Apply(r, _) => s"apply[${r.name}]"

case class CatRule(
    name: String,
    lhs: Expr,
    rhs: Expr,
    universals: List[Expr] = Nil
):
  override def toString: String =
    val cond =
      if universals.isEmpty then ""
      else s" where ${universals.mkString(", ")}"
    s"$name: $lhs ⟹ $rhs$cond"

type Proof = List[ProofStep]
