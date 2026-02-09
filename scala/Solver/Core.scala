// ==========================================
// Core.scala
// 基本的なデータ構造の定義（Appによるラムダ表現版）
// ==========================================

package romanesco.Solver.core

/**
 * メタ変数のユニークな識別子
 */
case class MetaId(ids: List[Int]) {
  override def toString: String = ids.mkString(".")
}

object MetaId {
  def apply(id: Int): MetaId = MetaId(List(id))
}

/**
 * 論理式を表現する抽象構文木
 */
enum Expr:
  case Var(name: String)    // 束縛変数 / 個体変数
  case Meta(id: MetaId)     // メタ変数
  case Sym(name: String)    // 定数
  case App(f: Expr, args: List[Expr])

  def apply(args: Expr*): Expr = App(this, args.toList)

  /** 目標の先頭記号（インデックス用）を返します */
  def headSymbol: String = this match
    case App(Sym(n), _) => n
    case Sym(n)         => n
    case Var(_)         => "_VAR_"
    case Meta(_)        => "_META_"
    case App(h, _)      => h.headSymbol

  override def toString: String = this match
    case Var(n)                         => n
    case Meta(id)                       => s"?$id"
    case Sym(n)                         => n
    case App(Expr.Sym("λ"), List(Expr.Var(v), b)) => s"λ$v. $b"
    case App(Expr.Sym("="), List(l, r)) => s"$l = $r"
    case App(h, args)                   =>
      if args.isEmpty then h.toString
      else s"$h(${args.mkString(",")})"

object Expr:
  def sym(name: String): Expr = Sym(name)
  def v(name: String): Expr = Var(name)
  def meta(id: Int): Expr = Meta(MetaId(id))
  def meta(ids: Int*): Expr = Meta(MetaId(ids.toList))

  def unapplyEq(e: Expr): Option[(Expr, Expr)] = e match
    case App(Sym("="), List(l, r)) => Some((l, r))
    case _                         => None

  // ラムダ式の抽出用パターン
  object Lam:
    def unapply(e: Expr): Option[(String, Expr)] = e match
      case App(Sym("λ"), List(Var(v), body)) => Some((v, body))
      case _ => None

/**
 * 圏論得推論規則
 */
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

/**
 * 証明のツリー構造
 */
enum ProofTree:
  case Node(goal: Expr, ruleName: String, children: List[ProofTree])
  case Leaf(goal: Expr, ruleName: String)

  def format(indent: Int = 0): String =
    val sp = "  " * indent
    this match
      case Leaf(g, r) => s"$sp[ $r ]  ⟹  $g"
      case Node(g, r, cs) =>
        val childrenStr = cs.map(_.format(indent + 1)).mkString("\n")
        s"$childrenStr\n$sp-------------------- ($r)\n$sp  ⟹  $g"

type Proof = ProofTree

/**
 * 証明の失敗トレース
 */
case class FailTrace(
    goal: Goal,
    reason: String,
    depth: Int,
    children: List[FailTrace] = Nil
):
  def format(indent: Int = 0): String =
    val sp = "  " * indent
    val base = s"$sp- [Depth $depth] Goal: ${goal.target}\n$sp  Reason: $reason"
    if children.isEmpty then base
    else s"$base\n${children.map(_.format(indent + 1)).mkString("\n")}"

// --- タクティクスシステム用の定義 ---

case class Goal(
    context: List[(String, Expr)], 
    target: Expr                   
):
  override def toString: String =
    val ctx = context.map((n, e) => s"  $n: $e").mkString("\n")
    s"--------------------------------\n$ctx\n--------------------------------\n  Goal: $target"

case class ProofState(
    goals: List[Goal],             
    completedProofs: List[Proof],  
    originalGoal: Expr             
):
  def isSolved: Boolean = goals.isEmpty
  def currentGoal: Option[Goal] = goals.headOption

type TacticResult = Either[String, ProofState]