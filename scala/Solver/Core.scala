// ==========================================
// Core.scala
// 基本的なデータ構造の定義
// ==========================================

package romanesco.Solver.core

/**
 * メタ変数のユニークな識別子。
 * 階層的な構造（例: 1.2.1）を持ち、どの探索パスで生成されたかを示す。
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
  case Var(name: String)    // 束縛変数
  case Meta(id: MetaId)     // メタ変数（未解決の穴）
  case Sym(name: String)    // 定数（∧, ∨, →, ⊤, ⊥, =, ...）
  case App(f: Expr, args: List[Expr])

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
  def meta(id: Int): Expr = Meta(MetaId(id))
  def meta(ids: Int*): Expr = Meta(MetaId(ids.toList))

  // 等式判定ヘルパー
  def unapplyEq(e: Expr): Option[(Expr, Expr)] = e match
    case App(Sym("="), List(l, r)) => Some((l, r))
    case _                         => None

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
 * 証明の1ステップ
 */
enum ProofStep:
  case Apply(rule: CatRule, subst: Map[MetaId, Expr])

  override def toString: String = this match
    case Apply(r, _) => s"apply[${r.name}]"

type Proof = List[ProofStep]

// --- タクティクスシステム用の定義 ---

/**
 * 解決すべき一つの証明課題（ゴール）
 */
case class Goal(
    context: List[(String, Expr)], // 仮定のリスト
    target: Expr                   // 結論
):
  override def toString: String =
    val ctx = context.map((n, e) => s"  $n: $e").mkString("\n")
    s"--------------------------------\n$ctx\n--------------------------------\n  Goal: $target"

/**
 * 現在の証明全体の進行状態
 */
case class ProofState(
    goals: List[Goal],             // 未解決のゴールのリスト
    completedProofs: List[Proof],  // 解決済みの証明ステップ
    originalGoal: Expr             // 元々の目標
):
  def isSolved: Boolean = goals.isEmpty
  def currentGoal: Option[Goal] = goals.headOption

/**
 * タクティクスの適用結果
 */
type TacticResult = Either[String, ProofState]
