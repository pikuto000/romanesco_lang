// ==========================================
// Rewriter.scala
// 圏論的項書き換えエンジン
// ==========================================

package romanesco.Solver.core

import LogicSymbols._

object Rewriter {

  /**
   * 与えられた式を圏論的公理に基づいて正規化します。
   */
  def normalize(expr: Expr): Expr = {
    val reduced = step(expr)
    if (reduced == expr) reduced
    else normalize(reduced) // 収束するまで繰り返す
  }

  private def step(expr: Expr): Expr = expr match {
    // 1. 再帰的に子要素を正規化
    case Expr.App(f, args) =>
      val nextF = step(f)
      val nextArgs = args.map(step)
      rewriteRule(Expr.App(nextF, nextArgs))
    
    case _ => expr
  }

  /**
   * 具体的な書き換え規則の適用
   */
  private def rewriteRule(expr: Expr): Expr = expr match {
    // --- 恒等射 (Identity) ---
    // f ∘ id -> f
    case Expr.App(Expr.Sym(Compose), List(f, Expr.Sym(Id))) => f
    // id ∘ f -> f
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Id), f)) => f

    // --- 結合法則 (Associativity) ---
    // (f ∘ g) ∘ h -> f ∘ (g ∘ h)  (右結合に統一)
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Compose), List(f, g)), h)) =>
      Expr.App(Expr.Sym(Compose), List(f, Expr.App(Expr.Sym(Compose), List(g, h))))

    // --- 積 (Product) ---
    // pi1 ∘ pair(f, g) -> f
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Proj1), Expr.App(Expr.Sym(Pair), List(f, g)))) => f
    // pi2 ∘ pair(f, g) -> g
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Proj2), Expr.App(Expr.Sym(Pair), List(f, g)))) => g

    // --- 和 (Coproduct) ---
    // case(f, g) ∘ inl -> f
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Case), List(f, g)), Expr.Sym(Inl))) => f
    // case(f, g) ∘ inr -> g
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Case), List(f, g)), Expr.Sym(Inr))) => g

    case _ => expr
  }
}
