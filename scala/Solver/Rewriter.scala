// ==========================================
// Rewriter.scala
// 圏論的項書き換えエンジン（Appラムダ対応）
// ==========================================

package romanesco.Solver.core

import LogicSymbols._

object Rewriter {

  def normalize(expr: Expr): Expr = {
    val reduced = step(expr)
    if (reduced == expr) reduced
    else normalize(reduced)
  }

  private def step(expr: Expr): Expr = expr match {
    case Expr.App(f, args) =>
      val nextF = step(f)
      val nextArgs = args.map(step)
      rewriteRule(Expr.App(nextF, nextArgs))
    case _ => expr
  }

  private def rewriteRule(expr: Expr): Expr = expr match {
    // --- 圏論的公理 ---
    case Expr.App(Expr.Sym(Compose), List(f, Expr.Sym(Id))) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Id), f)) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Compose), List(f, g)), h)) =>
      Expr.App(Expr.Sym(Compose), List(f, Expr.App(Expr.Sym(Compose), List(g, h))))
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Proj1), Expr.App(Expr.Sym(Pair), List(f, g)))) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Proj2), Expr.App(Expr.Sym(Pair), List(f, g)))) => g
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Case), List(f, g)), Expr.Sym(Inl))) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Case), List(f, g)), Expr.Sym(Inr))) => g

    // --- β-簡約 (Appベース版) ---
    case Expr.App(Expr.Lam(x, body), List(arg)) =>
      Prover.substVar(body, x, arg)

    case _ => expr
  }
}
