// ==========================================
// Rewriter.scala
// 圏論的項書き換えエンジン（リスト・自然数対応）
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

    // --- 自然数の演算 (Arithmetic) ---
    // plus(0, m) -> m
    case Expr.App(Expr.Sym("plus"), List(Expr.Sym(Zero), m)) => m
    // plus(S(n), m) -> S(plus(n, m))
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym(Succ), List(n)), m)) =>
      Expr.App(Expr.Sym(Succ), List(Expr.App(Expr.Sym("plus"), List(n, m))))

    // --- リストの演算 (Lists) ---
    // append(nil, ys) -> ys
    case Expr.App(Expr.Sym("append"), List(Expr.Sym("nil"), ys)) => ys
    // append(cons(x, xs), ys) -> cons(x, append(xs, ys))
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), ys)) =>
      Expr.App(Expr.Sym("cons"), List(x, Expr.App(Expr.Sym("append"), List(xs, ys))))

    case _ => expr
  }
}