// ==========================================
// Rewriter.scala
// 圏論的項書き換えエンジン（シンボル柔軟版）
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
    // --- ラムダ計算 (β簡約) ---
    case Expr.App(Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)), args) if args.nonEmpty =>
      val arg = args.head
      val rest = args.tail
      val substituted = Prover.substVar(body, v, arg)
      if (rest.isEmpty) substituted
      else Expr.App(substituted, rest)

    // --- 圏論的公理 ---
    case Expr.App(Expr.Sym(Compose), List(f, Expr.Sym(Id))) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Id), f)) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Compose), List(f, g)), h)) =>
      Expr.App(Expr.Sym(Compose), List(f, Expr.App(Expr.Sym(Compose), List(g, h))))
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Proj1), Expr.App(Expr.Sym(Pair), List(f, g)))) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.Sym(Proj2), Expr.App(Expr.Sym(Pair), List(f, g)))) => g
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Case), List(f, g)), Expr.Sym(Inl))) => f
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Case), List(f, g)), Expr.Sym(Inr))) => g

    // --- 自然数の演算 ---
    case Expr.App(Expr.Sym("plus"), List(Expr.Sym(Zero | Initial), m)) => m
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym(Succ), List(n)), m)) =>
      Expr.App(Expr.Sym(Succ), List(Expr.App(Expr.Sym("plus"), List(n, m))))

    // --- リストの演算 ---
    case Expr.App(Expr.Sym("append"), List(Expr.Sym("nil" | Initial | Zero), ys)) => ys
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), ys)) =>
      Expr.App(Expr.Sym("cons"), List(x, Expr.App(Expr.Sym("append"), List(xs, ys))))

    // --- HoTT path reduction ---
    // inv(refl) -> refl
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym(Refl), List(a)))) => 
      Expr.App(Expr.Sym(Refl), List(a))
    // inv(inv(p)) -> p
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym("inv"), List(p)))) => p
    // p ∘ refl -> p
    case Expr.App(Expr.Sym(Compose), List(p, Expr.App(Expr.Sym(Refl), List(_)))) => p
    // refl ∘ p -> p
    case Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym(Refl), List(_)), p)) => p
    // inv(p ∘ q) -> inv(q) ∘ inv(p)
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym(Compose), List(p, q)))) =>
      Expr.App(Expr.Sym(Compose), List(Expr.App(Expr.Sym("inv"), List(q)), Expr.App(Expr.Sym("inv"), List(p))))

    case _ => expr
  }
}
