// ==========================================
// Rewriter.scala
// 圏論的項書き換えエンジン（シンボル柔軟版）
// ==========================================

package romanesco.Solver.core

import LogicSymbols._

object Rewriter {

  def normalize(expr: Expr): Expr = {
    val reduced = step(expr)
    val acNormalized = acNormalize(reduced)
    if (acNormalized == expr) acNormalized
    else normalize(acNormalized)
  }

  private def step(expr: Expr): Expr = expr match {
    case Expr.App(f, args) =>
      val nextF = step(f)
      val nextArgs = args.map(step)
      rewriteRule(Expr.App(nextF, nextArgs))
    case _ => expr
  }

  /** AC (Associative-Commutative) Normalization
    * 分離論理 (*) やテンソル積 (⊗) などの可換・結合的演算子を正規化（ソート）する
    */
  private def acNormalize(expr: Expr): Expr = expr match {
    case Expr.App(Expr.Sym(op), args) if op == SepAnd || op == Tensor || op == And || op == Or =>
      def collect(e: Expr): List[Expr] = e match {
        case Expr.App(Expr.Sym(`op`), List(a, b)) => collect(a) ++ collect(b)
        case other => List(acNormalize(other))
      }
      val flattened = args.flatMap(collect)
      val sorted = flattened.sortBy(_.toString)
      sorted.reduceLeft((acc, e) => Expr.App(Expr.Sym(op), List(acc, e)))
    
    case Expr.App(f, args) => Expr.App(acNormalize(f), args.map(acNormalize))
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
    case Expr.App(Expr.Sym(op), List(f, Expr.Sym(idSym))) if op == Compose && idSym == Id => f
    case Expr.App(Expr.Sym(op), List(Expr.Sym(idSym), f)) if op == Compose && idSym == Id => f
    
    // Associativity for non-commutative operators (Compose, Concat)
    case Expr.App(Expr.Sym(op), List(Expr.App(Expr.Sym(op2), List(f, g)), h)) 
        if (op == Compose || op == Concat) && op == op2 =>
      Expr.App(Expr.Sym(op), List(f, Expr.App(Expr.Sym(op), List(g, h))))
    
    case Expr.App(Expr.Sym(op), List(Expr.Sym(p1), Expr.App(Expr.Sym(p), List(f, g)))) 
        if op == Compose && p1 == Proj1 && p == Pair => f
    case Expr.App(Expr.Sym(op), List(Expr.Sym(p2), Expr.App(Expr.Sym(p), List(f, g)))) 
        if op == Compose && p2 == Proj2 && p == Pair => g

    // --- 自然数の演算 ---
    case Expr.App(Expr.Sym("plus"), List(Expr.Sym(z), m)) if z == Zero || z == Initial => m
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym(s), List(n)), m)) if s == Succ =>
      Expr.App(Expr.Sym(Succ), List(Expr.App(Expr.Sym("plus"), List(n, m))))

    // --- リストの演算 ---
    case Expr.App(Expr.Sym("append"), List(Expr.Sym("nil"), ys)) => ys
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), ys)) =>
      Expr.App(Expr.Sym("cons"), List(x, Expr.App(Expr.Sym("append"), List(xs, ys))))

    // --- HoTT path reduction ---
    // inv(refl) -> refl
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym(r), List(a)))) if r == Refl => 
      Expr.App(Expr.Sym(Refl), List(a))
    // inv(inv(p)) -> p
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym("inv"), List(p)))) => p
    // p ∘ refl -> p
    case Expr.App(Expr.Sym(op), List(p, Expr.App(Expr.Sym(r), List(_)))) 
        if (op == Compose || op == Concat) && r == Refl => p
    // refl ∘ p -> p
    case Expr.App(Expr.Sym(op), List(Expr.App(Expr.Sym(r), List(_)), p))
        if (op == Compose || op == Concat) && r == Refl => p
    // inv(p ∘ q) -> inv(q) ∘ inv(p)
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym(op), List(p, q)))) if op == Compose || op == Concat =>
      Expr.App(Expr.Sym(op), List(Expr.App(Expr.Sym("inv"), List(q)), Expr.App(Expr.Sym("inv"), List(p))))
    // transport(P, refl, x) -> x
    case Expr.App(Expr.Sym(t), List(_, Expr.App(Expr.Sym(r), List(_)), x)) if t == Transport && r == Refl => x

    case _ => expr
  }
}
