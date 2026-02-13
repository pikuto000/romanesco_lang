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
    case Expr.App(Expr.Sym(op), args) if op == SepAnd || op == Tensor || op == And || op == Or || op == "plus" || op == "times" =>
      def collect(e: Expr): List[Expr] = e match {
        case Expr.App(Expr.Sym(`op`), List(a, b)) => collect(a) ++ collect(b)
        case other => List(acNormalize(other))
      }
      val flattened = args.flatMap(collect)
      val sorted = flattened.sortBy(_.toString)
      if (sorted.isEmpty) {
        if (op == Tensor) Expr.Sym(LOne) else Expr.Sym(True)
      } else {
        sorted.reduceLeft((acc, e) => Expr.App(Expr.Sym(op), List(acc, e)))
      }
    
    case Expr.App(f, args) => Expr.App(acNormalize(f), args.map(acNormalize))
    case _ => expr
  }

  private def rewriteRule(expr: Expr): Expr = expr match {
    // --- HoTT path reduction (Priority) ---
    // inv(refl) -> refl
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym(r), _))) if r == Refl => 
      expr.asInstanceOf[Expr.App].args.head
    // inv(inv(p)) -> p
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym("inv"), List(p)))) => p
    // refl ∘ p -> p
    case Expr.App(Expr.Sym(op), List(Expr.App(Expr.Sym(r), _), p))
        if (op == Compose || op == Concat) && r == Refl => p
    // p ∘ refl -> p
    case Expr.App(Expr.Sym(op), List(p, Expr.App(Expr.Sym(r), _))) 
        if (op == Compose || op == Concat) && r == Refl => p
    // transport(P, refl, x) -> x
    case Expr.App(Expr.Sym(t), List(_, Expr.App(Expr.Sym(r), _), x)) if t == Transport && r == Refl => x

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
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym("plus"), List(a, b)), c)) =>
      Expr.App(Expr.Sym("plus"), List(a, Expr.App(Expr.Sym("plus"), List(b, c))))

    // --- リスト의 演算 ---
    case Expr.App(Expr.Sym("append"), List(Expr.Sym("nil"), ys)) => ys
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), ys)) =>
      Expr.App(Expr.Sym("cons"), List(x, Expr.App(Expr.Sym("append"), List(xs, ys))))
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("append"), List(xs, ys)), zs)) =>
      Expr.App(Expr.Sym("append"), List(xs, Expr.App(Expr.Sym("append"), List(ys, zs))))

    case Expr.App(Expr.Sym("reverse"), List(Expr.Sym("nil"))) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("reverse"), List(xs)), Expr.App(Expr.Sym("cons"), List(x, Expr.Sym("nil")))))

    case Expr.App(Expr.Sym("map"), List(_, Expr.Sym("nil"))) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("map"), List(f, Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Expr.App(Expr.Sym("cons"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("map"), List(f, xs))))

    // --- ベクトルの演算 ---
    case Expr.App(Expr.Sym("vlength"), List(Expr.Sym("vnil"))) => Expr.Sym("0")
    case Expr.App(Expr.Sym("vlength"), List(Expr.App(Expr.Sym("vcons"), List(_, _, n)))) => Expr.App(Expr.Sym("S"), List(n))
    case Expr.App(Expr.Sym("vhead"), List(Expr.App(Expr.Sym("vcons"), List(x, _, _)))) => x
    case Expr.App(Expr.Sym("vappend"), List(Expr.Sym("vnil"), v)) => v
    case Expr.App(Expr.Sym("vappend"), List(Expr.App(Expr.Sym("vcons"), List(x, xs, n)), ys)) =>
      Expr.App(Expr.Sym("vcons"), List(x, Expr.App(Expr.Sym("vappend"), List(xs, ys)), Expr.App(Expr.Sym("plus"), List(n, Expr.App(Expr.Sym("vlength"), List(ys))))))

    // --- ファンクタ・モナド ---
    case Expr.App(Expr.Sym("fmap"), List(_, Expr.Sym("nil"))) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("fmap"), List(f, Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Expr.App(Expr.Sym("cons"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("fmap"), List(f, xs))))
    case Expr.App(Expr.Sym("return"), List(x)) => Expr.App(Expr.Sym("cons"), List(x, Expr.Sym("nil")))
    case Expr.App(Expr.Sym("bind"), List(Expr.Sym("nil"), _)) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("bind"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), f)) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("bind"), List(xs, f))))
    case Expr.App(Expr.Sym("bind"), List(m, Expr.Sym("return"))) => m
    case Expr.App(Expr.Sym("bind"), List(m, Expr.App(Expr.Sym("λ"), List(Expr.Var(x), Expr.App(Expr.Sym("return"), List(Expr.Var(x2))))))) if x == x2 => m
    case Expr.App(Expr.Sym("bind"), List(m, Expr.App(Expr.Sym("λ"), List(Expr.Var(x), Expr.App(Expr.Sym("cons"), List(Expr.Var(x2), Expr.Sym("nil"))))))) if x == x2 => m
    case Expr.App(Expr.Sym("bind"), List(Expr.App(Expr.Sym("append"), List(m1, m2)), g)) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("bind"), List(m1, g)), Expr.App(Expr.Sym("bind"), List(m2, g))))
    case Expr.App(Expr.Sym("id"), List(x)) => x
    case Expr.App(Expr.App(Expr.Sym("compose"), List(f, g)), List(x)) =>
      Expr.App(f, List(Expr.App(g, List(x))))
    case Expr.App(Expr.Sym(Concat), List(Expr.App(Expr.Sym(Concat), List(p, q)), r)) =>
      Expr.App(Expr.Sym(Concat), List(p, Expr.App(Expr.Sym(Concat), List(q, r))))

    // --- 論理の簡約 ---
    case Expr.App(Expr.Sym(Eq), List(l, r)) if l == r => Expr.Sym(True)
    case Expr.App(Expr.Sym(Path), List(_, l, r)) if l == r => Expr.Sym(True)
    case Expr.App(Expr.Sym(Globally), List(Expr.Sym(True))) => Expr.Sym(True)
    case Expr.App(Expr.Sym(op), List(Expr.Sym(True), p)) if op == And || op == Product => p
    case Expr.App(Expr.Sym(op), List(p, Expr.Sym(True))) if op == And || op == Product => p
    case Expr.App(Expr.Sym(op), List(Expr.Sym(False), _)) if op == And || op == Product => Expr.Sym(False)
    case Expr.App(Expr.Sym(op), List(_, Expr.Sym(False))) if op == And || op == Product => Expr.Sym(False)
    case Expr.App(Expr.Sym(op), List(Expr.Sym(True), _)) if op == Or || op == Coproduct => Expr.Sym(True)
    case Expr.App(Expr.Sym(op), List(_, Expr.Sym(True))) if op == Or || op == Coproduct => Expr.Sym(True)
    case Expr.App(Expr.Sym(op), List(Expr.Sym(False), p)) if op == Or || op == Coproduct => p
    case Expr.App(Expr.Sym(op), List(p, Expr.Sym(False))) if op == Or || op == Coproduct => p

    case Expr.App(Expr.Sym("bisim"), List(s1, s2)) =>
      Expr.App(Expr.Sym(Globally), List(Expr.App(Expr.Sym(Eq), List(Expr.App(Expr.Sym("head"), List(s1)), Expr.App(Expr.Sym("head"), List(s2))))))

    // --- ストリームの演算 ---
    case Expr.App(Expr.Sym("head"), List(Expr.App(Expr.Sym("cons_stream"), List(x, _)))) => x
    case Expr.App(Expr.Sym("tail"), List(Expr.App(Expr.Sym("cons_stream"), List(_, s)))) => s
    case Expr.App(Expr.Sym("head"), List(Expr.App(Expr.Sym("repeat"), List(x)))) => x
    case Expr.App(Expr.Sym("tail"), List(Expr.App(Expr.Sym("repeat"), List(x)))) => Expr.App(Expr.Sym("repeat"), List(x))
    case Expr.App(Expr.Sym("map_stream"), List(f, Expr.App(Expr.Sym("cons_stream"), List(x, s)))) =>
      Expr.App(Expr.Sym("cons_stream"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("map_stream"), List(f, s))))
    case Expr.App(Expr.Sym("head"), List(Expr.App(Expr.Sym("repeat"), List(x)))) => x
    case Expr.App(Expr.Sym("tail"), List(Expr.App(Expr.Sym("repeat"), List(x)))) => Expr.App(Expr.Sym("repeat"), List(x))
    case Expr.App(Expr.Sym("head"), List(Expr.App(Expr.Sym("map_stream"), List(f, s)))) =>
      Expr.App(f, List(Expr.App(Expr.Sym("head"), List(s))))
    case Expr.App(Expr.Sym("tail"), List(Expr.App(Expr.Sym("map_stream"), List(f, s)))) =>
      Expr.App(Expr.Sym("map_stream"), List(f, Expr.App(Expr.Sym("tail"), List(s))))

    case _ => expr
  }
}
