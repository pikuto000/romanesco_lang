// ==========================================
// Rewriter.scala
// 圏論的項書き換えエンジン（シンボル柔軟版）
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Solver.core.Prover

object Rewriter {

  def normalize(expr: Expr, rules: List[CatRule] = Nil, maxIter: Int = 100): Expr = {
    def loop(e: Expr, iter: Int): Expr = {
      if (iter <= 0) {
        // logger.log(s"Normalization limit reached for: $e")
        e
      } else {
        val reduced = step(e, rules)
        val acNormalized = acNormalize(reduced)
        if (acNormalized == e) e
        else loop(acNormalized, iter - 1)
      }
    }
    loop(expr, maxIter)
  }

  private def step(expr: Expr, rules: List[CatRule]): Expr = expr match {
    case Expr.App(f, args) =>
      val nextF = step(f, rules)
      val nextArgs = args.map(step(_, rules))
      rewriteRule(Expr.App(nextF, nextArgs), rules)
    case _ => expr
  }

  /** AC (Associative-Commutative) Normalization
    * 分離論理 (*) やテンソル積 (⊗) などの可換・結合的演算子を正規化（ソート）する
    */
  private def acNormalize(expr: Expr): Expr = expr match {
    case Expr.App(Expr.Sym(op), args) if op == SepAnd || op == Tensor || op == And || op == Or || op == "plus" =>
      def collect(e: Expr): List[Expr] = e match {
        case Expr.App(Expr.Sym(`op`), List(a, b)) => collect(a) ++ collect(b)
        case other => List(acNormalize(other))
      }
      val flattened = args.flatMap(collect)
      
      // Idempotency & Short-circuit (Only for non-linear operators)
      var processed = if (op == And || op == Or) {
        flattened.distinct.sortBy(_.toString)
      } else if (op == SepAnd || op == Tensor) {
        // Linear Logic: Keep order, No idempotency
        flattened
      } else {
        flattened.sortBy(_.toString)
      }
      
      if (op == And) {
        if (processed.contains(Expr.Sym(False)) || processed.contains(Expr.Sym("⊥"))) return Expr.Sym(False)
        processed = processed.filterNot(e => e == Expr.Sym(True) || e == Expr.Sym("⊤"))
      } else if (op == Or) {
        if (processed.contains(Expr.Sym(True)) || processed.contains(Expr.Sym("⊤"))) return Expr.Sym(True)
        processed = processed.filterNot(e => e == Expr.Sym(False) || e == Expr.Sym("⊥"))
      }

      if (processed.isEmpty) {
        if (op == Tensor) Expr.Sym(LPlus) else if (op == SepAnd) Expr.Sym(LOne) else if (op == Or) Expr.Sym(False) else Expr.Sym(True)
      } else if (processed.length == 1) {
        processed.head
      } else {
        processed.reduceLeft((acc, e) => Expr.App(Expr.Sym(op), List(acc, e)))
      }
    
    case Expr.App(f, args) => Expr.App(acNormalize(f), args.map(acNormalize))
    case _ => expr
  }

  private def rewriteRule(expr: Expr, rules: List[CatRule]): Expr = {
    // ユーザー定義ルールの適用
    val userRewritten = rules.view.flatMap { r =>
      Unifier.unify(expr, r.lhs, Unifier.emptySubst).map { s =>
        Unifier.applySubst(r.rhs, s)
      }
    }.headOption.getOrElse(expr)

    if (userRewritten != expr) return userRewritten

    expr match {
    // --- HoTT path reduction (Priority) ---
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym(r), _))) if r == Refl => 
      expr.asInstanceOf[Expr.App].args.head
    case Expr.App(Expr.Sym("inv"), List(Expr.App(Expr.Sym("inv"), List(p)))) => p
    case Expr.App(Expr.Sym(op), List(Expr.App(Expr.Sym(r), _), p))
        if (op == Compose || op == Concat) && r == Refl => p
    case Expr.App(Expr.Sym(op), List(p, Expr.App(Expr.Sym(r), _))) 
        if (op == Compose || op == Concat) && r == Refl => p
    
    // Transport computation
    case Expr.App(Expr.Sym(t), List(pred, p, v)) if t == Transport =>
      (pred, v) match {
        // Refl case
        case (_, _) if p match { case Expr.App(Expr.Sym(r), _) if r == Refl => true; case _ => false } => v
        
        // Product: transport(λz. A(z) × B(z), p, pair(u, v)) -> pair(transport(λz. A(z), p, u), transport(λz. B(z), p, v))
        case (Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Expr.App(Expr.Sym(prod), List(a, b)))), Expr.App(Expr.Sym(pair), List(u, v2)))
            if (prod == Product || prod == "×") && pair == Pair =>
          Expr.App(Expr.Sym(Pair), List(
            Expr.App(Expr.Sym(Transport), List(Expr.App(Expr.Sym("λ"), List(Expr.Var(z), a)), p, u)),
            Expr.App(Expr.Sym(Transport), List(Expr.App(Expr.Sym("λ"), List(Expr.Var(z), b)), p, v2))
          ))
        
        // Coproduct: transport(λz. A(z) + B(z), p, inl(u)) -> inl(transport(λz. A(z), p, u))
        case (Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Expr.App(Expr.Sym(coprod), List(a, b)))), Expr.App(Expr.Sym(inl), List(u)))
            if coprod == Coproduct && inl == Inl =>
          Expr.App(Expr.Sym(Inl), List(Expr.App(Expr.Sym(Transport), List(Expr.App(Expr.Sym("λ"), List(Expr.Var(z), a)), p, u))))
        
        case (Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Expr.App(Expr.Sym(coprod), List(a, b)))), Expr.App(Expr.Sym(inr), List(u)))
            if coprod == Coproduct && inr == Inr =>
          Expr.App(Expr.Sym(Inr), List(Expr.App(Expr.Sym(Transport), List(Expr.App(Expr.Sym("λ"), List(Expr.Var(z), b)), p, u))))

        // Function: transport(λz. A(z) → B(z), p, f) -> λx. transport(λz. B(z), p, f(transport(λz. A(z), inv(p), x)))
        case (Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Expr.App(Expr.Sym(arr), List(a, b)))), f)
            if arr == Implies || arr == "→" =>
          val x = Expr.Var("x_trans")
          Expr.App(Expr.Sym("λ"), List(x,
            Expr.App(Expr.Sym(Transport), List(
              Expr.App(Expr.Sym("λ"), List(Expr.Var(z), b)),
              p,
              Expr.App(f, List(Expr.App(Expr.Sym(Transport), List(
                Expr.App(Expr.Sym("λ"), List(Expr.Var(z), a)),
                Expr.App(Expr.Sym("inv"), List(p)),
                x
              ))))
            ))
          ))

        // Forall: transport(λz. ∀x:A(z). B(z, x), p, f) -> λx. transport(λz. B(z, transport(λz. A(z), inv(p), x)), p, f(transport(λz. A(z), inv(p), x)))
        case (Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Expr.App(Expr.Sym(Forall), argsB))), f) =>
          val (xVar, typeA, bodyB) = argsB match {
            case List(Expr.Var(x), t, b) => (x, t, b)
            case List(Expr.Var(x), b) => (x, Expr.Sym("Type"), b)
            case _ => (null, null, null)
          }
          if (xVar != null) {
            val xt = Expr.Var("x_trans")
            val transportedX = Expr.App(Expr.Sym(Transport), List(
              Expr.App(Expr.Sym("λ"), List(Expr.Var(z), typeA)),
              Expr.App(Expr.Sym("inv"), List(p)),
              xt
            ))
            Expr.App(Expr.Sym("λ"), List(xt,
              Expr.App(Expr.Sym(Transport), List(
                Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Prover.substVar(bodyB, xVar, transportedX))),
                p,
                Expr.App(f, List(transportedX))
              ))
            ))
          } else expr

        // Exists: transport(λz. ∃x:A(z). B(z, x), p, pair(u, v)) -> pair(transport(λz. A(z), p, u), transport(λz. B(z, transport(λz. A(z), refl, u)), p, v))
        // Wait, Exists is usually represented as a pair in Martin-Löf Type Theory (Sigma types)
        case (Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Expr.App(Expr.Sym(Exists), argsB))), Expr.App(Expr.Sym(pair), List(u, v)))
            if pair == Pair =>
          val (xVar, typeA, bodyB) = argsB match {
            case List(Expr.Var(x), t, b) => (x, t, b)
            case List(Expr.Var(x), b) => (x, Expr.Sym("Type"), b)
            case _ => (null, null, null)
          }
          if (xVar != null) {
            val transportedU = Expr.App(Expr.Sym(Transport), List(Expr.App(Expr.Sym("λ"), List(Expr.Var(z), typeA)), p, u))
            // The v term needs to be transported to the new type B(y, transportedU)
            // This is more complex because of the dependency.
            Expr.App(Expr.Sym(Pair), List(
              transportedU,
              Expr.App(Expr.Sym(Transport), List(
                Expr.App(Expr.Sym("λ"), List(Expr.Var(z), Prover.substVar(bodyB, xVar, Expr.App(Expr.Sym(Transport), List(Expr.App(Expr.Sym("λ"), List(Expr.Var(z), typeA)), p, u))))), // This might be slightly off in the path argument
                p,
                v
              ))
            ))
          } else expr

        case _ => 
          // logger.log(s"Transport reduction incomplete for: $pred")
          expr
      }

    case Expr.App(Expr.Sym(t), List(_, Expr.App(Expr.Sym(r), _), x)) if t == Transport && r == Refl => x
    
    // --- Cubical Kan Operations ---
    // comp(A, refl, u0) -> u0
    case Expr.App(Expr.Sym(c), List(_, Expr.App(Expr.Sym(r), _), u0)) if c == Comp && r == Refl => u0
    // fill(A, refl, u0) -> refl (approximation)
    case Expr.App(Expr.Sym(f), List(a, Expr.App(Expr.Sym(r), List(x)), _)) if f == Fill && r == Refl =>
      Expr.App(Expr.Sym(Refl), List(x))

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
    case Expr.App(Expr.Sym("plus"), List(n, Expr.Sym(z))) if z == Zero || z == Initial => n
    case Expr.App(Expr.Sym("plus"), List(n, Expr.App(Expr.Sym(s), List(m)))) if s == Succ =>
      Expr.App(Expr.Sym(Succ), List(Expr.App(Expr.Sym("plus"), List(n, m))))
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym("plus"), List(a, b)), c)) =>
      Expr.App(Expr.Sym("plus"), List(a, Expr.App(Expr.Sym("plus"), List(b, c))))

    // --- リスト的演算 ---
    case Expr.App(Expr.Sym("append"), List(Expr.Sym("nil"), ys)) => ys
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), ys)) =>
      Expr.App(Expr.Sym("cons"), List(x, Expr.App(Expr.Sym("append"), List(xs, ys))))
    case Expr.App(Expr.Sym("append"), List(xs, Expr.Sym("nil"))) => xs
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("append"), List(xs, ys)), zs)) =>
      Expr.App(Expr.Sym("append"), List(xs, Expr.App(Expr.Sym("append"), List(ys, zs))))

    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("reverse"), List(t)))) => t
    case Expr.App(Expr.Sym("mirror"), List(Expr.App(Expr.Sym("mirror"), List(t)))) => t
    case Expr.App(Expr.Sym("reverse"), List(Expr.Sym("nil"))) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("reverse"), List(xs)), Expr.App(Expr.Sym("cons"), List(x, Expr.Sym("nil")))))
    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("append"), List(xs, ys)))) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("reverse"), List(ys)), Expr.App(Expr.Sym("reverse"), List(xs))))

    case Expr.App(Expr.Sym("map"), List(_, Expr.Sym("nil"))) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("map"), List(f, Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Expr.App(Expr.Sym("cons"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("map"), List(f, xs))))

    // --- ベクトルの演算 ---
    case Expr.App(Expr.Sym("vlength"), List(Expr.Sym("vnil"))) => Expr.Sym("0")
    case Expr.App(Expr.Sym("vlength"), List(Expr.App(Expr.Sym("vcons"), List(_, _, n)))) => Expr.App(Expr.Sym("S"), List(n))
    case Expr.App(Expr.Sym("vhead"), List(Expr.App(Expr.Sym("vcons"), List(x, _, _)))) => x
    case Expr.App(Expr.Sym("vappend"), List(Expr.Sym("vnil"), v)) => v
    case Expr.App(Expr.App(Expr.Sym("vappend"), List(Expr.App(Expr.Sym("vcons"), List(x, xs, n)), ys)), List(zs)) =>
      Expr.App(Expr.Sym("vappend"), List(Expr.App(Expr.Sym("vcons"), List(x, xs, n)), Expr.App(Expr.Sym("vappend"), List(ys, zs))))

    // --- ファンクタ・モナド ---
    case Expr.App(Expr.Sym("fmap"), List(_, Expr.Sym("nil"))) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("fmap"), List(f, Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Expr.App(Expr.Sym("cons"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("fmap"), List(f, xs))))
    case Expr.App(Expr.Sym("return"), List(x)) => Expr.App(Expr.Sym("cons"), List(x, Expr.Sym("nil")))
    case Expr.App(Expr.Sym("bind"), List(Expr.Sym("nil"), _)) => Expr.Sym("nil")
    case Expr.App(Expr.Sym("bind"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), f)) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("bind"), List(xs, f))))
    case Expr.App(Expr.Sym("bind"), List(m, Expr.Sym("return"))) => m
    
    // List Monad 特化
    case Expr.App(Expr.Sym("bind_list"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), f)) =>
      Expr.App(Expr.Sym("append"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("bind_list"), List(xs, f))))
    case Expr.App(Expr.Sym("bind_list"), List(m, Expr.Sym("return_list"))) => m
    case Expr.App(Expr.Sym("bind_list"), List(Expr.App(Expr.Sym("bind_list"), List(m, f)), g)) =>
      Expr.App(Expr.Sym("bind_list"), List(m, Expr.App(Expr.Sym("λ"), List(Expr.Var("x"), Expr.App(Expr.Sym("bind_list"), List(Expr.App(f, List(Expr.Var("x"))), g))))))

    // Maybe Monad 特化
    case Expr.App(Expr.Sym("bind_maybe"), List(Expr.Sym("nothing"), _)) => Expr.Sym("nothing")
    case Expr.App(Expr.Sym("bind_maybe"), List(Expr.App(Expr.Sym("just"), List(x)), f)) =>
      Expr.App(f, List(x))
    case Expr.App(Expr.Sym("bind_maybe"), List(m, Expr.Sym("return_maybe"))) => m
    case Expr.App(Expr.Sym("bind_maybe"), List(Expr.App(Expr.Sym("bind_maybe"), List(m, f)), g)) =>
      Expr.App(Expr.Sym("bind_maybe"), List(m, Expr.App(Expr.Sym("λ"), List(Expr.Var("x"), Expr.App(Expr.Sym("bind_maybe"), List(Expr.App(f, List(Expr.Var("x"))), g))))))

    case Expr.App(Expr.Sym("id"), List(x)) => x
    case Expr.App(Expr.Sym("vmap"), List(_, Expr.Sym("vnil"))) => Expr.Sym("vnil")
    case Expr.App(Expr.Sym("vmap"), List(f, Expr.App(Expr.Sym("vcons"), List(x, xs, n)))) =>
      Expr.App(Expr.Sym("vcons"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("vmap"), List(f, xs)), n))
    case Expr.App(Expr.App(Expr.Sym("compose"), List(f, g)), List(x)) =>
      Expr.App(f, List(Expr.App(g, List(x))))
    case Expr.App(Expr.Sym(Concat), List(Expr.App(Expr.Sym(Concat), List(p, q)), r)) =>
      Expr.App(Expr.Sym(Concat), List(p, Expr.App(Expr.Sym(Concat), List(q, r))))

    // --- 否定の展開 ---
    case Expr.App(Expr.Sym(Not), List(a)) => Expr.App(Expr.Sym(Implies), List(a, Expr.Sym(False)))

    // --- 論理的簡約 ---
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

    // --- ストリーム的演算 ---
    case Expr.App(Expr.Sym("bisim"), List(s1, s2)) =>
      Expr.App(Expr.Sym(Globally), List(Expr.App(Expr.Sym(Eq), List(Expr.App(Expr.Sym("head"), List(s1)), Expr.App(Expr.Sym("head"), List(s2))))))
    case Expr.App(Expr.Sym("head"), List(Expr.App(Expr.Sym("cons_stream"), List(x, _)))) => x
    case Expr.App(Expr.Sym("tail"), List(Expr.App(Expr.Sym("cons_stream"), List(_, s)))) => s
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
}
