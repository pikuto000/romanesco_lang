// ==========================================
// Unifier.scala
// 単一化アルゴリズム（Appラムダ対応）
// ==========================================

package romanesco.Solver.core
import romanesco.Utils.Debug.logger
import LogicSymbols._

object Unifier:
  type Subst = Map[MetaId, Expr]
  def emptySubst: Subst = Map.empty

  def applySubst(e: Expr, s: Subst): Expr =
    if (s.isEmpty) e
    else e match
      case Expr.Meta(id) if s.contains(id) =>
        applySubst(s(id), s)
      case Expr.App(f, args) =>
        val nextF = applySubst(f, s)
        val nextArgs = args.map(applySubst(_, s))
        // 実際に変化があった場合のみ正規化（またはアプリケーションの場合は常に一回正規化）
        Rewriter.normalize(Expr.App(nextF, nextArgs))
      case _ => e

  def applySubstToRule(rule: CatRule, s: Subst): CatRule =
    logger.log(s"applySubstToRule: $rule, $s")
    CatRule(
      rule.name,
      applySubst(rule.lhs, s),
      applySubst(rule.rhs, s),
      rule.universals.map(applySubst(_, s))
    )

  def unify(e1: Expr, e2: Expr, subst: Subst): LazyList[Subst] =
    val r1 = applySubst(e1, subst)
    val r2 = applySubst(e2, subst)

    if r1 == r2 then
      logger.log(s"Unify Success: $r1 = $r2")
      LazyList(subst)
    else
      logger.log(s"Unifying: $r1 with $r2")
      val res = (r1, r2) match
        // --- ラムダ抽象の単一化 (App構造) ---
        case (Expr.Lam(v1, b1), Expr.Lam(v2, b2)) =>
          val newB2 = Prover.substVar(b2, v2, Expr.Var(v1))
          unify(b1, newB2, subst)

        // --- メタ変数同士のアプリケーション ---
        case (Expr.App(Expr.Meta(id1), args1), Expr.App(Expr.Meta(id2), args2))
            if id1 == id2 && args1.length == args2.length =>
          args1.zip(args2).foldLeft(LazyList(subst)) { case (sList, (a1, a2)) =>
            sList.flatMap(unify(a1, a2, _))
          }

        // --- Strict Universe Level Unification (Russell Paradox Avoidance) ---
        case (
              Expr.App(Expr.Sym(LogicSymbols.Type), args1),
              Expr.App(Expr.Sym(LogicSymbols.Type), args2)
            ) =>
          def getLevelDepth(e: Expr): Int = e match {
            case Expr.Meta(id) => id.ids.length
            case _             => -1 // Unknown/Var
          }
          val l1 = args1.headOption.map(getLevelDepth).getOrElse(-1)
          val l2 = args2.headOption.map(getLevelDepth).getOrElse(-1)

          if (l1 != -1 && l2 != -1 && l1 != l2) LazyList.empty
          else {
            unify(
              Expr.Sym(LogicSymbols.Type),
              Expr.Sym(LogicSymbols.Type),
              subst
            ).flatMap { s =>
              args1.zip(args2).foldLeft(LazyList(s)) {
                case (sList, (arg1, arg2)) =>
                  sList.flatMap(unify(arg1, arg2, _))
              }
            }
          }

        // --- 高階パターン単一化 (一般化) ---
        case (Expr.App(Expr.Meta(id), args), t) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else if (!occursCheckHigher(id, t, args))
            solveHigherOrder(id, args, t, subst)
          else LazyList.empty

        case (t, Expr.App(Expr.Meta(id), args)) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else if (!occursCheckHigher(id, t, args))
            solveHigherOrder(id, args, t, subst)
          else LazyList.empty

        // --- 基本的なメタ変数の単一化 ---
        case (Expr.Meta(id), t) if !occursCheckHigher(id, t, Nil) =>
          LazyList(subst + (id -> t))
        case (t, Expr.Meta(id)) if !occursCheckHigher(id, t, Nil) =>
          LazyList(subst + (id -> t))

        // --- 変数の単一化 ---
        case (Expr.Var(n1), Expr.Var(n2)) if n1 == n2 => LazyList(subst)

        // --- アプリケーションの単一化 (分解) ---
        case (Expr.App(h1, a1), Expr.App(h2, a2)) if a1.length == a2.length =>
          unify(h1, h2, subst).flatMap { s =>
            a1.zip(a2).foldLeft(LazyList(s)) { case (sList, (arg1, arg2)) =>
              sList.flatMap(unify(arg1, arg2, _))
            }
          }

        case _ => LazyList.empty

      res

  private def solveHigherOrder(
      id: MetaId,
      args: List[Expr],
      target: Expr,
      subst: Subst
  ): LazyList[Subst] = {
    val vars = args.indices.map(i => s"v_$i").toList
    val argFreeVars = args.flatMap(collectFreeVars).toSet

    // 1. まずターゲット内のメタ変数をPruningする
    def performPruning(t: Expr, s: Subst): LazyList[Subst] = {
      val freeVars = collectFreeVars(applySubst(t, s))
      val outOfScope = freeVars -- argFreeVars
      if (outOfScope.isEmpty) LazyList(s)
      else {
        def findAndPrune(e: Expr, currentS: Subst): LazyList[Subst] = applySubst(e, currentS) match {
          case Expr.App(Expr.Meta(mid), mArgs) =>
            val invalidIndices = mArgs.indices.filter(i => collectFreeVars(applySubst(mArgs(i), currentS)).exists(outOfScope.contains))
            if (invalidIndices.nonEmpty) {
              val mVars = mArgs.indices.map(i => s"mv_$i").toList
              val prunedBody = Expr.App(Expr.Meta(MetaId(mid.ids :+ 999)), 
                mArgs.indices.filterNot(invalidIndices.contains).map(i => Expr.Var(mVars(i))).toList)
              val lambda = mVars.foldRight(prunedBody) { (name, b) =>
                Expr.App(Expr.Sym("λ"), List(Expr.Var(name), b))
              }
              val nextS = currentS + (mid -> lambda)
              performPruning(t, nextS)
            } else {
              mArgs.foldLeft(LazyList(currentS)) { (sl, arg) => sl.flatMap(s2 => findAndPrune(arg, s2)) }
            }
          case Expr.App(f, as) =>
            (f :: as).foldLeft(LazyList(currentS)) { (sl, child) => sl.flatMap(s2 => findAndPrune(child, s2)) }
          case _ => LazyList(currentS)
        }
        findAndPrune(t, s)
      }
    }

    // 2. Pruningを実行してから抽象化を行う
    performPruning(target, subst).flatMap { s =>
      val currentTarget = applySubst(target, s)
      
      def abstractAll(t: Expr): LazyList[Expr] = {
        val matchingIndices = args.indices.filter(i => args(i) == t)
        val replacements = matchingIndices.to(LazyList).map(i => Expr.Var(vars(i)))
        
        val recursive = t match {
          case Expr.App(f, as) =>
            for {
              nextF <- abstractAll(f)
              nextArgs <- as.foldRight(LazyList(List.empty[Expr])) { (a, acc) =>
                for {
                  nextA <- abstractAll(a)
                  rest <- acc
                } yield nextA :: rest
              }
            } yield Expr.App(nextF, nextArgs)
          case Expr.Meta(_) => LazyList(t)
          case _ => LazyList(t)
        }
        (replacements #::: recursive).distinct
      }

      abstractAll(currentTarget).flatMap { body =>
        val freeInBody = collectFreeVars(body) -- vars.toSet
        if (freeInBody.subsetOf(argFreeVars)) {
          val lambda = vars.foldRight(body) { (name, b) =>
            Expr.App(Expr.Sym("λ"), List(Expr.Var(name), b))
          }
          LazyList(s + (id -> lambda))
        } else {
          LazyList.empty
        }
      }
    }
  }

  private def collectFreeVars(e: Expr): Set[String] = e match {
    case Expr.Var(n) => Set(n)
    case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) => collectFreeVars(body) - v
    case Expr.App(f, as) => collectFreeVars(f) ++ as.flatMap(collectFreeVars)
    case _ => Set.empty
  }

  private def occursCheckHigher(
      metaId: MetaId,
      expr: Expr,
      args: List[Expr]
  ): Boolean = {
    def collectMetaVars(e: Expr): Set[MetaId] = e match {
      case Expr.Meta(id)   => Set(id)
      case Expr.App(f, as) => collectMetaVars(f) ++ as.flatMap(collectMetaVars)
      case _               => Set.empty
    }

    if (collectMetaVars(expr).contains(metaId)) return true

    if (args.nonEmpty) {
      val exprVars = collectFreeVars(expr)
      val argVars = args.flatMap(collectFreeVars).toSet
      
      // Pruningが可能な場合（メタ変数アプリケーションを含む場合）は失敗させない
      def canPrune(e: Expr): Boolean = e match {
        case Expr.App(Expr.Meta(_), _) => true
        case Expr.App(f, as) => canPrune(f) || as.exists(canPrune)
        case _ => false
      }

      if (exprVars.exists(v => !argVars.contains(v)) && !canPrune(expr)) {
        return true
      }
    }
    
    false
  }

  private def occursCheckVar(varName: String, expr: Expr): Boolean = expr match
    case Expr.Var(n) => n == varName
    case Expr.App(h, args) =>
      occursCheckVar(varName, h) || args.exists(occursCheckVar(varName, _))
    case _ => false

  private def occursCheck(metaId: MetaId, expr: Expr): Boolean = expr match
    case Expr.Meta(id)     => id == metaId
    case Expr.App(h, args) =>
      occursCheck(metaId, h) || args.exists(occursCheck(metaId, _))
    case _ => false
