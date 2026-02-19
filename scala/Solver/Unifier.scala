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
      // logger.log(s"Unify Success: $r1 = $r2")
      LazyList(subst)
    else
      // logger.log(s"Unifying: $r1 with $r2")
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

          // Cumulative universes: Type(n) : Type(n+1)
          // If l1 < l2, then Type(l1) can be considered a subtype of Type(l2)
          // But for strict equality unification, we check equality.
          if (l1 != -1 && l2 != -1 && l1 != l2) {
            // logger.log(s"[UNIFY] Universe level mismatch: Type$l1 vs Type$l2")
            // Support simple subtyping/cumulativity if needed, 
            // but for now we keep it strict unless one is a meta-variable.
            LazyList.empty
          }
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
          else solveHigherOrder(id, args, t, subst)

        case (t, Expr.App(Expr.Meta(id), args)) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else solveHigherOrder(id, args, t, subst)

        // --- 基本的なメタ変数の単一化 ---
        case (Expr.Meta(id), t) =>
          if (occursCheck(id, t)) {
            // logger.log(s"[UNIFY FAIL] Occurs check failed for ?$id in $t")
            LazyList.empty
          } else LazyList(subst + (id -> t))
        case (t, Expr.Meta(id)) =>
          if (occursCheck(id, t)) {
            // logger.log(s"[UNIFY FAIL] Occurs check failed for ?$id in $t")
            LazyList.empty
          } else LazyList(subst + (id -> t))

        // --- 変数の単一化 ---
        case (Expr.Var(n1), Expr.Var(n2)) if n1 == n2 => LazyList(subst)
        case (Expr.Var(n1), Expr.Var(n2)) => 
          logger.log(s"[UNIFY FAIL] Variable mismatch: $n1 != $n2")
          LazyList.empty

        // --- シンボルの不一致 ---
        case (Expr.Sym(s1), Expr.Sym(s2)) if s1 != s2 =>
          logger.log(s"[UNIFY FAIL] Symbol mismatch: $s1 != $s2")
          LazyList.empty

        // --- アプリケーションの単一化 (分解) ---
        case (Expr.App(h1, a1), Expr.App(h2, a2)) if a1.length == a2.length =>
          unify(h1, h2, subst).flatMap { s =>
            a1.zip(a2).foldLeft(LazyList(s)) { case (sList, (arg1, arg2)) =>
              sList.flatMap(unify(arg1, arg2, _))
            }
          }
        
        case (Expr.App(h1, a1), Expr.App(h2, a2)) =>
          logger.log(s"[UNIFY FAIL] Argument length mismatch: $r1 vs $r2")
          LazyList.empty

        case _ => 
          logger.log(s"[UNIFY FAIL] Incompatible structures: $r1 (${r1.getClass.getSimpleName}) vs $r2 (${r2.getClass.getSimpleName})")
          LazyList.empty

      res

  private def solveHigherOrder(
      id: MetaId,
      args: List[Expr],
      target: Expr,
      subst: Subst
  ): LazyList[Subst] = {
    val vars = args.indices.map(i => s"bv$i").toList // Use bv for consistency with canonicalize
    val argFreeVars = args.flatMap(collectFreeVars).toSet

    // 1. まずターゲット内のメタ変数をPruningする
    // target 内のメタ変数が、id の args に含まれない自由変数に依存している場合、
    // その依存関係を解消（Pruning）しようと試みる。
    def performPruning(t: Expr, s: Subst): LazyList[(Expr, Subst)] = {
      val currentT = applySubst(t, s)
      val freeInT = collectFreeVars(currentT)
      val outOfScope = freeInT -- argFreeVars
      
      if (outOfScope.isEmpty) LazyList((currentT, s))
      else {
        // メタ変数アプリケーションを探して Pruning する
        def findAndPrune(e: Expr, currentS: Subst): LazyList[(Expr, Subst)] = {
          val actualE = applySubst(e, currentS)
          actualE match {
            case Expr.App(Expr.Meta(mid), mArgs) if mid != id =>
              // このメタ変数の引数のうち、outOfScope に含まれるものを削除した新しいメタ変数を生成する
              val mVars = mArgs.indices.map(i => s"mv$i").toList
              val validIndices = mArgs.indices.filter { i =>
                val argVars = collectFreeVars(applySubst(mArgs(i), currentS))
                (argVars intersect outOfScope).isEmpty
              }
              
              if (validIndices.length < mArgs.length) {
                val newMid = MetaId(mid.ids :+ 888) // Pruned ID
                val prunedBody = Expr.App(Expr.Meta(newMid), validIndices.map(i => Expr.Var(mVars(i))).toList)
                val lambda = mVars.foldRight(prunedBody) { (name, b) =>
                  Expr.App(Expr.Sym("λ"), List(Expr.Var(name), b))
                }
                val nextS = currentS + (mid -> lambda)
                performPruning(t, nextS)
              } else {
                // 引数自体を再帰的にチェック
                mArgs.foldLeft(LazyList((actualE, currentS))) { (acc, arg) =>
                  acc.flatMap { case (_, s) => findAndPrune(arg, s) }
                }
              }
            case Expr.App(f, as) =>
              (f :: as).foldLeft(LazyList((actualE, currentS))) { (acc, child) =>
                acc.flatMap { case (_, s) => findAndPrune(child, s) }
              }
            case _ => LazyList((actualE, currentS))
          }
        }
        findAndPrune(currentT, s)
      }
    }

    // 2. Pruningを実行してから抽象化を行う
    performPruning(target, subst).flatMap { case (prunedTarget, s) =>
      if (occursCheck(id, prunedTarget)) return LazyList.empty

      def abstractAll(t: Expr): LazyList[Expr] = {
        // args と一致する部分を対応する変数に置き換える
        val matchingIndices = args.indices.filter(i => applySubst(args(i), s) == t)
        if (matchingIndices.nonEmpty) {
          LazyList(Expr.Var(vars(matchingIndices.head)))
        } else {
          t match {
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
            case _ => LazyList(t)
          }
        }
      }

      abstractAll(prunedTarget).flatMap { body =>
        val freeInBody = collectFreeVars(body) -- vars.toSet
        if (freeInBody.subsetOf(argFreeVars)) {
          val lambda = vars.foldRight(body) { (name, b) =>
            Expr.App(Expr.Sym("λ"), List(Expr.Var(name), b))
          }
          LazyList(s + (id -> lambda))
        } else {
          // Pruning 後もスコープ外の変数が残っている場合は失敗
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
    // 互換性のために残すが、基本的には solveHigherOrder 内でチェックする
    occursCheck(metaId, expr)
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
