// ==========================================
// PersistentLogicSearch.scala
// 標準論理・持続的文脈の探索ロジック（ベース実装版）
// ==========================================

package romanesco.Solver.core

import scala.collection.concurrent.TrieMap
import LogicSymbols._

trait PersistentLogicSearch { self: Prover =>
  import Expr._
  import Unifier._

  private val globalFailureCache = TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()
  private val globalLemmaCache = TrieMap[(Expr, Set[Expr], List[Expr]), ProofTree]()

  def clearCaches(): Unit = {
    globalFailureCache.clear()
    globalLemmaCache.clear()
  }

  def checkLemma(goal: Expr, context: Set[Expr], linear: List[Expr]): Option[ProofTree] = {
    globalLemmaCache.get((goal, context, linear))
  }

  def recordLemma(goal: Expr, context: Set[Expr], linear: List[Expr], tree: ProofTree): Unit = {
    globalLemmaCache.putIfAbsent((goal, context, linear), tree)

    // Proof Mining: ゴールが等式であり、かつ文脈に依存しない（一般的な）性質であれば、動的ルールとして登録
    if (context.isEmpty && linear.isEmpty) {
      goal match {
        case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
          val (lhs, rhs) = args match {
            case List(_, l, r) if goal.headSymbol == Path => (l, r)
            case List(l, r) => (l, r)
            case _ => (null, null)
          }
          if (lhs != null && rhs != null && goal.complexity > 5) {
            val vars = Prover.collectVars(goal).toList.map(Expr.Var(_))
            val rule = CatRule(s"mined_${java.util.UUID.randomUUID().toString.take(4)}", lhs, rhs, vars)
            self.addDynamicRule(rule)
          }
        case _ =>
      }
    }
  }

  def checkGlobalFailure(goal: Expr, context: Set[Expr], linear: List[Expr], guarded: Boolean, remainingDepth: Int): Boolean = {
    globalFailureCache.get((goal, context, linear, guarded)).exists(_ >= remainingDepth)
  }

  def recordGlobalFailure(goal: Expr, context: Set[Expr], linear: List[Expr], guarded: Boolean, remainingDepth: Int): Unit = {
    val key = (goal, context, linear, guarded)
    def update(): Unit = {
      val current = globalFailureCache.get(key)
      if (current.isEmpty || remainingDepth > current.get) {
        if (current.isEmpty) {
          if (globalFailureCache.putIfAbsent(key, remainingDepth).isDefined) update()
        } else {
          if (!globalFailureCache.replace(key, current.get, remainingDepth)) update()
        }
      }
    }
    update()
  }

  def getGoalHooks(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): List[SolveTree[(ProofTree, Subst, Context)]] = Nil

  def getContextHooks(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    List(
      searchPersistentDecomposeContext(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history),
      searchRulesInContext(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history),
      searchRulesInPersistentContext(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history)
    )
  }

  private[core] def searchApply(
      name: String,
      goal: Expr,
      ant: Expr,
      cons: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    search(
      ant,
      rules,
      context,
      Nil,
      subst,
      depth + 1,
      limit,
      visited,
      raaCount,
      inductionCount,
      guarded,
      history
    ).flatMap { case (tA, s1, _) =>
      search(
        goal,
        rules,
        (s"$name.res", cons) :: context,
        linearContext,
        s1,
        depth + 1,
        limit,
        visited,
        raaCount,
        inductionCount,
        guarded,
        history
      ).map { case (tGoal, s2, restL) =>
        (
          ProofTree.Node(
            applySubst(goal, s2),
            s"apply[$name]",
            List(tGoal, tA)
          ),
          s2,
          restL
        )
      }
    }
  }

  private[core] def searchPersistentDecomposeContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val persistent = context.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        val newCtx = context.patch(i, List((s"$name.1", a), (s"$name.2", b)), 1)
        List(
          search(
            goal,
            rules,
            newCtx,
            linearContext,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          ).map { case (t, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), s"destruct[$name]", List(t)),
              s,
              restL
            )
          }
        )
      case _ => Nil
    }
    SolveTree.merge(persistent)
  }

  private[core] def searchRulesInContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val ctxRuleTrees = linearContext.zipWithIndex.flatMap {
      case ((name, hyp), i) =>
        applyRules(hyp, subst, depth, false).map {
          case (rewritten, univs, ruleName, nextS) =>
            val finalHyp = Rewriter.normalize(rewritten)
            if (finalHyp != hyp) {
              val nextLinear = linearContext.patch(i, List((name, finalHyp)), 1)
              search(
                goal,
                rules,
                context,
                nextLinear,
                nextS,
                depth + 1,
                limit,
                visited,
                raaCount,
                inductionCount,
                guarded,
                history
              )
                .map { case (t, s, restL) =>
                  (
                    ProofTree.Node(
                      applySubst(goal, s),
                      s"$ruleName[$name]",
                      List(t)
                    ),
                    s,
                    restL
                  )
                }
            } else SolveTree.Failure
        }
    }
    SolveTree.merge(ctxRuleTrees)
  }

  private[core] def searchRulesInPersistentContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val ctxRuleTrees = context.zipWithIndex.flatMap { case ((name, hyp), i) =>
      applyRules(hyp, subst, depth, false).map {
        case (rewritten, univs, ruleName, nextS) =>
          val finalHyp = Rewriter.normalize(rewritten)
          if (finalHyp != hyp) {
            val nextCtx = context.patch(i, List((name, finalHyp)), 1)
            search(
              goal,
              rules,
              nextCtx,
              linearContext,
              nextS,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              guarded,
              history
            )
              .map { case (t, s, restL) =>
                (
                  ProofTree.Node(
                    applySubst(goal, s),
                    s"$ruleName[$name]",
                    List(t)
                  ),
                  s,
                  restL
                )
              }
          } else SolveTree.Failure
      }
    }
    SolveTree.merge(ctxRuleTrees)
  }
}
