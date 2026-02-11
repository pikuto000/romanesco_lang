// ==========================================
// PersistentLogicSearch.scala
// 標準論理・持続的文脈の探索ロジック
// ==========================================

package romanesco.Solver.core

import LogicSymbols._

trait PersistentLogicSearch { self: Prover =>
  import Expr._
  import Unifier._

  /** 標準的な含意(→)の適用 */
  private[core] def searchApply(
      name: String,
      goal: Expr,
      a: Expr,
      b: Expr,
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
      a,
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
        (s"$name.res", b) :: context,
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

  /** 持続的文脈の分解 (∧ / ∨) */
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
}
