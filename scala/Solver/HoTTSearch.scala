// ==========================================
// HoTTSearch.scala
// HoTT (Homotopy Type Theory) 固有の探索ロジック
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._

trait HoTTSearch { self: Prover =>
  import Expr._
  import Unifier._

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
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    List(searchPathInduction(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history))
  }

  private[core] def searchPathInduction(
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
    if (inductionCount >= config.maxInduction) SolveTree.Failure()
    else {
      val piOptions = context.indices.flatMap { i =>
        context(i) match {
          case (pName, Expr.App(Expr.Sym(Path), List(_, x, Expr.Var(yName))))
              if x != Expr.Var(yName) =>
            val reflX = Expr.App(Expr.Sym(Refl), List(x))
            val substGoal =
              Prover.substVar(Prover.substVar(goal, yName, x), pName, reflX)
            val nextCtx = context
              .patch(i, Nil, 1)
              .map(h =>
                (
                  h._1,
                  Prover.substVar(Prover.substVar(h._2, yName, x), pName, reflX)
                )
              )
            List(
              search(
                substGoal,
                rules,
                nextCtx,
                linearContext,
                subst,
                depth + 1,
                limit,
                visited,
                raaCount,
                inductionCount + 1,
                guarded,
                history
              )
                .map { case (t, s, restL) =>
                  (
                    ProofTree.Node(
                      applySubst(goal, s),
                      s"path-induction[$pName]",
                      List(t)
                    ),
                    s,
                    restL
                  )
                }
            )
          case _ => Nil
        }
      }
      SolveTree.merge(piOptions)
    }
  }

  private[core] def getPathLevel(e: Expr): Int = e match {
    case Expr.App(Expr.Sym(Path), List(_, x, y)) => 1 + getPathLevel(x)
    case _                                       => 0
  }
}
