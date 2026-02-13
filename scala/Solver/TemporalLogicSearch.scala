// ==========================================
// TemporalLogicSearch.scala
// 時相論理固有の探索ロジック
// ==========================================

package romanesco.Solver.core

import LogicSymbols._

trait TemporalLogicSearch { self: Prover =>
  import Expr._
  import Unifier._

  override def getGoalHooks(
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
    goal.headSymbol match {
      case Globally | Next =>
        val a = goal match {
          case Expr.App(_, List(p)) => p
          case _ => return Nil
        }
        List(searchTemporalGoal(goal, a, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history))
      case _ => Nil
    }
  }

  /** 時相論理の目標分解 (Globally / Next) */
  private[core] def searchTemporalGoal(
      goal: Expr,
      a: Expr,
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
    goal.headSymbol match {
      case Globally =>
        val expansion =
          Expr.App(Expr.Sym(And), List(a, Expr.App(Expr.Sym(Next), List(goal))))
        search(
          expansion,
          rules,
          context,
          linearContext,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )
          .map { case (p, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "G-expansion", List(p)),
              s,
              restL
            )
          }
      case Next =>
        val nextCtx = context.flatMap {
          case (n, Expr.App(Expr.Sym(Globally), List(p))) =>
            Some((n, Expr.App(Expr.Sym(Globally), List(p))))
          case (n, Expr.App(Expr.Sym(Next), List(p))) => Some((s"next:$n", p))
          case _                                      => None
        }
        search(
          a,
          rules,
          nextCtx,
          Nil,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          true,
          history
        )
          .map { case (p, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "next-step", List(p)),
              s,
              restL
            )
          }
      case _ => SolveTree.Failure
    }
  }
}
