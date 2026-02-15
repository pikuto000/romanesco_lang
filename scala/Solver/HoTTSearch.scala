// ==========================================
// HoTTSearch.scala
// HoTT (Homotopy Type Theory) 固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class HoTTPlugin extends LogicPlugin {
  override def name: String = "HoTT"

  import Unifier._

  type Context = List[(String, Expr)]

  override def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    searchPathInduction(exprs, rules, context, state, subst, depth, limit, visited, guarded, prover)
  }

  private def searchPathInduction(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    if (state.inductionCount >= prover.config.maxInduction) Vector.empty
    else {
      context.indices.flatMap { i =>
        prover.checkDeadline()
        context(i) match {
          case (pName, Expr.App(Expr.Sym(Path), List(_, x, Expr.Var(yName)))) if x != Expr.Var(yName) =>
            val reflX = Expr.App(Expr.Sym(Refl), List(x))
            val substGoal = Prover.substVar(Prover.substVar(goal, yName, x), pName, reflX)
            val nextCtx = context.patch(i, Nil, 1).map(h => (h._1, Prover.substVar(Prover.substVar(h._2, yName, x), pName, reflX)))
            val subTree = prover.search(exprs :+ substGoal, nextCtx, state.incInduction, subst, depth + 1, limit, visited, guarded)
            allSuccesses(subTree).map { s =>
              val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"path-induction[$pName]", List(s.result.toOption.get.tree))))
              Tree.V(SearchNode(exprs :+ substGoal, s"path-induction[$pName]", depth, result, s.subst, s.context, s.linearContext), Vector(subTree))
            }
          case _ => Vector.empty
        }
      }.toVector
    }
  }
}
