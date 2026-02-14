// ==========================================
// HoTTSearch.scala
// HoTT (Homotopy Type Theory) 固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree

class HoTTPlugin extends LogicPlugin {
  override def name: String = "HoTT"

  import Unifier._

  type Context = List[(String, Expr)]

  override def getGoalHooks(
      exprs: Vector[Expr],
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
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    searchPathInduction(exprs, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, prover)
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }

  private def searchPathInduction(
      exprs: Vector[Expr],
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
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    if (inductionCount >= prover.config.maxInduction) Vector.empty
    else {
      context.indices.flatMap { i =>
        context(i) match {
          case (pName, Expr.App(Expr.Sym(Path), List(_, x, Expr.Var(yName)))) if x != Expr.Var(yName) =>
            val reflX = Expr.App(Expr.Sym(Refl), List(x))
            val substGoal = Prover.substVar(Prover.substVar(goal, yName, x), pName, reflX)
            val nextCtx = context.patch(i, Nil, 1).map(h => (h._1, Prover.substVar(Prover.substVar(h._2, yName, x), pName, reflX)))
            val subTree = prover.search(exprs :+ substGoal, nextCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount + 1, guarded)
            val success = findSuccess(subTree)
            val result = success match {
              case Some(s) => Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"path-induction[$pName]", List(s.result.toOption.get.tree))))
              case None => Left(FailTrace(Goal(context, linearContext, goal), s"path-induction[$pName] failed", depth))
            }
            Some(Tree.V(SearchNode(exprs :+ substGoal, s"path-induction[$pName]", depth, result, success.map(_.subst).getOrElse(subst), success.map(_.context).getOrElse(nextCtx), success.map(_.linearContext).getOrElse(linearContext)), Vector(subTree)))
          case _ => None
        }
      }.toVector
    }
  }
}
