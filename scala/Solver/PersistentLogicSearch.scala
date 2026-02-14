// ==========================================
// PersistentLogicSearch.scala
// 標準論理・持続的文脈の探索ロジック（ベース実装版） (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Types.Tree

class PersistentLogicPlugin extends LogicPlugin {
  override def name: String = "PersistentLogic"

  import Unifier._

  type Context = List[(String, Expr)]

  override def getContextHooks(
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
    searchPersistentDecomposeContext(exprs, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, prover)
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }

  private def searchPersistentDecomposeContext(
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
    context.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        val newCtx = context.patch(i, List((s"$name.1", a), (s"$name.2", b)), 1)
        val subTree = prover.search(exprs, newCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        val success = findSuccess(subTree)
        val result = success match {
          case Some(s) => Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"destruct[$name]", List(s.result.toOption.get.tree))))
          case None => Left(FailTrace(Goal(context, linearContext, goal), s"destruct[$name] failed", depth))
        }
        Some(Tree.V(SearchNode(exprs, s"destruct[$name]", depth, result, success.map(_.subst).getOrElse(subst), success.map(_.context).getOrElse(linearContext), success.map(_.linearContext).getOrElse(linearContext)), Vector(subTree)))
      case _ => None
    }.toVector
  }
}
