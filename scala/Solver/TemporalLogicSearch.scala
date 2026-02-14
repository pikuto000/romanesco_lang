// ==========================================
// TemporalLogicSearch.scala
// 時相論理固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Types.Tree

class TemporalLogicPlugin extends LogicPlugin {
  override def name: String = "TemporalLogic"

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
    val goal = exprs.last
    goal.headSymbol match {
      case Globally | Next =>
        val a = goal match {
          case Expr.App(_, List(p)) => p
          case _ => return Vector.empty
        }
        searchTemporalGoal(exprs, goal, a, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, prover)
      case _ => Vector.empty
    }
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }

  private def searchTemporalGoal(
      exprs: Vector[Expr],
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
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    goal.headSymbol match {
      case Globally =>
        // G(A) -> A AND X(G(A))
        val expansion = Expr.App(Expr.Sym(And), List(a, Expr.App(Expr.Sym(Next), List(goal))))
        val subTree = prover.search(exprs :+ expansion, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        val success = findSuccess(subTree)
        val result = success match {
          case Some(s) => Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "G-expansion", List(s.result.toOption.get.tree))))
          case None => Left(FailTrace(Goal(context, linearContext, goal), "G-expansion failed", depth))
        }
        Vector(Tree.V(SearchNode(exprs :+ expansion, "G-expansion", depth, result, success.map(_.subst).getOrElse(subst), success.map(_.context).getOrElse(context), success.map(_.linearContext).getOrElse(linearContext)), Vector(subTree)))
      
      case Next =>
        // X(A) -> ステップを進めて A を証明
        val nextCtx = context.flatMap {
          case (n, Expr.App(Expr.Sym(Globally), List(p))) => Some((n, Expr.App(Expr.Sym(Globally), List(p))))
          case (n, Expr.App(Expr.Sym(Next), List(p))) => Some((s"next:$n", p))
          case _ => None
        }
        val subTree = prover.search(exprs :+ a, nextCtx, Nil, subst, depth + 1, limit, visited, raaCount, inductionCount, true)
        val success = findSuccess(subTree)
        val result = success match {
          case Some(s) => Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "next-step", List(s.result.toOption.get.tree))))
          case None => Left(FailTrace(Goal(context, linearContext, goal), "next-step failed", depth))
        }
        Vector(Tree.V(SearchNode(exprs :+ a, "next-step", depth, result, success.map(_.subst).getOrElse(subst), success.map(_.context).getOrElse(nextCtx), success.map(_.linearContext).getOrElse(Nil)), Vector(subTree)))
      case _ => Vector.empty
    }
  }

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
    val goal = exprs.last
    context.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(Globally), List(a))), i) =>
        // G(A) -> A と X(G(A)) を直接文脈に追加
        val nextG = Expr.App(Expr.Sym(Next), List(Expr.App(Expr.Sym(Globally), List(a))))
        val newCtx = context.patch(i, List((s"$name.now", a), (s"$name.next", nextG)), 1)
        val subTree = prover.search(exprs, newCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        findSuccess(subTree).map { s =>
          Tree.V(SearchNode(exprs, s"G-elim[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"G-elim[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }
      case _ => None
    }.toVector
  }
}
