// ==========================================
// HoareLogicSearch.scala
// Hoare論理の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree

class HoareLogicPlugin extends LogicPlugin {
  override def name: String = "HoareLogic"

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
    goal match {
      case Expr.App(Expr.Sym("triple"), List(p, c, q)) =>
        searchHoare(exprs, p, c, q, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, prover)
      case _ => Vector.empty
    }
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }

  private def searchHoare(
      exprs: Vector[Expr],
      p: Expr,
      c: Expr,
      q: Expr,
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
    val syntaxDirected: Vector[Tree[SearchNode]] = c match {
      case Expr.Sym("skip") =>
        val nextGoal = Expr.App(Expr.Sym(Implies), List(p, q))
        val subTree = prover.search(exprs :+ nextGoal, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        val success = findSuccess(subTree)
        val result = success match {
          case Some(s) => Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "hoare-skip", List(s.result.toOption.get.tree))))
          case None => Left(FailTrace(Goal(context, linearContext, goal), "hoare-skip failed", depth))
        }
        Vector(Tree.V(SearchNode(exprs :+ nextGoal, "hoare-skip", depth, result, success.map(_.subst).getOrElse(subst), success.map(_.context).getOrElse(context), success.map(_.linearContext).getOrElse(linearContext)), Vector(subTree)))

      case Expr.App(Expr.Sym(":="), List(targetVar, e)) =>
        val varName = targetVar match {
          case Expr.Var(n) => n
          case Expr.Sym(n) => n
          case _ => null
        }
        if (varName != null) {
          val substitutedQ = Prover.substVar(q, varName, e)
          val nextGoal = Expr.App(Expr.Sym(Implies), List(p, substitutedQ))
          val subTree = prover.search(exprs :+ nextGoal, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          val success = findSuccess(subTree)
          val result = success match {
            case Some(s) => Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "hoare-assign", List(s.result.toOption.get.tree))))
            case None => Left(FailTrace(Goal(context, linearContext, goal), "hoare-assign failed", depth))
          }
          Vector(Tree.V(SearchNode(exprs :+ nextGoal, "hoare-assign", depth, result, success.map(_.subst).getOrElse(subst), success.map(_.context).getOrElse(context), success.map(_.linearContext).getOrElse(linearContext)), Vector(subTree)))
        } else Vector.empty

      case Expr.App(Expr.Sym(";"), List(c1, c2)) =>
        val midR = prover.freshMeta(depth)
        val goal1 = Expr.App(Expr.Sym("triple"), List(p, c1, midR))
        val tree1 = prover.search(exprs :+ goal1, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        
        findSuccess(tree1).toVector.flatMap { s1 =>
          val goal2 = Expr.App(Expr.Sym("triple"), List(applySubst(midR, s1.subst), c2, q))
          val tree2 = prover.search(exprs :+ goal2, s1.context, s1.linearContext, s1.subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          val success2 = findSuccess(tree2)
          val result = success2 match {
            case Some(s2) => Right(ProofResult(ProofTree.Node(applySubst(goal, s2.subst), "hoare-seq", List(s1.result.toOption.get.tree, s2.result.toOption.get.tree))))
            case None => Left(FailTrace(Goal(context, linearContext, goal), "hoare-seq failed", depth))
          }
          Vector(Tree.V(SearchNode(exprs :+ goal2, "hoare-seq", depth, result, success2.map(_.subst).getOrElse(s1.subst), success2.map(_.context).getOrElse(s1.context), success2.map(_.linearContext).getOrElse(s1.linearContext)), Vector(tree1, tree2)))
        }

      case _ => Vector.empty
    }

    syntaxDirected
  }
}
