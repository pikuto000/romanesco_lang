// ==========================================
// StandardPlugins.scala
// 基本公理・導入ルール・ユーザー定義ルールのプラグイン化
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.LogicSymbols._
import romanesco.Types.Tree
import scala.collection.mutable

/**
 * 公理 (Axiom) および反射律 (Reflexivity) を担当するプラグイン
 */
class AxiomPlugin extends LogicPlugin {
  override def name: String = "Axiom"

  import Unifier._

  override def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
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
    
    // 反射律
    val reflexivity = goal match {
      case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
        val l = args match { case List(_, x, y) if goal.headSymbol == Path => x; case List(x, y) => x; case _ => null }
        if (l != null) {
          val r = args.last
          unify(Rewriter.normalize(l), Rewriter.normalize(r), subst).map { s =>
            Tree.V(SearchNode(exprs, "reflexivity", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "reflexivity"))), s, context, linearContext), Vector.empty)
          }.toVector
        } else Vector.empty
      case _ => Vector.empty
    }

    // 公理 (Context)
    val axioms = (context.map((_, false)) ++ linearContext.map((_, true))).flatMap { case ((name, hyp), isLinear) =>
      val hn = Rewriter.normalize(applySubst(hyp, subst))
      unify(hn, goal, subst).map { s =>
        val nextL = if (isLinear) linearContext.filterNot(_._1 == name) else linearContext
        Tree.V(SearchNode(exprs, name, depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), name))), s, context, nextL), Vector.empty)
      }
    }.toVector

    reflexivity ++ axioms
  }
}

/**
 * 標準的な論理記号 (∀, →, ∧, ∨) の導入ルールを担当するプラグイン
 */
class IntroductionPlugin extends LogicPlugin {
  override def name: String = "Introduction"

  import Unifier._

  override def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
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
    val results = mutable.ArrayBuffer[Tree[SearchNode]]()

    goal match {
      case Expr.App(Expr.Sym(Forall), args) =>
        val (vName, body, typeOpt) = args match {
          case List(Expr.Var(v), b) => (v, b, None)
          case List(Expr.Var(v), t, b) => (v, b, Some(t))
          case _ => (null, null, None)
        }
        if (vName != null) {
          val freshVar = s"${vName}_$depth"
          val instantiated = Prover.substVar(body, vName, Expr.Var(freshVar))
          val newCtx = (freshVar, typeOpt.map(Prover.substVar(_, vName, Expr.Var(freshVar))).getOrElse(Expr.Sym("Type"))) :: context
          val subTree = prover.search(exprs :+ instantiated, newCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          findSuccess(subTree).foreach { s =>
            results += Tree.V(SearchNode(exprs :+ instantiated, "forall-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "forall-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }

      case Expr.App(Expr.Sym(Implies), List(a, b)) =>
        val subTree = prover.search(exprs :+ b, (s"h$depth", a) :: context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        findSuccess(subTree).foreach { s =>
          results += Tree.V(SearchNode(exprs :+ b, "implies-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "implies-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }

      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        val treeA = prover.search(exprs :+ a, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        findSuccess(treeA).foreach { sA =>
          val treeB = prover.search(exprs :+ b, sA.context, sA.linearContext, sA.subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          findSuccess(treeB).foreach { sB =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, sB.subst), "product-intro", List(sA.result.toOption.get.tree, sB.result.toOption.get.tree))))
            results += Tree.V(SearchNode(exprs, "product-intro", depth, result, sB.subst, sB.context, sB.linearContext), Vector(treeA, treeB))
          }
        }

      case Expr.App(Expr.Sym(Or | Coproduct), List(a, b)) =>
        List((a, "coproduct-intro-l"), (b, "coproduct-intro-r")).foreach { case (target, rname) =>
          val subTree = prover.search(exprs :+ target, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          findSuccess(subTree).foreach { s =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), rname, List(s.result.toOption.get.tree))))
            results += Tree.V(SearchNode(exprs :+ target, rname, depth, result, s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }

      case _ => ()
    }
    results.toVector
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }
}

/**
 * ユーザー定義ルール (config.rules) を担当するプラグイン
 */
class UserRulePlugin extends LogicPlugin {
  override def name: String = "UserRule"

  import Unifier._

  override def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
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
    val appliedRules = prover.applyRules(goal, subst, depth, true)
    
    appliedRules.flatMap { case (rewritten, univs, ruleName, nextS) =>
      val subTree = prover.search(exprs :+ rewritten, context, linearContext, nextS, depth + 1, limit, visited, raaCount, inductionCount, guarded)
      findSuccess(subTree).map { s =>
        Tree.V(SearchNode(exprs :+ rewritten, ruleName, depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), ruleName, List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
      }
    }.toVector
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }
}
