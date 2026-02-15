// ==========================================
// StandardPlugins.scala
// 基本公理・導入ルール・ユーザー定義ルールのプラグイン化
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.LogicSymbols._
import romanesco.Types.Tree
import scala.collection.mutable
import romanesco.Solver.core.Prover

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
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    
    // 真 (True)
    val trueSolve = if (goal == Expr.Sym(True) || goal == Expr.Sym("⊤")) {
      Vector(Tree.V(SearchNode(exprs, "true-intro", depth, Right(ProofResult(ProofTree.Leaf(goal, "true-intro"))), subst, context, state.linearContext), Vector.empty))
    } else Vector.empty

    // 矛盾 (False in context)
    val explosion = (context.map(_._2) ++ state.linearContext.map(_._2)).find(h => h == Expr.Sym(False) || h == Expr.Sym("⊥")).map { h =>
      Tree.V(SearchNode(exprs, "explosion", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, subst), "explosion"))), subst, context, state.linearContext), Vector.empty)
    }.toVector

    // 反射律
    val reflexivity = goal match {
      case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
        val l = args match { case List(_, x, y) if goal.headSymbol == Path => x; case List(x, y) => x; case _ => null }
        if (l != null) {
          val r = args.last
          unify(prover.normalize(l), prover.normalize(r), subst).map { s =>
            Tree.V(SearchNode(exprs, "reflexivity", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "reflexivity"))), s, context, state.linearContext), Vector.empty)
          }.toVector
        } else Vector.empty
      case _ => Vector.empty
    }

    // 公理 (Context)
    val axioms = (context.map((_, false)) ++ state.linearContext.map((_, true))).flatMap { case ((name, hyp), isLinear) =>
      val hn = prover.normalize(applySubst(hyp, subst))
      unify(hn, goal, subst).map { s =>
        val nextL = if (isLinear) state.linearContext.filterNot(_._1 == name) else state.linearContext
        Tree.V(SearchNode(exprs, name, depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), name))), s, context, nextL), Vector.empty)
      }
    }.toVector

    trueSolve ++ explosion ++ reflexivity ++ axioms
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
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
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
          val subTree = prover.search(exprs :+ instantiated, newCtx, state, subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).foreach { s =>
            results += Tree.V(SearchNode(exprs :+ instantiated, "forall-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "forall-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }

      case Expr.App(Expr.Sym(Implies), List(a, b)) =>
        val subTree = prover.search(exprs :+ b, (s"h$depth", a) :: context, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).foreach { s =>
          results += Tree.V(SearchNode(exprs :+ b, "implies-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "implies-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }

      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        val treeA = prover.search(exprs :+ a, context, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(treeA).foreach { sA =>
          val treeB = prover.search(exprs :+ b, sA.context, state.withLinear(sA.linearContext), sA.subst, depth + 1, limit, visited, guarded)
          allSuccesses(treeB).foreach { sB =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, sB.subst), "product-intro", List(sA.result.toOption.get.tree, sB.result.toOption.get.tree))))
            results += Tree.V(SearchNode(exprs, "product-intro", depth, result, sB.subst, sB.context, sB.linearContext), Vector(treeA, treeB))
          }
        }

      case Expr.App(Expr.Sym(Or | Coproduct), List(a, b)) =>
        List((a, "coproduct-intro-l"), (b, "coproduct-intro-r")).foreach { case (target, rname) =>
          val subTree = prover.search(exprs :+ target, context, state, subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).foreach { s =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), rname, List(s.result.toOption.get.tree))))
            results += Tree.V(SearchNode(exprs :+ target, rname, depth, result, s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }

      case Expr.App(Expr.Sym(Exists), args) =>
        val (vName, body) = args match {
          case List(Expr.Var(v), b) => (v, b)
          case List(Expr.Var(v), _, b) => (v, b)
          case _ => (null, null)
        }
        if (vName != null) {
          val meta = prover.freshMeta(depth)
          val instantiated = Prover.substVar(body, vName, meta)
          val subTree = prover.search(exprs :+ instantiated, context, state, subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).foreach { s =>
            results += Tree.V(SearchNode(exprs :+ instantiated, "exists-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "exists-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }

      case _ => ()
    }
    results.toVector
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
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    val appliedRules = prover.applyRules(goal, subst, depth, true)
    
    appliedRules.flatMap { case (rewritten, univs, ruleName, nextS) =>
      val subTree = prover.search(exprs :+ rewritten, context, state, nextS, depth + 1, limit, visited, guarded)
      allSuccesses(subTree).map { s =>
        Tree.V(SearchNode(exprs :+ rewritten, ruleName, depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), ruleName, List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
      }
    }.toVector
  }
}
