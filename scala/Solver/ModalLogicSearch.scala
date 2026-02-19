// ==========================================
// ModalLogicSearch.scala
// 様様論理 (Modal Logic / S4) 固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class ModalLogicPlugin extends LogicPlugin {
  override def name: String = "ModalLogic"

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
    val results = scala.collection.mutable.ArrayBuffer[Tree[SearchNode]]()

    prover.checkDeadline()
    val head = goal.headSymbol.toString
    if (head == Box || head == "□" || head.contains("Box")) {
      val a = goal.asInstanceOf[Expr.App].args.head
      def isPersistent(e: Expr): Boolean = {
        val s = e.headSymbol.toString
        s == Box || s == Globally || s == Bang || s == "!" || s == "□" || s == "G" || s.contains("Box") || s.contains("Globally")
      }
      val persistentCtx = context.filter(kv => isPersistent(kv._2))
      val persistentLinear = state.linearContext.filter(kv => isPersistent(kv._2))
      
      val subTree = prover.search(exprs :+ a, persistentCtx, state.withLinear(persistentLinear), subst, depth + 1, limit, visited, guarded)
      allSuccesses(subTree).foreach { s =>
        results += Tree.V(SearchNode(exprs :+ a, "box-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "box-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
      }

      (context ++ state.linearContext).zipWithIndex.foreach {
        case ((name, hyp), i) =>
          prover.checkDeadline()
          val isLinear = i >= context.length
          val nh = prover.normalize(applySubst(hyp, subst))
          val hHead = nh.headSymbol.toString
          if (hHead == Box || hHead == "□" || hHead.contains("Box")) {
            val inner = nh.asInstanceOf[Expr.App].args.head
            inner match {
              case Expr.App(f, List(hypA, hypB)) =>
                val fStr = f.toString
                if (List(Implies, "→", LImplies, "⊸", ImpliesAlt1, ImpliesAlt2).contains(f.headSymbol) || fStr.contains("→") || fStr.contains("⊸") || fStr.contains("竊・") || fStr.contains("竓ｸ")) {
                  unify(hypB, a, subst).foreach { s =>
                    val ba = Expr.App(Expr.Sym(Box), List(applySubst(hypA, s)))
                    if (!exprs.contains(ba)) {
                      val nextL = if (isLinear) state.linearContext.patch(i - context.length, Nil, 1) else state.linearContext
                      val subTreeK = prover.search(exprs :+ ba, context, state.withLinear(nextL), s, depth + 1, limit, visited, guarded)
                      allSuccesses(subTreeK).foreach { res =>
                        results += Tree.V(SearchNode(exprs :+ ba, s"box-dist[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, res.subst), s"box-dist[$name]", List(res.result.toOption.get.tree)))), res.subst, res.context, res.linearContext), Vector(subTreeK))
                      }
                    }
                  }
                }
              case _ => ()
            }
          }
      }
    } else if (head == Diamond || head == "◇" || head.contains("Diamond")) {
      val a = goal.asInstanceOf[Expr.App].args.head
      val subTree = prover.search(exprs :+ a, context, state, subst, depth + 1, limit, visited, guarded)
      allSuccesses(subTree).foreach { s =>
        results += Tree.V(SearchNode(exprs :+ a, "diamond-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "diamond-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
      }
    }

    results.toVector
  }

  override def getContextHooks(
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
    val results = scala.collection.mutable.ArrayBuffer[Tree[SearchNode]]()

    (context ++ state.linearContext).zipWithIndex.foreach {
      case ((name, hyp), i) =>
        prover.checkDeadline()
        val isLinear = i >= context.length
        val nh = prover.normalize(applySubst(hyp, subst))
        val hHead = nh.headSymbol.toString
        if (hHead == Box || hHead == "□" || hHead.contains("Box")) {
          val args = nh.asInstanceOf[Expr.App].args
          if (args.nonEmpty) {
            val a = args.head
            if (!context.exists(_._2 == a)) {
              val newCtx = (s"$name.T", a) :: context
              val nextL = if (isLinear) state.linearContext.patch(i - context.length, Nil, 1) else state.linearContext
              val subTree = prover.search(exprs, newCtx, state.withLinear(nextL), subst, depth + 1, limit, visited, guarded)
              allSuccesses(subTree).foreach { s =>
                results += Tree.V(SearchNode(exprs, s"box-elim-T[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"box-elim-T[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
              }
            }
          }
        } else if (hHead == Diamond || hHead == "◇" || hHead.contains("Diamond")) {
          val args = nh.asInstanceOf[Expr.App].args
          if (args.nonEmpty) {
            val a = args.head
            val gStr = goal.toString
            val isFalse = goal == Expr.Sym(False) || goal == Expr.Sym("⊥") || gStr.contains("⊥") || gStr.contains("False") || gStr.contains("竓･")
            if (isFalse) {
              def isPersistent(e: Expr): Boolean = {
                val s = e.headSymbol.toString
                s == Box || s == Globally || s == Bang || s == "!" || s == "□" || s == "G" || s.contains("Box") || s.contains("Globally")
              }
              val persistentCtx = context.filter(kv => isPersistent(kv._2))
              val persistentLinear = state.linearContext.filter(kv => isPersistent(kv._2))
              val newCtxD = (s"$name.elim", a) :: persistentCtx
              val subTreeD = prover.search(exprs, newCtxD, state.withLinear(persistentLinear), subst, depth + 1, limit, visited, guarded)
              allSuccesses(subTreeD).foreach { s =>
                results += Tree.V(SearchNode(exprs, s"dia-elim[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"dia-elim[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTreeD))
              }
            }
          }
        }
    }
    results.toVector
  }
}
