// ==========================================
// PersistentLogicSearch.scala
// 標準論理・持続的文脈の探索ロジック（ベース実装版） (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class PersistentLogicPlugin extends LogicPlugin {
  override def name: String = "PersistentLogic"

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
    val goal = exprs.last
    val results = scala.collection.mutable.ArrayBuffer[Tree[SearchNode]]()

    context.foreach { case (name, hyp) =>
      prover.checkDeadline()
      val (premises, conclusion) = decompose(hyp, () => prover.freshMeta(depth))
      unify(conclusion, goal, subst).foreach { s =>
        // 全ての前提を解決する必要がある
        def solvePremises(ps: List[Expr], currentS: Subst, currentCtx: List[(String, Expr)], solved: List[ProofTree]): LazyList[(Subst, List[(String, Expr)], List[ProofTree])] = ps match {
          case Nil => LazyList((currentS, currentCtx, solved))
          case p :: tail =>
            val targetP = applySubst(p, currentS)
            val subTree = prover.search(exprs :+ targetP, currentCtx, state, currentS, depth + 1, limit, visited, guarded)
            allSuccesses(subTree).flatMap { res =>
              solvePremises(tail, res.subst, res.context, solved :+ res.result.toOption.get.tree)
            }
        }

        prover.checkDeadline()
        solvePremises(premises, s, context, Nil).foreach { case (finalS, finalCtx, proofs) =>
          val pTree = ProofTree.Node(applySubst(goal, finalS), s"apply[$name]", proofs)
          results += Tree.V(SearchNode(exprs, s"apply[$name]", depth, Right(ProofResult(pTree)), finalS, finalCtx, state.linearContext), Vector.empty) // 簡略化のため subtree は記録しない
        }
      }
    }
    results.toVector
  }

  private def decompose(e: Expr, freshMeta: () => Expr): (List[Expr], Expr) = e match {
    case Expr.App(Expr.Sym(Implies), List(a, b)) =>
      val (ps, c) = decompose(b, freshMeta)
      (a :: ps, c)
    case Expr.App(Expr.Sym(Forall), args) if args.length >= 2 =>
      val (vName, body) = args match {
        case List(Expr.Var(v), b) => (v, b)
        case List(Expr.Var(v), _, b) => (v, b)
        case _ => (null, null)
      }
      if (vName != null) {
        val meta = freshMeta()
        decompose(Prover.substVar(body, vName, meta), freshMeta)
      } else (Nil, e)
    case _ => (Nil, e)
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

    context.zipWithIndex.foreach {
      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        prover.checkDeadline()
        val newCtx = context.patch(i, List((s"$name.1", a), (s"$name.2", b)), 1)
        val subTree = prover.search(exprs, newCtx, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).foreach { s =>
          val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"destruct[$name]", List(s.result.toOption.get.tree))))
          results += Tree.V(SearchNode(exprs, s"destruct[$name]", depth, result, s.subst, s.context, s.linearContext), Vector(subTree))
        }

      case ((name, Expr.App(Expr.Sym(Or | Coproduct), List(a, b))), i) =>
        prover.checkDeadline()
        // OR-elim (Case Analysis)
        val newCtxA = context.patch(i, List((s"$name.l", a)), 1)
        val subTreeA = prover.search(exprs, newCtxA, state, subst, depth + 1, limit, visited, guarded)
        val successesA = allSuccesses(subTreeA)
        
        successesA.foreach { sA =>
          val newCtxB = context.patch(i, List((s"$name.r", b)), 1)
          // Aの枝で見つかった代入を引き継いでBを探索
          val subTreeB = prover.search(exprs, newCtxB, state, sA.subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTreeB).foreach { sB =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, sB.subst), s"case-analysis[$name]", List(sA.result.toOption.get.tree, sB.result.toOption.get.tree))))
            results += Tree.V(SearchNode(exprs, s"case-analysis[$name]", depth, result, sB.subst, sB.context, sB.linearContext), Vector(subTreeA, subTreeB))
          }
        }

      case ((name, Expr.Sym(False | "⊥")), i) =>
        // 矛盾 (Explosion Principle)
        val result = Right(ProofResult(ProofTree.Leaf(applySubst(goal, subst), s"explosion[$name]")))
        results += Tree.V(SearchNode(exprs, s"explosion[$name]", depth, result, subst, context, state.linearContext), Vector.empty)

      case _ => ()
    }

    // Forward Modus Ponens for contradiction: if context has →(A, ⊥) and also A, derive ⊥
    val allHyps = context.map(_._2) ++ state.linearContext.map(_._2)
    context.foreach { case (name, hyp) =>
      prover.checkDeadline()
      val normHyp = prover.normalize(applySubst(hyp, subst))
      normHyp match {
        case Expr.App(Expr.Sym(op), List(a, Expr.Sym(False | "⊥"))) if op == Implies || op == "→" =>
          val normA = prover.normalize(applySubst(a, subst))
          val found = (context ++ state.linearContext).exists { case (_, h) =>
            val nh = prover.normalize(applySubst(h, subst))
            Unifier.unify(nh, normA, subst).nonEmpty
          }
          if (found) {
            val result = Right(ProofResult(ProofTree.Leaf(applySubst(goal, subst), s"forward-mp[$name]")))
            results += Tree.V(SearchNode(exprs, s"forward-mp[$name]", depth, result, subst, context, state.linearContext), Vector.empty)
          }
        case _ => ()
      }
    }

    results.toVector
  }
}
