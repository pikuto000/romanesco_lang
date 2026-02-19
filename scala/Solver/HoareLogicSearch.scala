// ==========================================
// HoareLogicSearch.scala
// Hoare論理の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class HoareLogicPlugin extends LogicPlugin {
  override def name: String = "HoareLogic"
  override def priority: Int = 41

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
    goal match {
      case Expr.App(Expr.Sym("triple"), List(p, c, q)) =>
        searchHoare(exprs, p, c, q, rules, context, state, subst, depth, limit, visited, guarded, prover)
      case _ => Vector.empty
    }
  }

  private def searchHoare(
      exprs: Vector[Expr],
      p: Expr,
      c: Expr,
      q: Expr,
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
    prover.checkDeadline()
    val syntaxDirected: Vector[Tree[SearchNode]] = c match {
      case Expr.Sym("skip") =>
        val nextGoal = Expr.App(Expr.Sym(Implies), List(p, q))
        val subTree = prover.search(exprs :+ nextGoal, context, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).map { s =>
          val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "hoare-skip", List(s.result.toOption.get.tree))))
          Tree.V(SearchNode(exprs :+ nextGoal, "hoare-skip", depth, result, s.subst, s.context, s.linearContext), Vector(subTree))
        }.toVector

      case Expr.App(Expr.Sym(":="), List(targetVar, e)) =>
        val varName = targetVar match {
          case Expr.Var(n) => n
          case Expr.Sym(n) => n
          case _ => null
        }
        if (varName != null) {
          val substitutedQ = Prover.substVar(q, varName, e)
          val nextGoal = Expr.App(Expr.Sym(Implies), List(p, substitutedQ))
          val subTree = prover.search(exprs :+ nextGoal, context, state, subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).map { s =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "hoare-assign", List(s.result.toOption.get.tree))))
            Tree.V(SearchNode(exprs :+ nextGoal, "hoare-assign", depth, result, s.subst, s.context, s.linearContext), Vector(subTree))
          }.toVector
        } else Vector.empty

      case Expr.App(Expr.Sym(";"), List(c1, c2)) =>
        val midR = prover.freshMeta(depth)
        val goal2 = Expr.App(Expr.Sym("triple"), List(midR, c2, q))
        val tree2 = prover.search(exprs :+ goal2, context, state, subst, depth + 1, limit, visited, guarded)
        
        allSuccesses(tree2).toVector.flatMap { s2 =>
          prover.checkDeadline()
          val goal1 = Expr.App(Expr.Sym("triple"), List(p, c1, applySubst(midR, s2.subst)))
          val tree1 = prover.search(exprs :+ goal1, s2.context, state.withLinear(s2.linearContext), s2.subst, depth + 1, limit, visited, guarded)
          allSuccesses(tree1).map { s1 =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s1.subst), "hoare-seq", List(s1.result.toOption.get.tree, s2.result.toOption.get.tree))))
            Tree.V(SearchNode(exprs :+ goal1, "hoare-seq", depth, result, s1.subst, s1.context, s1.linearContext), Vector(tree1, tree2))
          }
        }

      case Expr.App(Expr.Sym("if"), List(b, c1, c2)) =>
        // {P ∧ b} c1 {Q} and {P ∧ ¬b} c2 {Q}
        val condThen = prover.normalize(b)
        val condElse = prover.normalize(Expr.App(Expr.Sym(Not), List(b)))
        val preNorm = prover.normalize(p)
        
        val ctxThen = (s"hPre$depth", preNorm) :: (s"hThen$depth", condThen) :: context
        val treeThen = prover.search(exprs :+ Expr.App(Expr.Sym("triple"), List(preNorm, c1, q)), ctxThen, state, subst, depth + 1, limit, visited, guarded)
        
        allSuccesses(treeThen).toVector.flatMap { sThen =>
          val ctxElse = (s"hPre$depth", preNorm) :: (s"hElse$depth", condElse) :: context
          val treeElse = prover.search(exprs :+ Expr.App(Expr.Sym("triple"), List(preNorm, c2, q)), ctxElse, state, sThen.subst, depth + 1, limit, visited, guarded)
          allSuccesses(treeElse).map { sElse =>
            val result = Right(ProofResult(ProofTree.Node(applySubst(goal, sElse.subst), "hoare-if", List(sThen.result.toOption.get.tree, sElse.result.toOption.get.tree))))
            Tree.V(SearchNode(exprs, "hoare-if", depth, result, sElse.subst, sElse.context, sElse.linearContext), Vector(treeThen, treeElse))
          }
        }

      case Expr.App(Expr.Sym("while"), List(b, c, inv)) =>
        // 1. P → inv (Precondition implies invariant)
        val t1 = prover.search(exprs :+ Expr.App(Expr.Sym(Implies), List(p, inv)), context, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(t1).toVector.flatMap { s1 =>
          prover.checkDeadline()
          // 2. {inv ∧ b} c {inv} (Invariant preserved)
          val ctxBody = (s"hInv$depth", prover.normalize(inv)) :: (s"hWhile$depth", prover.normalize(b)) :: s1.context
          val t2 = prover.search(exprs :+ Expr.App(Expr.Sym("triple"), List(inv, c, inv)), ctxBody, state, s1.subst, depth + 1, limit, visited, guarded)
          allSuccesses(t2).toVector.flatMap { s2 =>
            prover.checkDeadline()
            // 3. inv ∧ ¬b → Q (Invariant and exit condition implies postcondition)
            val t3 = prover.search(exprs :+ Expr.App(Expr.Sym(Implies), List(Expr.App(Expr.Sym(And), List(inv, Expr.App(Expr.Sym(Not), List(b)))), q)), s2.context, state, s2.subst, depth + 1, limit, visited, guarded)
            allSuccesses(t3).map { s3 =>
              val result = Right(ProofResult(ProofTree.Node(applySubst(goal, s3.subst), "hoare-while", List(s1.result.toOption.get.tree, s2.result.toOption.get.tree, s3.result.toOption.get.tree))))
              Tree.V(SearchNode(exprs, "hoare-while", depth, result, s3.subst, s3.context, s3.linearContext), Vector(t1, t2, t3))
            }
          }
        }

      case _ => Vector.empty
    }

    syntaxDirected
  }
}
