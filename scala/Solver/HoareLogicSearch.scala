// ==========================================
// HoareLogicSearch.scala
// Hoare論理の探索ロジック
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._

trait HoareLogicSearch { self: Prover =>
  import Expr._
  import Unifier._

  def getHoareGoalHooks(
      goal: Expr,
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
      history: List[Expr]
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    goal match {
      case App(Sym("triple"), List(p, c, q)) =>
        List(searchHoare(p, c, q, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history))
      case _ => Nil
    }
  }

  private def searchHoare(
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
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    // Hoare論理のサブゴールを解くための特急コンフィグ
    val hoareConfig = config.copy(enabledDomains = Some(Set("general", "mapping", "arithmetic", "hoare", "list")))
    val hoareProver = new Prover(hoareConfig)
    hoareProver.deadline = self.deadline

    val syntaxDirected = c match {
      // {P} skip {Q}  => P -> Q
      case Sym("skip") =>
        hoareProver.search(App(Sym(Implies), List(p, q)), rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history)
          .map { case (t, s, l) => (ProofTree.Node(applySubst(App(Sym("triple"), List(p, c, q)), s), "hoare-skip", List(t)), s, l) }

      // {P} x := e {Q} => P -> Q[e/x]
      case App(Sym(":="), List(targetVar, e)) =>
        val varName = targetVar match {
          case Var(n) => n
          case Sym(n) => n
          case _ => null
        }
        if (varName != null) {
          val substitutedQ = Prover.substVar(q, varName, e)
          hoareProver.search(App(Sym(Implies), List(p, substitutedQ)), rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history)
            .map { case (t, s, l) => (ProofTree.Node(applySubst(App(Sym("triple"), List(p, c, q)), s), "hoare-assign", List(t)), s, l) }
        } else SolveTree.Failure()

      // {P} C1 ; C2 {Q} => {P} C1 {?R} AND {?R} C2 {Q}
      case App(Sym(";"), List(c1, c2)) =>
        val midR = freshMeta(depth)
        self.search(App(Sym("triple"), List(p, c1, midR)), rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history)
          .flatMap { case (t1, s1, l1) =>
            self.search(App(Sym("triple"), List(applySubst(midR, s1), c2, q)), rules, context, l1, s1, depth + 1, limit, visited, raaCount, inductionCount, guarded, history)
              .map { case (t2, s2, l2) =>
                (ProofTree.Node(applySubst(App(Sym("triple"), List(p, c, q)), s2), "hoare-seq", List(t1, t2)), s2, l2)
              }
          }

      // {P} if B then C1 else C2 {Q} => {P ∧ B} C1 {Q} AND {P ∧ ¬B} C2 {Q}
      case App(Sym("if"), List(b, c1, c2)) =>
        val goal1 = App(Sym("triple"), List(App(Sym(And), List(p, b)), c1, q))
        val goal2 = App(Sym("triple"), List(App(Sym(And), List(p, App(Sym(Not), List(b)))), c2, q))
        self.search(goal1, rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).flatMap { case (t1, s1, l1) =>
          self.search(goal2, rules, context, l1, s1, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (t2, s2, l2) =>
            (ProofTree.Node(applySubst(App(Sym("triple"), List(p, c, q)), s2), "hoare-if", List(t1, t2)), s2, l2)
          }
        }
      
      // {P} while B do C {Q} => P -> I AND {I ∧ B} C {I} AND (I ∧ ¬B) -> Q
      case App(Sym("while"), args) =>
        val (b, body, invI) = args match {
          case List(b, body, inv) => (b, body, inv)
          case List(b, body) => (b, body, freshMeta(depth))
          case _ => (null, null, null)
        }
        if (b != null) {
          val cond1 = Rewriter.normalize(App(Sym(Implies), List(p, invI)))
          val cond2 = App(Sym("triple"), List(Rewriter.normalize(App(Sym(And), List(invI, b))), body, invI))
          val cond3 = Rewriter.normalize(App(Sym(Implies), List(App(Sym(And), List(invI, App(Sym(Not), List(b)))), q)))

          hoareProver.search(cond1, rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).flatMap { case (t1, s1, l1) =>
            self.search(applySubst(cond2, s1), rules, context, l1, s1, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).flatMap { case (t2, s2, l2) =>
              hoareProver.search(applySubst(cond3, s2), rules, context, l2, s2, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (t3, s3, l3) =>
                (ProofTree.Node(applySubst(App(Sym("triple"), List(p, c, q)), s3), "hoare-while", List(t1, t2, t3)), s3, l3)
              }
            }
          }
        } else SolveTree.Failure()

      case _ => SolveTree.Failure()
    }

    // Rule of Consequence: {P} C {Q} is true if P -> P' and {P'} C {Q'} and Q' -> Q
    val consequence = if (depth < limit) {
      val pPrime = freshMeta(depth + 100)
      val qPrime = freshMeta(depth + 200)
      
      // Try to solve {P'} C {Q'} first, then verify P -> P' and Q' -> Q
      // To avoid immediate recursion, we only try this if C is not just a meta-variable
      c match {
        case Meta(_) => SolveTree.Failure()
        case _ => 
          self.search(App(Sym("triple"), List(pPrime, c, qPrime)), rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).flatMap { case (tC, s1, l1) =>
            val instPPrime = applySubst(pPrime, s1)
            val instQPrime = applySubst(qPrime, s1)
            
            hoareProver.search(App(Sym(Implies), List(p, instPPrime)), rules, context, linearContext, s1, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).flatMap { case (tP, s2, l2) =>
              hoareProver.search(App(Sym(Implies), List(applySubst(instQPrime, s2), q)), rules, context, l2, s2, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (tQ, s3, l3) =>
                (ProofTree.Node(applySubst(App(Sym("triple"), List(p, c, q)), s3), "hoare-consequence", List(tP, tC, tQ)), s3, l3)
              }
            }
          }
      }
    } else SolveTree.Failure()

    SolveTree.Choice(List(syntaxDirected, consequence))
  }
}
