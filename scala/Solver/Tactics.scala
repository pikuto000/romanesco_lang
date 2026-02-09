// ==========================================
// Tactics.scala
// タクティクスの実装（LazyList版Unifierへの対応）
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._

object Tactics {
  import Unifier._

  /**
   * 含意(→)や全称量化(∀)の導入。
   */
  def intro(state: ProofState, name: Option[String] = None): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, App(Sym(op), List(a, b)))) if op == Implies || op == ImpliesAlt1 || op == ImpliesAlt2 =>
        val hypName = name.getOrElse(s"h${ctx.size}")
        val newGoal = Goal((hypName, a) :: ctx, b)
        Right(state.copy(goals = newGoal :: state.goals.tail))
      
      case Some(Goal(ctx, App(Sym(Forall), List(Var(v), body)))) =>
        val freshVar = s"${v}_t${state.goals.size}"
        val instantiated = Prover.substVar(body, v, Var(freshVar))
        val newGoal = Goal(ctx, instantiated)
        Right(state.copy(goals = newGoal :: state.goals.tail))

      case _ => Left("intro: Goal is not an implication or universal quantification")
    }
  }

  /**
   * 連言(∧)の分解。
   */
  def split(state: ProofState): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, App(Sym(op), List(a, b)))) if op == And || op == Product =>
        val g1 = Goal(ctx, a)
        val g2 = Goal(ctx, b)
        Right(state.copy(goals = g1 :: g2 :: state.goals.tail))
      case _ => Left("split: Goal is not a conjunction")
    }
  }

  /**
   * 仮定の分解。
   */
  def destruct(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(goal @ Goal(ctx, target)) =>
        ctx.zipWithIndex.find(_._1._1 == hypName) match {
          case Some(((name, hypExpr), idx)) =>
            hypExpr match {
              case App(Sym(op), List(a, b)) if op == And || op == Product =>
                val newCtx = ctx.patch(idx, List((s"$name.1", a), (s"$name.2", b)), 1)
                Right(state.copy(goals = goal.copy(context = newCtx) :: state.goals.tail))
              
              case App(Sym(op), List(a, b)) if op == Or || op == Coproduct =>
                val g1 = Goal(ctx.patch(idx, List((s"$name.left", a)), 1), target)
                val g2 = Goal(ctx.patch(idx, List((s"$name.right", b)), 1), target)
                Right(state.copy(goals = g1 :: g2 :: state.goals.tail))
              
              case _ => Left(s"destruct: Hypothesis '$hypName' cannot be destructed")
            }
          case None => Left(s"destruct: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }

  /**
   * 自然数に関する帰納法。
   * 目標が ∀n. P(n) のとき、P(0) と ∀n. (P(n) → P(S(n))) に分割する。
   */
  def induction(state: ProofState): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, App(Sym(Forall), List(Var(v), body)))) =>
        // Base case: P(0)
        val baseGoal = Goal(ctx, Prover.substVar(body, v, Sym(Zero)))
        
        // Inductive step: ∀n. (P(n) → P(S(n)))
        val n = v // 再利用
        val pn = Prover.substVar(body, v, Var(n))
        val psn = Prover.substVar(body, v, App(Sym(Succ), List(Var(n))))
        val stepGoal = Goal(ctx, App(Sym(Forall), List(Var(n), App(Sym(Implies), List(pn, psn)))))
        
        Right(state.copy(goals = baseGoal :: stepGoal :: state.goals.tail))
      
      case _ => Left("induction: Goal is not a universal quantification")
    }
  }

  /**
   * 等式の反射律。左右が一致（正規化後）していれば解決する。
   */
  def reflexivity(state: ProofState): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, App(Sym(Eq), List(l, r)))) =>
        if (Rewriter.normalize(l) == Rewriter.normalize(r)) Right(state.copy(goals = state.goals.tail))
        else Left(s"reflexivity: $l and $r are not equal even after normalization")
      case _ => Left("reflexivity: Goal is not an equality")
    }
  }

  /**
   * 仮定にある等式を使って結論を書き換える。
   */
  def rewrite(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(goal @ Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, App(Sym(Eq), List(l, r)))) =>
            // 目標を正規化してから書き換えを試みる
            val normTarget = Rewriter.normalize(target)
            val rewritten = Prover.rewriteExpr(normTarget, l, r)
            if (rewritten != normTarget) Right(state.copy(goals = goal.copy(target = rewritten) :: state.goals.tail))
            else Left(s"rewrite: Hypothesis '$hypName' ($l = $r) not applicable to goal ($normTarget)")
          case Some((_, other)) => Left(s"rewrite: Hypothesis '$hypName' ($other) is not an equality")
          case None => Left(s"rewrite: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }

  /**
   * 指定した仮定を使って直接ゴールを解決する。
   */
  def exact(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, hypExpr)) =>
            unify(hypExpr, target, emptySubst).headOption match {
              case Some(_) => Right(state.copy(goals = state.goals.tail))
              case None => Left(s"exact: Hypothesis '$hypName' ($hypExpr) does not match goal ($target)")
            }
          case None => Left(s"exact: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }

  /**
   * 仮定のいずれかがゴールと一致すれば解決する。
   */
  def assumption(state: ProofState): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        val matched = ctx.exists { case (_, hyp) => unify(hyp, target, emptySubst).nonEmpty }
        if (matched) Right(state.copy(goals = state.goals.tail))
        else Left("assumption: No matching hypothesis found")
      case None => Left("No current goal")
    }
  }

  /**
   * 仮定を適用。
   */
  def applyTactic(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, hypExpr)) =>
            hypExpr match {
              case App(Sym(op), List(a, b)) if op == Implies || op == ImpliesAlt1 || op == ImpliesAlt2 =>
                unify(b, target, emptySubst).headOption match {
                  case Some(_) => Right(state.copy(goals = Goal(ctx, a) :: state.goals.tail))
                  case None => Left(s"apply: Conclusion of '$hypName' ($b) does not match goal ($target)")
                }
              case _ => Left(s"apply: Hypothesis '$hypName' is not an implication")
            }
          case None => Left(s"apply: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }
}
