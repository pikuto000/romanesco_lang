// ==========================================
// Tactics.scala
// タクティクスの実装
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._

object Tactics {
  import Unifier._

  /**
   * 含意(→)や全称量化(∀)の導入。
   * 目標が A → B のとき、Aを仮定に追加し目標をBにする。
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
   * 目標が A ∧ B のとき、AとBの2つの目標に分ける。
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
   * ∧ ならば2つの仮定に分け、∨ ならば2つのサブゴール（ケース分析）に分ける。
   */
  def destruct(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(goal @ Goal(ctx, target)) =>
        ctx.zipWithIndex.find(_._1._1 == hypName) match {
          case Some(((name, hypExpr), idx)) =>
            hypExpr match {
              // ∧ の分解
              case App(Sym(op), List(a, b)) if op == And || op == Product =>
                val newCtx = ctx.patch(idx, List((s"$name.1", a), (s"$name.2", b)), 1)
                Right(state.copy(goals = goal.copy(context = newCtx) :: state.goals.tail))
              
              // ∨ の分解 (ケース分析)
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
   * 指定した仮定を使って直接ゴールを解決する。
   */
  def exact(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, hypExpr)) =>
            unify(hypExpr, target, emptySubst) match {
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
        val matched = ctx.exists { case (_, hyp) => unify(hyp, target, emptySubst).isDefined }
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
                unify(b, target, emptySubst) match {
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