// ==========================================
// Tactics.scala
// タクティクスの実装（高度な書き換え・インスタンス化対応）
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

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
        val hypName = name.getOrElse(s"${v}_t${state.goals.size}")
        val instantiated = Prover.substVar(body, v, Var(hypName))
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
   */
  def induction(state: ProofState, varName: Option[String] = None): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, App(Sym(Forall), List(Var(v), body)))) =>
        val targetVar = varName.getOrElse(v)
        val baseGoal = Goal(ctx, Prover.substVar(body, v, Sym(Zero)))
        val n = targetVar
        val pn = Prover.substVar(body, v, Var(n))
        val psn = Prover.substVar(body, v, App(Sym(Succ), List(Var(n))))
        val stepGoal = Goal(ctx, App(Sym(Forall), List(Var(n), App(Sym(Implies), List(pn, psn)))))
        Right(state.copy(goals = baseGoal :: stepGoal :: state.goals.tail))
      case _ => Left("induction: Goal is not a universal quantification")
    }
  }

  /**
   * 等式の反射律。
   */
  def reflexivity(state: ProofState): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, App(Sym(Eq), List(l, r)))) =>
        if (Rewriter.normalize(l) == Rewriter.normalize(r)) Right(state.copy(goals = state.goals.tail))
        else Left(s"reflexivity: ${Rewriter.normalize(l)} and ${Rewriter.normalize(r)} are not equal")
      case _ => Left("reflexivity: Goal is not an equality")
    }
  }

  /**
   * 仮定にある等式を使って結論を書き換える（高度なインスタンス化対応版）。
   */
  def rewrite(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(goal @ Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, hypExpr)) =>
            // 1. 仮定をインスタンス化（メタ変数はユニークにするために state のハッシュ等を利用）
            var metaCounter = 0
            def instantiate(expr: Expr): Expr = expr match {
              case App(Sym(Forall), List(Var(v), body)) =>
                metaCounter += 1
                instantiate(Prover.substVar(body, v, Meta(MetaId(List(999, metaCounter)))))
              case _ => expr
            }
            
            val instHyp = instantiate(hypExpr)
            instHyp match {
              case App(Sym(Eq), List(l, r)) =>
                val normTarget = Rewriter.normalize(target)
                
                // 2. 単一化を伴う部分式書き換え
                def findAndReplace(expr: Expr): Option[(Expr, Subst)] = {
                  // 左辺 -> 右辺
                  unify(expr, l, emptySubst).headOption.map(s => (applySubst(r, s), s))
                    .orElse {
                      // 右辺 -> 左辺
                      unify(expr, r, emptySubst).headOption.map(s => (applySubst(l, s), s))
                    }
                    .orElse {
                      expr match {
                        case App(f, args) =>
                          args.indices.to(LazyList).flatMap { idx =>
                            findAndReplace(args(idx)).map { case (newArg, s) =>
                              val nextF = applySubst(f, s)
                              val nextArgs = args.patch(idx, List(newArg), 1).map(applySubst(_, s))
                              (App(nextF, nextArgs), s)
                            }
                          }.headOption
                        case _ => None
                      }
                    }
                }

                findAndReplace(normTarget) match {
                  case Some((rewritten, subst)) =>
                    val finalTarget = Rewriter.normalize(applySubst(rewritten, subst))
                    Right(state.copy(goals = goal.copy(target = finalTarget) :: state.goals.tail))
                  case None => Left(s"rewrite: Hypothesis '$hypName' not applicable to goal")
                }
              case _ => Left(s"rewrite: Hypothesis '$hypName' is not an equality")
            }
          case None => Left(s"rewrite: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }

  def exact(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, hypExpr)) =>
            unify(hypExpr, target, emptySubst).headOption match {
              case Some(_) => Right(state.copy(goals = state.goals.tail))
              case None => Left(s"exact: Hypothesis '$hypName' does not match goal")
            }
          case None => Left(s"exact: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }

  def assumption(state: ProofState): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        if (ctx.exists { case (_, hyp) => unify(hyp, target, emptySubst).nonEmpty }) 
          Right(state.copy(goals = state.goals.tail))
        else Left("assumption: No matching hypothesis found")
      case None => Left("No current goal")
    }
  }

  def applyHyp(state: ProofState, hypName: String): TacticResult = {
    state.currentGoal match {
      case Some(Goal(ctx, target)) =>
        ctx.find(_._1 == hypName) match {
          case Some((_, hypExpr)) =>
            hypExpr match {
              case App(Sym(op), List(a, b)) if op == Implies || op == ImpliesAlt1 || op == ImpliesAlt2 =>
                unify(b, target, emptySubst).headOption match {
                  case Some(_) => Right(state.copy(goals = Goal(ctx, a) :: state.goals.tail))
                  case None => Left(s"apply: Conclusion of '$hypName' does not match goal")
                }
              case _ => Left(s"apply: Hypothesis '$hypName' is not an implication")
            }
          case None => Left(s"apply: Hypothesis '$hypName' not found")
        }
      case None => Left("No current goal")
    }
  }
}