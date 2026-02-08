// ==========================================
// Prover.scala
// 証明探索エンジン
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import romanesco.Utils.Debug.logger

final class Prover(val classical: Boolean = false) {
  import Unifier._

  var metaCounter = 0
  def freshMeta: Expr = { metaCounter += 1; Expr.Meta(metaCounter) }

  type Context = List[(String, Expr)]

  def prove(
      goal: Expr,
      rules: List[CatRule] = StandardRules.all,
      maxDepth: Int = 30
  ): Option[Proof] =
    logger.log(s"prove begin. goal: $goal (classical: $classical)")
    metaCounter = 0
    search(goal, rules, Nil, emptySubst, 0, maxDepth, Set.empty, 0).map(_._1)

  private def search(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): Option[(Proof, Subst)] =
    val currentGoal = applySubst(goal, subst)
    val contextExprs = context.map(h => applySubst(h._2, subst)).toSet
    
    if depth > maxDepth then None
    else if visited.contains((currentGoal, contextExprs)) then None
    else
      logger.log(s"searching goal: $currentGoal (depth $depth, raa: $raaCount)")
      val nextVisited = visited + ((currentGoal, contextExprs))

      // 探索戦略（優先度順）
      searchAxiom(currentGoal, context, subst)
        .orElse(searchReflexivity(currentGoal, subst))
        .orElse(
          searchDecompose(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount)
        )
        .orElse(
          decomposeContext(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount)
        )
        .orElse(
          searchContext(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount)
        )
        .orElse(
          searchRules(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount)
        )
        .orElse {
          // 古典論理: 背理法 (RAA)
          if classical && raaCount < 1 && currentGoal != Expr.Sym("⊥") && currentGoal != Expr.Sym("0") then
            logger.log(s"trying RAA for goal: $currentGoal")
            val negation = Expr.App(Expr.Sym("→"), List(currentGoal, Expr.Sym("⊥")))
            search(Expr.Sym("⊥"), rules, (s"raa$depth", negation) :: context, subst, depth + 1, maxDepth, nextVisited, raaCount + 1)
              .map { case (p, s) =>
                val raaStep = ProofStep.Apply(CatRule("RAA", Expr.App(Expr.Sym("→"), List(negation, Expr.Sym("⊥"))), currentGoal), s)
                (p :+ raaStep, s)
              }
          else None
        }

  private def searchAxiom(
      goal: Expr,
      context: Context,
      subst: Subst
  ): Option[(Proof, Subst)] =
    context.view.flatMap { case (name, hyp) =>
      unify(hyp, goal, subst).map(s =>
        (List(ProofStep.Apply(CatRule(name, hyp, hyp), s)), s)
      )
    }.headOption

  private def searchReflexivity(
      goal: Expr,
      subst: Subst
  ): Option[(Proof, Subst)] =
    goal match
      case Expr.App(Expr.Sym("="), List(l, r)) =>
        unify(l, r, subst).map { s =>
          (List(ProofStep.Apply(StandardRules.eqRefl, s)), s)
        }
      case _ => None

  private def searchDecompose(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): Option[(Proof, Subst)] =
    goal match
      case Expr.App(Expr.Sym(op), List(a, b)) if op == "×" || op == "∧" =>
        for
          (proofA, s1) <- search(a, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
          (proofB, s2) <- search(b, rules, context, s1, depth + 1, maxDepth, visited, raaCount)
        yield
          val pairStep = ProofStep.Apply(StandardRules.productUniversal, s2)
          (proofA ++ proofB :+ pairStep, s2)

      case Expr.App(Expr.Sym(op), List(a, b))
          if op == "→" || op == "^" || op == "⊃" || op == "⇒" =>
        val (ant, cons) = if (op == "→" || op == "⊃" || op == "⇒") (a, b) else (b, a)
        val hypName = s"h$depth"
        search(cons, rules, (hypName, ant) :: context, subst, depth + 1, maxDepth, visited, raaCount)
          .map { case (proof, s) =>
            val lambdaStep = ProofStep.Apply(StandardRules.expUniversal, s)
            (proof :+ lambdaStep, s)
          }

      case Expr.App(Expr.Sym("∀"), List(Expr.Var(v), body)) =>
        val freshVar = s"${v}_$depth"
        val instantiated = substVar(body, v, Expr.Var(freshVar))
        search(instantiated, rules, context, subst, depth + 1, maxDepth, visited, raaCount).map {
          case (proof, s) =>
            val forallStep = ProofStep.Apply(StandardRules.forallCounit, s)
            (proof :+ forallStep, s)
        }

      case Expr.App(Expr.Sym("∃"), List(Expr.Var(v), body)) =>
        val witness = freshMeta
        val instantiated = substVar(body, v, witness)
        search(instantiated, rules, context, subst, depth + 1, maxDepth, visited, raaCount).map {
          case (proof, s) =>
            val existsStep = ProofStep.Apply(StandardRules.existsUnit, s)
            (proof :+ existsStep, s)
        }

      case Expr.App(Expr.Sym(op), List(a, b)) if op == "+" || op == "∨" =>
        search(a, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
          .map { case (p, s) =>
            val inlStep = ProofStep.Apply(CatRule("inl", a, goal), s)
            (p :+ inlStep, s)
          }
          .orElse(
            search(b, rules, context, subst, depth + 1, maxDepth, visited, raaCount).map {
              case (p, s) =>
                val inrStep = ProofStep.Apply(CatRule("inr", b, goal), s)
                (p :+ inrStep, s)
            }
          )

      case Expr.Sym(op) if op == "⊤" || op == "1" =>
        Some(
          (List(ProofStep.Apply(StandardRules.terminalUniversal, subst)), subst)
        )

      case _ => None

  private def searchRules(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): Option[(Proof, Subst)] =
    rules.view.flatMap { rule =>
      val (instRule, _) = instantiate(rule)
      unify(instRule.rhs, goal, subst).flatMap { s =>
        val universalsOk = instRule.universals.forall { cond =>
          search(cond, rules, context, s, depth + 1, maxDepth, visited, raaCount).isDefined
        }
        if universalsOk then
          search(instRule.lhs, rules, context, s, depth + 1, maxDepth, visited, raaCount)
            .map { case (proof, finalSubst) =>
              (proof :+ ProofStep.Apply(rule, finalSubst), finalSubst)
            }
        else None
      }
    }.headOption

  private def searchContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): Option[(Proof, Subst)] =
    context.view
      .flatMap { case (name, hyp) =>
        val ch = applySubst(hyp, subst)
        if ch == Expr.Sym("⊥") || ch == Expr.Sym("0") then
          Some((List(ProofStep.Apply(StandardRules.initialUniversal, subst)), subst))
        else
          ch match {
            case Expr.App(Expr.Sym("="), List(l, r)) =>
              val cl = applySubst(l, subst); val cr = applySubst(r, subst)
              val rw1 = rewriteExpr(goal, cl, cr)
              val res1 = if rw1 != goal then search(rw1, rules, context, subst, depth + 1, maxDepth, visited, raaCount).map {
                case (p, s) => (p :+ ProofStep.Apply(StandardRules.eqSubst, s), s)
              } else None
              res1.orElse {
                val rw2 = rewriteExpr(goal, cr, cl)
                if rw2 != goal then search(rw2, rules, context, subst, depth + 1, maxDepth, visited, raaCount).map {
                  case (p, s) => (p :+ ProofStep.Apply(StandardRules.eqSubst, s), s)
                } else None
              }
            case _ => None
          }
      }
      .headOption
      .orElse {
        context.view.flatMap { case (name, hyp) =>
          useHypothesis(name, hyp, goal, rules, context, subst, depth, maxDepth, visited, raaCount)
        }.headOption
      }

  private def useHypothesis(
      name: String,
      hyp: Expr,
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): Option[(Proof, Subst)] = {
    val currentHyp = applySubst(hyp, subst)
    if (currentHyp == Expr.Sym("⊥") || currentHyp == Expr.Sym("0")) {
      Some((List(ProofStep.Apply(StandardRules.initialUniversal, subst)), subst))
    } else {
      unify(currentHyp, goal, subst)
        .map(s => (List(ProofStep.Apply(CatRule(name, currentHyp, currentHyp), s)), s))
        .orElse {
          currentHyp match {
            case Expr.App(Expr.Sym(op), List(a, b)) if op == "×" || op == "∧" =>
              useHypothesis(s"$name.1", a, goal, rules, context, subst, depth, maxDepth, visited, raaCount)
                .orElse(useHypothesis(s"$name.2", b, goal, rules, context, subst, depth, maxDepth, visited, raaCount))
            case Expr.App(Expr.Sym("→") | Expr.Sym("⊃") | Expr.Sym("⇒"), List(a, b)) =>
              useHypothesis(name, b, goal, rules, context, subst, depth, maxDepth, visited, raaCount).flatMap { case (proofB, s1) =>
                search(a, rules, context, s1, depth + 1, maxDepth, visited, raaCount)
                  .map { case (proofA, s2) =>
                    (proofB ++ proofA :+ ProofStep.Apply(CatRule(name, a, b), s2), s2)
                  }
              }
            case Expr.App(Expr.Sym("∀"), List(Expr.Var(v), body)) =>
              val meta = freshMeta
              val inst = substVar(body, v, meta)
              useHypothesis(name, inst, goal, rules, context, subst, depth, maxDepth, visited, raaCount).map { case (proof, s) =>
                (proof :+ ProofStep.Apply(CatRule(name, currentHyp, inst), s), s)
              }
            case _ => None
          }
        }
    }
  }

  private def decomposeContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): Option[(Proof, Subst)] =
    context.zipWithIndex.view.flatMap {
      case ((name, Expr.App(Expr.Sym("∃"), List(Expr.Var(v), body))), i) =>
        val witness = freshMeta
        val instantiated = substVar(body, v, witness)
        val newCtx = context.patch(i, List((s"${name}.witness", instantiated)), 1)
        search(goal, rules, newCtx, subst, depth + 1, maxDepth, visited, raaCount)
      case ((name, Expr.App(Expr.Sym(op), List(a, b))), i) if op == "×" || op == "∧" =>
        val newCtx = context.patch(i, List((s"${name}.1", a), (s"${name}.2", b)), 1)
        search(goal, rules, newCtx, subst, depth + 1, maxDepth, visited, raaCount)
      case ((name, Expr.App(Expr.Sym(op), List(a, b))), i) if op == "+" || op == "∨" =>
        val leftCtx = context.patch(i, List((s"${name}.left", a)), 1)
        search(goal, rules, leftCtx, subst, depth + 1, maxDepth, visited, raaCount).flatMap { case (lp, ls) =>
          val rightCtx = context.patch(i, List((s"${name}.right", b)), 1)
          search(goal, rules, rightCtx, ls, depth + 1, maxDepth, visited, raaCount).map {
            case (rp, rs) => (lp ++ rp :+ ProofStep.Apply(StandardRules.coproductUniversal, rs), rs)
          }
        }
      case _ => None
    }.headOption

  private def instantiate(rule: CatRule): (CatRule, Map[String, Expr]) =
    val vars = collectVars(rule.lhs) ++ collectVars(rule.rhs) ++ rule.universals.flatMap(collectVars)
    val substMap = vars.map(v => v -> freshMeta).toMap
    val instLhs = applyVarSubst(rule.lhs, substMap)
    val instRhs = applyVarSubst(rule.rhs, substMap)
    val instUniv = rule.universals.map(applyVarSubst(_, substMap))
    (CatRule(rule.name, instLhs, instRhs, instUniv), substMap)

  private def collectVars(e: Expr): Set[String] = e match
    case Expr.Var(n)       => Set(n)
    case Expr.App(h, args) => collectVars(h) ++ args.flatMap(collectVars)
    case _                 => Set.empty

  private def applyVarSubst(e: Expr, s: Map[String, Expr]): Expr = e match
    case Expr.Var(n) if s.contains(n) => s(n)
    case Expr.App(h, args)            => Expr.App(applyVarSubst(h, s), args.map(applyVarSubst(_, s)))
    case _ => e

  private def substVar(expr: Expr, varName: String, replacement: Expr): Expr =
    expr match
      case Expr.Var(n) if n == varName => replacement
      case Expr.App(h, args)           => Expr.App(substVar(h, varName, replacement), args.map(substVar(_, varName, replacement)))
      case _ => expr

  private def rewriteExpr(expr: Expr, from: Expr, to: Expr): Expr =
    if expr == from then to
    else expr match
      case Expr.App(h, args) => Expr.App(rewriteExpr(h, from, to), args.map(rewriteExpr(_, from, to)))
      case _ => expr
}