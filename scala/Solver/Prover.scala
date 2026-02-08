// ==========================================
// Prover.scala
// 証明探索エンジン
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import romanesco.Utils.Debug.logger
import LogicSymbols._

final class Prover(val classical: Boolean = false) {
  import Unifier._

  private var metaCounter = 0
  private def freshMeta: Expr = { metaCounter += 1; Expr.Meta(metaCounter) }

  type Context = List[(String, Expr)]

  /**
   * 与えられた目標(goal)を証明します。
   */
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

      // 探索の優先順位
      searchAxiom(currentGoal, context, subst)
        .orElse(searchReflexivity(currentGoal, subst))
        .orElse(searchDecomposeGoal(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount))
        .orElse(searchDecomposeContext(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount))
        .orElse(searchContext(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount))
        .orElse(searchRules(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount))
        .orElse(searchClassical(currentGoal, rules, context, subst, depth, maxDepth, nextVisited, raaCount))

  // --- 各探索フェーズ ---

  /** 1. コンテキストにゴールが直接存在するか確認 (Axiom) */
  private def searchAxiom(goal: Expr, context: Context, subst: Subst): Option[(Proof, Subst)] =
    context.view.flatMap { case (name, hyp) =>
      unify(hyp, goal, subst).map(s => (List(ProofStep.Apply(CatRule(name, hyp, hyp), s)), s))
    }.headOption

  /** 2. 等式の反射性を確認 (Reflexivity) */
  private def searchReflexivity(goal: Expr, subst: Subst): Option[(Proof, Subst)] =
    goal match
      case Expr.App(Expr.Sym(Eq), List(l, r)) =>
        unify(l, r, subst).map { s => (List(ProofStep.Apply(StandardRules.eqRefl, s)), s) }
      case _ => None

  /** 3. ゴールを分解 (Intro rules) */
  private def searchDecomposeGoal(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, maxDepth: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): Option[(Proof, Subst)] =
    goal match
      // 連言 / 積
      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        for
          (proofA, s1) <- search(a, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
          (proofB, s2) <- search(b, rules, context, s1, depth + 1, maxDepth, visited, raaCount)
        yield
          (proofA ++ proofB :+ ProofStep.Apply(StandardRules.productUniversal, s2), s2)

      // 含意 / 指数
      case Expr.App(Expr.Sym(op), List(a, b))
          if op == Implies || op == Exp || op == ImpliesAlt1 || op == ImpliesAlt2 =>
        val (ant, cons) = if (op == Exp) (b, a) else (a, b)
        val hypName = s"h$depth"
        search(cons, rules, (hypName, ant) :: context, subst, depth + 1, maxDepth, visited, raaCount)
          .map { case (proof, s) => (proof :+ ProofStep.Apply(StandardRules.expUniversal, s), s) }

      // 全称量化
      case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
        val freshVar = s"${v}_$depth"
        val instantiated = substVar(body, v, Expr.Var(freshVar))
        search(instantiated, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
          .map { case (proof, s) => (proof :+ ProofStep.Apply(StandardRules.forallCounit, s), s) }

      // 存在量化
      case Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body)) =>
        val witness = freshMeta
        val instantiated = substVar(body, v, witness)
        search(instantiated, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
          .map { case (proof, s) => (proof :+ ProofStep.Apply(StandardRules.existsUnit, s), s) }

      // 選言 / 和
      case Expr.App(Expr.Sym(Or | Coproduct), List(a, b)) =>
        search(a, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
          .map { case (p, s) => (p :+ ProofStep.Apply(CatRule("inl", a, goal), s), s) }
          .orElse(
            search(b, rules, context, subst, depth + 1, maxDepth, visited, raaCount)
              .map { case (p, s) => (p :+ ProofStep.Apply(CatRule("inr", b, goal), s), s) }
          )

      // 真 / 終対象
      case Expr.Sym(True | Terminal) =>
        Some((List(ProofStep.Apply(StandardRules.terminalUniversal, subst)), subst))

      case _ => None

  /** 4. コンテキスト内の仮定を分解 (Elimination rules) */
  private def searchDecomposeContext(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, maxDepth: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): Option[(Proof, Subst)] =
    context.zipWithIndex.view.flatMap {
      // 存在量化の分解 (Witness抽出)
      case ((name, Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body))), i) =>
        val witness = freshMeta
        val instantiated = substVar(body, v, witness)
        val newCtx = context.patch(i, List((s"${name}.witness", instantiated)), 1)
        search(goal, rules, newCtx, subst, depth + 1, maxDepth, visited, raaCount)
      
      // 連言の分解
      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        val newCtx = context.patch(i, List((s"${name}.1", a), (s"${name}.2", b)), 1)
        search(goal, rules, newCtx, subst, depth + 1, maxDepth, visited, raaCount)
      
      // 選言の分解 (ケース分析)
      case ((name, Expr.App(Expr.Sym(Or | Coproduct), List(a, b))), i) =>
        val leftCtx = context.patch(i, List((s"${name}.left", a)), 1)
        search(goal, rules, leftCtx, subst, depth + 1, maxDepth, visited, raaCount).flatMap { case (lp, ls) =>
          val rightCtx = context.patch(i, List((s"${name}.right", b)), 1)
          search(goal, rules, rightCtx, ls, depth + 1, maxDepth, visited, raaCount).map {
            case (rp, rs) => (lp ++ rp :+ ProofStep.Apply(StandardRules.coproductUniversal, rs), rs)
          }
        }
      case _ => None
    }.headOption

  /** 5. コンテキスト内の仮定を利用 (Application / Rewriting) */
  private def searchContext(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, maxDepth: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): Option[(Proof, Subst)] =
    context.view.flatMap { case (name, hyp) =>
      val ch = applySubst(hyp, subst)
      // 偽 / 始対象からの推論 (Ex Falso)
      if ch == Expr.Sym(False) || ch == Expr.Sym(Initial) then
        Some((List(ProofStep.Apply(StandardRules.initialUniversal, subst)), subst))
      else ch match
        // 等式による書き換え
        case Expr.App(Expr.Sym(Eq), List(l, r)) =>
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
    }.headOption
    .orElse {
      // 含意や全称量化の適用
      context.view.flatMap { case (name, hyp) =>
        useHypothesis(name, hyp, goal, rules, context, subst, depth, maxDepth, visited, raaCount)
      }.headOption
    }

  /** 仮定(hyp)を再帰的に適用してゴールを導けるか試みる */
  private def useHypothesis(
      name: String, hyp: Expr, goal: Expr, rules: List[CatRule], context: Context,
      subst: Subst, depth: Int, maxDepth: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): Option[(Proof, Subst)] = {
    val currentHyp = applySubst(hyp, subst)
    if (currentHyp == Expr.Sym(False) || currentHyp == Expr.Sym(Initial)) {
      Some((List(ProofStep.Apply(StandardRules.initialUniversal, subst)), subst))
    } else {
      unify(currentHyp, goal, subst)
        .map(s => (List(ProofStep.Apply(CatRule(name, currentHyp, currentHyp), s)), s))
        .orElse {
          currentHyp match
            case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
              useHypothesis(s"$name.1", a, goal, rules, context, subst, depth, maxDepth, visited, raaCount)
                .orElse(useHypothesis(s"$name.2", b, goal, rules, context, subst, depth, maxDepth, visited, raaCount))
            case Expr.App(Expr.Sym(Implies | ImpliesAlt1 | ImpliesAlt2), List(a, b)) =>
              useHypothesis(name, b, goal, rules, context, subst, depth, maxDepth, visited, raaCount).flatMap { case (proofB, s1) =>
                search(a, rules, context, s1, depth + 1, maxDepth, visited, raaCount)
                  .map { case (proofA, s2) => (proofB ++ proofA :+ ProofStep.Apply(CatRule(name, a, b), s2), s2) }
              }
            case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
              val meta = freshMeta
              val inst = substVar(body, v, meta)
              useHypothesis(name, inst, goal, rules, context, subst, depth, maxDepth, visited, raaCount).map { case (proof, s) =>
                (proof :+ ProofStep.Apply(CatRule(name, currentHyp, inst), s), s)
              }
            case _ => None
        }
    }
  }

  /** 6. 追加ルール (外部定義ルール) */
  private def searchRules(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, maxDepth: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): Option[(Proof, Subst)] =
    rules.view.flatMap { rule =>
      val (instRule, _) = instantiate(rule)
      unify(instRule.rhs, goal, subst).flatMap { s =>
        val universalsOk = instRule.universals.forall { cond =>
          search(cond, rules, context, s, depth + 1, maxDepth, visited, raaCount).isDefined
        }
        if universalsOk then
          search(instRule.lhs, rules, context, s, depth + 1, maxDepth, visited, raaCount)
            .map { case (proof, finalSubst) => (proof :+ ProofStep.Apply(rule, finalSubst), finalSubst) }
        else None
      }
    }.headOption

  /** 7. 古典論理ルール (RAA) */
  private def searchClassical(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, maxDepth: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): Option[(Proof, Subst)] =
    if classical && raaCount < 1 && goal != Expr.Sym(False) && goal != Expr.Sym(Initial) then
      logger.log(s"trying RAA for goal: $goal")
      val negation = Expr.App(Expr.Sym(Implies), List(goal, Expr.Sym(False)))
      search(Expr.Sym(False), rules, (s"raa$depth", negation) :: context, subst, depth + 1, maxDepth, visited, raaCount + 1)
        .map { case (p, s) =>
          val raaStep = ProofStep.Apply(CatRule("RAA", Expr.App(Expr.Sym(Implies), List(negation, Expr.Sym(False))), goal), s)
          (p :+ raaStep, s)
        }
    else None

  // --- ユーティリティ ---

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
