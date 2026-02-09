// ==========================================
// Prover.scala
// 証明探索エンジン（ProofTree・Appラムダ対応）
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import romanesco.Utils.Debug.logger
import LogicSymbols._

final class Prover(
    val classical: Boolean = false, 
    rules: List[CatRule] = StandardRules.all,
    val maxRaa: Int = 2 
) {
  import Unifier._

  private var metaCounter = 0
  private var bestFail: Option[FailTrace] = None

  private val ruleIndex: Map[String, List[CatRule]] = {
    val allRules = if classical then rules ++ StandardRules.classical else rules
    allRules.groupBy(_.rhs.headSymbol)
  }

  private def freshMeta(depth: Int): Expr = {
    metaCounter += 1
    Expr.Meta(MetaId(List(depth, metaCounter)))
  }

  private def recordFail(goal: Goal, reason: String, depth: Int, children: List[FailTrace] = Nil): Unit = {
    val trace = FailTrace(goal, reason, depth, children)
    if (bestFail.isEmpty || depth >= bestFail.get.depth) {
      bestFail = Some(trace)
    }
  }

  type Context = List[(String, Expr)]

  def prove(
      goal: Expr,
      rules: List[CatRule] = StandardRules.all,
      maxDepth: Int = 30
  ): Either[FailTrace, ProofTree] =
    logger.log(s"prove begin. goal: $goal (classical: $classical, maxDepth: $maxDepth)")
    bestFail = None
    
    val result = (1 to maxDepth).view.flatMap { d =>
      logger.log(s"--- Iterative Deepening: current limit = $d ---")
      metaCounter = 0
      search(goal, rules, Nil, emptySubst, 0, d, Set.empty, 0).map(_._1)
    }.headOption

    result match {
      case Some(tree) => Right(tree)
      case None => Left(bestFail.getOrElse(FailTrace(Goal(Nil, goal), "Unknown failure", 0)))
    }

  private def search(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int
  ): LazyList[(ProofTree, Subst)] =
    val currentGoal = applySubst(goal, subst)
    val contextExprs = context.map(h => applySubst(h._2, subst)).toSet
    val currentGoalObj = Goal(context, currentGoal)

    if depth > limit then 
      recordFail(currentGoalObj, s"Depth limit reached ($limit)", depth)
      LazyList.empty
    else if visited.contains((currentGoal, contextExprs)) then
      recordFail(currentGoalObj, "Cycle detected", depth)
      LazyList.empty
    else
      val nextVisited = visited + ((currentGoal, contextExprs))

      val results = 
        searchAxiom(currentGoal, context, subst, depth) #:::
        searchReflexivity(currentGoal, subst, depth) #:::
        searchDecomposeGoal(currentGoal, rules, context, subst, depth, limit, nextVisited, raaCount) #:::
        searchDecomposeContext(currentGoal, rules, context, subst, depth, limit, nextVisited, raaCount) #:::
        searchContext(currentGoal, rules, context, subst, depth, limit, nextVisited, raaCount) #:::
        searchRules(currentGoal, rules, context, subst, depth, limit, nextVisited, raaCount) #:::
        searchClassical(currentGoal, rules, context, subst, depth, limit, nextVisited, raaCount)

      if (results.isEmpty) {
        recordFail(currentGoalObj, "No rules applicable", depth)
      }
      results

  private def searchAxiom(goal: Expr, context: Context, subst: Subst, depth: Int): LazyList[(ProofTree, Subst)] =
    context.view.to(LazyList).flatMap { case (name, hyp) =>
      unify(hyp, goal, subst).map(s => (ProofTree.Leaf(applySubst(goal, s), name), s))
    }

  private def searchReflexivity(goal: Expr, subst: Subst, depth: Int): LazyList[(ProofTree, Subst)] =
    goal match
      case Expr.App(Expr.Sym(Eq), List(l, r)) =>
        unify(l, r, subst).map { s => (ProofTree.Leaf(applySubst(goal, s), StandardRules.eqRefl.name), s) }
      case _ => LazyList.empty

  private def searchDecomposeGoal(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, limit: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): LazyList[(ProofTree, Subst)] =
    goal match
      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        for
          (treeA, s1) <- search(a, rules, context, subst, depth + 1, limit, visited, raaCount)
          (treeB, s2) <- search(b, rules, context, s1, depth + 1, limit, visited, raaCount)
        yield
          (ProofTree.Node(applySubst(goal, s2), StandardRules.productUniversal.name, List(treeA, treeB)), s2)

      case Expr.App(Expr.Sym(op), List(a, b))
          if op == Implies || op == Exp || op == ImpliesAlt1 || op == ImpliesAlt2 =>
        val (ant, cons) = if (op == Exp) (b, a) else (a, b)
        val hypName = s"h$depth"
        search(cons, rules, (hypName, ant) :: context, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), StandardRules.expUniversal.name, List(tree)), s) }

      case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
        val freshVar = s"${v}_$depth"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        search(instantiated, rules, context, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), StandardRules.forallCounit.name, List(tree)), s) }

      case Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body)) =>
        val witness = freshMeta(depth)
        val instantiated = Prover.substVar(body, v, witness)
        search(instantiated, rules, context, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), StandardRules.existsUnit.name, List(tree)), s) }

      case Expr.App(Expr.Sym(Or | Coproduct), List(a, b)) =>
        search(a, rules, context, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), "inl", List(tree)), s) } #:::
        search(b, rules, context, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), "inr", List(tree)), s) }

      case Expr.Sym(True | Terminal) =>
        LazyList((ProofTree.Leaf(goal, StandardRules.terminalUniversal.name), subst))

      case _ => LazyList.empty

  private def searchDecomposeContext(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, limit: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): LazyList[(ProofTree, Subst)] =
    context.zipWithIndex.view.to(LazyList).flatMap {
      case ((name, Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body))), i) =>
        val freshVar = s"${v}_${depth}_sk"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        val newCtx = context.patch(i, List((s"${name}.witness", instantiated)), 1)
        search(goal, rules, newCtx, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), s"destruct[$name]", List(tree)), s) }
      
      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        val newCtx = context.patch(i, List((s"${name}.1", a), (s"${name}.2", b)), 1)
        search(goal, rules, newCtx, subst, depth + 1, limit, visited, raaCount)
          .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), s"destruct[$name]", List(tree)), s) }
      
      case ((name, Expr.App(Expr.Sym(Or | Coproduct), List(a, b))), i) =>
        val leftCtx = context.patch(i, List((s"${name}.left", a)), 1)
        search(goal, rules, leftCtx, subst, depth + 1, limit, visited, raaCount).flatMap { case (treeL, ls) =>
          val rightCtx = context.patch(i, List((s"${name}.right", b)), 1)
          search(goal, rules, rightCtx, ls, depth + 1, limit, visited, raaCount).map {
            case (treeR, rs) => (ProofTree.Node(applySubst(goal, rs), s"destruct[$name]", List(treeL, treeR)), rs)
          }
        }
      case _ => LazyList.empty
    }

  private def searchContext(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, limit: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): LazyList[(ProofTree, Subst)] =
    context.view.to(LazyList).flatMap { case (name, hyp) =>
      val ch = applySubst(hyp, subst)
      if ch == Expr.Sym(False) || ch == Expr.Sym(Initial) then
        LazyList((ProofTree.Leaf(goal, s"absurd[$name]"), subst))
      else ch match
        case Expr.App(Expr.Sym(Eq), List(l, r)) =>
          val cl = applySubst(l, subst); val cr = applySubst(r, subst)
          val rw1 = Prover.rewriteExpr(goal, cl, cr)
          val res1 = if rw1 != goal then search(rw1, rules, context, subst, depth + 1, limit, visited, raaCount).map {
            case (tree, s) => (ProofTree.Node(applySubst(goal, s), s"rewrite[$name]", List(tree)), s)
          } else LazyList.empty
          res1 #::: {
            val rw2 = Prover.rewriteExpr(goal, cr, cl)
            if rw2 != goal then search(rw2, rules, context, subst, depth + 1, limit, visited, raaCount).map {
              case (tree, s) => (ProofTree.Node(applySubst(goal, s), s"rewrite[$name]", List(tree)), s)
            } else LazyList.empty
          }
        case _ => LazyList.empty
    } #::: {
      context.view.to(LazyList).flatMap { case (name, hyp) =>
        useHypothesis(name, hyp, goal, rules, context, subst, depth, limit, visited, raaCount)
      }
    }

  private def useHypothesis(
      name: String, hyp: Expr, goal: Expr, rules: List[CatRule], context: Context,
      subst: Subst, depth: Int, limit: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): LazyList[(ProofTree, Subst)] = {
    val currentHyp = applySubst(hyp, subst)
    if (currentHyp == Expr.Sym(False) || currentHyp == Expr.Sym(Initial)) {
      LazyList((ProofTree.Leaf(goal, s"absurd[$name]"), subst))
    } else {
      unify(currentHyp, goal, subst)
        .map(s => (ProofTree.Leaf(applySubst(goal, s), name), s)) #::: {
          currentHyp match
            case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
              useHypothesis(s"$name.1", a, goal, rules, context, subst, depth, limit, visited, raaCount) #:::
              useHypothesis(s"$name.2", b, goal, rules, context, subst, depth, limit, visited, raaCount)
            case Expr.App(Expr.Sym(Implies | ImpliesAlt1 | ImpliesAlt2), List(a, b)) =>
              useHypothesis(name, b, goal, rules, context, subst, depth, limit, visited, raaCount).flatMap { case (treeB, s1) =>
                search(a, rules, context, s1, depth + 1, limit, visited, raaCount)
                  .map { case (treeA, s2) => (ProofTree.Node(applySubst(goal, s2), s"apply[$name]", List(treeB, treeA)), s2) }
              }
            case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
              val meta = freshMeta(depth)
              val inst = Prover.substVar(body, v, meta)
              useHypothesis(name, inst, goal, rules, context, subst, depth, limit, visited, raaCount)
                .map { case (tree, s) => (ProofTree.Node(applySubst(goal, s), s"instantiate[$name]", List(tree)), s) }
            case _ => LazyList.empty
        }
    }
  }

  private def searchRules(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, limit: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): LazyList[(ProofTree, Subst)] =
    val head = goal.headSymbol
    val candidates = ruleIndex.getOrElse(head, Nil) ++ 
                     ruleIndex.getOrElse("_VAR_", Nil) ++ 
                     ruleIndex.getOrElse("_META_", Nil)

    candidates.to(LazyList).flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      unify(instRule.rhs, goal, subst).flatMap { s =>
        val universes = instRule.universals
        def solveUniversals(conds: List[Expr], currentSubst: Subst, solved: List[ProofTree]): LazyList[(List[ProofTree], Subst)] = conds match {
          case Nil => LazyList((solved, currentSubst))
          case head :: tail =>
            search(head, rules, context, currentSubst, depth + 1, limit, visited, raaCount).flatMap {
              case (tree, nextSubst) => solveUniversals(tail, nextSubst, solved :+ tree)
            }
        }

        solveUniversals(universes, s, Nil).flatMap { case (univTrees, s2) =>
          search(instRule.lhs, rules, context, s2, depth + 1, limit, visited, raaCount).map {
            case (mainTree, s3) => (ProofTree.Node(applySubst(goal, s3), rule.name, univTrees :+ mainTree), s3)
          }
        }
      }
    }

  private def searchClassical(
      goal: Expr, rules: List[CatRule], context: Context, subst: Subst,
      depth: Int, limit: Int, visited: Set[(Expr, Set[Expr])], raaCount: Int
  ): LazyList[(ProofTree, Subst)] =
    if classical && raaCount < maxRaa && goal != Expr.Sym(False) && goal != Expr.Sym(Initial) then
      val negation = Expr.App(Expr.Sym(Implies), List(goal, Expr.Sym(False)))
      search(Expr.Sym(False), rules, (s"raa$depth", negation) :: context, subst, depth + 1, limit, visited, raaCount + 1)
        .map { case (tree, s) =>
          (ProofTree.Node(applySubst(goal, s), "RAA", List(tree)), s)
        }
    else LazyList.empty
}

object Prover {
  import Unifier._

  def substVar(expr: Expr, varName: String, replacement: Expr): Expr =
    expr match
      case Expr.Var(n) if n == varName => replacement
      case Expr.Sym(n) if n == varName => replacement
      case Expr.App(h, args) =>
        expr match
          case Expr.Lam(v, body) if v == varName => expr
          case Expr.Lam(v, body) => Expr.App(Expr.Sym("λ"), List(Expr.Var(v), substVar(body, varName, replacement)))
          case _ => Expr.App(substVar(h, varName, replacement), args.map(substVar(_, varName, replacement)))
      case _ => expr

  def rewriteExpr(expr: Expr, from: Expr, to: Expr): Expr =
    if expr == from then to
    else expr match
      case Expr.App(h, args) => Expr.App(rewriteExpr(h, from, to), args.map(rewriteExpr(_, from, to)))
      case _ => expr

  def instantiate(rule: CatRule, freshMeta: () => Expr): (CatRule, Map[MetaId, Expr]) =
    val vars = collectVars(rule.lhs) ++ collectVars(rule.rhs) ++ rule.universals.flatMap(collectVars)
    val substMap = vars.map(v => v -> freshMeta()).toMap
    
    val metaSubstMap: Subst = substMap.flatMap { 
      case (name, Expr.Meta(id)) => Some(id -> Expr.Meta(id))
      case _ => None
    }

    val instLhs = applyVarSubst(rule.lhs, substMap)
    val instRhs = applyVarSubst(rule.rhs, substMap)
    val instUniv = rule.universals.map(applyVarSubst(_, substMap))
    (CatRule(rule.name, instLhs, instRhs, instUniv), metaSubstMap)

  private def collectVars(e: Expr): Set[String] = e match
    case Expr.Var(n)       => Set(n)
    case Expr.App(h, args) => 
      e match
        case Expr.Lam(v, body) => collectVars(body) - v
        case _ => collectVars(h) ++ args.flatMap(collectVars)
    case _                 => Set.empty

  private def applyVarSubst(e: Expr, s: Map[String, Expr]): Expr = e match
    case Expr.Var(n) if s.contains(n) => s(n)
    case Expr.App(h, args) => 
      e match
        case Expr.Lam(v, body) => Expr.App(Expr.Sym("λ"), List(Expr.Var(v), applyVarSubst(body, s)))
        case _ => Expr.App(applyVarSubst(h, s), args.map(applyVarSubst(_, s)))
    case _ => e
}
