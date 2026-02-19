// ==========================================
// ForwardReasoningSearch.scala
// 前向き推論プラグイン（モーダスポネンス、全称除去、等式書き換え、存在除去）
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Utils.Debug.logger

class ForwardReasoningPlugin extends LogicPlugin {
  override def name: String = "ForwardReasoning"
  override def priority: Int = 80

  import Unifier._

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

    // 既に前向き導出済みならスキップ（飽和済み）
    val alreadyDerived = state.custom.getOrElse("forwardDerived", Set.empty[Expr])
      .asInstanceOf[Set[Expr]]

    val allHyps = context // 線形コンテキストは含めない
    val derived = scala.collection.mutable.LinkedHashSet[(String, Expr)]()
    val derivedExprs = scala.collection.mutable.Set[Expr]()
    derivedExprs ++= alreadyDerived

    // 飽和ループ（最大1回）
    var currentCtx = allHyps
    var iteration = 0
    val maxIterations = 1

    while (iteration < maxIterations) {
      prover.checkDeadline()
      val newFacts = scala.collection.mutable.ListBuffer[(String, Expr)]()

      // a. 汎用モーダスポネンス
      for ((name, hyp) <- currentCtx) {
        val normHyp = prover.normalize(applySubst(hyp, subst))
        normHyp match {
          case Expr.App(Expr.Sym(op), List(a, b))
            if op == Implies || op == "→" || op == ImpliesAlt1 || op == ImpliesAlt2 =>
            val normA = prover.normalize(applySubst(a, subst))
            for ((_, h) <- currentCtx) {
              val normH = prover.normalize(applySubst(h, subst))
              unify(normH, normA, subst).foreach { s =>
                val conclusion = prover.normalize(applySubst(b, s))
                if (!derivedExprs.contains(conclusion.canonicalize()) &&
                    !currentCtx.exists(ch => prover.normalize(applySubst(ch._2, subst)).canonicalize() == conclusion.canonicalize())) {
                  newFacts += ((s"fwd-mp[$name]", conclusion))
                  derivedExprs += conclusion.canonicalize()
                }
              }
            }
          case _ => ()
        }
      }

      // b. 全称除去
      val groundTerms = collectGroundTerms(currentCtx.map(_._2), subst, prover) ++
        collectGroundTerms(List(goal), subst, prover)

      for ((name, hyp) <- currentCtx) {
        val normHyp = prover.normalize(applySubst(hyp, subst))
        normHyp match {
          case Expr.App(Expr.Sym(Forall), args) =>
            val (vName, body) = args match {
              case List(Expr.Var(v), b) => (v, b)
              case List(Expr.Var(v), _, b) => (v, b)
              case _ => (null, null)
            }
            if (vName != null) {
              for (t <- groundTerms.take(5)) { // 候補を制限
                val instantiated = prover.normalize(Prover.substVar(body, vName, t))
                if (!derivedExprs.contains(instantiated.canonicalize()) &&
                    !currentCtx.exists(ch => prover.normalize(applySubst(ch._2, subst)).canonicalize() == instantiated.canonicalize())) {
                  newFacts += ((s"fwd-forall-elim[$name]", instantiated))
                  derivedExprs += instantiated.canonicalize()
                }
              }
            }
          case _ => ()
        }
      }

      // c. 等式の前向き書き換え
      for ((name, hyp) <- currentCtx) {
        val normHyp = prover.normalize(applySubst(hyp, subst))
        normHyp match {
          case Expr.App(Expr.Sym(Eq), List(a, b)) =>
            for ((hName, h) <- currentCtx if hName != name) {
              val normH = prover.normalize(applySubst(h, subst))
              if (normH.contains(a) && normH != normHyp) {
                val rewritten = prover.normalize(substituteExpr(normH, a, b))
                if (rewritten != normH &&
                    !derivedExprs.contains(rewritten.canonicalize()) &&
                    !currentCtx.exists(ch => prover.normalize(applySubst(ch._2, subst)).canonicalize() == rewritten.canonicalize())) {
                  newFacts += ((s"fwd-eq-rewrite[$name,$hName]", rewritten))
                  derivedExprs += rewritten.canonicalize()
                }
              }
            }
          case _ => ()
        }
      }

      // d. 存在除去
      for ((name, hyp) <- currentCtx) {
        val normHyp = prover.normalize(applySubst(hyp, subst))
        normHyp match {
          case Expr.App(Expr.Sym(Exists), args) =>
            val (vName, body) = args match {
              case List(Expr.Var(v), b) => (v, b)
              case List(Expr.Var(v), _, b) => (v, b)
              case _ => (null, null)
            }
            if (vName != null) {
              val fresh = Expr.Var(s"witness_${name}_$depth")
              val instantiated = prover.normalize(Prover.substVar(body, vName, fresh))
              if (!derivedExprs.contains(instantiated.canonicalize()) &&
                  !currentCtx.exists(ch => prover.normalize(applySubst(ch._2, subst)).canonicalize() == instantiated.canonicalize())) {
                newFacts += ((s"fwd-exists-elim[$name]", instantiated))
                derivedExprs += instantiated.canonicalize()
              }
            }
          case _ => ()
        }
      }

      // e. 前向きルールインデックスによる導出
      for ((name, hyp) <- currentCtx) {
        val normHyp = prover.normalize(applySubst(hyp, subst))
        val fwdResults = prover.forwardApplyRules(normHyp, subst, depth)
        for ((result, ruleName, s) <- fwdResults) {
          val normResult = prover.normalize(applySubst(result, s))
          if (!derivedExprs.contains(normResult.canonicalize()) &&
              !currentCtx.exists(ch => prover.normalize(applySubst(ch._2, subst)).canonicalize() == normResult.canonicalize())) {
            newFacts += ((s"fwd-rule[$ruleName,$name]", normResult))
            derivedExprs += normResult.canonicalize()
          }
        }
      }

      if (newFacts.isEmpty) {
        iteration = maxIterations // 固定点到達
      } else {
        derived ++= newFacts
        currentCtx = currentCtx ++ newFacts.toList
        iteration += 1
      }
    }

    if (derived.isEmpty) return Vector.empty

    logger.log(s"ForwardReasoning: ${derived.size} new facts derived")

    // 導出した事実をコンテキストに追加して後続探索
    val enrichedCtx = context ++ derived.toList
    val newState = state.copy(
      custom = state.custom + ("forwardDerived" -> derivedExprs.toSet)
    )

    val subTree = prover.search(exprs, enrichedCtx, newState, subst, depth + 1, limit, visited, guarded)
    allSuccesses(subTree).map { s =>
      val proofChildren = List(s.result.toOption.get.tree)
      val result = Right(ProofResult(
        ProofTree.Node(applySubst(goal, s.subst), "forward-reasoning", proofChildren)
      ))
      Tree.V(
        SearchNode(exprs, "forward-reasoning", depth, result, s.subst, s.context, s.linearContext),
        Vector(subTree)
      )
    }.toVector
  }

  /** 式中のサブ式を置換 */
  private def substituteExpr(expr: Expr, target: Expr, replacement: Expr): Expr = {
    if (expr == target) replacement
    else expr match {
      case Expr.App(f, args) =>
        Expr.App(substituteExpr(f, target, replacement), args.map(substituteExpr(_, target, replacement)))
      case _ => expr
    }
  }

  /** コンテキストとゴールから具体的な項（非変数・非メタ）を収集 */
  private def collectGroundTerms(
      exprs: List[Expr],
      subst: Unifier.Subst,
      prover: ProverInterface
  ): Set[Expr] = {
    val terms = scala.collection.mutable.Set[Expr]()
    def collect(e: Expr): Unit = e match {
      case Expr.Var(_) | Expr.Meta(_) => ()
      case Expr.Sym(n) if n != Implies && n != Forall && n != Exists && n != And && n != Or && n != Not && n != Eq =>
        terms += e
      case Expr.App(Expr.Sym(n), _) if Set(Implies, Forall, Exists, And, Or, Not, "→", Eq).contains(n) =>
        e match {
          case Expr.App(_, args) => args.foreach(collect)
          case _ => ()
        }
      case Expr.App(f, args) =>
        terms += e
        collect(f)
        args.foreach(collect)
      case _ => ()
    }
    exprs.foreach(e => collect(prover.normalize(Unifier.applySubst(e, subst))))
    terms.toSet
  }
}
