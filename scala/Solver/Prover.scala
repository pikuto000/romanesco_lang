// ==========================================
// Prover.scala
// 証明探索エンジン（安全性重視・ループ防止修正版）
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import romanesco.Utils.Debug.logger
import LogicSymbols._

final class Prover(val config: ProverConfig = ProverConfig.default) {
  import Unifier._

  private var metaCounter = 0
  private var bestFail: Option[FailTrace] = None
  private val failureCache = mutable.Map[(Expr, Set[Expr], Set[Expr], Int, Boolean), FailTrace]()

  private val algebras =
    if (config.algebras.nonEmpty) config.algebras
    else StandardRules.defaultAlgebras

  private val ruleIndex: Map[String, List[CatRule]] = {
    val allRules =
      if config.classical then config.rules ++ StandardRules.classical
      else config.rules
    allRules.groupBy(_.rhs.headSymbol)
  }

  private def freshMeta(depth: Int): Expr = {
    metaCounter += 1
    Expr.Meta(MetaId(List(depth, metaCounter)))
  }

  private def recordFail(trace: FailTrace): Unit = {
    if (bestFail.isEmpty || trace.depth > bestFail.get.depth) {
      bestFail = Some(trace)
    }
  }

  type Context = List[(String, Expr)]

  def prove(
      goal: Expr,
      rules: List[CatRule] = Nil, // 互換性のため残すが、config.rulesを推奨
      maxDepth: Int = 30
  ): Either[FailTrace, ProofResult] =
    val effectiveRules = if (rules.nonEmpty) rules else config.rules
    logger.log(
      s"prove begin. goal: $goal (classical: ${config.classical}, maxDepth: $maxDepth)"
    )
    bestFail = None

    val result = (1 to maxDepth).view.flatMap { d =>
      logger.log(s"--- Iterative Deepening: current limit = $d ---")
      metaCounter = 0
      failureCache.clear()
      val proofs =
        search(goal, effectiveRules, Nil, Nil, emptySubst, 0, d, Set.empty, 0, 0, false)
      // Linearly valid proof must consume all initial resources (which is Nil here)
      proofs.filter(_._3.isEmpty).map(_._1)
    }.headOption

    result match {
      case Some(tree) =>
        val lemma = if (config.generateLemmas) generateLemma(tree) else None
        Right(ProofResult(tree, lemma))
      case None =>
        Left(
          bestFail.getOrElse(FailTrace(Goal(Nil, Nil, goal), "No proof found", 0))
        )
    }

  private def generateLemma(tree: ProofTree): Option[CatRule] = {
    val provedGoal = tree match {
      case ProofTree.Node(g, _, _) => g
      case ProofTree.Leaf(g, _)    => g
    }

    // モードによるフィルタリング
    val modeOk = config.lemmaMode match {
      case LemmaGenerationMode.All           => true
      case LemmaGenerationMode.InductionOnly =>
        def hasInduction(t: ProofTree): Boolean = t match {
          case ProofTree.Node(_, rule, cs) =>
            rule.startsWith("induction") || cs.exists(hasInduction)
          case _ => false
        }
        hasInduction(tree)
      case LemmaGenerationMode.EqualityOnly =>
        provedGoal match {
          case Expr.App(Expr.Sym(Eq | Path), _) => true
          case _                                => false
        }
      case LemmaGenerationMode.ManualOnly => false
    }

    if (!modeOk) return None

    // 変数を収集して一般化するヘルパー
    def collectFreeVars(e: Expr): Set[String] = e match {
      case Expr.Var(n)       => Set(n)
      case Expr.App(h, args) =>
        val vars = args.flatMap(collectFreeVars).toSet
        h match {
          case Expr.Var(n) => vars + n
          case _           => vars
        }
      case _ => Set.empty
    }

    // ProofTreeから簡約形を抽出するヘルパー
    def extractSimplifiedRhs(t: ProofTree, originalLhs: Expr): Expr = {
      t match {
        case ProofTree.Leaf(
              Expr.App(Expr.Sym(Eq | Path), List(_, r)),
              "reflexivity" | "path-reflexivity"
            ) =>
          // reflexivity が使われた場合、その時の正規化された右辺（または左辺）が簡約形
          r
        case ProofTree.Node(_, _, children) if children.nonEmpty =>
          // 再帰的に探索（等式に関する最新の簡約形を探す）
          // 単純化のため、最初の子供から抽出を試みる
          extractSimplifiedRhs(children.head, originalLhs)
        case _ =>
          // 見つからなければ等式の右辺をそのまま使う
          originalLhs match {
            case Expr.App(Expr.Sym(Eq | Path), List(_, r)) => r
            case _                                         => originalLhs
          }
      }
    }

    // 等式の場合
    provedGoal match {
      case Expr.App(Expr.Sym(Eq | Path), List(lhs, rhs)) =>
        // 最終的な簡約形を rhs に設定することを試みる
        val finalRhs = extractSimplifiedRhs(tree, provedGoal) match {
          case Expr.App(Expr.Sym(Eq | Path), List(_, r)) => r
          case other                                     => other
        }

        println(s"[DEBUG] generateLemma check: lhs=$lhs, finalRhs=$finalRhs")
        val isPath = provedGoal.headSymbol == Path
        if (config.excludeTrivialLemmas && lhs == finalRhs && !isPath) {
          println(s"[DEBUG] Skipping trivial lemma for $lhs")
          return None
        }

        // 重複チェック（既存のルールと同一の lhs/rhs を持つか）
        val isDuplicate =
          config.rules.exists(r => r.lhs == lhs && r.rhs == finalRhs)
        if (isDuplicate) return None

        val vars = collectFreeVars(lhs) ++ collectFreeVars(finalRhs)

        // 意味のある名前の生成
        val lhsHead = lhs.headSymbol.toLowerCase.replaceAll("[^a-z0-9]", "_")
        val rhsHead =
          finalRhs.headSymbol.toLowerCase.replaceAll("[^a-z0-9]", "_")
        val prefix =
          if (provedGoal.headSymbol == Path) "path_lemma" else "lemma"
        val baseName = s"${prefix}_${lhsHead}_to_${rhsHead}"
        println(s"[DEBUG] Generating lemma: $baseName for $lhs -> $finalRhs")
        val ruleName =
          s"${baseName}_${java.util.UUID.randomUUID().toString.take(4)}"

        val universals = vars.toList.map(Expr.Var(_))
        Some(CatRule(ruleName, lhs, finalRhs, universals))

      case _ if config.lemmaMode == LemmaGenerationMode.All =>
        // 等式以外でも All モードなら生成を試みる
        val vars = collectFreeVars(provedGoal)
        val head =
          provedGoal.headSymbol.toLowerCase.replaceAll("[^a-z0-9]", "_")
        val ruleName =
          s"lemma_${head}_${java.util.UUID.randomUUID().toString.take(4)}"
        val universals = vars.toList.map(Expr.Var(_))
        Some(CatRule(ruleName, provedGoal, Expr.Sym(True), universals))

      case _ => None
    }
  }

  private def getPathLevel(e: Expr): Int = e match {
    case Expr.App(Expr.Sym(Path), List(sub, _, _)) => 1 + getPathLevel(sub)
    case _                                         => 0
  }

  private def isCoinductive(e: Expr): Boolean = e.headSymbol == Globally

  private def search(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] = {
    logger.log("search begin")
    val currentGoalRaw = applySubst(goal, subst)
    
    // Check maxComplexity
    if (currentGoalRaw.complexity > config.maxComplexity) {
      recordFail(
        FailTrace(Goal(context, linearContext, currentGoalRaw), s"Max complexity reached (${currentGoalRaw.complexity})", depth)
      )
      return LazyList.empty
    }

    val currentGoalCan = currentGoalRaw.canonicalize
    val contextExprs = context.map(h => applySubst(h._2, subst).canonicalize).toSet
    val linearExprs = linearContext.map(h => applySubst(h._2, subst).canonicalize).toSet
    val currentGoalObj = Goal(context, linearContext, currentGoalRaw)

    val remainingDepth = limit - depth
    if (failureCache.contains((currentGoalCan, contextExprs, linearExprs, remainingDepth, guarded))) {
      return LazyList.empty
    }

    if depth > limit then
      recordFail(FailTrace(currentGoalObj, s"Depth limit reached", depth))
      LazyList.empty
    else if visited.contains((currentGoalCan, contextExprs, linearExprs)) then
      if (guarded && isCoinductive(currentGoalCan)) {
        // Co-inductive success: A cycle was reached through a guarded (time-stepping) path.
        LazyList((ProofTree.Leaf(applySubst(goal, subst), "co-induction"), subst, linearContext))
      } else {
        recordFail(FailTrace(currentGoalObj, "Cycle detected", depth))
        LazyList.empty
      }
    else
      val nextVisited = visited + ((currentGoalCan, contextExprs, linearExprs))

      // 優先順位: 自明な解決 -> 構造分解 -> レンマ適用 -> 論理規則
      val results =
        searchAxiom(currentGoalRaw, context, linearContext, subst, depth) #:::
          searchReflexivity(currentGoalRaw, linearContext, subst, depth) #:::
          searchInduction(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          ) #:::
          searchPathInduction(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          ) #:::
          searchDecomposeGoal(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          ) #:::
          searchDecomposeContext(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          ) #:::
          searchRules(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          ) #:::
          searchContext(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          ) #:::
          searchClassical(
            currentGoalRaw,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount,
            guarded
          )

      if (results.isEmpty) {
        val fail =
          FailTrace(currentGoalObj, "Goal unresolvable in this branch", depth)
        failureCache((currentGoalCan, contextExprs, linearExprs, remainingDepth, guarded)) = fail
        recordFail(fail)
      }
      results
  }

  private def searchAxiom(
      goal: Expr,
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst, Context)] =
    logger.log(s"searching Axiom: $goal")
    // Persistent context check: Can only use if all linear resources are consumed
    val persistent = if (linearContext.isEmpty) {
      context.view.to(LazyList).flatMap { case (name, hyp) =>
        unify(hyp, goal, subst)
          .map(s => (ProofTree.Leaf(applySubst(goal, s), name), s, Nil))
      }
    } else LazyList.empty

    // Linear context check: Must consume exactly one resource, and it must be the ONLY one
    val linear = if (linearContext.length == 1) {
      val (name, hyp) = linearContext.head
      unify(hyp, goal, subst)
        .map(s => (ProofTree.Leaf(applySubst(goal, s), s"linear:$name"), s, Nil))
    } else LazyList.empty

    persistent #::: linear

  private def searchReflexivity(
      goal: Expr,
      linearContext: Context,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst, Context)] =
    logger.log(s"searching Reflexivity: $goal")
    // Reflexivity should also consume all linear resources (or be used only when context is empty)
    if (linearContext.nonEmpty) return LazyList.empty

    goal match
      case Expr.App(Expr.Sym(Eq), List(l, r)) =>
        val nl = Rewriter.normalize(applySubst(l, subst))
        val nr = Rewriter.normalize(applySubst(r, subst))
        unify(nl, nr, subst).map { s =>
          (ProofTree.Leaf(applySubst(goal, s), "reflexivity"), s, Nil)
        }
      case Expr.App(Expr.Sym(Path), List(_, l, r)) => // Path(Type, l, r)
        val nl = Rewriter.normalize(applySubst(l, subst))
        val nr = Rewriter.normalize(applySubst(r, subst))
        unify(nl, nr, subst).map { s =>
          (ProofTree.Leaf(applySubst(goal, s), "path-reflexivity"), s, Nil)
        }
      case _ => LazyList.empty

  private def searchDecomposeGoal(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] =
    logger.log(s"searching Decompose: $goal")
    goal match
      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        for
          (treeA, s1, restL1) <- search(
            a,
            rules,
            context,
            linearContext,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded
          )
          (treeB, s2, restL2) <- search(
            b,
            rules,
            context,
            restL1,
            s1,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded
          )
        yield (
          ProofTree.Node(
            applySubst(goal, s2),
            "product-universal",
            List(treeA, treeB)
          ),
          s2,
          restL2
        )

      // Linear Tensor Product (⊗) or Separating Conjunction (*)
      case Expr.App(Expr.Sym(op), List(a, b)) if op == Tensor || op == SepAnd =>
        val introName = if (op == Tensor) "tensor-intro" else "sep-and-intro"
        // Try all possible splits of linearContext
        (0 to linearContext.length).to(LazyList).flatMap { n =>
          linearContext.combinations(n).flatMap { leftPart =>
            val rightPart = linearContext.filterNot(leftPart.contains)
            for
              (treeA, s1, restLA) <- search(a, rules, context, leftPart, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
              if restLA.isEmpty
              (treeB, s2, restLB) <- search(b, rules, context, rightPart, s1, depth + 1, limit, visited, raaCount, inductionCount, guarded)
              if restLB.isEmpty
            yield (
              ProofTree.Node(applySubst(goal, s2), introName, List(treeA, treeB)),
              s2,
              Nil
            )
          }
        }

      case Expr.App(Expr.Sym(op), List(a, b))
          if op == Implies || op == Exp || op == ImpliesAlt1 || op == ImpliesAlt2 =>
        val (ant, cons) = if (op == Exp) (b, a) else (a, b)
        val hypName = s"h$depth"
        search(
          cons,
          rules,
          (hypName, ant) :: context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded
        )
          .map { case (tree, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "exp-universal", List(tree)),
              s,
              restL
            )
          }

      // Linear Implication (⊸)
      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        val hypName = s"lin$depth"
        search(
          b,
          rules,
          context,
          (hypName, a) :: linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded
        )
          .flatMap { case (tree, s, restL) =>
            // Linear implication must consume its argument
            if (!restL.exists(_._1 == hypName)) {
              LazyList((
                ProofTree.Node(applySubst(goal, s), "linear-implies-intro", List(tree)),
                s,
                restL
              ))
            } else LazyList.empty
          }

      case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), typeExpr, body)) =>
        // Typed Forall: ∀v:T. B
        val freshVar = s"${v}_$depth"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        val instantiatedType = Prover.substVar(typeExpr, v, Expr.Var(freshVar))
        search(
          instantiated,
          rules,
          (freshVar, instantiatedType) :: context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded
        )
          .map { case (proof, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "forall-intro-typed", List(proof)),
              s,
              restL
            )
          }

      case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
        // Untyped Forall: ∀v. B
        val freshVar = s"${v}_$depth"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        search(
          instantiated,
          rules,
          context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded
        )
          .map { case (proof, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "forall-intro", List(proof)),
              s,
              restL
            )
          }

      case Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body)) =>
        val witness = freshMeta(depth)
        val instantiated = Prover.substVar(body, v, witness)
        search(
          instantiated,
          rules,
          context,
          linearContext,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded
        )
          .map { case (proof, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "exists-intro", List(proof)),
              s,
              restL
            )
          }

      case Expr.App(Expr.Sym(Or | Coproduct), List(a, b)) =>
        search(
          a,
          rules,
          context,
          linearContext,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded
        )
          .map { case (p, s, restL) =>
            (ProofTree.Node(applySubst(goal, s), "inl", List(p)), s, restL)
          } #:::
          search(
            b,
            rules,
            context,
            linearContext,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded
          )
            .map { case (p, s, restL) =>
              (ProofTree.Node(applySubst(goal, s), "inr", List(p)), s, restL)
            }

      case Expr.Sym(True | Terminal) =>
        LazyList((ProofTree.Leaf(goal, "terminal-universal"), subst, linearContext))

      // --- 時相論理: Globally (G) ---
      case Expr.App(Expr.Sym(Globally), List(a)) =>
        // G(A) -> A ∧ X(G(A))
        val expansion = Expr.App(Expr.Sym(And), List(a, Expr.App(Expr.Sym(Next), List(goal))))
        search(expansion, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded)
          .map { case (p, s, restL) => (ProofTree.Node(applySubst(goal, s), "G-expansion", List(p)), s, restL) }

      // --- 時相論理: Next (X) ---
      case Expr.App(Expr.Sym(Next), List(a)) =>
        // Step forward in time: guarded = true
        val nextContext = context.flatMap {
          case (n, Expr.App(Expr.Sym(Globally), List(p))) => Some((n, Expr.App(Expr.Sym(Globally), List(p))))
          case (n, Expr.App(Expr.Sym(Next), List(p))) => Some((s"next:$n", p))
          case _ => None
        }
        search(a, rules, nextContext, Nil, subst, depth + 1, limit, visited, raaCount, inductionCount, true)
          .map { case (p, s, restL) => (ProofTree.Node(applySubst(goal, s), "next-step", List(p)), s, restL) }

      case _ => LazyList.empty

  private def searchInduction(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] =
    logger.log(s"searching Induction: $goal")
    if (inductionCount >= config.maxInduction) LazyList.empty
    else
      goal match {
        // Handle both typed and untyped quantifiers
        case Expr.App(Expr.Sym(Forall), args) if args.length == 2 || args.length == 3 =>
          val (vName, body) = args match {
            case List(Expr.Var(v), b) => (v, b)
            case List(Expr.Var(v), _, b) => (v, b)
            case _ => return LazyList.empty
          }
          val targetAlgebras = algebras.filter(a => vName.startsWith(a.varPrefix))
          val candidateAlgebras =
            if (targetAlgebras.nonEmpty) targetAlgebras else algebras

          candidateAlgebras.to(LazyList).flatMap { algebra =>
            def solveConstructors(
                cs: List[ConstructorDef],
                currentSubst: Subst,
                currentL: Context,
                solved: List[ProofTree]
            ): LazyList[(List[ProofTree], Subst, Context)] = cs match {
              case Nil       => LazyList((solved, currentSubst, currentL))
              case c :: tail =>
                val constructorTerm =
                  if (c.argTypes.isEmpty) Expr.Sym(c.symbol)
                  else {
                    val argsC = c.argTypes.zipWithIndex.map {
                      case (ArgType.Recursive, i) =>
                        Expr.Var(s"${vName}_${depth}_$i")
                      case (ArgType.Constant, i) => Expr.Var(s"a_${depth}_$i")
                    }
                    Expr.App(Expr.Sym(c.symbol), argsC)
                  }
                val instanceGoal = Prover.substVar(body, vName, constructorTerm)

                // 道コンストラクタの場合の追加ゴール生成
                val finalGoal = c.ctorType match {
                  case ConstructorType.Point          => instanceGoal
                  case ConstructorType.Path(from, to) =>
                    val p_from = Prover.substVar(body, vName, from)
                    val p_to = Prover.substVar(body, vName, to)
                    Expr.App(
                      Expr.Sym(Path),
                      List(Expr.Sym("Type"), p_from, p_to)
                    )
                }

                val ihs = constructorTerm match {
                  case Expr.App(_, argsC) =>
                    c.argTypes.zip(argsC).collect {
                      case (ArgType.Recursive, arg) =>
                        (s"IH_${arg}", Prover.substVar(body, vName, arg))
                    }
                  case _ => Nil
                }
                search(
                  finalGoal,
                  rules,
                  ihs ++ context,
                  currentL,
                  currentSubst,
                  depth + 1,
                  limit,
                  visited,
                  raaCount,
                  inductionCount + 1,
                  guarded
                ).flatMap { case (tree, nextSubst, nextL) =>
                  solveConstructors(tail, nextSubst, nextL, solved :+ tree)
                }
            }
            solveConstructors(algebra.constructors, subst, linearContext, Nil).map {
              case (trees, finalSubst, finalL) =>
                (
                  ProofTree.Node(
                    applySubst(goal, finalSubst),
                    s"induction[${algebra.name}]",
                    trees
                  ),
                  finalSubst,
                  finalL
                )
            }
          }
        case _ => LazyList.empty
      }

  private def searchPathInduction(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] = {
    if (inductionCount >= config.maxInduction) LazyList.empty
    else {
      context.indices.to(LazyList).flatMap { i =>
        val (pName, hyp) = context(i)
        hyp match {
          case Expr.App(Expr.Sym(Path), List(_, x, Expr.Var(yName))) if x != Expr.Var(yName) =>
            // Path Induction (J-rule):
            // 1. Identification: replace y with x
            // 2. Identification: replace path variable pName with refl(x)
            val reflX = Expr.App(Expr.Sym(Refl), List(x))
            val subst1Goal = Prover.substVar(goal, yName, x)
            val substGoal = Prover.substVar(subst1Goal, pName, reflX)
            
            val nextContext = context.patch(i, Nil, 1).map { case (n, h) => 
              val h1 = Prover.substVar(h, yName, x)
              val h2 = Prover.substVar(h1, pName, reflX)
              (n, h2)
            }
            val nextLinear = linearContext.map { case (n, h) => 
              val h1 = Prover.substVar(h, yName, x)
              val h2 = Prover.substVar(h1, pName, reflX)
              (n, h2)
            }
            
            search(substGoal, rules, nextContext, nextLinear, subst, depth + 1, limit, visited, raaCount, inductionCount + 1, guarded)
              .map { case (tree, s, restL) => 
                (ProofTree.Node(applySubst(goal, s), s"path-induction[$pName]", List(tree)), s, restL)
              }
          case _ => LazyList.empty
        }
      }
    }
  }

  private def searchDecomposeContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] =
    logger.log(s"searching Decompose Context: $goal")
    // Persistent context decomposition (Standard)
    val persistentDec = context.zipWithIndex.view.to(LazyList).flatMap {
      case ((name, Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body))), i) =>
        val freshVar = s"${v}_${depth}_sk"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        val newCtx = context.patch(i, List((s"${name}.witness", instantiated)), 1)
        search(goal, rules, newCtx, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded)
          .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), s"destruct[$name]", List(tree)), s, restL) }
      
      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        val newCtx = context.patch(i, List((s"$name.1", a), (s"$name.2", b)), 1)
        search(goal, rules, newCtx, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded)
          .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), s"destruct[$name]", List(tree)), s, restL) }

      case _ => LazyList.empty
    }
    // Linear context decomposition (Important for Linear Logic and Separation Logic!)
    val linearDec = linearContext.zipWithIndex.view.to(LazyList).flatMap {
      case ((name, Expr.App(Expr.Sym(op), List(a, b))), i) if op == Tensor || op == SepAnd =>
        val destructName = if (op == Tensor) "tensor-destruct" else "sep-and-destruct"
        val newL = linearContext.patch(i, List((s"${name}.1", a), (s"${name}.2", b)), 1)
        search(goal, rules, context, newL, subst, depth, limit, visited, raaCount, inductionCount, guarded)
          .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), s"$destructName[$name]", List(tree)), s, restL) }
      case _ => LazyList.empty
    }
    persistentDec #::: linearDec

  private def searchContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] =
    logger.log(s"searching Context: $goal")
    // Persistent context rewrites
    val persistent = context.view.to(LazyList).flatMap { case (name, hyp) =>
      val ch = applySubst(hyp, subst)
      if (ch == Expr.Sym(False) || ch == Expr.Sym(Initial)) {
        LazyList((ProofTree.Leaf(goal, s"absurd[$name]"), subst, linearContext))
      } else {
        val instHyp = instantiateHyp(ch, depth)
        instHyp match {
          case Expr.App(Expr.Sym(Eq), List(l, r)) =>
            findAndReplace(goal, l, r, subst).flatMap { case (rewritten, finalS) =>
              val finalGoal = Rewriter.normalize(rewritten)
              if (finalGoal != goal) {
                search(finalGoal, rules, context, linearContext, finalS, depth + 1, limit, visited, raaCount, inductionCount, guarded)
                  .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), s"rewrite[$name]", List(tree)), s, restL) }
              } else LazyList.empty
            }
          case Expr.App(Expr.Sym(Path), List(_, l, r)) =>
            findAndReplace(goal, l, r, subst).flatMap { case (rewritten, finalS) =>
              val finalGoal = Rewriter.normalize(rewritten)
              if (finalGoal != goal) {
                search(finalGoal, rules, context, linearContext, finalS, depth + 1, limit, visited, raaCount, inductionCount, guarded)
                  .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), s"rewrite[$name]", List(tree)), s, restL) }
              } else LazyList.empty
            }
          case _ => LazyList.empty
        }
      }
    }
    persistent #::: {
      // Use linear or persistent hypotheses
      val linearHypBuffer = linearContext.zipWithIndex.map(h => ((h._1._1, h._1._2), true))
      val persistentHypBuffer = context.map(h => (h, false))
      (persistentHypBuffer ++ linearHypBuffer).to(LazyList).flatMap {
        case ((name, hyp), isLinear) =>
          useHypothesis(name, hyp, goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, isLinear, guarded)
      }
    }

  private def findAndReplace(goal: Expr, l: Expr, r: Expr, s: Subst): LazyList[(Expr, Subst)] = {
    unify(goal, applySubst(l, s), s).map(ns => (applySubst(r, ns), ns)) #::: {
      goal match {
        case Expr.App(f, args) =>
          args.indices.to(LazyList).flatMap { i =>
            findAndReplace(args(i), l, r, s).map { case (newA, ns) =>
              (Expr.App(applySubst(f, ns), args.patch(i, List(newA), 1).map(applySubst(_, ns))), ns)
            }
          }
        case _ => LazyList.empty
      }
    }
  }

  private def instantiateHyp(expr: Expr, depth: Int): Expr = expr match {
    case Expr.App(Expr.Sym(Forall), args) if args.length == 2 || args.length == 3 =>
      val (v, body) = args match {
        case List(Expr.Var(vName), b) => (vName, b)
        case List(Expr.Var(vName), _, b) => (vName, b)
        case _ => return expr
      }
      instantiateHyp(Prover.substVar(body, v, freshMeta(depth)), depth)
    case _ => expr
  }

  private def useHypothesis(
      name: String,
      hyp: Expr,
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      isLinear: Boolean,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] = {
    val currentHyp = applySubst(hyp, subst)
    val nextLinear = if (isLinear) {
      linearContext.filterNot(_._1 == name) 
    } else linearContext

    unify(currentHyp, goal, subst)
      .map(s => (ProofTree.Leaf(applySubst(goal, s), name), s, nextLinear)) #::: {
      currentHyp match {
        case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
          // Linear Application: (A ⊸ B) uses A to get B
          (0 to nextLinear.length).to(LazyList).flatMap { n =>
            nextLinear.combinations(n).flatMap { leftPart =>
              val rightPart = linearContext.filterNot(leftPart.contains) // Use full linearContext for split
              val leftLinear = if (isLinear) leftPart.filterNot(_._1 == name) else leftPart
              val rightLinear = if (isLinear) rightPart.filterNot(_._1 == name) else rightPart
              
              search(a, rules, context, leftLinear, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded).flatMap { case (treeA, s1, restLA) =>
                if (restLA.isEmpty) {
                  useHypothesis(name, b, goal, rules, context, rightLinear, s1, depth, limit, visited, raaCount, inductionCount, false, guarded)
                    .map { case (treeB, s2, restL) =>
                      (ProofTree.Node(applySubst(goal, s2), s"linear-apply[$name]", List(treeB, treeA)), s2, restL)
                    }
                } else LazyList.empty
              }
            }
          }
        case Expr.App(Expr.Sym(Implies | ImpliesAlt1 | ImpliesAlt2), List(a, b)) =>
          // Persistent implication
          search(a, rules, context, Nil, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded).flatMap { case (treeA, s1, _) =>
            useHypothesis(name, b, goal, rules, context, linearContext, s1, depth, limit, visited, raaCount, inductionCount, isLinear, guarded)
              .map { case (treeB, s2, restL) =>
                (ProofTree.Node(applySubst(goal, s2), s"apply[$name]", List(treeB, treeA)), s2, restL)
              }
          }
        case Expr.App(Expr.Sym(Globally), List(a)) =>
          // G(A) can be used to prove A
          useHypothesis(name, a, goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, isLinear, guarded)

        case Expr.App(Expr.Sym(Forall), args) if args.length == 2 || args.length == 3 =>
          // Instantiate Forall when using hypothesis
          val (v, body) = args match {
            case List(Expr.Var(vName), b) => (vName, b)
            case List(Expr.Var(vName), _, b) => (vName, b)
            case _ => return LazyList.empty
          }
          val instantiated = Prover.substVar(body, v, freshMeta(depth))
          useHypothesis(name, instantiated, goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, isLinear, guarded)

        case _ => LazyList.empty
      }
    }
  }

  private def searchRules(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] = {
    logger.log(s"searching Rules: $goal")
    val normGoal = Rewriter.normalize(goal)
    rules.to(LazyList).flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      findAndReplace(normGoal, instRule.lhs, instRule.rhs, subst).flatMap { case (rewritten, nextS) =>
        val finalGoal = Rewriter.normalize(rewritten)
        if (finalGoal != normGoal) {
          search(finalGoal, rules, context, linearContext, nextS, depth + 1, limit, visited, raaCount, inductionCount, guarded)
            .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), rule.name, List(tree)), s, restL) }
        } else LazyList.empty
      }
    }
  }

  private def searchClassical(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], Set[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): LazyList[(ProofTree, Subst, Context)] =
    if (config.classical && raaCount < config.maxRaa && goal != Expr.Sym(False)) {
      val negation = Expr.App(Expr.Sym(Implies), List(goal, Expr.Sym(False)))
      search(Expr.Sym(False), rules, (s"raa$depth", negation) :: context, linearContext, subst, depth + 1, limit, visited, raaCount + 1, inductionCount, guarded)
        .map { case (tree, s, restL) => (ProofTree.Node(applySubst(goal, s), "RAA", List(tree)), s, restL) }
    } else LazyList.empty
}

object Prover {
  import Unifier._
  def substVar(expr: Expr, varName: String, replacement: Expr): Expr =
    expr match {
      case Expr.Var(n) if n == varName => replacement
      case Expr.Sym(n) if n == varName => replacement
      case Expr.App(h, args)           =>
        expr match {
          case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body))
              if v == varName =>
            expr
          case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
            Expr.App(
              Expr.Sym("λ"),
              List(Expr.Var(v), substVar(body, varName, replacement))
            )
          case _ =>
            Expr.App(
              substVar(h, varName, replacement),
              args.map(substVar(_, varName, replacement))
            )
        }
      case _ => expr
    }
  def rewriteExpr(expr: Expr, from: Expr, to: Expr): Expr =
    if (expr == from) to
    else
      expr match
        case Expr.App(h, args) =>
          Expr.App(rewriteExpr(h, from, to), args.map(rewriteExpr(_, from, to)))
        case _ => expr

  def instantiate(
      rule: CatRule,
      freshMeta: () => Expr
  ): (CatRule, Map[MetaId, Expr]) =
    val vars =
      collectVars(rule.lhs) ++ collectVars(rule.rhs) ++ rule.universals.flatMap(
        collectVars
      )
    val substMap = vars.map(v => v -> freshMeta()).toMap
    val metaSubstMap: Subst = substMap.flatMap {
      case (name, Expr.Meta(id)) => Some(id -> Expr.Meta(id))
      case _                     => None
    }
    val instLhs = applyVarSubst(rule.lhs, substMap)
    val instRhs = applyVarSubst(rule.rhs, substMap)
    val instUniv = rule.universals.map(applyVarSubst(_, substMap))
    (CatRule(rule.name, instLhs, instRhs, instUniv), metaSubstMap)

  private def collectVars(e: Expr): Set[String] = e match
    case Expr.Var(n)       => Set(n)
    case Expr.App(h, args) =>
      e match {
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
          collectVars(body) - v
        case _ => collectVars(h) ++ args.flatMap(collectVars)
      }
    case _ => Set.empty

  private def applyVarSubst(e: Expr, s: Map[String, Expr]): Expr = e match
    case Expr.Var(n) if s.contains(n) => s(n)
    case Expr.App(h, args)            =>
      e match {
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
          Expr.App(Expr.Sym("λ"), List(Expr.Var(v), applyVarSubst(body, s)))
        case _ => Expr.App(applyVarSubst(h, s), args.map(applyVarSubst(_, s)))
      }
    case _ => e
}
