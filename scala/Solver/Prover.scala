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
  private val failureCache = mutable.Map[(Expr, Set[Expr], Int), FailTrace]()

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
      search(goal, effectiveRules, Nil, emptySubst, 0, d, Set.empty, 0, 0).map(
        _._1
      )
    }.headOption

    result match {
      case Some(tree) =>
        val lemma = if (config.generateLemmas) generateLemma(tree) else None
        Right(ProofResult(tree, lemma))
      case None =>
        Left(
          bestFail.getOrElse(FailTrace(Goal(Nil, goal), "No proof found", 0))
        )
    }

  private def generateLemma(tree: ProofTree): Option[CatRule] = {
    val provedGoal = tree match {
      case ProofTree.Node(g, _, _) => g
      case ProofTree.Leaf(g, _)    => g
    }

    // モードによるフィルタリング
    val modeOk = config.lemmaMode match {
      case LemmaGenerationMode.All => true
      case LemmaGenerationMode.InductionOnly =>
        def hasInduction(t: ProofTree): Boolean = t match {
          case ProofTree.Node(_, rule, cs) =>
            rule.startsWith("induction") || cs.exists(hasInduction)
          case _ => false
        }
        hasInduction(tree)
      case LemmaGenerationMode.EqualityOnly =>
        provedGoal match {
          case Expr.App(Expr.Sym(Eq), _) => true
          case _                         => false
        }
      case LemmaGenerationMode.ManualOnly => false
    }

    if (!modeOk) return None

    // 変数を収集して一般化するヘルパー
    def collectFreeVars(e: Expr): Set[String] = e match {
      case Expr.Var(n) => Set(n)
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
        case ProofTree.Leaf(Expr.App(Expr.Sym(Eq), List(l, r)), "reflexivity") =>
          // reflexivity が使われた場合、その時の正規化された右辺（または左辺）が簡約形
          r
        case ProofTree.Node(_, _, children) if children.nonEmpty =>
          // 再帰的に探索（等式に関する最新の簡約形を探す）
          // 単純化のため、最初の子供から抽出を試みる
          extractSimplifiedRhs(children.head, originalLhs)
        case _ =>
          // 見つからなければ等式の右辺をそのまま使う
          originalLhs match {
            case Expr.App(Expr.Sym(Eq), List(_, r)) => r
            case _                                  => originalLhs
          }
      }
    }

    // 等式の場合
    provedGoal match {
      case Expr.App(Expr.Sym(Eq), List(lhs, rhs)) =>
        // 最終的な簡約形を rhs に設定することを試みる
        val finalRhs = extractSimplifiedRhs(tree, provedGoal) match {
          case Expr.App(Expr.Sym(Eq), List(_, r)) => r
          case other                              => other
        }

        if (config.excludeTrivialLemmas && lhs == finalRhs) return None

        // 重複チェック（既存のルールと同一の lhs/rhs を持つか）
        val isDuplicate = config.rules.exists(r => r.lhs == lhs && r.rhs == finalRhs)
        if (isDuplicate) return None

        val vars = collectFreeVars(lhs) ++ collectFreeVars(finalRhs)

        // 意味のある名前の生成
        val lhsHead = lhs.headSymbol.toLowerCase.replaceAll("[^a-z0-9]", "_")
        val rhsHead = finalRhs.headSymbol.toLowerCase.replaceAll("[^a-z0-9]", "_")
        val baseName = s"lemma_${lhsHead}_to_${rhsHead}"
        val ruleName =
          s"${baseName}_${java.util.UUID.randomUUID().toString.take(4)}"

        val universals = vars.toList.map(Expr.Var(_))
        Some(CatRule(ruleName, lhs, finalRhs, universals))

      case _ if config.lemmaMode == LemmaGenerationMode.All =>
        // 等式以外でも All モードなら生成を試みる
        val vars = collectFreeVars(provedGoal)
        val head = provedGoal.headSymbol.toLowerCase.replaceAll("[^a-z0-9]", "_")
        val ruleName =
          s"lemma_${head}_${java.util.UUID.randomUUID().toString.take(4)}"
        val universals = vars.toList.map(Expr.Var(_))
        Some(CatRule(ruleName, provedGoal, Expr.Sym(True), universals))

      case _ => None
    }
  }

  private def search(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] = {
    val currentGoal = applySubst(goal, subst)
    val contextExprs = context.map(h => applySubst(h._2, subst)).toSet
    val currentGoalObj = Goal(context, currentGoal)

    val remainingDepth = limit - depth
    if (failureCache.contains((currentGoal, contextExprs, remainingDepth))) {
      return LazyList.empty
    }

    if depth > limit then
      recordFail(FailTrace(currentGoalObj, s"Depth limit reached", depth))
      LazyList.empty
    else if visited.contains((currentGoal, contextExprs)) then
      recordFail(FailTrace(currentGoalObj, "Cycle detected", depth))
      LazyList.empty
    else
      val nextVisited = visited + ((currentGoal, contextExprs))

      // 優先順位: 自明な解決 -> 構造分解 -> レンマ適用 -> 論理規則
      val results =
        searchAxiom(currentGoal, context, subst, depth) #:::
          searchReflexivity(currentGoal, subst, depth) #:::
          searchInduction(
            currentGoal,
            rules,
            context,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount
          ) #:::
          searchDecomposeGoal(
            currentGoal,
            rules,
            context,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount
          ) #:::
          searchDecomposeContext(
            currentGoal,
            rules,
            context,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount
          ) #:::
          searchRules(
            currentGoal,
            rules,
            context,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount
          ) #:::
          searchContext(
            currentGoal,
            rules,
            context,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount
          ) #:::
          searchClassical(
            currentGoal,
            rules,
            context,
            subst,
            depth,
            limit,
            nextVisited,
            raaCount,
            inductionCount
          )

      if (results.isEmpty) {
        val fail =
          FailTrace(currentGoalObj, "Goal unresolvable in this branch", depth)
        failureCache((currentGoal, contextExprs, remainingDepth)) = fail
        recordFail(fail)
      }
      results
  }

  private def searchAxiom(
      goal: Expr,
      context: Context,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst)] =
    context.view.to(LazyList).flatMap { case (name, hyp) =>
      unify(hyp, goal, subst)
        .map(s => (ProofTree.Leaf(applySubst(goal, s), name), s))
    }

  private def searchReflexivity(
      goal: Expr,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst)] =
    goal match
      case Expr.App(Expr.Sym(Eq), List(l, r)) =>
        val nl = Rewriter.normalize(applySubst(l, subst))
        val nr = Rewriter.normalize(applySubst(r, subst))
        unify(nl, nr, subst).map { s =>
          (ProofTree.Leaf(applySubst(goal, s), "reflexivity"), s)
        }
      case _ => LazyList.empty

  private def searchDecomposeGoal(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] =
    goal match
      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        for
          (treeA, s1) <- search(
            a,
            rules,
            context,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount
          )
          (treeB, s2) <- search(
            b,
            rules,
            context,
            s1,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount
          )
        yield (
          ProofTree.Node(
            applySubst(goal, s2),
            "product-universal",
            List(treeA, treeB)
          ),
          s2
        )

      case Expr.App(Expr.Sym(op), List(a, b))
          if op == Implies || op == Exp || op == ImpliesAlt1 || op == ImpliesAlt2 =>
        val (ant, cons) = if (op == Exp) (b, a) else (a, b)
        val hypName = s"h$depth"
        search(
          cons,
          rules,
          (hypName, ant) :: context,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .map { case (tree, s) =>
            (
              ProofTree.Node(applySubst(goal, s), "exp-universal", List(tree)),
              s
            )
          }

      case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
        val freshVar = s"${v}_$depth"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        search(
          instantiated,
          rules,
          context,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .map { case (proof, s) =>
            (
              ProofTree.Node(applySubst(goal, s), "forall-intro", List(proof)),
              s
            )
          }

      case Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body)) =>
        val witness = freshMeta(depth)
        val instantiated = Prover.substVar(body, v, witness)
        search(
          instantiated,
          rules,
          context,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .map { case (proof, s) =>
            (
              ProofTree.Node(applySubst(goal, s), "exists-intro", List(proof)),
              s
            )
          }

      case Expr.App(Expr.Sym(Or | Coproduct), List(a, b)) =>
        search(
          a,
          rules,
          context,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .map { case (p, s) =>
            (ProofTree.Node(applySubst(goal, s), "inl", List(p)), s)
          } #:::
          search(
            b,
            rules,
            context,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount
          )
            .map { case (p, s) =>
              (ProofTree.Node(applySubst(goal, s), "inr", List(p)), s)
            }

      case Expr.Sym(True | Terminal) =>
        LazyList((ProofTree.Leaf(goal, "terminal-universal"), subst))

      case _ => LazyList.empty

  private def searchInduction(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] =
    if (inductionCount >= config.maxInduction) LazyList.empty
    else
      goal match {
        case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
          val targetAlgebras = algebras.filter(a => v.startsWith(a.varPrefix))
          val candidateAlgebras =
            if (targetAlgebras.nonEmpty) targetAlgebras else algebras

          candidateAlgebras.to(LazyList).flatMap { algebra =>
            def solveConstructors(
                cs: List[ConstructorDef],
                currentSubst: Subst,
                solved: List[ProofTree]
            ): LazyList[(List[ProofTree], Subst)] = cs match {
              case Nil       => LazyList((solved, currentSubst))
              case c :: tail =>
                val constructorTerm =
                  if (c.argTypes.isEmpty) Expr.Sym(c.symbol)
                  else {
                    val args = c.argTypes.zipWithIndex.map {
                      case (ArgType.Recursive, i) =>
                        Expr.Var(s"${v}_${depth}_$i")
                      case (ArgType.Constant, i) => Expr.Var(s"a_${depth}_$i")
                    }
                    Expr.App(Expr.Sym(c.symbol), args)
                  }
                val instanceGoal = Prover.substVar(body, v, constructorTerm)
                val ihs = constructorTerm match {
                  case Expr.App(_, args) =>
                    c.argTypes.zip(args).collect {
                      case (ArgType.Recursive, arg) =>
                        (s"IH_${arg}", Prover.substVar(body, v, arg))
                    }
                  case _ => Nil
                }
                search(
                  instanceGoal,
                  rules,
                  ihs ++ context,
                  currentSubst,
                  depth + 1,
                  limit,
                  visited,
                  raaCount,
                  inductionCount + 1
                ).flatMap { case (tree, nextSubst) =>
                  solveConstructors(tail, nextSubst, solved :+ tree)
                }
            }
            solveConstructors(algebra.constructors, subst, Nil).map {
              case (trees, finalSubst) =>
                (
                  ProofTree.Node(
                    applySubst(goal, finalSubst),
                    s"induction[${algebra.name}]",
                    trees
                  ),
                  finalSubst
                )
            }
          }
        case _ => LazyList.empty
      }

  private def searchDecomposeContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] =
    context.zipWithIndex.view.to(LazyList).flatMap {
      case ((name, Expr.App(Expr.Sym(Exists), List(Expr.Var(v), body))), i) =>
        val freshVar = s"${v}_${depth}_sk"
        val instantiated = Prover.substVar(body, v, Expr.Var(freshVar))
        val newCtx =
          context.patch(i, List((s"${name}.witness", instantiated)), 1)
        search(
          goal,
          rules,
          newCtx,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .map { case (tree, s) =>
            (
              ProofTree.Node(
                applySubst(goal, s),
                s"destruct[$name]",
                List(tree)
              ),
              s
            )
          }

      case ((name, Expr.App(Expr.Sym(And | Product), List(a, b))), i) =>
        val newCtx =
          context.patch(i, List((s"${name}.1", a), (s"${name}.2", b)), 1)
        search(
          goal,
          rules,
          newCtx,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .map { case (tree, s) =>
            (
              ProofTree.Node(
                applySubst(goal, s),
                s"destruct[$name]",
                List(tree)
              ),
              s
            )
          }

      case ((name, Expr.App(Expr.Sym(Or | Coproduct), List(a, b))), i) =>
        val leftCtx = context.patch(i, List((s"${name}.left", a)), 1)
        search(
          goal,
          rules,
          leftCtx,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount
        )
          .flatMap { case (treeL, ls) =>
            val rightCtx = context.patch(i, List((s"${name}.right", b)), 1)
            search(
              goal,
              rules,
              rightCtx,
              ls,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount
            ).map { case (treeR, rs) =>
              (
                ProofTree.Node(
                  applySubst(goal, rs),
                  s"destruct[$name]",
                  List(treeL, treeR)
                ),
                rs
              )
            }
          }
      case _ => LazyList.empty
    }

  private def searchContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] =
    context.view.to(LazyList).flatMap { case (name, hyp) =>
      val ch = applySubst(hyp, subst)
      if (ch == Expr.Sym(False) || ch == Expr.Sym(Initial)) {
        LazyList((ProofTree.Leaf(goal, s"absurd[$name]"), subst))
      } else {
        def instantiateHyp(expr: Expr): Expr = expr match {
          case Expr.App(Expr.Sym(Forall), List(Expr.Var(v), body)) =>
            instantiateHyp(Prover.substVar(body, v, freshMeta(depth)))
          case _ => expr
        }

        val instHyp = instantiateHyp(ch)

        instHyp match {
          case Expr.App(Expr.Sym(Eq), List(l, r)) =>
            val normalizedGoal = Rewriter.normalize(goal)

            def findAndReplace(
                expr: Expr,
                currentS: Subst
            ): LazyList[(Expr, Subst)] = {
              unify(expr, applySubst(l, currentS), currentS)
                .map(s => (applySubst(r, s), s)) #::: {
                expr match {
                  case Expr.App(f, args) =>
                    args.indices.to(LazyList).flatMap { idx =>
                      findAndReplace(args(idx), currentS).map {
                        case (newArg, nextS) =>
                          val nextF = applySubst(f, nextS)
                          val nextArgs = args
                            .patch(idx, List(newArg), 1)
                            .map(applySubst(_, nextS))
                          (Expr.App(nextF, nextArgs), nextS)
                      }
                    }
                  case _ => LazyList.empty
                }
              }
            }

            findAndReplace(normalizedGoal, subst).flatMap {
              case (rewritten, finalS) =>
                val finalGoal = Rewriter.normalize(rewritten)
                if (finalGoal != normalizedGoal) {
                  // 安全のため必ず深さを進める
                  search(
                    finalGoal,
                    rules,
                    context,
                    finalS,
                    depth + 1,
                    limit,
                    visited,
                    raaCount,
                    inductionCount
                  ).map { case (tree, s) =>
                    (
                      ProofTree.Node(
                        applySubst(goal, s),
                        s"rewrite[$name]",
                        List(tree)
                      ),
                      s
                    )
                  }
                } else LazyList.empty
            }
          case _ => LazyList.empty
        }
      }
    } #::: {
      context.view.to(LazyList).flatMap { case (name, hyp) =>
        useHypothesis(
          name,
          hyp,
          goal,
          rules,
          context,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount
        )
      }
    }

  private def useHypothesis(
      name: String,
      hyp: Expr,
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] = {
    val currentHyp = applySubst(hyp, subst)
    if (currentHyp == Expr.Sym(False) || currentHyp == Expr.Sym(Initial)) {
      LazyList((ProofTree.Leaf(goal, s"absurd[$name]"), subst))
    } else {
      unify(currentHyp, goal, subst)
        .map(s => (ProofTree.Leaf(applySubst(goal, s), name), s)) #::: {
        currentHyp match {
          case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
            useHypothesis(
              s"$name.1",
              a,
              goal,
              rules,
              context,
              subst,
              depth,
              limit,
              visited,
              raaCount,
              inductionCount
            ) #:::
              useHypothesis(
                s"$name.2",
                b,
                goal,
                rules,
                context,
                subst,
                depth,
                limit,
                visited,
                raaCount,
                inductionCount
              )
          case Expr.App(
                Expr.Sym(Implies | ImpliesAlt1 | ImpliesAlt2),
                List(a, b)
              ) =>
            useHypothesis(
              name,
              b,
              goal,
              rules,
              context,
              subst,
              depth,
              limit,
              visited,
              raaCount,
              inductionCount
            ).flatMap { case (treeB, s1) =>
              search(
                a,
                rules,
                context,
                s1,
                depth + 1,
                limit,
                visited,
                raaCount,
                inductionCount
              )
                .map { case (treeA, s2) =>
                  (
                    ProofTree.Node(
                      applySubst(goal, s2),
                      s"apply[$name]",
                      List(treeB, treeA)
                    ),
                    s2
                  )
                }
            }
          case _ => LazyList.empty
        }
      }
    }
  }

  private def searchRules(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] = {
    val normGoal = Rewriter.normalize(goal)

    rules.to(LazyList).flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))

      def findAndReplaceWithRule(
          expr: Expr,
          currentS: Subst
      ): LazyList[(Expr, Subst)] = {
        unify(expr, instRule.lhs, currentS)
          .map(s => (applySubst(instRule.rhs, s), s)) #::: {
          expr match {
            case Expr.App(f, args) =>
              args.indices.to(LazyList).flatMap { idx =>
                findAndReplaceWithRule(args(idx), currentS).map {
                  case (newArg, nextS) =>
                    val nextF = applySubst(f, nextS)
                    val nextArgs =
                      args.patch(idx, List(newArg), 1).map(applySubst(_, nextS))
                    (Expr.App(nextF, nextArgs), nextS)
                }
              }
            case _ => LazyList.empty
          }
        }
      }

      findAndReplaceWithRule(normGoal, subst).flatMap {
        case (rewritten, nextS) =>
          val finalGoal = Rewriter.normalize(rewritten)
          if (finalGoal != normGoal) {
            search(
              finalGoal,
              rules,
              context,
              nextS,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount
            ).map { case (tree, s) =>
              (ProofTree.Node(applySubst(goal, s), rule.name, List(tree)), s)
            }
          } else LazyList.empty
      }
    }
  }

  private def searchClassical(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr])],
      raaCount: Int,
      inductionCount: Int
  ): LazyList[(ProofTree, Subst)] =
    if (
      config.classical && raaCount < config.maxRaa && goal != Expr.Sym(
        False
      ) && goal != Expr.Sym(Initial)
    ) {
      val negation = Expr.App(Expr.Sym(Implies), List(goal, Expr.Sym(False)))
      search(
        Expr.Sym(False),
        rules,
        (s"raa$depth", negation) :: context,
        subst,
        depth + 1,
        limit,
        visited,
        raaCount + 1,
        inductionCount
      )
        .map { case (tree, s) =>
          (ProofTree.Node(applySubst(goal, s), "RAA", List(tree)), s)
        }
    } else LazyList.empty
}

object Prover {
  import Unifier._
  def substVar(expr: Expr, varName: String, replacement: Expr): Expr =
    expr match
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