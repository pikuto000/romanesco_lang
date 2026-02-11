// ==========================================
// Prover.scala
// 証明探索エンジン（公平な探索・循環検知強化版）
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import romanesco.Utils.Debug.logger
import LogicSymbols._
import scala.util.boundary

final class Prover(val config: ProverConfig = ProverConfig.default)
    extends LinearLogicSearch
    with TemporalLogicSearch
    with PersistentLogicSearch
    with HoTTSearch {
  import Unifier._

  private val metaCounter = new AtomicInteger(0)
  private val bestFail = new AtomicReference[Option[FailTrace]](None)

  // (Goal, Context, LinearContext, Guarded) -> Max Depth Failed
  private val failureCache =
    TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()
  // (Goal, Context, LinearContext, Guarded) -> Min Depth Reached
  private val visitedGlobal =
    TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()
  private[core] var deadline: Long = 0

  private val algebras =
    if (config.algebras.nonEmpty) config.algebras
    else StandardRules.defaultAlgebras

  // LHSのヘッドシンボルでインデックス化
  private val ruleIndex: Map[String, List[CatRule]] = {
    val allRules =
      if (config.classical) config.rules ++ StandardRules.classical
      else config.rules
    allRules.groupBy(_.lhs.headSymbol)
  }

  private[core] def freshMeta(depth: Int): Expr = {
    Expr.Meta(MetaId(List(depth, metaCounter.incrementAndGet())))
  }

  private def recordFail(trace: FailTrace): Unit = {
    bestFail.updateAndGet {
      case None                                         => Some(trace)
      case Some(current) if trace.depth > current.depth => Some(trace)
      case other                                        => other
    }
  }

  type Context = List[(String, Expr)]

  def prove(
      goal: Expr,
      rules: List[CatRule] = Nil,
      maxDepth: Int = 30,
      timeoutMs: Long = 15000
  ): Either[FailTrace, ProofResult] =
    val effectiveRules = if (rules.nonEmpty) rules else config.rules

    // ログ設定の初期化
    logger.resetDepth()
    logger.setMaxDepth(3) // 深さ3までのみ詳細ログを出力

    logger.log(s"prove begin. goal: $goal (maxDepth: $maxDepth)")
    bestFail.set(None)
    failureCache.clear()
    visitedGlobal.clear()
    deadline = System.currentTimeMillis() + timeoutMs

    boundary {
      val result = (1 to maxDepth).view.flatMap { d =>
        if (System.currentTimeMillis() > deadline)
          boundary.break(
            Left(FailTrace(Goal(Nil, Nil, goal), "Watchdog timeout", 0))
          )

        logger.log(s"--- Iterative Deepening: current limit = $d ---")
        metaCounter.set(0)
        visitedGlobal.clear() // Regression found if kept across iterations
        // failureCache.clear() // failureCache can be kept safely? Maybe not with co-induction.
        failureCache.clear() // To be safe, clear it as well.
        val tree = search(
          goal,
          effectiveRules,
          Nil,
          Nil,
          emptySubst,
          0,
          d,
          Set.empty,
          0,
          0,
          false,
          Nil
        )
        val proofs = tree.solveParallel()
        proofs.filter(_._3.isEmpty).map(_._1).headOption
      }.headOption

      result match {
        case Some(tree) =>
          val lemma = if (config.generateLemmas) generateLemma(tree) else None
          Right(ProofResult(tree, lemma))
        case None =>
          Left(
            bestFail
              .get()
              .getOrElse(FailTrace(Goal(Nil, Nil, goal), "No proof found", 0))
          )
      }
    }

  private def generateLemma(tree: ProofTree): Option[CatRule] = {
    val provedGoal = tree match {
      case ProofTree.Node(g, _, _) => g
      case ProofTree.Leaf(g, _)    => g
    }
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

    provedGoal match {
      case Expr.App(Expr.Sym(Eq | Path), List(lhs, rhs)) =>
        val targetRhs = finalRhs(tree)
        val isDuplicate =
          config.rules.exists(r => r.lhs == lhs && r.rhs == targetRhs)
        if (isDuplicate) return None
        val vars = collectFreeVars(lhs) ++ collectFreeVars(targetRhs)
        val ruleName = s"lemma_${java.util.UUID.randomUUID().toString.take(4)}"
        Some(CatRule(ruleName, lhs, targetRhs, vars.toList.map(Expr.Var(_))))
      case _ => None
    }
  }

  private def finalRhs(t: ProofTree): Expr = t match {
    case ProofTree.Leaf(Expr.App(_, List(_, r)), _) => r
    case ProofTree.Node(_, _, cs) if cs.nonEmpty    => finalRhs(cs.last)
    case _                                          => Expr.Sym("?")
  }

  private[core] def search(
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
  ): SolveTree[(ProofTree, Subst, Context)] = SolveTree.Step { () =>
    logger.increaseDepth() // 深さを増やす
    val result = searchImpl(
      goal,
      rules,
      context,
      linearContext,
      subst,
      depth,
      limit,
      visited,
      raaCount,
      inductionCount,
      guarded,
      history
    )
    logger.decreaseDepth() // 深さを戻す
    result
  }

  private def searchImpl(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    if (System.currentTimeMillis() > deadline) SolveTree.Failure
    else {
      val currentGoalRaw = Rewriter.normalize(applySubst(goal, subst))
      if (currentGoalRaw.complexity > config.maxComplexity) {
        logger.log(s"[COMPLEXITY LIMIT] Goal: $currentGoalRaw")
        SolveTree.Failure
      } else if (getPathLevel(currentGoalRaw) > config.maxPathLevel) {
        logger.log(s"[PATH LEVEL LIMIT] Goal: $currentGoalRaw")
        SolveTree.Failure
      } else {
        val currentGoalCan = currentGoalRaw.canonicalize
        val contextExprs =
          context
            .map(h => Rewriter.normalize(applySubst(h._2, subst)).canonicalize)
            .toSet
        val linearExprs = linearContext
          .map(h => Rewriter.normalize(applySubst(h._2, subst)).canonicalize)
          .sortBy(_.toString)

        val stateKey = (currentGoalCan, contextExprs, linearExprs, guarded)
        val remainingDepth = limit - depth

        // ローカルな循環検知（および余帰納法）を優先
        if (visited.contains((currentGoalCan, contextExprs, linearExprs))) {
          if (guarded) {
            SolveTree.Success(
              (
                ProofTree.Leaf(currentGoalRaw, "co-induction"),
                subst,
                linearContext
              )
            )
          } else {
            logger.log(s"[CYCLE DETECTED] Goal: $currentGoalCan")
            SolveTree.Failure
          }
        }
        // ヒストリカルな成長検知 (簡易的な埋め込み判定)
        // ヘッドシンボルが同じ場合にのみ成長を判定することで、G(A) -> A ∧ X(G(A)) のような展開を許容する
        else if (
          history.exists(h =>
            h.headSymbol == currentGoalCan.headSymbol && h.complexity < currentGoalCan.complexity && isEmbedding(
              h,
              currentGoalCan
            )
          )
        ) {
          logger.log(
            s"[GROWTH DETECTED] Goal $currentGoalCan grows from previous $history"
          )
          SolveTree.Failure
        } else if (visitedGlobal.get(stateKey).exists(_ <= depth)) {
          logger.log(
            s"[PRUNED] Already visited state at depth ${visitedGlobal(stateKey)} (current depth: $depth): $currentGoalCan"
          )
          SolveTree.Failure
        } else if (failureCache.get(stateKey).exists(_ >= remainingDepth)) {
          logger.log(s"[CACHE HIT] Failure cached for state: $currentGoalCan")
          SolveTree.Failure
        } else if (depth > limit) SolveTree.Failure
        else {
          visitedGlobal(stateKey) = depth
          val nextVisited =
            visited + ((currentGoalCan, contextExprs, linearExprs))
          val nextHistory = currentGoalCan :: history

          // 優先順位: Axiom -> Reflexivity -> DecomposeGoal -> Induction -> Path Induction -> DecomposeContext -> Rules -> RulesInContext -> Context -> Classical
          val branches = List(
            SolveTree.fromLazyList(
              searchAxiom(currentGoalRaw, context, linearContext, subst, depth)
            ),
            SolveTree.fromLazyList(
              searchReflexivity(currentGoalRaw, linearContext, subst, depth)
            ),
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
              guarded,
              nextHistory
            ),
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
              guarded,
              nextHistory
            ),
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
              guarded,
              nextHistory
            ),
            searchCancellation(
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
              guarded,
              nextHistory
            ),
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
              guarded,
              nextHistory
            ),
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
              guarded,
              nextHistory
            ),
            searchRulesInContext(
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
              guarded,
              nextHistory
            ),
            searchRulesInPersistentContext(
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
              guarded,
              nextHistory
            ),
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
              guarded,
              nextHistory
            ),
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
              guarded,
              nextHistory
            )
          )

          SolveTree.Memo(
            SolveTree.Choice(branches),
            success => {
              if (!success) {
                val currentVal = failureCache.getOrElse(stateKey, -1)
                if (remainingDepth > currentVal) {
                  failureCache(stateKey) = remainingDepth
                }
              }
            }
          )
        }
      }
    }
  }

  private def isEmbedding(h: Expr, g: Expr): Boolean = {
    def loop(e1: Expr, e2: Expr): Boolean = {
      val coupling = (e1, e2) match {
        case (Expr.Sym(s1), Expr.Sym(s2))   => s1 == s2
        case (Expr.Var(v1), Expr.Var(v2))   => v1 == v2
        case (Expr.Meta(m1), Expr.Meta(m2)) => m1 == m2
        case (Expr.App(f1, a1), Expr.App(f2, a2)) if a1.length == a2.length =>
          loop(f1, f2) && a1.zip(a2).forall((arg1, arg2) => loop(arg1, arg2))
        case _ => false
      }
      coupling || (e2 match {
        case Expr.App(f, args) => loop(e1, f) || args.exists(a => loop(e1, a))
        case _                 => false
      })
    }
    h.complexity < g.complexity && loop(h, g)
  }

  private def searchAxiom(
      goal: Expr,
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst, Context)] = {
    val persistent = context.view.to(LazyList).flatMap { case (name, hyp) =>
      unify(Rewriter.normalize(applySubst(hyp, subst)), goal, subst)
        .map(s => (ProofTree.Leaf(applySubst(goal, s), name), s, linearContext))
    }

    val linear = if (linearContext.length == 1) {
      val (name, hyp) = linearContext.head
      unify(Rewriter.normalize(applySubst(hyp, subst)), goal, subst).map(s =>
        (ProofTree.Leaf(applySubst(goal, s), s"linear:$name"), s, Nil)
      )
    } else LazyList.empty

    persistent #::: linear
  }

  private def searchReflexivity(
      goal: Expr,
      linearContext: Context,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst, Context)] = {
    goal match {
      case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
        val l = args match {
          case List(t, x, y) if goal.headSymbol == Path => x
          case List(x, y)                               => x
          case _                                        => return LazyList.empty
        }
        val r = args.last
        unify(Rewriter.normalize(l), Rewriter.normalize(r), subst).map { s =>
          (ProofTree.Leaf(applySubst(goal, s), "reflexivity"), s, linearContext)
        }
      case _ => LazyList.empty
    }
  }

  private def searchDecomposeGoal(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    goal match {
      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
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
          guarded,
          history
        ).flatMap { case (tA, s1, restL1) =>
          search(
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
            guarded,
            history
          ).map { case (tB, s2, restL2) =>
            (
              ProofTree.Node(
                applySubst(goal, s2),
                "product-intro",
                List(tA, tB)
              ),
              s2,
              restL2
            )
          }
        }

      case Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)) =>
        searchLinearGoal(
          goal,
          a,
          b,
          rules,
          context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )

      case Expr.App(
            Expr.Sym(Implies | Exp | ImpliesAlt1 | ImpliesAlt2),
            List(a, b)
          ) =>
        val (ant, cons) = if (goal.headSymbol == Exp) (b, a) else (a, b)
        search(
          cons,
          rules,
          (s"h$depth", ant) :: context,
          linearContext,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )
          .map { case (t, s, restL) =>
            (
              ProofTree.Node(applySubst(goal, s), "implies-intro", List(t)),
              s,
              restL
            )
          }

      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        val hypName = s"lin$depth"
        search(
          b,
          rules,
          context,
          (hypName, a) :: linearContext,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )
          .flatMap { case (t, s, restL) =>
            if (!restL.exists(_._1 == hypName)) {
              SolveTree.Success(
                (
                  ProofTree
                    .Node(applySubst(goal, s), "linear-implies-intro", List(t)),
                  s,
                  restL
                )
              )
            } else SolveTree.Failure
          }

      case Expr.App(Expr.Sym(Forall), args) =>
        boundary {
          val (vName, body, typeOpt) = args match {
            case List(Expr.Var(v), b)    => (v, b, None)
            case List(Expr.Var(v), t, b) => (v, b, Some(t))
            case _                       => boundary.break(SolveTree.Failure)
          }
          val freshVar = s"${vName}_$depth"
          val instantiated = Prover.substVar(body, vName, Expr.Var(freshVar))
          val newCtx = typeOpt match {
            case Some(t) =>
              (
                freshVar,
                Prover.substVar(t, vName, Expr.Var(freshVar))
              ) :: context
            case None => (freshVar, Expr.Sym("Type")) :: context
          }
          search(
            instantiated,
            rules,
            newCtx,
            linearContext,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          )
            .map { case (p, s, restL) =>
              (
                ProofTree.Node(applySubst(goal, s), "forall-intro", List(p)),
                s,
                restL
              )
            }
        }

      case Expr.Sym(True) =>
        SolveTree.Success((ProofTree.Leaf(goal, "true-intro"), subst, Nil))
      case Expr.Sym(Terminal) if linearContext.isEmpty =>
        SolveTree.Success((ProofTree.Leaf(goal, "terminal-intro"), subst, Nil))

      case Expr.App(Expr.Sym(Globally), List(a)) =>
        searchTemporalGoal(
          goal,
          a,
          rules,
          context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )

      case Expr.App(Expr.Sym(Next), List(a)) =>
        searchTemporalGoal(
          goal,
          a,
          rules,
          context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )

      case _ => SolveTree.Failure
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
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val persistent = searchPersistentDecomposeContext(
      goal,
      rules,
      context,
      linearContext,
      subst,
      depth,
      limit,
      visited,
      raaCount,
      inductionCount,
      guarded,
      history
    )
    val linear = searchLinearDecomposeContext(
      goal,
      rules,
      context,
      linearContext,
      subst,
      depth,
      limit,
      visited,
      raaCount,
      inductionCount,
      guarded,
      history
    )
    SolveTree.merge(List(persistent, linear))
  }

  private def searchInduction(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    if (inductionCount >= config.maxInduction) SolveTree.Failure
    else
      goal match {
        case Expr.App(Expr.Sym(Forall), args) =>
          boundary {
            val (vName, body) = args match {
              case List(Expr.Var(v), b)    => (v, b)
              case List(Expr.Var(v), _, b) => (v, b)
              case _                       => boundary.break(SolveTree.Failure)
            }
            val targetAlgebras =
              algebras.filter(a => vName.startsWith(a.varPrefix))
            val candidates =
              if (targetAlgebras.nonEmpty) targetAlgebras else algebras

            val algTrees = candidates.map { algebra =>
              def solveConstructors(
                  cs: List[ConstructorDef],
                  s: Subst,
                  l: Context,
                  solved: List[ProofTree]
              ): SolveTree[(List[ProofTree], Subst, Context)] = cs match {
                case Nil       => SolveTree.Success((solved, s, l))
                case c :: tail =>
                  val cTerm =
                    if (c.argTypes.isEmpty) Expr.Sym(c.symbol)
                    else
                      Expr.App(
                        Expr.Sym(c.symbol),
                        c.argTypes.zipWithIndex.map {
                          case (ArgType.Recursive, i) =>
                            Expr.Var(s"${vName}_$i")
                          case (ArgType.Constant, i) => Expr.Var(s"a_$i")
                        }
                      )
                  val g = Prover.substVar(body, vName, cTerm)
                  val finalGoal = c.ctorType match {
                    case ConstructorType.Point          => g
                    case ConstructorType.Path(from, to) =>
                      val p_from = Prover.substVar(body, vName, from)
                      val p_to = Prover.substVar(body, vName, to)
                      Expr.App(
                        Expr.Sym(Path),
                        List(Expr.Sym("Type"), p_from, p_to)
                      )
                  }
                  val ihs = cTerm match {
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
                    l,
                    s,
                    depth + 1,
                    limit,
                    visited,
                    raaCount,
                    inductionCount + 1,
                    guarded,
                    history
                  ).flatMap { case (t, ns, nl) =>
                    solveConstructors(tail, ns, nl, solved :+ t)
                  }
              }
              solveConstructors(algebra.constructors, subst, linearContext, Nil)
                .map { case (ts, fs, fl) =>
                  (
                    ProofTree.Node(
                      applySubst(goal, fs),
                      s"induction[${algebra.name}]",
                      ts
                    ),
                    fs,
                    fl
                  )
                }
            }
            SolveTree.merge(algTrees)
          }
        case _ => SolveTree.Failure
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
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val ruleTrees = applyRules(goal, subst, depth).map {
      case (rewritten, ruleName, nextS) =>
        val finalGoal = Rewriter.normalize(rewritten)
        if (finalGoal != goal) {
          search(
            finalGoal,
            rules,
            context,
            linearContext,
            nextS,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          )
            .map { case (t, s, restL) =>
              (ProofTree.Node(applySubst(goal, s), ruleName, List(t)), s, restL)
            }
        } else SolveTree.Failure
    }
    SolveTree.merge(ruleTrees)
  }

  private def searchRulesInContext(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val ctxRuleTrees = linearContext.zipWithIndex.flatMap {
      case ((name, hyp), i) =>
        applyRules(hyp, subst, depth).map { case (rewritten, ruleName, nextS) =>
          val finalHyp = Rewriter.normalize(rewritten)
          if (finalHyp != hyp) {
            val nextLinear = linearContext.patch(i, List((name, finalHyp)), 1)
            search(
              goal,
              rules,
              context,
              nextLinear,
              nextS,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              guarded,
              history
            )
              .map { case (t, s, restL) =>
                (
                  ProofTree.Node(
                    applySubst(goal, s),
                    s"$ruleName[$name]",
                    List(t)
                  ),
                  s,
                  restL
                )
              }
          } else SolveTree.Failure
        }
    }
    SolveTree.merge(ctxRuleTrees)
  }

  private def searchRulesInPersistentContext(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val ctxRuleTrees = context.zipWithIndex.flatMap { case ((name, hyp), i) =>
      applyRules(hyp, subst, depth).map { case (rewritten, ruleName, nextS) =>
        val finalHyp = Rewriter.normalize(rewritten)
        if (finalHyp != hyp) {
          val nextCtx = context.patch(i, List((name, finalHyp)), 1)
          search(
            goal,
            rules,
            nextCtx,
            linearContext,
            nextS,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          )
            .map { case (t, s, restL) =>
              (
                ProofTree.Node(
                  applySubst(goal, s),
                  s"$ruleName[$name]",
                  List(t)
                ),
                s,
                restL
              )
            }
        } else SolveTree.Failure
      }
    }
    SolveTree.merge(ctxRuleTrees)
  }

  private def searchContext(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val rewrites = context.flatMap { case (name, hyp) =>
      Rewriter.normalize(applySubst(hyp, subst)) match {
        case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
          val l = args match {
            case List(t, x, y) if hyp.headSymbol == Path => x
            case List(x, y)                              => x
            case _                                       => null
          }
          if (l != null) {
            val r = args.last
            List(
              SolveTree
                .fromLazyList(findAndReplace(goal, l, r, subst))
                .flatMap { case (rewritten, finalS) =>
                  val finalGoal = Rewriter.normalize(rewritten)
                  if (finalGoal != goal) {
                    search(
                      finalGoal,
                      rules,
                      context,
                      linearContext,
                      finalS,
                      depth + 1,
                      limit,
                      visited,
                      raaCount,
                      inductionCount,
                      guarded,
                      history
                    )
                      .map { case (t, s, restL) =>
                        (
                          ProofTree.Node(
                            applySubst(goal, s),
                            s"rewrite[$name]",
                            List(t)
                          ),
                          s,
                          restL
                        )
                      }
                  } else SolveTree.Failure
                }
            )
          } else Nil
        case _ => Nil
      }
    }
    val uses =
      (context.map(h => (h, false)) ++ linearContext.map(h => (h, true))).map {
        case ((name, hyp), isLinear) =>
          useHypothesis(
            name,
            hyp,
            goal,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            visited,
            raaCount,
            inductionCount,
            isLinear,
            guarded,
            history
          )
      }
    SolveTree.merge(rewrites ++ uses)
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
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      isLinear: Boolean,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val currentHyp = applySubst(hyp, subst)
    val nextLinear =
      if (isLinear) linearContext.filterNot(_._1 == name) else linearContext

    val basic = SolveTree.fromLazyList(
      unify(currentHyp, goal, subst).map(s =>
        (ProofTree.Leaf(applySubst(goal, s), name), s, nextLinear)
      )
    )

    val rewrite = currentHyp match {
      case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
        val l = args match {
          case List(t, x, y) if currentHyp.headSymbol == Path => x
          case List(x, y)                                     => x
          case _                                              => null
        }
        if (l != null) {
          val r = args.last
          SolveTree.fromLazyList(findAndReplace(goal, l, r, subst)).flatMap {
            case (rewritten, nextS) =>
              val finalGoal = Rewriter.normalize(rewritten)
              if (finalGoal != goal) {
                search(
                  finalGoal,
                  rules,
                  context,
                  nextLinear,
                  nextS,
                  depth + 1,
                  limit,
                  visited,
                  raaCount,
                  inductionCount,
                  guarded,
                  history
                ).map { case (t, s, restL) =>
                  (
                    ProofTree
                      .Node(applySubst(goal, s), s"rewrite[$name]", List(t)),
                    s,
                    restL
                  )
                }
              } else SolveTree.Failure
          }
        } else SolveTree.Failure
      case _ => SolveTree.Failure
    }

    val advanced = currentHyp match {

      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>

        searchLinearApply(
          name,
          goal,
          a,
          b,
          rules,
          context,
          nextLinear,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )

      case Expr.App(
            Expr.Sym(Implies | ImpliesAlt1 | ImpliesAlt2),

            List(a, b)
          ) =>

        searchApply(
          name,
          goal,
          a,
          b,
          rules,
          context,
          linearContext,
          subst,
          depth,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        )

      case Expr.App(Expr.Sym(Forall | Globally), args) =>

        boundary {
          if (currentHyp.headSymbol == Globally) {
            useHypothesis(
              name,
              args(0),
              goal,
              rules,
              context,
              linearContext,
              subst,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              isLinear,
              guarded,
              history
            )
          } else {
            val instantiated = args match {
              case List(Expr.Var(v), b) =>
                Prover.substVar(b, v, freshMeta(depth))
              case List(Expr.Var(v), _, b) =>
                Prover.substVar(b, v, freshMeta(depth))
              case _ => boundary.break(SolveTree.Failure)
            }
            useHypothesis(
              name,
              instantiated,
              goal,
              rules,
              context,
              linearContext,
              subst,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              isLinear,
              guarded,
              history
            )
          }
        }
      case _ => SolveTree.Failure
    }
    SolveTree.merge(List(basic, rewrite, advanced))
  }

  private def searchClassical(
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
  ): SolveTree[(ProofTree, Subst, Context)] = {
    if (
      config.classical && raaCount < config.maxRaa && goal != Expr.Sym(False)
    ) {
      val neg = Expr.App(Expr.Sym(Implies), List(goal, Expr.Sym(False)))
      search(
        Expr.Sym(False),
        rules,
        (s"raa$depth", neg) :: context,
        linearContext,
        subst,
        depth + 1,
        limit,
        visited,
        raaCount + 1,
        inductionCount,
        guarded,
        history
      )
        .map { case (t, s, restL) =>
          (ProofTree.Node(applySubst(goal, s), "RAA", List(t)), s, restL)
        }
    } else SolveTree.Failure
  }

  private def applyRules(
      e: Expr,
      subst: Subst,
      depth: Int
  ): List[(Expr, String, Subst)] = {
    val h = e.headSymbol
    val candidates = ruleIndex.getOrElse(h, Nil) ++
      ruleIndex.getOrElse("_VAR_", Nil) ++
      ruleIndex.getOrElse("_META_", Nil)

    val atRoot = candidates.flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      unify(e, instRule.lhs, subst)
        .map(s => (applySubst(instRule.rhs, s), rule.name, s))
        .toList
    }

    val inChildren = e match {
      case Expr.App(f, args) =>
        args.indices.flatMap { i =>
          applyRules(args(i), subst, depth).map {
            case (newArg, ruleName, nextS) =>
              (
                Expr.App(
                  applySubst(f, nextS),
                  args.patch(i, List(newArg), 1).map(applySubst(_, nextS))
                ),
                ruleName,
                nextS
              )
          }
        }
      case _ => Nil
    }

    atRoot ++ inChildren
  }

  private def findAndReplace(
      goal: Expr,
      l: Expr,
      r: Expr,
      s: Subst
  ): LazyList[(Expr, Subst)] = {
    unify(goal, applySubst(l, s), s).map(ns => (applySubst(r, ns), ns)) #::: {
      goal match {
        case Expr.App(f, args) =>
          args.indices.to(LazyList).flatMap { i =>
            findAndReplace(args(i), l, r, s).map { case (nA, ns) =>
              (
                Expr.App(
                  applySubst(f, ns),
                  args.patch(i, List(nA), 1).map(applySubst(_, ns))
                ),
                ns
              )
            }
          }
        case _ => LazyList.empty
      }
    }
  }
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
  def instantiate(
      rule: CatRule,
      freshMeta: () => Expr
  ): (CatRule, Map[MetaId, Expr]) = {
    val vars =
      collectVars(rule.lhs) ++ collectVars(rule.rhs) ++ rule.universals.flatMap(
        collectVars
      )
    val substMap = vars.map(v => v -> freshMeta()).toMap
    val instLhs = applyVarSubst(rule.lhs, substMap)
    val instRhs = applyVarSubst(rule.rhs, substMap)
    val instUniv = rule.universals.map(applyVarSubst(_, substMap))
    val metaSubst = substMap.collect { case (_, Expr.Meta(id)) =>
      id -> Expr.Meta(id)
    }
    (CatRule(rule.name, instLhs, instRhs, instUniv), metaSubst)
  }
  private def collectVars(e: Expr): Set[String] = e match {
    case Expr.Var(n)       => Set(n)
    case Expr.App(h, args) =>
      e match {
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
          collectVars(body) - v
        case _ => collectVars(h) ++ args.flatMap(collectVars)
      }
    case _ => Set.empty
  }
  private def applyVarSubst(e: Expr, s: Map[String, Expr]): Expr = e match {
    case Expr.Var(n) if s.contains(n) => s(n)
    case Expr.App(h, args)            =>
      e match {
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
          Expr.App(Expr.Sym("λ"), List(Expr.Var(v), applyVarSubst(body, s)))
        case _ => Expr.App(applyVarSubst(h, s), args.map(applyVarSubst(_, s)))
      }
    case _ => e
  }
}
