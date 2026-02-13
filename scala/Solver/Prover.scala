// ==========================================
// Prover.scala
// 証明探索エンジン（モジュール化・高機能版・非局所return排除・安定版・visited修正完了）
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

  private val visitedGlobal =
    TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()
  private[core] var deadline: Long = 0

  private val algebras =
    if (config.algebras.nonEmpty) config.algebras
    else StandardRules.defaultAlgebras

  private val forwardRuleIndex: Map[String, List[CatRule]] = {
    val allRules = if (config.classical) config.rules ++ StandardRules.classical else config.rules
    allRules.groupBy(_.lhs.headSymbol)
  }

  private val backwardRuleIndex: Map[String, List[CatRule]] = {
    val allRules = if (config.classical) config.rules ++ StandardRules.classical else config.rules
    allRules.groupBy(_.rhs.headSymbol)
  }

  override def getGoalHooks(
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
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    super[LinearLogicSearch].getGoalHooks(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history) ++
    super[TemporalLogicSearch].getGoalHooks(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history) ++
    super[HoTTSearch].getGoalHooks(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history)
  }

  override def getContextHooks(
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
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    super[LinearLogicSearch].getContextHooks(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history) ++
    super[PersistentLogicSearch].getContextHooks(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history)
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
      timeoutMs: Long = 10000,
      initialGoal: Option[Goal] = None
  ): Either[FailTrace, ProofResult] =
    val effectiveRules = if (rules.nonEmpty) rules else config.rules

    logger.resetDepth()
    logger.log(s"prove begin. goal: $goal (maxDepth: $maxDepth)")
    
    bestFail.set(None)
    clearCaches() 
    visitedGlobal.clear()
    deadline = System.currentTimeMillis() + timeoutMs

    val startGoal = initialGoal.getOrElse(Goal(Nil, Nil, goal))

    boundary {
      val result = (1 to maxDepth).view.flatMap { d =>
        if (System.currentTimeMillis() > deadline)
          boundary.break(Left(FailTrace(startGoal, "Watchdog timeout", 0)))

        logger.log(s"--- Iterative Deepening: current limit = $d ---")
        metaCounter.set(0)
        visitedGlobal.clear() 
        
        val tree = search(
          startGoal.target,
          effectiveRules,
          startGoal.context,
          startGoal.linearContext,
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
          Left(bestFail.get().getOrElse(FailTrace(startGoal, "No proof found", 0)))
      }
    }

  private def generateLemma(tree: ProofTree): Option[CatRule] = {
    val provedGoal = tree match {
      case ProofTree.Node(g, _, _) => g
      case ProofTree.Leaf(g, _)    => g
    }
    val modeOk = config.lemmaMode match {
      case LemmaGenerationMode.All => true
      case LemmaGenerationMode.InductionOnly =>
        def hasInduction(t: ProofTree): Boolean = t match {
          case ProofTree.Node(_, rule, cs) => rule.startsWith("induction") || cs.exists(hasInduction)
          case _ => false
        }
        hasInduction(tree)
      case LemmaGenerationMode.EqualityOnly =>
        provedGoal match {
          case Expr.App(Expr.Sym(Eq | Path), _) => true
          case _ => false
        }
      case LemmaGenerationMode.ManualOnly => false
    }
    if (!modeOk) return None

    provedGoal match {
      case Expr.App(Expr.Sym(Eq | Path), List(lhs, rhs)) =>
        val targetRhs = finalRhs(tree)
        val vars = (Prover.collectVars(lhs) ++ Prover.collectVars(targetRhs)).toList.map(Expr.Var(_))
        Some(CatRule(s"lemma_${java.util.UUID.randomUUID().toString.take(4)}", lhs, targetRhs, vars))
      case _ => None
    }
  }

  private def finalRhs(t: ProofTree): Expr = t match {
    case ProofTree.Leaf(Expr.App(_, List(_, r)), _) => r
    case ProofTree.Node(_, _, cs) if cs.nonEmpty => finalRhs(cs.last)
    case _ => Expr.Sym("?")
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
    logger.increaseDepth()
    val res = if (System.currentTimeMillis() > deadline) SolveTree.Failure
    else {
      val currentGoalRaw = Rewriter.normalize(applySubst(goal, subst))
      if (currentGoalRaw.complexity > config.maxComplexity || getPathLevel(currentGoalRaw) > config.maxPathLevel) {
        SolveTree.Failure
      } else {
        val currentGoalCan = currentGoalRaw.canonicalize
        val currentGoalStruct = currentGoalRaw.getStructuralPattern
        val contextExprs = context.map(h => Rewriter.normalize(applySubst(h._2, subst)).canonicalize).toSet
        val linearExprs = linearContext.map(h => Rewriter.normalize(applySubst(h._2, subst)).canonicalize).sortBy(_.toString)
        val stateKey = (currentGoalCan, contextExprs, linearExprs, guarded)
        val remainingDepth = limit - depth

        checkLemma(currentGoalCan, contextExprs, linearExprs.toList) match {
          case Some(cachedTree) => SolveTree.Success((cachedTree, subst, linearContext))
          case None =>
            // 循環検知：α同値または構造的同値
            if (visited.exists(v => (v._1.canonicalize == currentGoalCan || v._1.getStructuralPattern == currentGoalStruct) && v._2 == contextExprs && v._3 == linearExprs)) {
              if (guarded) SolveTree.Success((ProofTree.Leaf(goal, "co-induction"), subst, linearContext))
              else SolveTree.Failure
            }
            else if (history.exists(h => h.headSymbol == currentGoalCan.headSymbol && h.complexity < currentGoalCan.complexity && romanesco.Utils.Misc.isEmbedding(h, currentGoalCan))) {
              SolveTree.Failure
            }
            else if (history.count(h => h.getStructuralPattern == currentGoalStruct) > 2) {
              SolveTree.Failure
            }
            else if (visitedGlobal.get(stateKey).exists(_ <= depth)) SolveTree.Failure
            else if (checkGlobalFailure(currentGoalCan, contextExprs, linearExprs.toList, guarded, remainingDepth)) SolveTree.Failure
            else if (depth > limit) SolveTree.Failure
            else {
              visitedGlobal.put(stateKey, depth)
              val nextVisited = visited + ((currentGoalCan, contextExprs, linearExprs))
              val nextHistory = currentGoalRaw :: history

              val branches = List(
                SolveTree.fromLazyList(searchAxiom(currentGoalRaw, context, linearContext, subst, depth)),
                SolveTree.fromLazyList(searchReflexivity(currentGoalRaw, linearContext, subst, depth)),
                searchDecomposeGoal(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory),
                SolveTree.merge(getGoalHooks(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory)),
                searchContext(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory),
                SolveTree.merge(getContextHooks(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory)),
                SolveTree.DeepStep(() => searchRules(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory)),
                SolveTree.DeepStep(() => searchInduction(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory)),
                SolveTree.DeepStep(() => searchClassical(currentGoalRaw, rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, nextHistory))
              )

              SolveTree.Memo(
                SolveTree.Choice(branches),
                resultOpt => {
                  resultOpt match {
                    case Some((proof, _, _)) =>
                      recordLemma(currentGoalCan, contextExprs, linearExprs.toList, proof)
                    case None =>
                      recordGlobalFailure(currentGoalCan, contextExprs, linearExprs.toList, guarded, remainingDepth)
                  }
                }
              )
            }
        }
      }
    }
    logger.decreaseDepth()
    res
  }

  private def searchAxiom(
      goal: Expr,
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int
  ): LazyList[(ProofTree, Subst, Context)] = {
    val goalCan = goal.canonicalize
    def tryHyp(h: Expr, n: String, isL: Boolean): LazyList[(ProofTree, Subst, Context)] = {
      val hn = Rewriter.normalize(applySubst(h, subst))
      if (hn.canonicalize == goalCan) {
        LazyList((ProofTree.Leaf(applySubst(goal, subst), if (isL) s"linear:$n" else n), subst, if (isL) Nil else linearContext))
      } else {
        unify(hn, goal, subst).map(s => (ProofTree.Leaf(applySubst(goal, s), if (isL) s"linear:$n" else n), s, if (isL) Nil else linearContext)) #::: {
          hn match {
            case Expr.App(Expr.Sym(Forall), args) =>
              val inst = args match {
                case List(Expr.Var(v), b) => Prover.substVar(b, v, freshMeta(depth))
                case List(Expr.Var(v), _, b) => Prover.substVar(b, v, freshMeta(depth))
                case _ => null
              }
              if (inst != null) tryHyp(inst, n, isL) else LazyList.empty
            case Expr.App(Expr.Sym(Globally), List(a)) => tryHyp(a, n, isL)
            case _ => LazyList.empty
          }
        }
      }
    }
    context.view.to(LazyList).flatMap(h => tryHyp(h._2, h._1, false)) #::: (if (linearContext.length == 1) tryHyp(linearContext.head._2, linearContext.head._1, true) else LazyList.empty)
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
          case List(x, y) => x
          case _ => null
        }
        if (l != null) {
          val r = args.last
          unify(Rewriter.normalize(l), Rewriter.normalize(r), subst).map(s => (ProofTree.Leaf(applySubst(goal, s), "reflexivity"), s, linearContext))
        } else LazyList.empty
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
      case Expr.App(Expr.Sym(Forall), args) =>
        val (vName, body, typeOpt) = args match {
          case List(Expr.Var(v), b) => (v, b, None)
          case List(Expr.Var(v), t, b) => (v, b, Some(t))
          case _ => (null, null, None)
        }
        if (vName != null) {
          val freshVar = s"${vName}_$depth"
          val instantiated = Prover.substVar(body, vName, Expr.Var(freshVar))
          val newCtx = (freshVar, typeOpt.map(Prover.substVar(_, vName, Expr.Var(freshVar))).getOrElse(Expr.Sym("Type"))) :: context
          search(instantiated, rules, newCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (p, s, rl) =>
            (ProofTree.Node(applySubst(goal, s), "forall-intro", List(p)), s, rl)
          }
        } else SolveTree.Failure
      case Expr.App(Expr.Sym(Exists), args) =>
        val (vName, body) = args match {
          case List(Expr.Var(v), b) => (v, b)
          case List(Expr.Var(v), _, b) => (v, b)
          case _ => (null, null)
        }
        if (vName != null) {
          val meta = freshMeta(depth)
          search(Prover.substVar(body, vName, meta), rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (p, s, rl) =>
            (ProofTree.Node(applySubst(goal, s), "exists-intro", List(p)), s, rl)
          }
        } else SolveTree.Failure
      case Expr.App(Expr.Sym(Implies | Exp | ImpliesAlt1 | ImpliesAlt2), List(a, b)) =>
        val (ant, cons) = if (goal.headSymbol == Exp) (b, a) else (a, b)
        search(cons, rules, (s"h$depth", ant) :: context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (t, s, rl) =>
          (ProofTree.Node(applySubst(goal, s), "implies-intro", List(t)), s, rl)
        }
      case Expr.App(Expr.Sym(And | Product), List(a, b)) =>
        search(a, rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).flatMap { case (tA, s1, rl1) =>
          search(b, rules, context, rl1, s1, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (tB, s2, rl2) =>
            (ProofTree.Node(applySubst(goal, s2), "product-intro", List(tA, tB)), s2, rl2)
          }
        }
      case Expr.Sym(True) => SolveTree.Success((ProofTree.Leaf(goal, "true-intro"), subst, Nil))
      case _ => SolveTree.Failure
    }
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
          val l = args match { case List(t, x, y) if hyp.headSymbol == Path => x; case List(x, y) => x; case _ => null }
          if (l != null) Some(SolveTree.fromLazyList(findAndReplace(goal, l, args.last, subst)).flatMap { case (rewritten, finalS) =>
            val finalGoal = Rewriter.normalize(rewritten)
            if (finalGoal != goal) search(finalGoal, rules, context, linearContext, finalS, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (t, s, rl) =>
              (ProofTree.Node(applySubst(goal, s), s"rewrite[$name]", List(t)), s, rl)
            } else SolveTree.Failure
          }) else None
        case _ => None
      }
    }
    val uses = (context.map((_, false)) ++ linearContext.map((_, true))).map { case ((name, hyp), isL) =>
      useHypothesis(name, hyp, goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, isL, guarded, history)
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
      isL: Boolean,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val curH = applySubst(hyp, subst)
    val nextL = if (isL) linearContext.filterNot(_._1 == name) else linearContext
    val basic = SolveTree.fromLazyList(unify(curH, goal, subst).map(s => (ProofTree.Leaf(applySubst(goal, s), name), s, nextL)))
    val advanced = curH match {
      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        searchLinearApply(name, goal, a, b, rules, context, nextL, subst, depth, limit, visited, raaCount, inductionCount, guarded, history)
      case Expr.App(Expr.Sym(Implies | ImpliesAlt1 | ImpliesAlt2), List(a, b)) =>
        searchApply(name, goal, a, b, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history)
      case Expr.App(Expr.Sym(Forall | Globally), args) =>
        val inst = if (curH.headSymbol == Globally) args(0) else args match {
          case List(Expr.Var(v), b) => Prover.substVar(b, v, freshMeta(depth))
          case List(Expr.Var(v), _, b) => Prover.substVar(b, v, freshMeta(depth))
          case _ => null
        }
        if (inst != null) {
          useHypothesis(name, inst, goal, rules, context, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, isL, guarded, history)
        } else SolveTree.Failure
      case _ => SolveTree.Failure
    }
    SolveTree.merge(List(basic, advanced))
  }

  protected[core] def searchRules(
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
    val ruleTrees = (applyRules(goal, subst, depth, true) ++ applyRules(goal, subst, depth, false)).map { case (rewritten, univs, ruleName, nextS) =>
      val finalGoal = Rewriter.normalize(rewritten)
      if (finalGoal != goal || univs.nonEmpty) {
        def solveUnivs(us: List[Expr], s: Subst, l: Context, solved: List[ProofTree]): SolveTree[(List[ProofTree], Subst, Context)] = us match {
          case Nil => SolveTree.Success((solved, s, l))
          case u :: tail =>
            val un = Rewriter.normalize(applySubst(u, s))
            un match {
              case Expr.Var(_) | Expr.Meta(_) => solveUnivs(tail, s, l, solved)
              case _ =>
                val axL = searchAxiom(un, context, l, s, depth)
                val comb = axL #::: (if (axL.isEmpty) searchReflexivity(un, l, s, depth) else LazyList.empty)
                if (comb.isEmpty) SolveTree.Failure
                else SolveTree.Choice(comb.map { case (t, ns, nl) => solveUnivs(tail, ns, nl, solved :+ t) }.toList)
            }
        }
        solveUnivs(univs, nextS, linearContext, Nil).flatMap { case (tsUniv, fs, fl) =>
          search(finalGoal, rules, context, fl, fs, depth + 1, limit, visited, raaCount, inductionCount, guarded, history).map { case (t, s, rl) =>
            (ProofTree.Node(applySubst(goal, s), ruleName, tsUniv :+ t), s, rl)
          }
        }
      } else SolveTree.Failure
    }
    SolveTree.merge(ruleTrees)
  }

  protected[core] def searchInduction(
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
    else {
      val options = goal match {
        case Expr.App(Expr.Sym(Forall), args) =>
          val (vn, body, topt) = args match {
            case List(Expr.Var(v), b) => (v, b, None)
            case List(Expr.Var(v), t, b) => (v, b, Some(t))
            case _ => (null, null, None)
          }
          if (vn != null) {
            algebras.filter(a => topt.contains(Expr.Sym(a.name)) || vn.startsWith(a.varPrefix)).flatMap(algebra =>
              solveInduction(vn, body, goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history, Some(algebra))
            )
          } else Nil
        case _ => Nil
      }
      SolveTree.merge(options)
    }
  }

  private def solveInduction(vn: String, body: Expr, og: Expr, rules: List[CatRule], context: Context, linearContext: Context, subst: Subst, depth: Int, limit: Int, visited: Set[(Expr, Set[Expr], List[Expr])], raaCount: Int, inductionCount: Int, guarded: Boolean, history: List[Expr], target: Option[InitialAlgebra]): List[SolveTree[(ProofTree, Subst, Context)]] = {
    target.toList.map { algebra =>
      def solve(cs: List[ConstructorDef], s: Subst, l: Context, solved: List[ProofTree]): SolveTree[(List[ProofTree], Subst, Context)] = cs match {
        case Nil => SolveTree.Success((solved, s, l))
        case c :: tail =>
          val ct = if (c.argTypes.isEmpty) Expr.Sym(c.symbol) else Expr.App(Expr.Sym(c.symbol), c.argTypes.zipWithIndex.map { case (ArgType.Recursive, i) => Expr.Var(s"${vn}_$i"); case (ArgType.Constant, i) => Expr.Var(s"a_$i") })
          val ihs = ct match { case Expr.App(_, argsC) => c.argTypes.zip(argsC).collect { case (ArgType.Recursive, arg) => (s"IH_${arg}", Prover.substVar(body, vn, arg)) }; case _ => Nil }
          val currentIndGoal = c.ctorType match {
            case ConstructorType.Point => Prover.substVar(body, vn, ct)
            case ConstructorType.Path(from, to) =>
              Expr.App(Expr.Sym(Path), List(Expr.Sym("Type"), Prover.substVar(body, vn, from), Prover.substVar(body, vn, to)))
          }
          search(currentIndGoal, rules, ihs ++ context, l, s, depth + 1, limit, visited, raaCount, inductionCount + 1, guarded, history).flatMap { case (t, ns, nl) => solve(tail, ns, nl, solved :+ t) }
      }
      solve(algebra.constructors, subst, linearContext, Nil).map { case (ts, fs, fl) => (ProofTree.Node(applySubst(og, fs), s"induction[${algebra.name}]", ts), fs, fl) }
    }
  }

  protected[core] def searchClassical(
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
    if (config.classical && raaCount < config.maxRaa && goal != Expr.Sym(False)) {
      search(Expr.Sym(False), rules, (s"raa$depth", Expr.App(Expr.Sym(Implies), List(goal, Expr.Sym(False)))) :: context, linearContext, subst, depth + 1, limit, visited, raaCount + 1, inductionCount, guarded, history)
        .map { case (t, s, rl) => (ProofTree.Node(applySubst(goal, s), "RAA", List(t)), s, rl) }
    } else SolveTree.Failure
  }

  protected[core] def applyRules(e: Expr, subst: Subst, depth: Int, isGoal: Boolean): List[(Expr, List[Expr], String, Subst)] = {
    val index = if (isGoal) backwardRuleIndex else forwardRuleIndex
    val candidates = index.getOrElse(e.headSymbol, Nil) ++ index.getOrElse("_VAR_", Nil) ++ index.getOrElse("_META_", Nil)
    candidates.flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      val (m, r) = if (isGoal) (instRule.rhs, instRule.lhs) else (instRule.lhs, instRule.rhs)
      unify(e, m, subst).map(s => (applySubst(r, s), instRule.universals, rule.name, s)).filter { case (next, _, rname, _) =>
        if (isGoal) {
          val nextN = Rewriter.normalize(next)
          nextN.complexity < e.complexity || rname.matches(".*(dist|mapping|is|expansion|unfold|step|lemma|G-|bisim).*")
        } else Rewriter.normalize(next) != e
      }.toList
    } ++ (e match {
      case Expr.App(f, args) => args.indices.flatMap(i => applyRules(args(i), subst, depth, isGoal).map { case (na, us, rn, ns) =>
        (Expr.App(applySubst(f, ns), args.patch(i, List(na), 1).map(applySubst(_, ns))), us, rn, ns)
      })
      case _ => Nil
    })
  }

  protected[core] def findAndReplace(goal: Expr, l: Expr, r: Expr, s: Subst): LazyList[(Expr, Subst)] = {
    unify(goal, applySubst(l, s), s).map(ns => (applySubst(r, ns), ns)) #::: {
      goal match {
        case Expr.App(f, args) => args.indices.to(LazyList).flatMap(i => findAndReplace(args(i), l, r, s).map { case (na, ns) =>
          (Expr.App(applySubst(f, ns), args.patch(i, List(na), 1).map(applySubst(_, ns))), ns)
        })
        case _ => LazyList.empty
      }
    }
  }
}

object Prover {
  def substVar(expr: Expr, varName: String, replacement: Expr): Expr = expr match {
    case Expr.Var(n) if n == varName => replacement
    case Expr.Sym(n) if n == varName => replacement
    case Expr.App(h, args) =>
      expr match {
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) if v == varName => expr
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
          Expr.App(Expr.Sym("λ"), List(Expr.Var(v), substVar(body, varName, replacement)))
        case _ => Expr.App(substVar(h, varName, replacement), args.map(substVar(_, varName, replacement)))
      }
    case _ => expr
  }

  def instantiate(rule: CatRule, freshMeta: () => Expr): (CatRule, Map[MetaId, Expr]) = {
    val vars = collectVars(rule.lhs) ++ collectVars(rule.rhs) ++ rule.universals.flatMap(collectVars)
    val substMap = vars.map(v => v -> freshMeta()).toMap
    def applyVarSubst(e: Expr, s: Map[String, Expr]): Expr = e match {
      case Expr.Var(n) if s.contains(n) => s(n)
      case Expr.App(h, args) =>
        e match {
          case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
            Expr.App(Expr.Sym("λ"), List(Expr.Var(v), applyVarSubst(body, s)))
          case _ => Expr.App(applyVarSubst(h, s), args.map(applyVarSubst(_, s)))
        }
      case _ => e
    }
    (CatRule(rule.name, applyVarSubst(rule.lhs, substMap), applyVarSubst(rule.rhs, substMap), rule.universals.map(applyVarSubst(_, substMap))), substMap.collect { case (_, Expr.Meta(id)) => id -> Expr.Meta(id) })
  }

  def collectVars(e: Expr): Set[String] = e match {
    case Expr.Var(n) => Set(n)
    case Expr.App(h, args) =>
      e match {
        case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) => collectVars(body) - v
        case _ => collectVars(h) ++ args.flatMap(collectVars)
      }
    case _ => Set.empty
  }
}
