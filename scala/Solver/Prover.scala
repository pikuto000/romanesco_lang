// ==========================================
// Prover.scala
// 証明探索エンジン（純粋プラグインベース・Treeアーキテクチャ）
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable
import scala.collection.concurrent.TrieMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import romanesco.Utils.Debug.logger
import LogicSymbols._
import scala.util.boundary
import romanesco.Types.Tree

final class TimeoutException extends RuntimeException("Watchdog timeout")

final class Prover(val config: ProverConfig = ProverConfig.default)
    extends ProverInterface {
  import Unifier._

  var deadline: Long = 0
  private val dynamicRules = TrieMap[String, CatRule]()
  private val dynamicAlgebras = mutable.ListBuffer[InitialAlgebra]()
  private val globalFailureCache =
    TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()
  private val globalLemmaCache = TrieMap[
    (Expr, Set[Expr], List[Expr]),
    (ProofTree, Subst, List[(String, Expr)])
  ]()

  // 全てのロジックをプラグインとして管理
  private val plugins: List[LogicPlugin] = List(
    new AxiomPlugin(), // 基本公理
    new HoTTPlugin(), // HoTT (加速門)
    new CubicalPlugin(), // Cubical (区間変数・道の計算)
    new IntroductionPlugin(), // 標準導入ルール
    new LinearLogicPlugin(), // 線形・分離論理
    new TemporalLogicPlugin(), // 時相論理
    new PersistentLogicPlugin(), // 標準分解ルール
    new ForwardReasoningPlugin(), // 前向き推論
    new ModalLogicPlugin(), // 様相論理
    new HoareLogicPlugin(), // Hoare論理
    new UserRulePlugin(), // ユーザー定義ルール
    new InductionPlugin(), // 帰納法
    new RewritePlugin() // 書き換え
  )

  def freshMeta(depth: Int): Expr = {
    Expr.Meta(MetaId(List(depth, Prover.globalMetaCounter.incrementAndGet())))
  }

  // addHIT removed: HIT DSL should be integrated into the HoTT plug-in.

  override def getAlgebras: List[InitialAlgebra] =
    config.algebras ++ dynamicAlgebras.toList

  def addDynamicRule(rule: CatRule): Unit = {
    if (!dynamicRules.contains(rule.toString)) {
      dynamicRules.put(rule.toString, rule)
    }
  }

  private lazy val backwardRuleIndex = {
    val allRules =
      if (config.classical) config.rules ++ StandardRules.classical
      else config.rules
    allRules.groupBy(_.rhs.headSymbol)
  }

  private lazy val forwardRuleIndex = {
    val allRules =
      if (config.classical) config.rules ++ StandardRules.classical
      else config.rules
    allRules.groupBy(_.lhs.headSymbol)
  }

  def applyRules(
      e: Expr,
      subst: Subst,
      depth: Int,
      isGoal: Boolean
  ): List[(Expr, List[Expr], String, Subst)] = {
    val candidates = backwardRuleIndex.getOrElse(e.headSymbol, Nil) ++
      dynamicRules.values.filter(r => r.rhs.headSymbol == e.headSymbol)

    candidates.flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      unify(e, instRule.rhs, subst)
        .map(s =>
          (
            normalize(applySubst(instRule.lhs, s)),
            instRule.universals,
            rule.name,
            s
          )
        )
        .toList
    }
  }

  def forwardApplyRules(
      e: Expr,
      subst: Subst,
      depth: Int
  ): List[(Expr, String, Subst)] = {
    val candidates = forwardRuleIndex.getOrElse(e.headSymbol, Nil) ++
      dynamicRules.values.filter(r => r.lhs.headSymbol == e.headSymbol)

    candidates.flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      unify(e, instRule.lhs, subst)
        .map(s =>
          (
            normalize(applySubst(instRule.rhs, s)),
            rule.name,
            s
          )
        )
        .toList
    }
  }

  def normalize(e: Expr): Expr = Rewriter.normalize(e, config.rules ++ dynamicRules.values.toList)

  def checkDeadline(): Unit = {
    if (System.currentTimeMillis() > deadline) throw new TimeoutException()
  }

  def prove(
      goal: Expr,

      rules: List[CatRule] = Nil,

      maxDepth: Int = 30,

      timeoutMs: Long = 15000,

      initialGoal: Option[Goal] = None
  ): Either[FailTrace, ProofResult] = {

    val startTime = System.nanoTime()
    deadline = System.currentTimeMillis() + timeoutMs
    println(
      f"  [prove] timeout=${timeoutMs}ms, maxDepth=$maxDepth, goal=${goal.toString}"
    )

    val startGoal = initialGoal.getOrElse(Goal(Nil, Nil, goal))

    val res = boundary {
      try {
        val result = (1 to maxDepth).view.flatMap { d =>
          checkDeadline()

          logger.log(s"--- Iterative Deepening: current limit = $d ---")

          val searchTree = search(
            Vector(startGoal.target),
            startGoal.context,
            LogicState(startGoal.linearContext),
            emptySubst,
            0,
            d,
            Set.empty,
            false
          )
          allSuccesses(searchTree)
            .filter(_.linearContext.isEmpty)
            .map(node => (node.result.toOption.get.tree, searchTree))
            .headOption
        }.headOption

        result match {
          case Some((proof, fullTree)) =>
            Right(ProofResult(proof, None, Some(fullTree)))
          case None => Left(FailTrace(startGoal, "No proof found", 0))
        }
      } catch {
        case _: TimeoutException =>
          Left(FailTrace(startGoal, "Watchdog timeout", 0))
      }
    }

    val elapsed = (System.nanoTime() - startTime) / 1_000_000.0
    println(f"  [time] ${elapsed}%.1fms - ${
        if (res.isRight) "solved" else "failed"
      }: ${goal.toString.take(80)}")

    res
  }

  protected[core] def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] =

    tree match {

      case Tree.E() => None

      case Tree.V(node, branches) =>

        if (node.isSuccess) Some(node)
        else branches.view.flatMap(findSuccess).headOption

    }

  protected[core] def allSuccesses(
      tree: Tree[SearchNode]
  ): LazyList[SearchNode] = {
    tree match {
      case Tree.V(node, children) =>
        val current =
          if (node.result.isRight) LazyList(node) else LazyList.empty
        current ++ children.to(LazyList).flatMap(allSuccesses)
      case _ => LazyList.empty
    }
  }

  def search(
      exprs: Vector[Expr],

      context: List[(String, Expr)],

      state: LogicState,

      subst: Subst,

      depth: Int,

      limit: Int,

      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],

      guarded: Boolean
  ): Tree[SearchNode] = {
    checkDeadline()

    logger.increaseDepth()

    val currentGoal = exprs.last

    val currentGoalRaw = normalize(applySubst(currentGoal, subst))

    val currentGoalCan = currentGoalRaw.canonicalize()

    logger.log(s"Search: $currentGoalRaw (depth $depth/$limit)")

    // 停止条件

    if (depth > limit) {

      logger.log(s"Limit reached: $currentGoalRaw")

      logger.decreaseDepth()

      return Tree.V(
        SearchNode(
          exprs,

          "limit",

          depth,

          Left(
            FailTrace(
              Goal(context, state.linearContext, currentGoal),

              "Limit reached",

              depth
            )
          ),

          subst,

          context,

          state.linearContext
        ),

        Vector.empty
      )

    }

    // 循環検知
    val contextExprs = context
      .map(h => normalize(applySubst(h._2, subst)).canonicalize())
      .toSet
    val linearExprs = state.linearContext
      .map(h => normalize(applySubst(h._2, subst)).canonicalize())
      .sortBy(_.toString)

    // キャッシュのチェック (Lemma Cache)
    globalLemmaCache.get((currentGoalCan, contextExprs, linearExprs)).foreach {
      case (p, s, l) =>
        logger.log(s"Lemma Cache hit: $currentGoalRaw")
        logger.decreaseDepth()
        return Tree.V(
          SearchNode(
            exprs,
            "lemma-cache",
            depth,
            Right(ProofResult(p)),
            s,
            context,
            l
          ),
          Vector.empty
        )
    }

    // キャッシュのチェック (Failure Cache - サブセット考慮)
    // 線形リソースが「減少」のみする場合、多いリソースで失敗したなら少ないリソースでも失敗するはず
    val failureHit = globalFailureCache.find {
      case ((g, cExprs, lExprs, gded), d) =>
        g == currentGoalCan && cExprs == contextExprs && gded == guarded && linearExprs
          .forall(lExprs.contains) && d >= limit - depth
    }
    if (failureHit.isDefined) {
      logger.log(s"Failure Cache (subset) hit: $currentGoalRaw")
      logger.decreaseDepth()
      return Tree.V(
        SearchNode(
          exprs,
          "failure-cache",
          depth,
          Left(
            FailTrace(
              Goal(context, state.linearContext, currentGoal),
              "Cached failure",
              depth
            )
          ),
          subst,
          context,
          state.linearContext
        ),
        Vector.empty
      )
    }

    if (
      visited.exists(v =>
        v._1 == currentGoalCan && v._2 == contextExprs && v._3 == linearExprs
      )
    ) {
      if (guarded) {
        logger.log(s"Co-induction: $currentGoalRaw")
        val p = ProofTree.Leaf(currentGoalRaw, "co-induction")
        logger.decreaseDepth()
        return Tree.V(
          SearchNode(
            exprs,
            "co-induction",
            depth,
            Right(ProofResult(p)),
            subst,
            context,
            state.linearContext
          ),
          Vector.empty
        )
      }
      logger.log(s"Cycle detected: $currentGoalRaw")
      logger.decreaseDepth()
      return Tree.V(
        SearchNode(
          exprs,
          "cycle",
          depth,
          Left(
            FailTrace(
              Goal(context, state.linearContext, currentGoal),
              "Cycle",
              depth
            )
          ),
          subst,
          context,
          state.linearContext
        ),
        Vector.empty
      )
    }

    // 発散検知

    if (
      exprs.exists(prev =>
        prev.headSymbol == currentGoalCan.headSymbol && prev.complexity < currentGoalCan.complexity && romanesco.Utils.Misc
          .isEmbedding(prev, currentGoalCan)
      )
    ) {

      logger.log(s"Divergence detected: $currentGoalRaw")

      logger.decreaseDepth()

      return Tree.V(
        SearchNode(
          exprs,

          "divergence",

          depth,

          Left(
            FailTrace(
              Goal(context, state.linearContext, currentGoal),

              "Divergence",

              depth
            )
          ),

          subst,

          context,

          state.linearContext
        ),

        Vector.empty
      )

    }

    val nextVisited =
      visited + ((currentGoalCan, contextExprs, linearExprs, state))

    val branches = mutable.ArrayBuffer[Tree[SearchNode]]()

    // 全てのプラグインからブランチを収集
    plugins.foreach { p =>
      checkDeadline()
      val goalHooks = p.getGoalHooks(
        exprs,
        config.rules,
        context,
        state,
        subst,
        depth,
        limit,
        nextVisited,
        guarded,
        this
      )
      if (goalHooks.nonEmpty)
        logger.log(
          s"Plugin ${p.name} goal hooks returned ${goalHooks.size} branches"
        )
      branches ++= goalHooks

      checkDeadline()
      val contextHooks = p.getContextHooks(
        exprs,
        config.rules,
        context,
        state,
        subst,
        depth,
        limit,
        nextVisited,
        guarded,
        this
      )
      if (contextHooks.nonEmpty)
        logger.log(
          s"Plugin ${p.name} context hooks returned ${contextHooks.size} branches"
        )
      branches ++= contextHooks
    }

    logger.log(s"Total branches for $currentGoalRaw: ${branches.size}")

    // 並行分岐アーキテクチャ: 結果の集約
    val finalSuccess = allSuccesses(
      Tree.V(
        SearchNode(
          exprs,
          "choice",
          depth,
          Left(
            FailTrace(
              Goal(context, state.linearContext, currentGoal),
              "Dummy",
              depth
            )
          ),
          subst,
          context,
          state.linearContext
        ),
        branches.toVector
      )
    ).headOption
    val finalResult = finalSuccess match {
      case Some(s) =>
        logger.log(s"Success: $currentGoalRaw using ${s.ruleName}")
        // 成功をキャッシュに保存
        globalLemmaCache.put(
          (currentGoalCan, contextExprs, linearExprs),
          (s.result.toOption.get.tree, s.subst, s.linearContext)
        )
        s.result
      case None =>
        logger.log(s"Fail: $currentGoalRaw")
        // 失敗をキャッシュに保存 (現在の残り深さを記録)
        globalFailureCache.put(
          (currentGoalCan, contextExprs, linearExprs, guarded),
          limit - depth
        )
        Left(
          FailTrace(
            Goal(context, state.linearContext, currentGoal),
            "All branches failed",
            depth
          )
        )
    }
    val ruleName = if (branches.isEmpty) "failure" else "choice"

    logger.decreaseDepth()
    Tree.V(
      SearchNode(
        exprs,
        ruleName,
        depth,
        finalResult,
        finalSuccess.map(_.subst).getOrElse(subst),
        finalSuccess.map(_.context).getOrElse(context),
        finalSuccess.map(_.linearContext).getOrElse(state.linearContext)
      ),
      branches.toVector
    )
  }

}

object Prover {
  private[core] val globalMetaCounter = new AtomicInteger(0)
  def substVar(expr: Expr, varName: String, replacement: Expr): Expr =
    expr match {
      case Expr.Var(n) if n == varName => replacement
      case Expr.Sym(n) if n == varName => replacement
      case Expr.App(h, args)           =>
        Expr.App(
          substVar(h, varName, replacement),
          args.map(substVar(_, varName, replacement))
        )
      case _ => expr
    }
  def instantiate(
      rule: CatRule,
      freshMeta: () => Expr
  ): (CatRule, Map[MetaId, Expr]) = {
    val vars = collectVars(rule.lhs) ++ collectVars(rule.rhs)
    val substMap = vars.map(v => v -> freshMeta()).toMap
    def applyVarSubst(e: Expr): Expr = e match {
      case Expr.Var(n) if substMap.contains(n) => substMap(n)
      case Expr.App(h, args)                   =>
        Expr.App(applyVarSubst(h), args.map(applyVarSubst))
      case _ => e
    }
    (
      CatRule(rule.name, applyVarSubst(rule.lhs), applyVarSubst(rule.rhs)),
      Map.empty
    )
  }
  def collectVars(e: Expr): Set[String] = e match {
    case Expr.Var(n)       => Set(n)
    case Expr.App(h, args) => collectVars(h) ++ args.flatMap(collectVars)
    case _                 => Set.empty
  }
  def freeVars(e: Expr): Set[String] = e match {
    case Expr.Var(n)                                      => Set(n)
    case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) => freeVars(body) - v
    case Expr.App(f, args) => freeVars(f) ++ args.flatMap(freeVars).toSet
    case _                 => Set.empty
  }
  def collectSymbols(e: Expr): Set[String] = e match {
    case Expr.Sym(n)       => Set(n)
    case Expr.App(h, args) => collectSymbols(h) ++ args.flatMap(collectSymbols)
    case _                 => Set.empty
  }
}
