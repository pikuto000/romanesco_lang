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

final class Prover(val config: ProverConfig = ProverConfig.default) extends ProverInterface {
  import Unifier._

  var deadline: Long = 0
  private val dynamicRules = TrieMap[String, CatRule]()
  private val dynamicAlgebras = mutable.ListBuffer[InitialAlgebra]()
  private val globalFailureCache = TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()
  private val globalLemmaCache = TrieMap[(Expr, Set[Expr], List[Expr]), ProofTree]()

  // 全てのロジックをプラグインとして管理
  private val plugins: List[LogicPlugin] = List(
    new AxiomPlugin(),        // 基本公理
    new IntroductionPlugin(), // 標準導入ルール
    new LinearLogicPlugin(),  // 線形・分離論理
    new TemporalLogicPlugin(),// 時相論理
    new PersistentLogicPlugin(), // 標準分解ルール
    new HoTTPlugin(),         // HoTT
    new HoareLogicPlugin(),   // Hoare論理
    new UserRulePlugin()      // ユーザー定義ルール
  )

  def freshMeta(depth: Int): Expr = {
    Expr.Meta(MetaId(List(depth, Prover.globalMetaCounter.incrementAndGet())))
  }

  def addHIT(hit: InitialAlgebra): Unit = {
    if (!dynamicAlgebras.exists(_.name == hit.name)) { dynamicAlgebras += hit }
  }

  def addDynamicRule(rule: CatRule): Unit = {
    if (!dynamicRules.contains(rule.toString)) { dynamicRules.put(rule.toString, rule) }
  }

  private lazy val backwardRuleIndex = {
    val allRules = if (config.classical) config.rules ++ StandardRules.classical else config.rules
    allRules.groupBy(_.rhs.headSymbol)
  }

  def applyRules(e: Expr, subst: Subst, depth: Int, isGoal: Boolean): List[(Expr, List[Expr], String, Subst)] = {
    val candidates = backwardRuleIndex.getOrElse(e.headSymbol, Nil) ++ 
                     dynamicRules.values.filter(r => r.rhs.headSymbol == e.headSymbol)
    
    candidates.flatMap { rule =>
      val (instRule, _) = Prover.instantiate(rule, () => freshMeta(depth))
      unify(e, instRule.rhs, subst).map(s => (applySubst(instRule.lhs, s), instRule.universals, rule.name, s)).toList
    }
  }

  def prove(
      goal: Expr,
      rules: List[CatRule] = Nil,
      maxDepth: Int = 30,
      timeoutMs: Long = 15000,
      initialGoal: Option[Goal] = None
  ): Either[FailTrace, ProofResult] = {
    deadline = System.currentTimeMillis() + timeoutMs
    val startGoal = initialGoal.getOrElse(Goal(Nil, Nil, goal))

    boundary {
      val result = (1 to maxDepth).view.flatMap { d =>
        if (System.currentTimeMillis() > deadline) boundary.break(Left(FailTrace(startGoal, "Watchdog timeout", 0)))
        val searchTree = search(Vector(startGoal.target), startGoal.context, startGoal.linearContext, emptySubst, 0, d, Set.empty, 0, 0, false)
        findSuccess(searchTree).map(node => (node.result.toOption.get.tree, searchTree))
      }.headOption

      result match {
        case Some((proof, fullTree)) => Right(ProofResult(proof, None, Some(fullTree)))
        case None => Left(FailTrace(startGoal, "No proof found", 0))
      }
    }
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }

  def search(
      exprs: Vector[Expr],
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean
  ): Tree[SearchNode] = {
    val currentGoal = exprs.last
    val currentGoalRaw = Rewriter.normalize(applySubst(currentGoal, subst))
    val currentGoalCan = currentGoalRaw.canonicalize()

    // 停止条件
    if (System.currentTimeMillis() > deadline || depth > limit) {
      return Tree.V(SearchNode(exprs, "limit", depth, Left(FailTrace(Goal(context, linearContext, currentGoal), "Limit reached", depth)), subst, context, linearContext), Vector.empty)
    }

    // 循環検知
    val contextExprs = context.map(h => Rewriter.normalize(applySubst(h._2, subst)).canonicalize()).toSet
    val linearExprs = linearContext.map(h => Rewriter.normalize(applySubst(h._2, subst)).canonicalize()).sortBy(_.toString)
    if (visited.exists(v => v._1 == currentGoalCan && v._2 == contextExprs && v._3 == linearExprs)) {
      if (guarded) {
        val p = ProofTree.Leaf(currentGoalRaw, "co-induction")
        return Tree.V(SearchNode(exprs, "co-induction", depth, Right(ProofResult(p)), subst, context, linearContext), Vector.empty)
      }
      return Tree.V(SearchNode(exprs, "cycle", depth, Left(FailTrace(Goal(context, linearContext, currentGoal), "Cycle", depth)), subst, context, linearContext), Vector.empty)
    }
    
    // 発散検知
    if (exprs.exists(prev => prev.headSymbol == currentGoalCan.headSymbol && prev.complexity < currentGoalCan.complexity && romanesco.Utils.Misc.isEmbedding(prev, currentGoalCan))) {
      return Tree.V(SearchNode(exprs, "divergence", depth, Left(FailTrace(Goal(context, linearContext, currentGoal), "Divergence", depth)), subst, context, linearContext), Vector.empty)
    }

    val nextVisited = visited + ((currentGoalCan, contextExprs, linearExprs))
    val branches = mutable.ArrayBuffer[Tree[SearchNode]]()

    // 純粋にプラグインのみを呼び出す
    plugins.foreach { p =>
      branches ++= p.getGoalHooks(exprs, config.rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, this)
      branches ++= p.getContextHooks(exprs, config.rules, context, linearContext, subst, depth, limit, nextVisited, raaCount, inductionCount, guarded, this)
    }

    // 結果の集計
    val finalSuccess = branches.view.flatMap(findSuccess).headOption
    val finalResult = finalSuccess match {
      case Some(s) => s.result
      case None => Left(FailTrace(Goal(context, linearContext, currentGoal), "All branches failed", depth))
    }

    Tree.V(SearchNode(exprs, "search", depth, finalResult, finalSuccess.map(_.subst).getOrElse(subst), finalSuccess.map(_.context).getOrElse(context), finalSuccess.map(_.linearContext).getOrElse(linearContext)), branches.toVector)
  }
}

object Prover {
  private[core] val globalMetaCounter = new AtomicInteger(0)
  def substVar(expr: Expr, varName: String, replacement: Expr): Expr = expr match {
    case Expr.Var(n) if n == varName => replacement
    case Expr.Sym(n) if n == varName => replacement
    case Expr.App(h, args) => Expr.App(substVar(h, varName, replacement), args.map(substVar(_, varName, replacement)))
    case _ => expr
  }
  def instantiate(rule: CatRule, freshMeta: () => Expr): (CatRule, Map[MetaId, Expr]) = {
    val vars = collectVars(rule.lhs) ++ collectVars(rule.rhs)
    val substMap = vars.map(v => v -> freshMeta()).toMap
    def applyVarSubst(e: Expr): Expr = e match {
      case Expr.Var(n) if substMap.contains(n) => substMap(n)
      case Expr.App(h, args) => Expr.App(applyVarSubst(h), args.map(applyVarSubst))
      case _ => e
    }
    (CatRule(rule.name, applyVarSubst(rule.lhs), applyVarSubst(rule.rhs)), Map.empty)
  }
  def collectVars(e: Expr): Set[String] = e match {
    case Expr.Var(n) => Set(n)
    case Expr.App(h, args) => collectVars(h) ++ args.flatMap(collectVars)
    case _ => Set.empty
  }
  def collectSymbols(e: Expr): Set[String] = e match {
    case Expr.Sym(n) => Set(n)
    case Expr.App(h, args) => collectSymbols(h) ++ args.flatMap(collectSymbols)
    case _ => Set.empty
  }
}
