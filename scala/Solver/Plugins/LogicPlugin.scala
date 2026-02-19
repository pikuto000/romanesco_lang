package romanesco.Solver.core

import romanesco.Types.Tree

/**
 * 論理固有の状態（線形文脈、制限カウンターなど）を保持するクラス。
 */
case class LogicState(
    linearContext: List[(String, Expr)] = Nil,
    raaCount: Int = 0,
    inductionCount: Int = 0,
    hitEnabled: Boolean = true,
    custom: Map[String, Any] = Map.empty
) {
  def withLinear(lc: List[(String, Expr)]): LogicState = copy(linearContext = lc)
  def incRAA: LogicState = copy(raaCount = raaCount + 1)
  def incInduction: LogicState = copy(inductionCount = inductionCount + 1)
}

/**
 * 証明器のコア・インターフェース
 * SolveTreeを廃止し、Tree[SearchNode]を直接構築する設計に移行
 */
trait ProverInterface {
  def search(
      exprs: Vector[Expr], // 探索履歴（スタック）
      context: List[(String, Expr)],
      state: LogicState,
      subst: Unifier.Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean
  ): Tree[SearchNode]
  
  def freshMeta(depth: Int): Expr
  def config: ProverConfig
  def deadline: Long

  def applyRules(
      e: Expr,
      subst: Unifier.Subst,
      depth: Int,
      isGoal: Boolean
  ): List[(Expr, List[Expr], String, Unifier.Subst)]
  
  def addDynamicRule(rule: CatRule): Unit
  def normalize(e: Expr): Expr
  def checkDeadline(): Unit
  def getAlgebras: List[InitialAlgebra] = Nil

  /** 前向き推論: LHSにマッチするルールを適用しRHSを導出 */
  def forwardApplyRules(
      e: Expr,
      subst: Unifier.Subst,
      depth: Int
  ): List[(Expr, String, Unifier.Subst)]
}

/**
 * 各論理体系のプラグイン
 */
trait LogicPlugin extends Plugin {
  
  def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      state: LogicState,
      subst: Unifier.Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = Vector.empty

  def getContextHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      state: LogicState,
      subst: Unifier.Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = Vector.empty

  // ユーティリティ: 成功ブランチの抽出 (並行分岐対応版)
  protected def allSuccesses(tree: Tree[SearchNode]): LazyList[SearchNode] = {
    tree match {
      case Tree.V(node, children) =>
        if (node.ruleName == "choice") {
          // OR-分岐ノードなので、その子供たちから成功を探す
          children.to(LazyList).flatMap(allSuccesses)
        } else if (node.isSuccess) {
          // ルール適用に成功したノード
          LazyList(node)
        } else {
          // 失敗ノードなどの場合は、その下層も一応探索する
          children.to(LazyList).flatMap(allSuccesses)
        }
      case _ => LazyList.empty
    }
  }
}
