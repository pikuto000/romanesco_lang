package romanesco.Solver.core

import romanesco.Types.Tree

/**
 * 証明器のコア・インターフェース
 * SolveTreeを廃止し、Tree[SearchNode]を直接構築する設計に移行
 */
trait ProverInterface {
  def search(
      exprs: Vector[Expr], // 探索履歴（スタック）
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
      subst: Unifier.Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
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
}

/**
 * 各論理体系のプラグイン
 */
trait LogicPlugin {
  def name: String
  
  def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
      subst: Unifier.Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = Vector.empty

  def getContextHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      linearContext: List[(String, Expr)],
      subst: Unifier.Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = Vector.empty
}
