// ==========================================
// CubicalPlugin.scala
// キュービカル型理論に基づく区間変数の操作と道の計算
// ==========================================

package romanesco.Solver.core

import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class CubicalPlugin extends LogicPlugin {
  override def name: String = "Cubical"
  override def priority: Int = 21

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
    // 1. 文脈内の道を検知し、端点の計算規則を動的に生成
    context.foreach {
      case (name, Expr.App(Expr.Sym(Path), List(_, x, y))) =>
        // path(A, x, y) 型の項 p に対して p(0) -> x, p(1) -> y を追加
        val p = Expr.Var(name)
        val rule0 = CatRule(s"path_start_$name", Expr.App(p, List(Expr.Sym(I0))), x)
        val rule1 = CatRule(s"path_end_$name", Expr.App(p, List(Expr.Sym(I1))), y)
        prover.asInstanceOf[Prover].addDynamicRule(rule0)
        prover.asInstanceOf[Prover].addDynamicRule(rule1)
      case _ => ()
    }

    // 2. 区間変数 i に対して i=0, i=1 のケース分割（必要に応じて実装）
    Vector.empty
  }

  override def getGoalHooks(
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
    val results = scala.collection.mutable.ArrayBuffer[Tree[SearchNode]]()

    // パス導入 (λi. p(i))
    // ゴールが path(A, x, y) のとき、i:Interval |- goal(i) を証明できれば成功
    goal match {
      case Expr.App(Expr.Sym(Path), List(a, x, y)) if depth < limit =>
        val iName = s"i_${depth}"
        val iVar = Expr.Var(iName)
        
        // p(0) = x かつ p(1) = y を満たす関数を構築
        // 簡易実装として、メタ変数による関数構築
        // TODO: 本格的なパス抽象化の実装
      case _ => ()
    }

    results.toVector
  }
}
