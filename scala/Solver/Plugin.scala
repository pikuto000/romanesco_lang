// ==========================================
// Plugin.scala
// プラグイン基底トレイト
// ==========================================

package romanesco.Solver.core

/**
 * 全てのプラグインの基底トレイト。
 * 論理プラグイン (LogicPlugin) も代数プラグイン (AlgebraPlugin) もこれを継承する。
 */
trait Plugin {
  def name: String
  def priority: Int = 100
  def dependencies: List[String] = Nil
  def providedRules: List[CatRule] = Nil
  def providedAlgebras: List[InitialAlgebra] = Nil
  def normalizeHook(expr: Expr): Option[Expr] = None
  def isEnabled(config: ProverConfig): Boolean = true
}

/**
 * 正規化コンテキスト（将来の拡張用）
 */
case class NormalizationContext(rules: List[CatRule], maxIter: Int, currentIter: Int)
