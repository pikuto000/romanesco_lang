// ==========================================
// LogicSymbols.scala
// 論理演算子の定義
// ==========================================

package romanesco.Solver.core

object LogicSymbols {
  val And = "∧"
  val Or = "∨"
  val Implies = "→"
  val Forall = "∀"
  val Exists = "∃"
  val Eq = "="
  val True = "⊤"
  val False = "⊥"
  
  // 圏論的な別名
  val Product = "×"
  val Coproduct = "+"
  val Exp = "^"
  val Terminal = "1"
  val Initial = "0"
  
  // その他の表記揺れ
  val ImpliesAlt1 = "⊃"
  val ImpliesAlt2 = "⇒"
}
