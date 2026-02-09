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
  val Lambda = "λ"
  val Eq = "="
  val True = "⊤"
  val False = "⊥"
  
  // 圏論的な別名
  val Product = "×"
  val Coproduct = "+"
  val Exp = "^"
  val Terminal = "1"
  val Initial = "0"

  // 圏論的演算子
  val Compose = "∘"
  val Id = "id"
  val Pair = "pair"   // <f, g>
  val Proj1 = "pi1"   // π1
  val Proj2 = "pi2"   // π2
  val Case = "case"   // [f, g]
  val Inl = "inl"
  val Inr = "inr"
  
  // その他の表記揺れ
  val ImpliesAlt1 = "⊃"
  val ImpliesAlt2 = "⇒"
}
