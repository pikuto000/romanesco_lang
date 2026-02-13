// ==========================================
// LogicSymbols.scala
// 論理演算子の定義
// ==========================================

package romanesco.Solver.core

object LogicSymbols {
  val And = "∧"
  val Or = "∨"
  val Implies = "→"
  val Not = "¬"
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
  val Pair = "pair" // <f, g>
  val Proj1 = "pi1" // π1
  val Proj2 = "pi2" // π2
  val Case = "case" // [f, g]
  val Inl = "inl"
  val Inr = "inr"

  // 自然数 (Natural Numbers)
  val Zero = "0"
  val Succ = "S"

  // モーダル論理 (Modal Logic)
  val Box = "□"
  val Diamond = "◇"
  val Knowledge = "K"
  val Obligation = "O"

  // 線形論理 (Linear Logic)
  val LImplies = "⊸"
  val Bang = "!"
  val Question = "?"
  val Tensor = "⊗"
  val LPlus = "⊕"
  val LOne = "1"
  val LZero = "0"
  val LTop = "⊤"
  val LWith = "&"

  // 時相論理 (Temporal Logic)
  val Globally = "G"
  val Finally = "F"
  val Next = "X"
  val Until = "U"

  // 分離論理 (Separation Logic)
  val SepAnd = "*"
  val PointsTo = "↦"

  // HoTT (Homotopy Type Theory)
  val Path = "path"
  val Univ = "Univalence"
  val Type = "Type"
  val Refl = "refl"
  val Transport = "transport"
  val Concat = "concat" // パスの合成演算子
  val Cube = "cube" // 立方体モデル近似
  val Comp = "comp" // 幾何学的合成 (Kan composition)
  val Fill = "fill" // 幾何学的充填 (Kan filling)

  // Hoare Logic
  val Triple = "triple" // {P} C {Q}
  val Assign = ":="
  val Seq = ";"
  val If = "if"
  val While = "while"
  val Skip = "skip"

  // その他の表記揺れ
  val ImpliesAlt1 = "⊃"
  val ImpliesAlt2 = "⇒"
}
