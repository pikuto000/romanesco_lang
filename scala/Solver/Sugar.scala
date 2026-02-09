// ==========================================
// Sugar.scala
// 便利レイヤー（エイリアス・DSL）
// ==========================================

package romanesco.Solver.sugar

import romanesco.Solver.core._
import romanesco.Solver.core.LogicSymbols._

object ExprBuilder:
  import Expr._

  // 基本構築
  def sym(name: String): Expr = Expr.Sym(name)
  def v(name: String): Expr = Expr.Var(name)
  def meta(id: Int): Expr = Expr.Meta(MetaId(id))
  def meta(ids: Int*): Expr = Expr.Meta(MetaId(ids.toList))

  // よく使う記号
  val ⊤ = sym("⊤")
  val ⊥ = sym("⊥")
  val `1` = sym("1")
  val `0` = sym("0")

  // ラムダ抽象 sugar (Appベース)
  def lam(v: String, body: Expr): Expr = Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body))

  // 中置演算子
  extension (e: Expr)
    // 論理演算子
    def ===(other: Expr): Expr = sym("=")(e, other)
    def →(other: Expr): Expr = sym("→")(e, other)
    def ∧(other: Expr): Expr = sym("∧")(e, other)
    def ∨(other: Expr): Expr = sym("∨")(e, other)
    def ⊃(other: Expr): Expr = e → other // 含意の別名

    // 圏論的演算子
    def ×(other: Expr): Expr = sym("×")(e, other)
    def +(other: Expr): Expr = sym("+")(e, other)
    def ^(other: Expr): Expr = sym("^")(e, other)
    def ∘(other: Expr): Expr = sym("∘")(e, other)

// NOTE: ProofBuilder and ProofDSL are currently broken due to ProofTree migration.
// They need to be redesigned to build trees instead of lists.