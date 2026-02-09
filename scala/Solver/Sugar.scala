// ==========================================
// Sugar.scala
// 便利レイヤー（エイリアス・DSL）
// ==========================================

package romanesco.Solver.sugar

import romanesco.Solver.core._

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

object ProofBuilder:
  import StandardRules._

  // 恒等証明
  def identity: Proof = Nil

  // 規則適用
  def apply(rule: CatRule, subst: Map[MetaId, Expr] = Map.empty): ProofStep =
    ProofStep.Apply(rule, subst)

  // 随伴関連
  object Adjoint:
    def unit(name: String, subst: Map[MetaId, Expr] = Map.empty): ProofStep =
      val rule = name match
        case "∀" => forallUnit
        case "∃" => existsUnit
        case _   => throw new Exception(s"Unknown adjoint: $name")
      ProofStep.Apply(rule, subst)

    def counit(name: String, subst: Map[MetaId, Expr] = Map.empty): ProofStep =
      val rule = name match
        case "∀" => forallCounit
        case "∃" => existsCounit
        case _   => throw new Exception(s"Unknown adjoint: $name")
      ProofStep.Apply(rule, subst)

  // よく使うパターン
  object Common:
    def intro(name: String, subst: Map[MetaId, Expr] = Map.empty): ProofStep =
      name match
        case "∧" | "×" => apply(productUniversal, subst)
        case "→" | "⇒" => apply(expUniversal, subst)
        case "∨" | "+" => throw new Exception("Or intro needs left/right")
        case "∃"       => Adjoint.unit("∃", subst)
        case "⊤" | "1" => apply(terminalUniversal, subst)
        case _         => throw new Exception(s"Unknown intro: $name")

    def elim(name: String, subst: Map[MetaId, Expr] = Map.empty): ProofStep =
      name match
        case "∧" | "×" => apply(fstBeta, subst) // または sndBeta
        case "→"       => apply(lambdaBeta, subst)
        case "∨" | "+" => apply(caseInlBeta, subst) // または caseInrBeta
        case "∀"       => Adjoint.counit("∀", subst)
        case "⊥" | "0" => apply(initialUniversal, subst)
        case _         => throw new Exception(s"Unknown elim: $name")

    def refl(term: Expr): ProofStep =
      apply(eqRefl, Map.empty)

    def symm(subst: Map[MetaId, Expr] = Map.empty): ProofStep =
      apply(eqSymm, subst)

    def trans(subst: Map[MetaId, Expr] = Map.empty): ProofStep =
      apply(eqTrans, subst)

// 可読な証明記述DSL
object ProofDSL:
  import ExprBuilder._
  import ProofBuilder._

  class ProofBuilderDSL:
    private var steps = List.empty[ProofStep]

    def intro(name: String)(using
        subst: Map[MetaId, Expr] = Map.empty
    ): this.type =
      steps = steps :+ Common.intro(name, subst)
      this

    def elim(name: String)(using subst: Map[MetaId, Expr] = Map.empty): this.type =
      steps = steps :+ Common.elim(name, subst)
      this

    def rewrite(rule: CatRule)(using
        subst: Map[MetaId, Expr] = Map.empty
    ): this.type =
      steps = steps :+ ProofBuilder.apply(rule, subst)
      this

    def η(name: String)(using subst: Map[MetaId, Expr] = Map.empty): this.type =
      steps = steps :+ Adjoint.unit(name, subst)
      this

    def ε(name: String)(using subst: Map[MetaId, Expr] = Map.empty): this.type =
      steps = steps :+ Adjoint.counit(name, subst)
      this

    def build: Proof = steps

  def proof(body: ProofBuilderDSL ?=> Unit): Proof =
    given pb: ProofBuilderDSL = new ProofBuilderDSL
    body
    pb.build