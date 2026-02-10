// ==========================================
// StandardRules.scala (修正版)
// ==========================================

package romanesco.Solver.core

object StandardRules:
  import Expr._

  // 等式用のヘルパー
  private def eq(l: Expr, r: Expr): Expr = sym("=")(l, r)

  // --- 恒等射・合成 ---
  val idLeft = CatRule(
    "id-left",
    sym("∘")(sym("id"), v("f")),
    v("f")
  )

  val idRight = CatRule(
    "id-right",
    sym("∘")(v("f"), sym("id")),
    v("f")
  )

  val compAssoc = CatRule(
    "comp-assoc",
    sym("∘")(sym("∘")(v("f"), v("g")), v("h")),
    sym("∘")(v("f"), sym("∘")(v("g"), v("h")))
  )

  // --- 積 (Product / And) ---
  val productUniversal = CatRule(
    "product-universal",
    sym("pair")(v("f"), v("g")),
    v("h"),
    List(
      eq(sym("∘")(sym("π₁"), v("h")), v("f")),
      eq(sym("∘")(sym("π₂"), v("h")), v("g"))
    )
  )

  val fstBeta = CatRule(
    "fst-β",
    sym("π₁")(sym("pair")(v("a"), v("b"))),
    v("a")
  )

  val sndBeta = CatRule(
    "snd-β",
    sym("π₂")(sym("pair")(v("a"), v("b"))),
    v("b")
  )

  val productEta = CatRule(
    "product-η",
    sym("pair")(sym("π₁")(v("p")), sym("π₂")(v("p"))),
    v("p")
  )

  // --- 余積 (Coproduct / Or) ---
  val coproductUniversal = CatRule(
    "coproduct-universal",
    sym("case")(v("s"), v("f"), v("g")),
    v("h"),
    List(
      eq(sym("∘")(v("h"), sym("inl")), v("f")),
      eq(sym("∘")(v("h"), sym("inr")), v("g"))
    )
  )

  val caseInlBeta = CatRule(
    "case-inl-β",
    sym("case")(sym("inl")(v("x")), v("f"), v("g")),
    v("f")(v("x"))
  )

  val caseInrBeta = CatRule(
    "case-inr-β",
    sym("case")(sym("inr")(v("y")), v("f"), v("g")),
    v("g")(v("y"))
  )

  // --- 指数対象 (Exponential / Arrow) ---
  val expUniversal = CatRule(
    "exp-universal",
    sym("curry")(v("f")),
    v("g"),
    List(
      eq(
        sym("∘")(sym("eval"), sym("×")(v("g"), sym("id"))),
        v("f")
      )
    )
  )

  val lambdaBeta = CatRule(
    "lambda-β",
    sym("eval")(sym("pair")(sym("λ")(v("x"), v("body")), v("arg"))),
    sym("subst")(v("body"), v("x"), v("arg"))
  )

  val lambdaEta = CatRule(
    "lambda-η",
    sym("λ")(v("x"), v("f")(v("x"))),
    v("f")
  )

  // --- 終対象 (Terminal / True) ---
  val terminalUniversal = CatRule(
    "terminal-universal",
    sym("!")(v("A")),
    v("f"),
    List( /* ∃! f: A → 1 */ )
  )

  val terminalUnique = CatRule(
    "terminal-unique",
    v("f"),
    sym("!")(v("A")),
    List( /* if target is 1 */ )
  )

  // --- 始対象 (Initial / False) ---
  val initialUniversal = CatRule(
    "initial-universal",
    sym("absurd")(v("f")),
    v("g"),
    List( /* ∃! g: 0 → A */ )
  )

  // --- 等式 ---
  val eqRefl = CatRule(
    "eq-refl",
    sym("refl")(v("a")),
    eq(v("a"), v("a"))
  )

  val eqSymm = CatRule(
    "eq-symm",
    eq(v("a"), v("b")),
    eq(v("b"), v("a"))
  )

  val eqTrans = CatRule(
    "eq-trans",
    sym("∘")(eq(v("a"), v("b")), eq(v("b"), v("c"))),
    eq(v("a"), v("c"))
  )

  val eqSubst = CatRule(
    "eq-subst",
    sym("rewrite")(eq(v("a"), v("b")), v("P")(v("a"))),
    v("P")(v("b"))
  )

  // --- 随伴: ∃ ⊣ subst ⊣ ∀ ---
  val existsUnit = CatRule(
    "∃-η",
    v("P"),
    sym("∃")(v("x"), sym("subst")(v("P"), v("y"), v("x")))
  )

  val existsCounit = CatRule(
    "∃-ε",
    sym("∃")(v("x"), v("P")),
    sym("subst")(v("P"), v("x"), v("witness"))
  )

  val forallUnit = CatRule(
    "∀-η",
    sym("subst")(sym("∀")(v("x"), v("P")), v("x"), v("t")),
    v("P")
  )

  val forallCounit = CatRule(
    "∀-ε",
    sym("∀")(v("x"), sym("subst")(v("P"), v("y"), v("x"))),
    v("P")
  )

  // --- 論理結合子と圏論的構造の対応 ---
  val andIsProd = CatRule(
    "and-is-×",
    sym("∧")(v("A"), v("B")),
    sym("×")(v("A"), v("B"))
  )

  val orIsCoprod = CatRule(
    "or-is-+",
    sym("∨")(v("A"), v("B")),
    sym("+")(v("A"), v("B"))
  )

  val arrowIsExp = CatRule(
    "→-is-^",
    sym("→")(v("A"), v("B")),
    sym("^")(v("B"), v("A"))
  )

  val trueIsTerminal = CatRule(
    "⊤-is-1",
    sym("⊤"),
    sym("1")
  )

  val falseIsInitial = CatRule(
    "⊥-is-0",
    sym("⊥"),
    sym("0")
  )

  // 全ての標準規則
  // 基本的な論理・圏論的構造は Prover.scala にハードコードされているため、
  // ここではデフォルトで適用する追加規則は空にする。
  val all: List[CatRule] = Nil

  // カテゴリー別
  val products = List(productUniversal, fstBeta, sndBeta, productEta)
  val coproducts = List(coproductUniversal, caseInlBeta, caseInrBeta)
  val exponentials = List(expUniversal, lambdaBeta, lambdaEta)
  val limits = List(terminalUniversal, terminalUnique)
  val colimits = List(initialUniversal)
  val equality = List(eqRefl, eqSymm, eqTrans, eqSubst)
  val adjoints = List(existsUnit, existsCounit, forallUnit, forallCounit)
  val logicMapping =
    List(andIsProd, orIsCoprod, arrowIsExp, trueIsTerminal, falseIsInitial)

  // --- モーダル論理 ---
  import LogicSymbols._
  val modalK = CatRule(
    "modal-K",
    sym(Box)(sym("→")(v("A"), v("B"))),
    sym("→")(sym(Box)(v("A")), sym(Box)(v("B")))
  )

  val modalT = CatRule(
    "modal-T",
    sym(Box)(v("A")),
    v("A")
  )

  val modal4 = CatRule(
    "modal-4",
    sym(Box)(v("A")),
    sym(Box)(sym(Box)(v("A")))
  )

  val modal5 = CatRule(
    "modal-5",
    sym(Diamond)(v("A")),
    sym(Box)(sym(Diamond)(v("A")))
  )

  val modalDuality = CatRule(
    "modal-duality",
    sym(Diamond)(v("A")),
    sym("→")(sym(Box)(sym("→")(v("A"), sym("⊥"))), sym("⊥"))
  )

  val modal = List(modalK, modalT, modal4, modal5, modalDuality)

  // --- 線形論理 ---
  val linearImplies = CatRule(
    "linear-implies-beta",
    sym(LImplies)(v("A"), v("B")),
    sym("→")(v("A"), v("B")) // 簡易的なマッピング
  )

  val tensorUniversal = CatRule(
    "tensor-universal",
    sym(Tensor)(v("A"), v("B")),
    sym(Product)(v("A"), v("B")) // Tensor ⊗ を Product × にマッピング
  )

  val linearBang = CatRule(
    "linear-bang-elim",
    sym(Bang)(v("A")),
    v("A")
  )

  val linear = List(linearImplies, tensorUniversal, linearBang)

  // --- 時相論理 ---
  val temporalG = CatRule(
    "G-expansion",
    sym(Globally)(v("A")),
    sym(And)(v("A"), sym(Next)(sym(Globally)(v("A"))))
  )

  val temporalF = CatRule(
    "F-expansion",
    sym(Finally)(v("A")),
    sym(Or)(v("A"), sym(Next)(sym(Finally)(v("A"))))
  )

  val temporalU = CatRule(
    "U-expansion",
    sym(Until)(v("A"), v("B")),
    sym(Or)(v("B"), sym(And)(v("A"), sym(Next)(sym(Until)(v("A"), v("B")))))
  )

  val temporal = List(temporalG, temporalF, temporalU)

  // --- 分離論理 ---
  val sepAndCommutative = CatRule(
    "sep-and-comm",
    sym(SepAnd)(v("A"), v("B")),
    sym(SepAnd)(v("B"), v("A"))
  )

  val sepAndToAnd = CatRule(
    "sep-and-to-and",
    sym(SepAnd)(v("A"), v("B")),
    sym(And)(v("A"), v("B")) // 簡易的なマッピング
  )

  val separation = List(sepAndCommutative, sepAndToAnd)

  // --- 古典論理 ---
  val em = CatRule(
    "EM",
    sym("⊤"),
    sym("∨")(v("A"), sym("→")(v("A"), sym("⊥")))
  )

  val dne = CatRule(
    "DNE",
    sym("→")(sym("→")(v("A"), sym("⊥")), sym("⊥")),
    v("A")
  )

  val classical = List(em, dne)

  // --- HoTT (Homotopy Type Theory) ---
  val pathRefl = CatRule(
    "path-refl",
    sym(Refl)(v("a")),
    sym(Path)(v("A"), v("a"), v("a"))
  )

  val pathInv = CatRule(
    "path-inv",
    sym("inv")(sym(Path)(v("A"), v("a"), v("b"))),
    sym(Path)(v("A"), v("b"), v("a"))
  )

  val univalence = CatRule(
    "univalence",
    sym("equiv")(v("A"), v("B")),
    sym(Path)(sym(Type)(v("L")), v("A"), v("B"))
  )

  val hott = List(pathRefl, pathInv, univalence)

  // --- 標準の初期代数 ---
  import LogicSymbols._
  val defaultAlgebras = List(
    InitialAlgebra("Nat", List(ConstructorDef(Zero, Nil), ConstructorDef(Succ, List(ArgType.Recursive))), "n"),
    InitialAlgebra("List", List(ConstructorDef("nil", Nil), ConstructorDef("cons", List(ArgType.Constant, ArgType.Recursive))), "xs"),
    InitialAlgebra("Tree", List(ConstructorDef("leaf", Nil), ConstructorDef("node", List(ArgType.Recursive, ArgType.Constant, ArgType.Recursive))), "t")
  )
