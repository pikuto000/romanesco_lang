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
      eq(sym("∘")(sym("pi1"), v("h")), v("f")),
      eq(sym("∘")(sym("pi2"), v("h")), v("g"))
    )
  )

  val fstBeta = CatRule(
    "fst-β",
    sym("pi1")(sym("pair")(v("a"), v("b"))),
    v("a")
  )

  val sndBeta = CatRule(
    "snd-β",
    sym("pi2")(sym("pair")(v("a"), v("b"))),
    v("b")
  )

  val productEta = CatRule(
    "product-η",
    sym("pair")(sym("pi1")(v("p")), sym("pi2")(v("p"))),
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

  // カテゴリー別
  val products = List(productUniversal, fstBeta, sndBeta, productEta)
  val coproducts = List(coproductUniversal, caseInlBeta, caseInrBeta)
  val exponentials = List(expUniversal, lambdaEta)
  val colimits = List(initialUniversal)
  val equality = List(eqRefl) // eqSymm is removed from automatic rules

  val tensorIsProd = CatRule(
    "tensor-is-×",
    sym("⊗")(v("A"), v("B")),
    sym("×")(v("A"), v("B"))
  )

  val lImpliesIsExp = CatRule(
    "⊸-is-^",
    sym("⊸")(v("A"), v("B")),
    sym("^")(v("B"), v("A"))
  )

  val logicMapping =
    List(andIsProd, orIsCoprod, arrowIsExp, trueIsTerminal, falseIsInitial)

  val linearMapping = List(tensorIsProd, lImpliesIsExp)

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
  val linearBang = CatRule(
    "linear-bang-elim",
    sym(Bang)(v("A")),
    v("A")
  )

  val linear = List(linearBang)

  // --- 時相論理 ---
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

  val temporal = List(temporalF, temporalU)

  // --- 分離論理 ---
  val sepAndCommutative = CatRule(
    "sep-and-comm",
    sym(SepAnd)(v("A"), v("B")),
    sym(SepAnd)(v("B"), v("A"))
  )

  val separation = List(sepAndCommutative)

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
    sym(Path)(sym(Type), v("A"), v("B"))
  )

  val pathToEquiv = CatRule(
    "path-to-equiv",
    sym(Path)(sym(Type), v("A"), v("B")),
    sym("equiv")(v("A"), v("B"))
  )

  val pathConcatRule = CatRule(
    "path-concat",
    sym(Concat)(sym(Path)(v("A"), v("a"), v("b")), sym(Path)(v("A"), v("b"), v("c"))),
    sym(Path)(v("A"), v("a"), v("c"))
  )

  val concatReflLeft = CatRule(
    "concat-refl-left",
    sym(Concat)(sym(Refl)(v("x")), v("p")),
    v("p")
  )

  val concatReflRight = CatRule(
    "concat-refl-right",
    sym(Concat)(v("p"), sym(Refl)(v("x"))),
    v("p")
  )

  val concatPathReflLeft = CatRule(
    "concat-path-refl-left",
    sym(Concat)(sym(Path)(v("A"), v("x"), v("x")), v("p")),
    v("p")
  )

  val concatPathReflRight = CatRule(
    "concat-path-refl-right",
    sym(Concat)(v("p"), sym(Path)(v("A"), v("x"), v("x"))),
    v("p")
  )

  val transportRefl = CatRule(
    "transport-refl",
    sym(Transport)(v("P"), sym(Refl)(v("x")), v("v")),
    v("v")
  )

  val transportPathRefl = CatRule(
    "transport-path-refl",
    sym(Transport)(v("P"), sym(Path)(v("A"), v("x"), v("x")), v("v")),
    v("v")
  )

  val pathInvRefl = CatRule(
    "path-inv-refl",
    sym("inv")(sym(Refl)(v("x"))),
    sym(Refl)(v("x"))
  )

  val transportRule = CatRule(
    "transport",
    sym(Transport)(v("P"), sym(Path)(sym(Type), v("A"), v("B")), v("x")),
    v("P")(v("B"))
  )

  val cubeRule = CatRule(
    "cube-reduction",
    sym(Cube)(v("A"), v("p"), v("q"), v("r"), v("s")),
    sym(Path)(sym(Path)(v("A"), v("x"), v("y")), v("p"), v("r")), 
    List(
      eq(v("p"), sym(Path)(v("A"), v("x"), v("y"))),
      eq(v("q"), sym(Path)(v("A"), v("x"), v("z"))),
      eq(v("r"), sym(Path)(v("A"), v("z"), v("w"))),
      eq(v("s"), sym(Path)(v("A"), v("y"), v("w")))
    )
  )

  val hott = List(pathRefl, pathInv, univalence, pathConcatRule, concatReflLeft, concatReflRight, concatPathReflLeft, concatPathReflRight, transportRefl, transportPathRefl, pathInvRefl, transportRule, cubeRule)

  // 全ての標準規則を populate
  val all: List[CatRule] = products ++ coproducts ++ exponentials ++ colimits ++ equality ++ logicMapping ++ modal ++ linear ++ separation ++ hott

  // --- 標準の初期代数 ---
  import LogicSymbols._
  val defaultAlgebras = List(
    InitialAlgebra("Nat", List(ConstructorDef(Zero, Nil), ConstructorDef(Succ, List(ArgType.Recursive))), "n"),
    InitialAlgebra("List", List(ConstructorDef("nil", Nil), ConstructorDef("cons", List(ArgType.Constant, ArgType.Recursive))), "xs"),
    InitialAlgebra("Tree", List(ConstructorDef("leaf", Nil), ConstructorDef("node", List(ArgType.Recursive, ArgType.Constant, ArgType.Recursive))), "t"),
    InitialAlgebra("S1", List(
      ConstructorDef("base", Nil),
      ConstructorDef("loop", Nil, ConstructorType.Path(sym("base"), sym("base")))
    ), "x")
  )
