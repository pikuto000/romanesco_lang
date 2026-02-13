// ==========================================
// StandardRules.scala
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.LogicSymbols._

object StandardRules:
  import Expr._

  private def eq(l: Expr, r: Expr): Expr = sym("=")(l, r)

  val idLeft = CatRule("id-left", sym("∘")(sym("id"), v("f")), v("f"))
  val idRight = CatRule("id-right", sym("∘")(v("f"), sym("id")), v("f"))
  val compAssoc = CatRule("comp-assoc", sym("∘")(sym("∘")(v("f"), v("g")), v("h")), sym("∘")(v("f"), sym("∘")(v("g"), v("h"))))

  val fstBeta = CatRule("fst-β", sym("pi1")(sym("pair")(v("a"), v("b"))), v("a"))
  val sndBeta = CatRule("snd-β", sym("pi2")(sym("pair")(v("a"), v("b"))), v("b"))
  val productEta = CatRule("product-η", sym("pair")(sym("pi1")(v("p")), sym("pi2")(v("p"))), v("p"))

  val caseInlBeta = CatRule("case-inl-β", sym("case")(sym("inl")(v("x")), v("f"), v("g")), v("f")(v("x")))
  val caseInrBeta = CatRule("case-inr-β", sym("case")(sym("inr")(v("y")), v("f"), v("g")), v("g")(v("y")))

  val lambdaEta = CatRule("lambda-η", sym("λ")(v("x"), v("f")(v("x"))), v("f"))

  val eqRefl = CatRule("eq-refl", sym("refl")(v("a")), eq(v("a"), v("a")))

  val andIsProd = CatRule("and-is-×", sym(And)(v("A"), v("B")), sym(Product)(v("A"), v("B")))
  val orIsCoprod = CatRule("or-is-+", sym(Or)(v("A"), v("B")), sym(Coproduct)(v("A"), v("B")))
  val arrowIsExp = CatRule("→-is-^", sym(Implies)(v("A"), v("B")), sym(Exp)(v("B"), v("A")))
  val trueIsTerminal = CatRule("⊤-is-1", sym(True), sym(Terminal))
  val falseIsInitial = CatRule("⊥-is-0", sym(False), sym(Initial))

  val products = List(fstBeta, sndBeta, productEta)
  val coproducts = List(caseInlBeta, caseInrBeta)
  val exponentials = List(lambdaEta)
  val colimits = Nil
  val equality = List(eqRefl)

  val tensorIsProd = CatRule("tensor-is-×", sym(Tensor)(v("A"), v("B")), sym(Product)(v("A"), v("B")))
  val lImpliesIsExp = CatRule("⊸-is-^", sym(LImplies)(v("A"), v("B")), sym(Exp)(v("B"), v("A")))

  val logicMapping = List(andIsProd, orIsCoprod, arrowIsExp, trueIsTerminal, falseIsInitial)
  val linearMapping = List(tensorIsProd, lImpliesIsExp)

  val modalK = CatRule("modal-K", sym(Box)(sym(Implies)(v("A"), v("B"))), sym(Implies)(sym(Box)(v("A")), sym(Box)(v("B"))))
  val modalKLinear = CatRule("modal-K-linear", sym(Box)(sym(LImplies)(v("A"), v("B"))), sym(LImplies)(sym(Box)(v("A")), sym(Box)(v("B"))))
  val modalT = CatRule("modal-T", sym(Box)(v("A")), v("A"))
  val modal4 = CatRule("modal-4", sym(Box)(v("A")), sym(Box)(sym(Box)(v("A"))))
  val modal5 = CatRule("modal-5", sym(Diamond)(v("A")), sym(Box)(sym(Diamond)(v("A"))))
  val modalDuality = CatRule("modal-duality", sym(Diamond)(v("A")), sym(Implies)(sym(Box)(sym(Implies)(v("A"), sym(False))), sym(False)))
  val modalDistTensor = CatRule("modal-dist-tensor", sym(Box)(sym(Tensor)(v("A"), v("B"))), sym(Tensor)(sym(Box)(v("A")), sym(Box)(v("B"))))
  val modalDistForall = CatRule("modal-dist-forall", sym(Box)(sym(Forall)(v("x"), v("P")(v("x")))), sym(Forall)(v("x"), sym(Box)(v("P")(v("x")))))

  val modal = List(modalK, modalKLinear, modalT, modalDuality, modalDistTensor, modalDistForall, modal4, modal5)

  val linearBang = CatRule("linear-bang-elim", sym(Bang)(v("A")), v("A"))
  val linear = List(linearBang)

  val temporalGDistTensor = CatRule("G-dist-tensor", sym(Globally)(sym(Tensor)(v("A"), v("B"))), sym(Tensor)(sym(Globally)(v("A")), sym(Globally)(v("B"))))
  val temporalGDistSepAnd = CatRule("G-dist-sepand", sym(Globally)(sym(SepAnd)(v("A"), v("B"))), sym(SepAnd)(sym(Globally)(v("A")), sym(Globally)(v("B"))))
  val temporalGDistLImplies = CatRule("G-dist-limplies", sym(Globally)(sym(LImplies)(v("A"), v("B"))), sym(LImplies)(sym(Globally)(v("A")), sym(Globally)(v("B"))))
  val temporalF = CatRule("F-expansion", sym(Finally)(v("A")), sym(Or)(v("A"), sym(Next)(sym(Finally)(v("A")))))
  val temporalU = CatRule("U-expansion", sym(Until)(v("A"), v("B")), sym(Or)(v("B"), sym(And)(v("A"), sym(Next)(sym(Until)(v("A"), v("B"))))))
  val temporal = List(temporalGDistTensor, temporalGDistSepAnd, temporalGDistLImplies, temporalF, temporalU)

  val separation = List(CatRule("sep-and-comm", sym(SepAnd)(v("A"), v("B")), sym(SepAnd)(v("B"), v("A"))))

  val classical = List(
    CatRule("EM", sym(True), sym(Or)(v("A"), sym(Implies)(v("A"), sym(False)))),
    CatRule("DNE", sym(Implies)(sym(Implies)(v("A"), sym(False)), sym(False)), v("A"))
  )

  val pathRefl = CatRule("path-refl", sym(Refl)(v("a")), sym(Path)(v("A"), v("a"), v("a")))
  val pathInv = CatRule("path-inv", sym("inv")(sym(Path)(v("A"), v("a"), v("b"))), sym(Path)(v("A"), v("b"), v("a")))
  val univalence = CatRule("univalence", sym("equiv")(v("A"), v("B")), sym(Path)(sym(Type), v("A"), v("B")))
  val pathToEquiv = CatRule("path-to-equiv", sym(Path)(sym(Type), v("A"), v("B")), sym("equiv")(v("A"), v("B")))
  val pathConcatRule = CatRule("path-concat", sym(Concat)(sym(Path)(v("A"), v("a"), v("b")), sym(Path)(v("A"), v("b"), v("c"))), sym(Path)(v("A"), v("a"), v("c")))
  
  val hott = List(pathRefl, pathInv, univalence, pathConcatRule, pathToEquiv)

  val natPlusRules = List(
    CatRule("plus_0", sym("plus")(sym("0"), v("n")), v("n"), List(v("n"))),
    CatRule("plus_S", sym("plus")(sym("S")(v("n")), v("m")), sym("S")(sym("plus")(v("n"), v("m"))), List(v("n"), v("m")))
  )

  val listAppendRules = List(
    CatRule("append_nil", sym("append")(sym("nil"), v("ys")), v("ys"), List(v("ys"))),
    CatRule("append_cons", sym("append")(sym("cons")(v("x"), v("xs")), v("ys")), sym("cons")(v("x"), sym("append")(v("xs"), v("ys"))), List(v("x"), v("xs"), v("ys"))),
    CatRule("reverse_nil", sym("reverse")(sym("nil")), sym("nil")),
    CatRule("reverse_cons", sym("reverse")(sym("cons")(v("x"), v("xs"))), sym("append")(sym("reverse")(v("xs")), sym("cons")(v("x"), sym("nil"))), List(v("x"), v("xs")))
  )

  val all: List[CatRule] = products ++ coproducts ++ exponentials ++ colimits ++ equality ++ logicMapping ++ modal ++ linear ++ separation ++ hott ++ natPlusRules ++ listAppendRules

  val defaultAlgebras = List(
    InitialAlgebra("Nat", List(ConstructorDef(Zero, Nil), ConstructorDef(Succ, List(ArgType.Recursive))), "n"),
    InitialAlgebra("List", List(ConstructorDef("nil", Nil), ConstructorDef("cons", List(ArgType.Constant, ArgType.Recursive))), "xs"),
    InitialAlgebra("Tree", List(ConstructorDef("leaf", Nil), ConstructorDef("node", List(ArgType.Recursive, ArgType.Constant, ArgType.Recursive))), "t"),
    InitialAlgebra("S1", List(ConstructorDef("base", Nil), ConstructorDef("loop", Nil, ConstructorType.Path(sym("base"), sym("base")))), "s"),
    InitialAlgebra("Maybe", List(ConstructorDef("nothing", Nil), ConstructorDef("just", List(ArgType.Constant))), "m")
  )

  val natAlgebra = defaultAlgebras(0)
  val listAlgebra = defaultAlgebras(1)
  val treeAlgebra = defaultAlgebras(2)
  val s1Algebra = defaultAlgebras(3)
  val maybeAlgebra = defaultAlgebras(4)

  val nat = natAlgebra
  val list = listAlgebra
  val tree = treeAlgebra
  val s1 = s1Algebra
  val maybe = maybeAlgebra
