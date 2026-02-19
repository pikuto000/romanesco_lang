package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

object CubicalBasicTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    println("=== Cubical Type Theory Basic Tests ===")

    var passed = 0
    var failed = 0

    def test(name: String, expr: Expr, expected: Expr): Unit = {
      val result = Rewriter.normalize(expr)
      if (result == expected) {
        println(s"  ✓ $name")
        passed += 1
      } else {
        println(s"  ✗ $name: expected $expected, got $result")
        failed += 1
      }
    }

    // 面制約の正規化テスト
    println("\n--- Face Constraint Normalization ---")
    test("∧ᶠ(I1, φ) → φ", sym(FaceAnd)(sym(I1), sym("φ")), sym("φ"))
    test("∧ᶠ(I0, φ) → I0", sym(FaceAnd)(sym(I0), sym("φ")), sym(I0))
    test("∨ᶠ(I0, φ) → φ", sym(FaceOr)(sym(I0), sym("φ")), sym("φ"))
    test("∨ᶠ(I1, φ) → I1", sym(FaceOr)(sym(I1), sym("φ")), sym(I1))
    test("¬ᶠ(I0) → I1", sym(FaceNeg)(sym(I0)), sym(I1))
    test("¬ᶠ(I1) → I0", sym(FaceNeg)(sym(I1)), sym(I0))
    test("¬ᶠ(¬ᶠ(φ)) → φ", sym(FaceNeg)(sym(FaceNeg)(sym("φ"))), sym("φ"))

    // hcomp簡約テスト
    println("\n--- HComp Reduction ---")
    test(
      "hcomp(A, I1, u, u0) → u(I1)",
      sym(HComp)(sym("A"), sym(I1), sym("u"), sym("u0")),
      sym("u")(sym(I1))
    )
    test(
      "hcomp(A, I0, u, u0) → u0",
      sym(HComp)(sym("A"), sym(I0), sym("u"), sym("u0")),
      sym("u0")
    )

    // transport 依存チェックテスト
    println("\n--- Transport Dependency Check ---")
    // P が道変数に依存しない場合: transport(λz. A, loop, b) → b
    test(
      "transport(λz. A, loop, b) → b (independent)",
      sym(Transport)(sym("λ")(v("z"), sym("A")), sym("loop"), sym("b")),
      sym("b")
    )
    // P が道変数に依存する場合: 簡約しない
    val dependentTransport =
      sym(Transport)(sym("λ")(v("z"), sym("B")(v("z"))), sym("loop"), sym("b"))
    val dependentResult = Rewriter.normalize(dependentTransport)
    if (dependentResult == dependentTransport) {
      println(s"  ✓ transport(λz. B(z), loop, b) unchanged (dependent)")
      passed += 1
    } else {
      println(
        s"  ✗ transport(λz. B(z), loop, b) should not reduce, got $dependentResult"
      )
      failed += 1
    }

    // isSet テスト（証明探索）- TestParser使用
    println("\n--- isSet Proof Search ---")
    val hottRules = StandardRules.hott ++ List(StandardRules.pathToEquiv)
    val rules = hottRules ++ StandardRules.all
    val config =
      ProverConfig(rules = rules, maxInduction = 3, maxComplexity = 300)
    val prover = new Prover(config)

    // isSet(A) → ∀x:A. ∀y:A. ∀p:path(A, x, y). ∀q:path(A, x, y). path(path(A, x, y), p, q)
    val isSetGoal = romanesco.Solver.TestParser.parse(
      "isSet(A) → ∀x:A. ∀y:A. ∀p:path(A, x, y). ∀q:path(A, x, y). path(path(A, x, y), p, q)"
    )

    print("  isSet(A) → path equality ... ")
    // logger.switch(true)
    val isSetResult = prover.prove(isSetGoal, maxDepth = 15, timeoutMs = 15000)
    // logger.switch(false)
    isSetResult match {
      case Right(_) =>
        println("✓ OK (Solved)")
        passed += 1
      case Left(trace) =>
        println(s"✗ FAIL: ${trace.reason}")
        failed += 1
    }

    println(s"\n=== Results: $passed passed, $failed failed ===")
    if (failed > 0) sys.exit(1)
  }
}
