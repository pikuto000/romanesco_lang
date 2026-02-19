package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

object CubicalAdvancedTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    println("=== Cubical Type Theory Advanced Tests ===")

    val hottRules = StandardRules.hott
    val config = ProverConfig(rules = hottRules ++ StandardRules.all, maxComplexity = 300)
    val prover = new Prover(config)

    var passed = 0
    var failed = 0

    def assertNormalize(name: String, expr: Expr, expected: Expr): Unit = {
      val result = Rewriter.normalize(expr, config.rules)
      if (result == expected) {
        println(s"  ✓ $name")
        passed += 1
      } else {
        println(s"  ✗ $name: expected $expected, got $result")
        failed += 1
      }
    }

    // 1. De Morgan Algebra & AC Normalization
    println("\n--- De Morgan & AC Normalization ---")
    testAC()

    assertNormalize("face-dm-and", 
      sym(FaceNeg)(sym(FaceAnd)(v("i"), v("j"))), 
      sym(FaceOr)(sym(FaceNeg)(v("i")), sym(FaceNeg)(v("j"))))
    
    assertNormalize("face-dm-or", 
      sym(FaceNeg)(sym(FaceOr)(v("i"), v("j"))), 
      sym(FaceAnd)(sym(FaceNeg)(v("i")), sym(FaceNeg)(v("j"))))

    // 2. Path Endpoint Calculation (Dynamic Rules)
    println("\n--- Path Endpoint Calculation ---")
    // p : path(A, a, b) |- p(I0) = a
    val pathGoal = romanesco.Solver.TestParser.parse("∀p:path(A, a, b). p(I0) = a")
    print("  p : path(A, a, b) |- p(I0) = a ... ")
    prover.prove(pathGoal, maxDepth = 10) match {
      case Right(_) => println("✓ OK"); passed += 1
      case Left(t) => println(s"✗ FAIL: ${t.reason}"); failed += 1
    }

    print("  p : path(A, a, b) |- p(I1) = b ... ")
    val pathGoalEnd = romanesco.Solver.TestParser.parse("∀p:path(A, a, b). p(I1) = b")
    prover.prove(pathGoalEnd, maxDepth = 10) match {
      case Right(_) => println("✓ OK"); passed += 1
      case Left(t) => println(s"✗ FAIL: ${t.reason}"); failed += 1
    }

    println(s"\n=== Results: $passed passed, $failed failed ===")
    if (failed > 0) sys.exit(1)
  }

  def testAC(): Unit = {
    val e1 = Rewriter.normalize(sym(FaceAnd)(v("j"), v("i")), StandardRules.all)
    val e2 = Rewriter.normalize(sym(FaceAnd)(v("i"), v("j")), StandardRules.all)
    if (e1 == e2) {
      println("  ✓ AC Normalization (i ∧ j = j ∧ i)")
    } else {
      println(s"  ✗ AC Normalization failed: $e1 vs $e2")
    }
  }
}
