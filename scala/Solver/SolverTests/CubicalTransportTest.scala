package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

object CubicalTransportTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    println("=== Cubical Type Theory: Transport & HComp Computation Tests ===")

    val hottRules = StandardRules.hott
    val config = ProverConfig(rules = hottRules ++ StandardRules.all, maxComplexity = 300)
    
    var passed = 0
    var failed = 0

    def test(name: String, expr: Expr, expectedHead: String): Unit = {
      val result = Rewriter.normalize(expr, config.rules)
      if (result.headSymbol == expectedHead) {
        println(s"  ✓ $name (computed to $expectedHead)")
        passed += 1
      } else {
        println(s"  ✗ $name: expected head $expectedHead, got $result")
        failed += 1
      }
    }

    // 1. Function Type Transport
    println("\n--- Function Type Transport ---")
    val functionTransport = sym(Transport)(
      sym("λ")(v("z"), sym("→")(sym("A"), sym("B")(v("z")))),
      sym("p"),
      sym("f")
    )
    test("transport through function type", functionTransport, "λ")

    // 2. Pair Type HComp
    println("\n--- Product Type HComp ---")
    val productHComp = sym(HComp)(
      sym("×")(sym("A"), sym("B")),
      sym("φ"),
      sym("u"),
      sym("pair")(sym("a"), sym("b"))
    )
    test("hcomp through product type", productHComp, "pair")

    // 3. Path Type HComp (Dimension Raising)
    println("\n--- Path Type HComp (Dimension Raising) ---")
    val pathHComp = sym(HComp)(
      sym(Path)(sym("A"), sym("x"), sym("y")),
      sym("φ"),
      sym("u"),
      sym("p")
    )
    test("hcomp through path type (dimension raising)", pathHComp, "λ")

    println(s"\n=== Results: $passed passed, $failed failed ===")
    if (failed > 0) sys.exit(1)
  }
}
