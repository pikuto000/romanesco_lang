package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger
import romanesco.Utils.Debug.logger.Level

object ArithmeticTests {
  def run(): Unit = {
    println("=== Extended Arithmetic Verification ===")

    val extendedArithRules = List(
      CatRule(
        "plus_comm",
        sym("plus")(v("n"), v("m")),
        sym("plus")(v("m"), v("n")),
        domain = "arithmetic"
      ),
      CatRule(
        "plus_assoc",
        sym("plus")(sym("plus")(v("n"), v("m")), v("k")),
        sym("plus")(v("n"), sym("plus")(v("m"), v("k"))),
        domain = "arithmetic"
      ),
      CatRule(
        "times_0",
        sym("times")(sym("0"), v("n")),
        sym("0"),
        domain = "arithmetic"
      ),
      CatRule(
        "times_S",
        sym("times")(sym("S")(v("n")), v("m")),
        sym("plus")(v("m"), sym("times")(v("n"), v("m"))),
        domain = "arithmetic"
      )
    )

    val prover = new Prover(
      ProverConfig(
        rules = extendedArithRules ++ StandardRules.all,
        algebras = StandardRules.defaultAlgebras
      )
    )

    val cases = List(
      ("plus(S(0), S(0)) = S(S(0))", true),
      ("plus(n, plus(m, k)) = plus(m, plus(n, k))", true)
    )

    cases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      val goal = TestParser.parse(input)
      prover.prove(goal, maxDepth = 15) match {
        case Right(result) =>
          println("✓ OK")
          println(result.tree.format())
        case Left(trace) =>
          if (expected) {
            println("✗ FAIL"); println(s"reason: ${trace.reason}")
          } else println("✓ OK (Failed as expected)")
      }
    }
  }
}

object HoareTests {
  def run(): Unit = {
    println("\n=== Hoare Logic Program Verification ===")
    val prover = new Prover(ProverConfig(rules = StandardRules.all))

    val cases = List(
      ("{x = 1} skip {x = 1}", true),
      ("{⊤} x := 1 {x = 1}", true),
      ("{⊤} x := 1 ; y := 2 {x = 1 ∧ y = 2}", true)
    )

    cases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      val goal = TestParser.parse(input)
      prover.prove(goal, maxDepth = 10) match {
        case Right(result) =>
          println("✓ OK")
          println(result.tree.format())
        case Left(trace) =>
          if (expected) {
            println("✗ FAIL"); println(s"reason: ${trace.reason}")
          } else println("✓ OK (Failed as expected)")
      }
    }
  }
}

@main def runMathAndVerificationTests(): Unit = {
  ArithmeticTests.run()
  HoareTests.run()
}
