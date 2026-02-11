package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object HoTTTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val hottRules = StandardRules.hott ++ List(StandardRules.pathToEquiv)
    val rules = hottRules ++ StandardRules.all
    // Increase maxInduction to 3 for Path Associativity
    val config =
      ProverConfig(rules = rules, maxInduction = 3, maxComplexity = 100)
    val prover = new Prover(config)

    println("=== HoTT: Path Induction and HIT Test ===")

    val testCases = List(
      ("∀x. ∀y. ∀p:path(A,x,y). concat(p, refl(y)) = p", true),
      ("∀x. ∀y. ∀p:path(A,x,y). concat(refl(x), p) = p", true),
      (
        "∀x. ∀y. ∀z. ∀w. ∀p:path(A,x,y). ∀q:path(A,y,z). ∀r:path(A,z,w). concat(concat(p, q), r) = concat(p, concat(q, r))",
        true
      ),
      ("∀x. ∀y. ∀p:path(A,x,y). inv(inv(p)) = p", true),
      // HIT: Circle S1 induction
      ("∀x:S1. P(base) → P(x)", true),
      // Univalence (Direct application of pathToEquiv rule)
      ("equiv(A, B) → path(Type, A, B)", true)
    )

    testCases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = 20)
        result match {
          case Right(_)    => println("✓ OK (Solved)")
          case Left(trace) =>
            if (expected) {
              println("✗ FAIL (Should have been solved)")
              // println(trace.format())
            } else println("✓ OK (Failed as expected)")
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
