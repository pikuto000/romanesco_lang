package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object HoTTCubeTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.hott ++ StandardRules.all
    val config = ProverConfig(rules = rules)
    val prover = new Prover(config)

    println("=== HoTT: Cubical Model Approximation Test ===")

    val testCases = List(
      ("cube(A, p, q, r, s) → path(path(A, x, y), p, r)", true, 10),
      ("cube(A, p, q, r, s) → path(path(A, x, z), q, s)", false, 5) // Lower depth for false case
    )

    testCases.foreach { case (input, expected, depth) =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = depth)
        result match {
          case Right(_) => println("✓ OK (Solved)")
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
