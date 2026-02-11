package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object SeparationLogicTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.separation ++ StandardRules.all
    val config = ProverConfig(rules = rules, maxComplexity = 200, maxParallelism = 1)
    val prover = new Prover(config)

    println("=== Separation Logic: Resource Management Test ===")

    val testCases = List(
      ("A * B ⊸ B * A", true, 10),
      ("(A * B) * C ⊸ A * (B * C)", true, 10),
      (
        "(A ⊸ B) ⊸ (A * C ⊸ B * C)", 
        true,
        15
      ),
      ("A * A ⊸ A", false, 5), // Fail fast
      ("A ⊸ A * A", false, 5), // Fail fast
      ("(∀v. (x ↦ v ⊸ P(v))) ⊸ (x ↦ 5 ⊸ P(5))", true, 8),
      ("A * B * C ⊸ C * A * B", true, 8),
      ("(∀v. (x ↦ v * R)) ⊸ (x ↦ 5 * R)", true, 8)
    )

    testCases.foreach { case (input, expected, depth) =>
      print(s"Case: $input (depth $depth) ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = depth)
        result match {
          case Right(_) =>
            if (expected) println("✓ OK (Solved)")
            else println(s"✗ FAIL (Should have failed)")
          case Left(trace) =>
            if (!expected) println("✓ OK (Failed as expected)")
            else
              println(
                s"✗ FAIL (Should have been solved), reason: ${trace.reason}"
              )
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
