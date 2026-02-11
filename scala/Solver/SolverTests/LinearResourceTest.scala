package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object LinearResourceTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val config = ProverConfig(rules = StandardRules.linear)
    val prover = new Prover(config)

    println("=== Linear Logic: Resource Consumption Test ===")

    val testCases = List(
      ("A ⊸ A", true),
      ("(A ⊗ B) ⊸ (B ⊗ A)", true),
      ("A ⊸ (A ⊗ A)", false), // Double use
      ("A ⊸ B ⊸ (A ⊗ B)", true),
      ("(A ⊗ A) ⊸ A", false)  // Not possible if A ⊗ A means two distinct A's
    )

    testCases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = 10)
        result match {
          case Right(_) if expected => println("✓ OK (Solved as expected)")
          case Right(_) if !expected => println("✗ FAIL (Solved but should have failed)")
          case Left(_) if !expected => println("✓ OK (Failed as expected)")
          case Left(trace) if expected => 
            println("✗ FAIL (Failed but should have been solved)")
            println(trace.format())
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
