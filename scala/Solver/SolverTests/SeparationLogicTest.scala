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
    val config = ProverConfig(rules = rules)
    val prover = new Prover(config)

    println("=== Separation Logic: Resource Management Test ===")

    // Use ⊸ (LImplies) to ensure antecedents go to Linear Context
    val testCases = List(
      ("A * B ⊸ B * A", true),
      ("(A * B) * C ⊸ A * (B * C)", true),
      ("(A ⊸ B) ⊸ (A * C ⊸ B * C)", true),
      ("A * A ⊸ A", false), 
      ("A ⊸ A * A", false),
      ("(∀v. (x ↦ v ⊸ P(v))) ⊸ (x ↦ 5 ⊸ P(5))", true),
      ("A * B * C ⊸ C * A * B", true),
      ("(∀v. (x ↦ v * R)) ⊸ (x ↦ 5 * R)", true) // Quantifier for variable v
    )

    testCases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = 10)
        result match {
          case Right(_) if expected => println("✓ OK (Solved)")
          case Right(_) if !expected => println("✗ FAIL (Should have failed)")
          case Left(_) if !expected => println("✓ OK (Failed)")
          case Left(trace) if expected => 
            println("✗ FAIL (Should have been solved)")
            // println(trace.format())
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
