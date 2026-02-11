package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object TemporalLogicTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules =
      StandardRules.temporal ++ StandardRules.logicMapping ++ StandardRules.products
    val config = ProverConfig(rules = rules, maxParallelism = 1)
    val prover = new Prover(config)

    println("=== Temporal Logic: Co-induction Test ===")

    val testCases = List(
      "G(A) → A",
      "G(A) → X(A)",
      "G(A) → G(G(A))",
      "G(A → B) → G(A) → G(B)" 
    )

    testCases.foreach { input =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = 30)
        result match {
          case Right(_)    => println("✓ OK (Solved)")
          case Left(trace) =>
            println(s"✗ FAIL. Reason: ${trace.reason}")
          // println(trace.format())
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
