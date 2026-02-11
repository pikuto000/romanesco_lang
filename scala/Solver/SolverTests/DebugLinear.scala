package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object DebugLinear {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val config = ProverConfig(rules = StandardRules.linear, maxComplexity = 50)
    val prover = new Prover(config)

    println("=== Debug: A ⊸ B ⊸ (A ⊗ B) ===")
    val goal = TestParser.parse("A ⊸ B ⊸ (A ⊗ B)")
    val result = prover.prove(goal, maxDepth = 10)
    result match {
      case Right(res) =>
        println("✓ Solved!")
        println(res.tree.format())
      case Left(trace) =>
        println("✗ Failed")
        println(trace.format())
    }
  }
}
