package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object DebugHoare {
  def main(args: Array[String]): Unit = {
    logger.switch(true)
    logger.setMaxDepth(8)
    val rules = StandardRules.all
    val config = ProverConfig(rules = rules, maxComplexity = 300)
    val prover = new Prover(config)
    val input = "{⊤} if (x = 0) x := 1 else x := 2 {x = 1 ∨ x = 2}"
    val goal = TestParser.parse(input)
    println(s"Parsed goal: $goal")
    prover.prove(goal, maxDepth = 8, timeoutMs = 10000) match {
      case Right(result) => println(s"SOLVED: ${result.tree.format()}")
      case Left(trace) => println(s"FAILED: ${trace.reason}")
    }
  }
}
