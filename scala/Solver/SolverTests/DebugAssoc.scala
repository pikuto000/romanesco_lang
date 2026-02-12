package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object DebugAssoc {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    logger.setMaxDepth(10)
    val rules = StandardRules.hott ++ StandardRules.all
    val config =
      ProverConfig(rules = rules, maxInduction = 3, maxComplexity = 200)
    val prover = new Prover(config)

    println("=== Debug: Path Associativity ===")
    val goal = TestParser.parse(
      "∀x. ∀y. ∀z. ∀w. ∀p:path(A,x,y). ∀q:path(A,y,z). ∀r:path(A,z,w). concat(concat(p, q), r) = concat(p, concat(q, r))"
    )
    val result = prover.prove(goal, maxDepth = 25)
    result match {
      case Right(res) =>
        println("✓ Solved!")
        println(res.tree.format())
      case Left(trace) =>
        println("✗ Failed")
      // println(trace.format())
    }
  }
}
