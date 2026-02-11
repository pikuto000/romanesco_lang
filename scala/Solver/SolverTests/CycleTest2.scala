package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

object CycleTest2 {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = List(
      CatRule("p-to-p", sym("P")(v("x")), sym("P")(v("x")))
    )
    val config = ProverConfig(rules = rules, maxComplexity = 10)
    val prover = new Prover(config)

    println("=== Cycle Detection Test 2 (Inner Branch) ===")

    // Direct loop: P(a) -> P(a)
    // In one depth limit, say d=3, it should try:
    // P(a) --[p-to-p]--> P(a) --[p-to-p]--> P(a)
    // The second P(a) should be caught by `visited`.
    val goal1 = sym("P")(sym("a"))
    println(s"Testing loop: $goal1")
    val res1 = prover.prove(goal1, maxDepth = 3)
    // We expect "[CYCLE DETECTED]" in logs.
  }
}
