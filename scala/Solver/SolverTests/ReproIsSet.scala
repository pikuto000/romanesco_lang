
import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser

object ReproIsSet {
  def main(args: Array[String]): Unit = {
    val rules = StandardRules.hott
    val config = ProverConfig(rules = rules, maxInduction = 1)
    val prover = new Prover(config)

    // A が isSet であることを前提に、∀x, y, p, q. path² を証明する
    val goal = TestParser.parse("isSet(A) → ∀x:A. ∀y:A. ∀p:path(A, x, y). ∀q:path(A, x, y). path(path(A, x, y), p, q)")
    
    println(s"Goal: $goal")
    val result = prover.prove(goal, maxDepth = 10, timeoutMs = 2000)
    
    result match {
      case Right(res) => 
        println("SUCCESS")
        println(res.tree.format())
      case Left(trace) => 
        println("FAILED")
        println(trace.format())
    }
  }
}
