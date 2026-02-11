package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object TypeLevelTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val config = ProverConfig.default
    val prover = new Prover(config)

    println("=== Strict Universe Level Unification Test ===")

    // Helper to create Type(level)
    def typeL(level: Int): Expr = Expr.typeLevel(level)

    // Goals to test
    // 1. Type0 = Type0 (Should succeed)
    // 2. Type0 = Type1 (Should fail)

    val t0 = typeL(0)
    val t1 = typeL(1)

    val goalSuccess = Expr.App(Expr.Sym("="), List(t0, t0))
    val goalFail = Expr.App(Expr.Sym("="), List(t0, t1))

    println(s"Proving: $goalSuccess ...")
    prover.prove(goalSuccess, maxDepth = 5) match {
      case Right(_) => println("✓ OK (Solved)")
      case Left(_)  => println("✗ FAIL (Should have been solved)")
    }

    println(s"Proving: $goalFail ...")
    prover.prove(goalFail, maxDepth = 5) match {
      case Right(_) => println("✗ FAIL (Should have failed)")
      case Left(_)  => println("✓ OK (Failed as expected)")
    }
  }
}
