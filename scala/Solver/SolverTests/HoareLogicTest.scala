package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object HoareLogicTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.all
    val config = ProverConfig(rules = rules, maxComplexity = 300)
    val prover = new Prover(config)

    println("=== Hoare Logic: Program Verification Test ===")

    val testCases = List(
      // 1. Skip
      ("{x = 1} skip {x = 1}", true),
      
      // 2. Assignment
      ("{⊤} x := 1 {x = 1}", true),
      
      // 3. Sequence
      ("{⊤} x := 1 ; y := 2 {x = 1 ∧ y = 2}", true),
      
      // 4. Conditional
      ("{⊤} if (x = 0) x := 1 else x := 2 {x = 1 ∨ x = 2}", true),

      // 5. While (Loop with explicit invariant)
      ("{x = 0} while [x = 0 ∨ x = 1] (x = 0) x := 1 {x = 1}", true)
    )

    testCases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = 15, timeoutMs = 15000)
        result match {
          case Right(_)    => println("✁EOK (Solved)")
          case Left(trace) =>
            if (expected) {
              println(s"✁EFAIL (Should have been solved) - Reason: ${trace.reason}")
              // println(trace.format())
            } else println("✁EOK (Failed as expected)")
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
