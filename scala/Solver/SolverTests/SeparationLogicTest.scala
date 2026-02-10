package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger

@main def testSeparationLogic = {
  logger.switch(false)
  println("=== Separation Logic Test ===")

  val sepRules = StandardRules.separation
  val allRules = StandardRules.all ++ sepRules

  val testCases = List(
    "A * B → B * A",    // Commutativity
    "A * B → A ∧ B"     // Mapping to And
  )

  val config = ProverConfig(classical = false, rules = allRules)
  val prover = new Prover(config)

  testCases.foreach { input =>
    println(s"\n[Test Case] $input")
    try {
      val expr = TestParser.parse(input)
      prover.prove(expr) match {
        case Right(result) =>
          println(s"✓ Solved:\n${result.tree.format(1)}")
          result.generatedLemma.foreach(l => println(s"  Generated Lemma: $l"))
        case Left(trace) =>
          println("✗ Failed to prove")
          println(trace.format(1))
      }
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}