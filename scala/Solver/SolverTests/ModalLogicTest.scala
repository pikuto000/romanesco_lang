package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger

@main def testModalLogic = {
  logger.switch(false)
  println("=== Modal Logic Test ===")

  val modalRules = StandardRules.modal
  val allRules = StandardRules.all ++ modalRules

  val testCases = List(
    "□(A → B) → (□A → □B)", // Axiom K
    "□A → A", // Axiom T
    "□A → □□A", // Axiom 4
    "◇A → □◇A", // Axiom 5
    "◇A → (□(A → ⊥) → ⊥)" // Duality
  )

  val config = ProverConfig(classical = false, rules = allRules)
  val prover = new Prover(config)

  testCases.foreach { input =>
    println(s"[Test Case] $input")
    try {
      val expr = TestParser.parse(input)
      prover.prove(expr) match {
        case Right(result) =>
          println(s"✓ Solved:${result.tree.format(1)}")
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
