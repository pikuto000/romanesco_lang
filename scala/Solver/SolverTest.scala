// ==========================================
// SolverTest.scala
// CLI・REPL・テスト
// ==========================================

package romanesco

import romanesco.Solver._
import romanesco.Solver.core._
import romanesco.Solver.sugar._
import scala.io.StdIn
import romanesco.Utils.Debug.logger

@main def runProver(args: String*): Unit =
  println("=== Romanesco Prover v3.0 (Category Theory Edition) ===")
  println("Core: 5 types (Expr: 4 + ProofStep: 1)")
  println("Type 'help' for commands, 'exit' to quit")
  println()

  if args.nonEmpty then
    val source = scala.io.Source.fromFile(args.head)
    try
      for
        line <- source.getLines if line.trim.nonEmpty && !line.startsWith("//")
      do processInput(line)
    finally
      source.close
  else
    while true do
      print("> ")
      val input = StdIn.readLine
      if input == null || input == "exit" then return
      else if input == "help" then showHelp
      else if input == "enableDebug" then logger.switch(true)
      else if input == "disableDebug" then logger.switch(false)
      else if input.trim.nonEmpty then processInput(input)

def showHelp: Unit =
  println("""
Commands:
  <expr>          - Prove the expression
  help            - Show this help
  exit            - Exit the prover

Syntax:
  A ∧ B          - Conjunction (and)
  A ∨ B          - Disjunction (or)
  A → B          - Implication
  ∀x. P          - Universal quantification
  ∃x. P          - Existential quantification
  a = b          - Equality
  ⊤              - True
  ⊥              - False

Examples:
  A → A
  A ∧ B → B ∧ A
  ∀x. P(x) → ∃x. P(x)
  """)

def processInput(input: String, classical: Boolean = false): Unit =
  try
    val expr = TestParser.parse(input)
    println(s"Goal: $expr (Classical: $classical)")

    val prover = new Prover(classical = classical)
    prover.prove(expr) match
      case Right(proof) =>
        println(s"✓ Proof found (${proof.length} steps):")
        proof.zipWithIndex.foreach { case (step, i) =>
          println(s"  ${i + 1}. $step")
        }
      case Left(trace) =>
        println("✗ No proof found. Failure trace:")
        println(trace.format())
  catch case e: Exception => println(s"Error: ${e.getMessage}")

@main def testSomeCases = {
  logger.switch(false)

  val intuitionisticCases = List(
    "A → A",
    "A ∧ B → B ∧ A",
    "A ∨ B → B ∨ A",
    "(A ∧ B → C) → (A → (B → C))",
    "a = b ∧ b = c → a = c",
    "(((A → ⊥) → ⊥) → ⊥) → (A → ⊥)",
    "A ∨ (A → ⊥)", // Should fail
    "A ∨ B → A"    // Should fail
  )

  val classicalCases = List(
    "A ∨ (A → ⊥)",                // Law of Excluded Middle
    "((A → ⊥) → ⊥) → A",          // Double Negation Elimination
    "((A → B) → A) → A",          // Peirce's Law
    "(A → B) → ((A → ⊥) ∨ B)"     // Implication as Or
  )

  println("=== Intuitionistic Logic Tests ===")
  intuitionisticCases.foreach { input =>
    println(s"\n[Test Case] $input")
    try {
      val expr = TestParser.parse(input)
      val prover = new Prover(classical = false)
      val result = romanesco.Utils.times.watch { prover.prove(expr) }
      result match
        case Right(proof) => println(s"✓ Solved in ${proof.length} steps")
        case Left(trace) => 
          println("✗ Failed to prove")
          if (input == "A ∨ B → A" || input == "A ∨ (A → ⊥)") {
            println("Failure Reason:")
            println(trace.format(1))
          }
    } catch {
      case e: Exception => println(s"Error parsing/proving '$input': ${e.getMessage}")
    }
  }

  println("\n=== Classical Logic Tests ===")
  classicalCases.foreach { input =>
    println(s"\n[Test Case] $input")
    try {
      val expr = TestParser.parse(input)
      val prover = new Prover(classical = true)
      val result = romanesco.Utils.times.watch { prover.prove(expr) }
      result match
        case Right(proof) => 
          println(s"✓ Solved in ${proof.length} steps")
          // proof.zipWithIndex.foreach { case (step, i) => println(s"  ${i + 1}. $step") }
        case Left(trace) => 
          println("✗ Failed to prove")
          println(trace.format())
    } catch {
      case e: Exception => println(s"Error parsing/proving '$input': ${e.getMessage}")
    }
  }
}
