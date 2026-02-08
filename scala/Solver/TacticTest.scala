// ==========================================
// TacticTest.scala
// タクティクスシステムの動作確認
// ==========================================

package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.core.Tactics._

@main def testTactics() = {
  println("=== Tactic System Test ===")

  // 目標: A ∧ B → B ∧ A
  val goalExpr = TestParser.parse("A ∧ B → B ∧ A")
  val initialState = ProofState(List(Goal(Nil, goalExpr)), Nil, goalExpr)

  println(s"Initial state:\n${initialState.currentGoal.get}")

  val result = for {
    s1 <- intro(initialState, Some("h0"))
    _ = println(s"\nAfter intro:\n${s1.currentGoal.get}")

    s2 <- destruct(s1, "h0")
    _ = println(s"\nAfter destruct h0:\n${s2.currentGoal.get}")

    s3 <- split(s2)
    _ = println(s"\nAfter split (Subgoal 1):\n${s3.currentGoal.get}")

    s4 <- exact(s3, "h0.2")
    _ = println(s"\nAfter solve B (Subgoal 2):\n${s4.currentGoal.get}")

    s5 <- exact(s4, "h0.1")
  } yield s5

  result match {
    case Right(finalState) =>
      if (finalState.isSolved)
        println("\n✓ Goal solved successfully using tactics!")
      else println(s"\nRemaining goals: ${finalState.goals.size}")
    case Left(error) =>
      println(s"\n✗ Tactic error: $error")
  }
}
