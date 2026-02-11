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
  val initialState = ProofState(List(Goal(Nil, Nil, goalExpr)), Nil, goalExpr)

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

  println("\n=== Induction Test: plus(n, 0) = n ===")
  val inductionGoal = TestParser.parse("∀n. plus(n, 0) = n")
  val inductionState = ProofState(List(Goal(Nil, Nil, inductionGoal)), Nil, inductionGoal)
  
  val indResult = for {
    s1 <- induction(inductionState)
    _  = println(s"\nBase Case:\n${s1.goals.head}")
    _  = println(s"\nInductive Step:\n${s1.goals.tail.head}")
    
    // Base Case を解決 (plus(0, 0) = 0)
    s2 <- reflexivity(s1)
    _  = println(s"\nBase Case solved! Next goal:\n${s2.currentGoal.get}")
    
    // Inductive Step を解決
    s3 <- intro(s2) // n を導入
    _  = println(s"\nAfter intro n:\n${s3.currentGoal.get}")
    
    s4 <- intro(s3, Some("IH")) // IH を導入
    _  = println(s"\nAfter intro IH:\n${s4.currentGoal.get}")
    
    s5 <- rewrite(s4, "IH")
    _  = println(s"\nAfter rewrite IH:\n${s5.currentGoal.get}")
    
    s6 <- reflexivity(s5)
  } yield s6

  indResult match {
    case Right(finalState) =>
      if (finalState.isSolved) println("\n✓ Induction goal solved successfully!")
      else println(s"\nRemaining goals: ${finalState.goals.size}")
    case Left(error) =>
      println(s"\n✗ Tactic error: $error")
  }
}
