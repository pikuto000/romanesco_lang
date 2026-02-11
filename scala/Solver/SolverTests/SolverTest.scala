// ==========================================
// SolverTest.scala
// CLI・REPL・テスト（手動テスト修正版）
// ==========================================

package romanesco

import romanesco.Solver._
import romanesco.Solver.core._
import romanesco.Solver.sugar._
import scala.io.StdIn
import romanesco.Utils.Debug.logger

@main def testSomeCases = {
  logger.switch(false)

  // 証明済みの補助定理をルールに変換
  val lemmas = List(
    CatRule("plus_n_0", TestParser.parse("plus(n, 0)"), TestParser.parse("n")),
    CatRule(
      "plus_n_Sm",
      TestParser.parse("plus(n, S(m))"),
      TestParser.parse("S(plus(n, m))")
    )
  )

  val autoInductionCases = List(
    "∀n. plus(n, 0) = n",
    "∀n. plus(0, n) = n",
    "∀n. ∀m. plus(n, S(m)) = S(plus(n, m))",
    "∀xs. append(xs, nil) = xs",
    "∀xs. append(nil, xs) = xs",
    "∀xs. ∀ys. ∀zs. append(append(xs, ys), zs) = append(xs, append(ys, zs))",
    "∀n. ∀m. plus(n, m) = plus(m, n)", // 交換法則（Lemmasが必要）
    "∀n. ∀m. ∀k. plus(plus(n, m), k) = plus(n, plus(m, k))" // 結合法則
  )

  println("\n=== Induction Test: plus(n, S(m)) = S(plus(n, m)) ===")
  try {
    val expr = TestParser.parse("∀n. ∀m. plus(n, S(m)) = S(plus(n, m))")
    var state = ProofState(List(Goal(Nil, Nil, expr)), Nil, expr)

    def printState(s: ProofState): Unit = {
      println("Goals:")
      s.goals.zipWithIndex.foreach { (g, i) =>
        println(s"  $i: $g")
      }
    }

    // 1. induction n
    state =
      Tactics.induction(state).getOrElse(throw Exception("Induction failed"))

    // 2. solve base: plus(0, S(m)) = S(plus(0, m))
    // intro m (ここが重要！)
    state = Tactics
      .intro(state, Some("m"))
      .getOrElse(throw Exception("Intro m failed"))
    // reflexivity
    state = Tactics
      .reflexivity(state)
      .getOrElse(throw Exception("Reflexivity failed"))

    println("\nBase Case solved. Next goal:")
    printState(state)

    // 3. intro n, intro IH, intro m
    state = Tactics
      .intro(state, Some("n_step"))
      .getOrElse(throw Exception("Intro n_step failed"))
    state = Tactics
      .intro(state, Some("IH"))
      .getOrElse(throw Exception("Intro IH failed"))
    state = Tactics
      .intro(state, Some("m"))
      .getOrElse(throw Exception("Intro m failed"))

    println("\nAfter intros:")
    printState(state)

    // 4. rewrite IH
    state = Tactics
      .rewrite(state, "IH")
      .getOrElse(throw Exception("Rewrite IH failed"))
    println("\nAfter rewrite IH:")
    printState(state)

    // 5. reflexivity
    state = Tactics
      .reflexivity(state)
      .getOrElse(throw Exception("Final reflexivity failed"))
    println("\n✓ Manual induction goal solved successfully!")
  } catch {
    case e: Exception =>
      println(s"✗ Error in manual induction: ${e.getMessage}")
  }

  println("\n=== Automatic Induction Tests ===")
  autoInductionCases.foreach { input =>
    println(s"\n[Test Case] $input")
    try {
      val expr = TestParser.parse(input)
      val rules =
        if (input.contains("m, n")) StandardRules.all ++ lemmas
        else StandardRules.all
      val config = ProverConfig(classical = false, rules = rules)
      val prover = new Prover(config)
      val result = prover.prove(expr)
      result match
        case Right(res) =>
          println(s"✓ Solved:\n${res.tree.format(1)}")
          res.generatedLemma.foreach(l => println(s"  Generated Lemma: $l"))
        case Left(trace) =>
          println("✗ Failed to prove")
          println(trace.format(1))
    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
