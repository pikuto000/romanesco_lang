package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

object CycleTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = List(
      CatRule("p-to-p", sym("P")(v("x")), sym("P")(v("x")))
    )
    val config = ProverConfig(rules = rules, maxComplexity = 10)
    val prover = new Prover(config)

    println("=== Cycle Detection Test ===")

    // Simple loop: P(a) -> P(a)
    val goal1 = sym("P")(sym("a"))
    println(s"Testing simple loop: $goal1")
    val res1 = prover.prove(goal1, maxDepth = 5)
    res1 match {
      case Left(trace) => println(s"Failed as expected: ${trace.reason}")
      case Right(_)    => println("Error: Solved a looping goal!")
    }

    // Loop with fresh variables: ∀x. P(x) -> ∀x. P(x)
    val goal2 = sym(Forall)(v("x"), sym("P")(v("x")))
    println(s"Testing loop with Forall: $goal2")
    val res2 = prover.prove(goal2, maxDepth = 10)
    res2 match {
      case Left(trace) => println(s"Failed: ${trace.reason}")
      case Right(_)    => println("Error: Solved a looping goal!")
    }

    // Complex loop: P(a) -> Q(a) -> P(a)
    val rules2 = List(
      CatRule("p-to-q", sym("P")(v("x")), sym("Q")(v("x"))),
      CatRule("q-to-p", sym("Q")(v("x")), sym("P")(v("x")))
    )
    val config2 = ProverConfig(rules = rules2, maxComplexity = 10)
    val prover2 = new Prover(config2)
    println(s"Testing complex loop: P(a) -> Q(a) -> P(a)")
    val res3 = prover2.prove(sym("P")(sym("a")), maxDepth = 10)
    res3 match {
      case Left(trace) => println(s"Failed: ${trace.reason}")
      case Right(_)    => println("Error: Solved a looping goal!")
    }

    // Growth loop: P(a) -> P(f(a)) -> P(f(f(a))) ...
    val rules3 = List(
      CatRule("p-growth", sym("P")(v("x")), sym("P")(sym("f")(v("x"))))
    )
    val config3 = ProverConfig(rules = rules3, maxComplexity = 20)
    val prover3 = new Prover(config3)
    println(s"Testing growth loop: P(a) -> P(f(a))")
    val res4 = prover3.prove(sym("P")(sym("a")), maxDepth = 10)
    res4 match {
      case Left(trace) => println(s"Failed: ${trace.reason}")
      case Right(_)    => println("Error: Solved a growing goal!")
    }
  }
}
