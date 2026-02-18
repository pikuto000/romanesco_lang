package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object FailingCasesDebug {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    logger.setMaxDepth(30)

    val allRules = StandardRules.all ++
      StandardRules.linear ++
      StandardRules.modal ++
      StandardRules.temporal ++
      StandardRules.logicMapping

    val config = ProverConfig(
      rules = allRules,
      maxComplexity = 500,
      maxParallelism = 1 // ログを読みやすくするためシングルスレッド
    )
    val prover = new Prover(config)

    val failingCases = List(
      "□G(A ⊸ B) → G(□A ⊸ □B)",
      "∀x. □(P(x) ⊸ Q(x)) → ∀x. (□P(x) ⊸ □Q(x))"
    )

    println("=== Debugging Failing Cases ===")

    failingCases.foreach { input =>
      println(s"Target: $input")
      val goal = TestParser.parse(input)
      // timeoutMs は prove メソッドに渡す
      prover.prove(goal, maxDepth = 30, timeoutMs = 60000) match {
        case Right(_)    => println("✓ Fixed!")
        case Left(trace) =>
          println("✗ Still Failing")
          println(s"Reason: ${trace.reason}")
      }
    }
  }
}
