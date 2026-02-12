package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object CombinedLogicTest {
  def main(args: Array[String]): Unit = {
    // 複数の論理体系のルールを統合
    val allRules = StandardRules.all ++
      StandardRules.linear ++
      StandardRules.modal ++
      StandardRules.temporal ++
      StandardRules.logicMapping ++
      StandardRules.linearMapping

    val config = ProverConfig(
      rules = allRules,
      maxComplexity = 300,
      maxParallelism = 8,
      maxInduction = 2
    )
    val prover = new Prover(config)

    println("=== Combined Multi-Logic Test ===")

    val testCases = List(
      // 1. Linear + Temporal: Distribution of G over Linear Implication
      "G(A ⊸ B) → (G(A) ⊸ G(B))",

      // 2. Linear + Modal: Axiom K for Linear Implication
      "□(A ⊸ B) → (□A ⊸ □B)",

      // 3. Separation + Temporal: Temporal persistence of framed resource
      "G(A * B) ⊸ (G(A) * G(B))",

      // 5. Linear + Temporal + Modal: Interaction between G, □, and Linear Implication
      "□G(A ⊸ B) → G(□A ⊸ □B)",

      // 6. Linear + Modal: Distribution of □ over *
      "□(A * B) → (□A * □B)",

      // 7. Temporal + Linear: Interaction of G and Linear implication with quantifiers
      "G(∀x. A(x) ⊸ B(x)) → ∀x. (G(A(x)) ⊸ G(B(x)))",

      // 4. Forall + Linear + Modal
      "∀x. □(P(x) ⊸ Q(x)) → ∀x. (□P(x) ⊸ □Q(x))"
    )

    testCases.zipWithIndex.foreach { case (input, i) =>
      println(s"\n[Test Case] $input")

      // if (i == 3) {
      //  logger.switch(false)
      //  logger.setMaxDepth(5)
      // } else {
      //  logger.switch(false)
      // }
      try {
        val goal = TestParser.parse(input)
        // 非常に複雑なため、maxDepthを30に設定
        prover.prove(goal, maxDepth = 30) match {
          case Right(res) =>
            println(s"✓ Solved!")
            println(res.tree.format(1))
          case Left(trace) =>
            println(s"✗ Failed. Reason: ${trace.reason}")
          // println(trace.format())
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
