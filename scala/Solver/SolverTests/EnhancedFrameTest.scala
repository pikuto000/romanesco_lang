package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object EnhancedFrameTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.all
    val config = ProverConfig(rules = rules, maxComplexity = 300)
    val prover = new Prover(config)

    println("=== Enhanced Frame Inference Test ===")

    val testCases = List(
      // 既存のテスト（動作確認）
      ("∃F. (A * B ⊸ A * F)", true, 10, "Basic frame"),
      ("∃F. (x ↦ 1 * y ↦ 2 ⊸ x ↦ 1 * F)", true, 10, "Points-to frame"),
      ("∃F. (A * B * C ⊸ B * F)", true, 10, "Multi-resource frame"),
      
      // 新規テスト（最小フレーム）
      ("∃F. (A * B * C * D ⊸ A * B * F)", true, 15, "Minimal frame (C * D)"),
      ("∃F. (x ↦ 1 * y ↦ 2 * z ↦ 3 ⊸ y ↦ 2 * F)", true, 15, "Minimal frame (x ↦ 1 * z ↦ 3)"),
      
      // フレームルール
      ("(x ↦ 1 ⊸ x ↦ 2) → (x ↦ 1 * y ↦ 3 ⊸ x ↦ 2 * y ↦ 3)", true, 20, "Frame rule application"),
      
      // プログラム検証風
      ("∃F. (file_exists(f) * buffer(b) ⊸ file_open(f) * F)", true, 15, "File handle frame"),
      ("∃F. (lock_free(l) * x ↦ 1 ⊸ lock_held(l) * x ↦ 1 * F)", true, 15, "Lock and memory frame")
    )

    testCases.foreach { case (input, expected, depth, description) =>
      print(s"Case: $description ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = depth)
        result match {
          case Right(_) =>
            if (expected) println("✓ OK")
            else println("✗ FAIL (Should have failed)")
          case Left(trace) =>
            if (!expected) println("✓ OK (Failed as expected)")
            else println(s"✗ FAIL: ${trace.reason}")
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
