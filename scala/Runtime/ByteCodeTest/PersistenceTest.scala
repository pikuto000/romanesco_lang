// ==========================================
// PersistenceTest.scala
// Scala でコンパイル・エクスポートし、Zig でロード・実行する一貫テスト
// ==========================================

package romanesco.Runtime

import java.io.File
import scala.sys.process._

@main def PersistenceTest(): Unit =
  val compiler = new BytecodeCompiler()
  val exporter = new BytecodeExporter()

  println("=== Persistence Integration Test ===")

  // 1. テストプログラムの定義 (λx. x + 40)(2) = 42
  val code = Array(
    // Lambda: id=0, env=[], body=[r0+r1], arity=1
    Op.MakeClosure(
      0,
      Array(
        Op.LoadConst(1, Value.Atom(40)),
        Op.Add(2, 0, 1), // arg is at 0
        Op.Return(2)
      ),
      Array.empty,
      1
    ),
    Op.LoadConst(1, Value.Atom(2)),
    Op.Call(2, 0, Array(1)),
    Op.Return(2)
  )

  val rbcPath = "integration_test.rbc"
  exporter.save(code, rbcPath)
  println(s"  ✓ Bytecode exported to $rbcPath")

  // 2. Zig VM で実行 (外部プロセス呼び出し)
  println("  Running Zig VM...")

  // Zig プロジェクトをビルド
  val buildCmd = "powershell.exe -NoProfile -Command \"cd Zig; zig build\""
  val buildResult = Process(buildCmd).!
  if (buildResult != 0) {
    println("  ✗ Zig build failed")
    sys.exit(1)
  }

  // Zig VM を実行
  val runCmd = s"Zig/zig-out/bin/Zig.exe $rbcPath"
  val runResult = Process(runCmd).!

  println(s"  ✓ Integration test finished with exit code $runResult")
