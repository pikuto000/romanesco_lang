// ==========================================
// LLVMCodeGenTest.scala
// 網羅的な LLVM バックエンド検証スイート
// ==========================================

package romanesco.Runtime

import java.io._

@main def LLVMCodeGenTest(): Unit =
  var passed = 0
  var failed = 0

  def test(name: String)(body: => Unit): Unit =
    try
      body
      passed += 1
      println(s"  ✓ $name")
    catch
      case e: Throwable =>
        failed += 1
        println(s"  ✗ $name: ${e.getMessage}")

  println("=== LLVMCodeGenTest (Comprehensive Suite) ===")

  def runAndGetExitCode(code: Array[Op]): Int =
    val gen = new LLVMCodeGen()
    val ir = gen.generate(code, entryName = "romanesco_main", embedRuntime = true)
    val wrapper = ir + """
  define i32 @main() {
    %v = call %Value @romanesco_main()
    %tag = extractvalue %Value %v, 0
    %is_int = icmp eq i8 %tag, 6
    br i1 %is_int, label %int_case, label %default_case
  int_case:
    %ptr = extractvalue %Value %v, 1
    %val64 = ptrtoint ptr %ptr to i64
    %val32 = trunc i64 %val64 to i32
    ret i32 %val32
  default_case:
    ret i32 99
  }
  """
    val tmpFile = File.createTempFile("romanesco_", ".ll")
    tmpFile.deleteOnExit()
    val writer = new PrintWriter(tmpFile)
    writer.print(wrapper)
    writer.close()

    val process = Runtime.getRuntime().exec(Array("lli", tmpFile.getAbsolutePath()))
    process.waitFor()
    process.exitValue()

  val lliAvailable = try { Runtime.getRuntime().exec(Array("lli", "--version")).waitFor() == 0 } catch { case _: Exception => false }

  if (lliAvailable) {
    println("\n--- 実行時挙動テスト (lli) ---")
    
    test("定数 42") {
      val res = runAndGetExitCode(Array(Op.LoadConst(0, Value.Atom(42)), Op.Return(0)))
      if (res != 42) throw AssertionError(s"期待=42, 実際=$res")
    }

    test("算術演算: (10 + 20) * 2 - 15 = 45") {
      val res = runAndGetExitCode(Array(
        Op.LoadConst(0, Value.Atom(10)),
        Op.LoadConst(1, Value.Atom(20)),
        Op.Add(2, 0, 1),      // 30
        Op.LoadConst(3, Value.Atom(2)),
        Op.Mul(4, 2, 3),      // 60
        Op.LoadConst(5, Value.Atom(15)),
        Op.Sub(6, 4, 5),      // 45
        Op.Return(6)
      ))
      if (res != 45) throw AssertionError(s"期待=45, 実際=$res")
    }

    test("ネストしたペア: proj1(proj2((1, (2, 3)))) = 2") {
      val res = runAndGetExitCode(Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.LoadConst(2, Value.Atom(3)),
        Op.MakePair(3, 1, 2),  // (2, 3)
        Op.MakePair(4, 0, 3),  // (1, (2, 3))
        Op.Proj2(5, 4),        // (2, 3)
        Op.Proj1(6, 5),        // 2
        Op.Return(6)
      ))
      if (res != 2) throw AssertionError(s"期待=2, 実際=$res")
    }

    test("Move の連鎖: x -> y -> z") {
      val res = runAndGetExitCode(Array(
        Op.LoadConst(0, Value.Atom(77)),
        Op.Move(1, 0),
        Op.Move(2, 1),
        Op.Return(2)
      ))
      if (res != 77) throw AssertionError(s"期待=77, 実際=$res")
    }

    test("Borrow の利用: コピーせずに2回使う") {
      val res = runAndGetExitCode(Array(
        Op.LoadConst(0, Value.Atom(10)),
        Op.Borrow(1, 0), // 10 を借用
        Op.Add(2, 0, 1), // 10 + 10 = 20
        Op.Return(2)
      ))
      if (res != 20) throw AssertionError(s"期待=20, 実際=$res")
    }

    test("スタックアロケーションの共存: 一方はエスケープ、一方はスタック") {
      // r2 (10, 20) は proj1 されて死ぬのでスタック
      // r5 (30, 40) は return されるのでヒープ
      val res = runAndGetExitCode(Array(
        Op.LoadConst(0, Value.Atom(10)),
        Op.LoadConst(1, Value.Atom(20)),
        Op.MakePair(2, 0, 1), // Stack
        Op.Proj1(3, 2),       // 10
        Op.LoadConst(4, Value.Atom(40)),
        Op.MakePair(5, 3, 4), // Escape (r5 is returned)
        Op.Proj1(6, 5),       // 10
        Op.Return(6)
      ))
      if (res != 10) throw AssertionError(s"期待=10, 実際=$res")
    }
  }

  println("\n--- IR 構造テスト ---")
  
  test("スタックアロケーション最適化の確認") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.LoadConst(0, Value.Atom(1)),
      Op.LoadConst(1, Value.Atom(2)),
      Op.MakePair(2, 0, 1), // does not escape
      Op.Proj1(3, 2),
      Op.Return(3)
    ))
    if (!ir.contains("alloca %Pair")) throw AssertionError("エスケープしないペアが alloca されていない")
  }

  println(s"\n最終結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if (failed > 0) sys.exit(1)
