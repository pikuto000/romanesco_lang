// ==========================================
// NativeCompileTest.scala
// 純粋 LLVM IR からのネイティブコンパイル検証
// ==========================================

package romanesco.Runtime

import java.io._
import scala.sys.process._

@main def NativeCompileTest(): Unit =
  println("=== Pure LLVM IR Native Compilation Test ===")

  val codeGen = new LLVMCodeGen()

  def compileAndRun(name: String, code: Array[Op]): Unit =
    println(s"\n>>> Test: $name")

    try {
      // 1. 完全な LLVM IR 生成 (ランタイム込み)
      val ir = codeGen.generate(
        code,
        entryName = "romanesco_main",
        embedRuntime = true
      )

      // 2. ネイティブ main ラッパー (エントリポイント)
      val wrapper = ir + """
  define i32 @main() {
    %v_ptr = alloca %Value
    %v = call %Value @romanesco_main()
    store %Value %v, ptr %v_ptr
    %val = call i64 @rt_get_int(ptr %v_ptr)
    call void @rt_print_int(i64 %val)
    ret i32 0
  }
  """
      val llFile = new File("pure_test.ll")
      val writer = new PrintWriter(llFile)
      writer.print(wrapper)
      writer.close()

      // 3. Clang によるコンパイル (runtime.c は不要！)
      val isWin = System.getProperty("os.name").toLowerCase.contains("win")
      val exeName = if (isWin) "pure_test.exe" else "pure_test"

      println(s"  Compiling with clang (Self-contained)...")
      val compileResult = Process(
        Seq(
          "clang",
          "-O3",
          "-Wno-override-module",
          "-o",
          exeName,
          "pure_test.ll"
        )
      ).!

      if (compileResult != 0) {
        throw new RuntimeException(
          s"Clang compilation failed with exit code $compileResult"
        )
      }

      // 4. 実行
      println(s"  Running $exeName...")
      val output =
        if (isWin) Seq(".\\" + exeName).!!.trim else Seq("./" + exeName).!!.trim
      println(s"  Output: $output")

      // クリーンアップ
      llFile.delete()
      new File(exeName).delete()

    } catch {
      case e: Exception => println(s"  ! Error: ${e.getMessage}")
    }

  // テスト 1: 算術演算 (500 * 2 + 7 = 1007)
  val ops1 = Array(
    Op.LoadConst(0, Value.Atom(500)),
    Op.LoadConst(1, Value.Atom(2)),
    Op.Mul(2, 0, 1),
    Op.LoadConst(3, Value.Atom(7)),
    Op.Add(4, 2, 3),
    Op.Return(4)
  )
  compileAndRun("Self-contained Arithmetic", ops1)

  // テスト 2: シンプルなクロージャ (λx. x + 1)(10) = 11
  val body2 = Array(
    Op.LoadConst(1, Value.Atom(1)),
    Op.Add(2, 0, 1), // regs[0] は引数 x (rt_setup_regsにより配置)
    Op.Return(2)
  )
  val ops2 = Array(
    Op.MakeClosure(0, body2, Array.empty, 1),
    Op.LoadConst(1, Value.Atom(10)),
    Op.Call(2, 0, Array(1)),
    Op.Return(2)
  )
  compileAndRun("Simple Closure", ops2)

