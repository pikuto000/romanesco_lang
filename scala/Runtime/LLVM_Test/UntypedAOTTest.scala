// ==========================================
// UntypedAOTTest.scala
// プロファイル駆動型 AOT コンパイル (PGO-AOT) の検証
// ==========================================

package romanesco.Runtime.LLVM_Test

import romanesco.Runtime._
import romanesco.Utils.Debug
import java.io._
import scala.sys.process._
import scala.collection.mutable.ArrayBuffer

@main def UntypedAOTTest(): Unit =
  Debug.logger.switch(true)
  Debug.logger.info("=== Profile-Guided Untyped AOT Compilation Test ===")

  val codeGen = new LLVMCodeGen()
  val pvm = new ProfilingVM()

  // 最適化対象のコード: (x + y) * z
  def getOps() = Array(
    Op.Add(3, 0, 1), // x + y
    Op.Mul(4, 3, 2), // (x + y) * z
    Op.Return(4)
  )

  val code = getOps()

  // 1. プロファイリング実行 (VM)
  Debug.logger.info(">>> Step 1: Profiling in VM...")
  val regs = ArrayBuffer[Value](
    Value.Atom(3), Value.Atom(4), Value.Atom(2)
  )
  while (regs.length < 32) regs += Value.Unit

  // VMで実行して統計を取る
  pvm.runWithProfiling(code, regsInput = Some(regs))
  val profile = pvm.profileData
  Debug.logger.info(s"  Profile captured (Values: 3, 4, 2 -> 14)")

  // 2. プロファイル情報を用いた AOT コンパイル
  Debug.logger.info(">>> Step 2: AOT Compiling with Profile Data (PGO)...")
  try {
    // 比較のため、最初はプロファイルなしでコンパイルを試みる
    val irNoProfile = codeGen.generate(code, entryName = "pgo_main_no", embedRuntime = true, profile = None)
    // 続いてプロファイルあり
    val ir = codeGen.generate(
      code,
      entryName = "pgo_main",
      embedRuntime = true,
      profile = Some(profile)
    )

    // デバッグ: 生成された IR を一部表示
    Debug.logger.info(s"  Generated IR Sample (PGO path):\n${ir.linesIterator.toSeq.filter(_.contains("add")).mkString("\n")}")

    val wrapper = ir + """
  define i32 @main() {
    %regs = alloca %Value, i32 32
    ; 全レジスタを初期化
    %i_ptr = alloca i32
    store i32 0, ptr %i_ptr
    br label %init_loop
  init_loop:
    %i = load i32, ptr %i_ptr
    %cond = icmp ult i32 %i, 32
    br i1 %cond, label %init_body, label %init_done
  init_body:
    %p = getelementptr %Value, ptr %regs, i32 %i
    call void @rt_init_unit(ptr %p)
    %next = add i32 %i, 1
    store i32 %next, ptr %i_ptr
    br label %init_loop
  init_done:
    ; 引数セット: x=3, y=4, z=2
    %p0 = getelementptr %Value, ptr %regs, i32 0
    call void @rt_make_int(ptr %p0, i64 3)
    %p1 = getelementptr %Value, ptr %regs, i32 1
    call void @rt_make_int(ptr %p1, i64 4)
    %p2 = getelementptr %Value, ptr %regs, i32 2
    call void @rt_make_int(ptr %p2, i64 2)
    
    ; 実行
    %res = call %Value @pgo_main(ptr %regs)
    
    %res_ptr = alloca %Value
    store %Value %res, ptr %res_ptr
    %val = call i64 @rt_get_int(ptr %res_ptr)
    call void @rt_print_int(i64 %val)
    ret i32 0
  }
  """
    val llFile = new File("pgo_test.ll")
    val writer = new PrintWriter(llFile)
    writer.print(wrapper)
    writer.close()

    // 3. ネイティブコンパイル
    val isWin = System.getProperty("os.name").toLowerCase.contains("win")
    val exeName = if (isWin) "pgo_test.exe" else "pgo_test"

    Debug.logger.info(s"  Compiling with clang...")
    val compileResult = Process(Seq("clang", "-O3", "-Wno-override-module", "-o", exeName, "pgo_test.ll")).!
    if (compileResult != 0) throw new RuntimeException(s"Clang failed with $compileResult")

    // 4. 実行と検証
    Debug.logger.info(s">>> Step 3: Running PGO Binary ($exeName)...")
    val rawOutput = if (isWin) Seq(".\\" + exeName).!! else Seq("./" + exeName).!!
    val output = rawOutput.trim.linesIterator.toSeq.lastOption.getOrElse("")
    
    Debug.logger.info(s"  Output: '$output' (Expected: '14')")
    assert(output == "14", s"Output mismatch: expected '14', got '$output'")

    // クリーンアップ
    llFile.delete()
    if (new File(exeName).exists()) new File(exeName).delete()
    Debug.logger.info("\nPGO-AOT test completed successfully.")

  } catch {
    case e: Exception => 
      Debug.logger.error(s"  ! Error: ${e.getMessage}")
      throw e
  }
