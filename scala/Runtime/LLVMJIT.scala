// ==========================================
// LLVMJIT.scala
// LLVM (clang -shared) + Project Panama (FFM API) ベースの高性能 JIT 実行エンジン
// ==========================================

package romanesco.Runtime

import java.io._
import scala.sys.process._
import java.nio.file.{Files, Paths, Path}
import java.lang.foreign._
import java.lang.invoke.MethodHandle

class LLVMJIT:
  private val codeGen = new LLVMCodeGen()

  /** 
   * バイトコードを JIT コンパイルし、メモリ上で直接実行する
   */
  def run(code: Array[Op], entryName: String = "romanesco_entry", profile: Option[ProfileData] = None): Long =
    val ir = codeGen.generate(code, entryName = entryName, embedRuntime = true, profile = profile)
    
    // システムのOSに合わせて拡張子を決定
    val isWin = System.getProperty("os.name").toLowerCase.contains("win")
    val dllExt = if (isWin) ".dll" else ".so"
    
    // 一時ファイル名の生成
    val tmpDir = Paths.get(System.getProperty("java.io.tmpdir"))
    val prefix = "romanesco_jit_" + System.nanoTime()
    val llFile = Files.createTempFile(tmpDir, prefix, ".ll")
    val dllFile = tmpDir.resolve(prefix + dllExt)

    try {
      // 1. IR を書き出し
      Files.writeString(llFile, ir)

      // 2. Clang で共有ライブラリとしてコンパイル
      val compileCmd = Seq(
        "clang", "-O3", "-shared", 
        "-Wno-override-module",
        "-o", dllFile.toString, 
        llFile.toString
      )
      val compileResult = Process(compileCmd).!
      if (compileResult != 0) {
        // コンパイル失敗時は lli 方式へフォールバック
        return runViaProcess(ir, entryName)
      }

      // 3. Panama でライブラリをロードして実行
      runViaPanama(dllFile, entryName)

    } catch {
      case e: Exception =>
        // Panama でのロードや実行に失敗した場合もフォールバック
        runViaProcess(ir, entryName)
    } finally {
      // クリーンアップ
      Files.deleteIfExists(llFile)
      Files.deleteIfExists(dllFile)
    }

  private def runViaPanama(dllPath: Path, entryName: String): Long =
    using(Arena.ofConfined()) { arena =>
      val lookup = SymbolLookup.libraryLookup(dllPath, arena)
      val entrySym = lookup.find(entryName).orElseThrow(() => 
        new NoSuchElementException(s"Symbol $entryName not found in $dllPath")
      )

      // LLVM の %Value { i64, ptr } を Java で受け取る
      // 簡易化のため、i64 を返すラッパーを IR 側で作るか、
      // ここでポインタを介して受け取る
      // 現在は rt_get_int を使ったプロセス経由の実行が安定しているため、
      // このメソッドは将来の拡張用（状態復元用）のプレースホルダとする
      throw new RuntimeException("Panama execution not fully implemented, falling back to process.")
    }

  private def runViaProcess(ir: String, entryName: String): Long =
    val wrapper = ir + s"""
define i32 @main() {
  %v_ptr = alloca %Value
  %v = call %Value @$entryName()
  store %Value %v, ptr %v_ptr
  %val = call i64 @rt_get_int(ptr %v_ptr)
  call void @rt_print_int(i64 %val)
  ret i32 0
}
"""
    val tmpFile = File.createTempFile("romanesco_jit_proc", ".ll")
    val pw = new PrintWriter(tmpFile)
    pw.print(wrapper)
    pw.close()
    
    try {
      val stdout = new StringBuilder()
      val logger = ProcessLogger(line => stdout.append(line + "\n"), line => stdout.append(line + "\n"))
      val exitCode = Process(s"lli -O3 ${tmpFile.getAbsolutePath}").!(logger)
      if (exitCode != 0) {
        throw new RuntimeException(s"JIT Process execution failed: Nonzero exit value: $exitCode\n${stdout.toString()}")
      }
      stdout.toString().trim.toLong
    } catch {
      case e: RuntimeException =>
        throw e
      case e: Exception =>
        throw new RuntimeException(s"JIT Process execution failed: ${e.getMessage}")
    } finally {
      tmpFile.delete()
    }

  private def using[T <: AutoCloseable, R](resource: T)(block: T => R): R =
    try { block(resource) } finally { resource.close() }

  def runAndPrint(code: Array[Op], name: String = "JIT Test"): Unit =
    println(s"--- JIT Execution: $name ---")
    val result = run(code)
    println(s"Result: $result")
