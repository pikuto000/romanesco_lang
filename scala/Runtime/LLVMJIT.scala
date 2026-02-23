// ==========================================
// LLVMJIT.scala
// LLVM (clang -shared) + Project Panama (FFM API) ベースの高性能 JIT 実行エンジン
// ==========================================

package romanesco.Runtime

import java.io._
import scala.sys.process._
import java.nio.file.{Files, Paths, Path}
import java.lang.foreign._
import java.lang.invoke.{MethodHandle, MethodType}
import scala.collection.mutable.ArrayBuffer

class LLVMJIT:
  private val codeGen = new LLVMCodeGen()
  private val linker = Linker.nativeLinker()

  /** 
   * バイトコードを JIT コンパイルし、メモリ上で直接実行する
   * VM の regs を直接 LLVM へ渡すことが可能
   */
  def run(code: Array[Op], entryName: String = "romanesco_entry", profile: Option[ProfileData] = None, vmRegs: Option[ArrayBuffer[Value]] = None): Long =
    val ir = codeGen.generate(code, entryName = entryName, embedRuntime = true, profile = profile)
    
    val isWin = System.getProperty("os.name").toLowerCase.contains("win")
    val dllExt = if (isWin) ".dll" else ".so"
    
    val tmpDir = Paths.get(System.getProperty("java.io.tmpdir"))
    val prefix = "romanesco_jit_" + System.nanoTime()
    val llFile = Files.createTempFile(tmpDir, prefix, ".ll")
    val dllFile = tmpDir.resolve(prefix + dllExt)

    try {
      Files.writeString(llFile, ir)

      // Clang でコンパイル
      val compileCmd = Seq(
        "clang", "-O3", "-shared", 
        "-Wno-override-module",
        "-o", dllFile.toString, 
        llFile.toString
      )
      val compileResult = Process(compileCmd).!
      if (compileResult != 0) return runViaProcess(ir, entryName)

      // Panama で実行 (OSR/共有メモリ対応)
      runViaPanama(dllFile, entryName, vmRegs)

    } catch {
      case e: Exception =>
        // 123 系統のエラーは Deopt なのでそのまま投げる
        if (e.getMessage.contains("123")) throw e
        runViaProcess(ir, entryName)
    } finally {
      Files.deleteIfExists(llFile)
      Files.deleteIfExists(dllFile)
    }

  private def runViaPanama(dllPath: Path, entryName: String, vmRegs: Option[ArrayBuffer[Value]]): Long =
    using(Arena.ofConfined()) { arena =>
      val lookup = SymbolLookup.libraryLookup(dllPath, arena)
      val entrySym = lookup.find(entryName).orElseThrow(() => 
        new NoSuchElementException(s"Symbol $entryName not found in $dllPath")
      )

      // 関数の型定義: %Value func(ptr %regs)
      // Note: LLVM の %Value { i64, ptr } 戻り値は ABI によって
      // 直接返されるか、第1引数に隠しポインタとして渡される
      // ここでは、regs を介して値を返してもらうため、戻り値は無視し、regs を同期する
      val descriptor = FunctionDescriptor.of(NativeRuntime.VALUE_LAYOUT, ValueLayout.ADDRESS)
      val handle = linker.downcallHandle(entrySym, descriptor)

      val regCount = vmRegs.map(_.length).getOrElse(32)
      val nativeRegs = arena.allocate(NativeRuntime.VALUE_LAYOUT, regCount)
      
      // VM レジスタを Native へコピー
      vmRegs.foreach { regs =>
        for i <- 0 until regCount do
          NativeRuntime.syncToNative(regs(i), nativeRegs.asSlice(i * NativeRuntime.VALUE_LAYOUT.byteSize()))
      }

      // 実行！
      try {
        val resultValue = handle.invoke(nativeRegs).asInstanceOf[MemorySegment]
        
        // Native レジスタを VM へ書き戻す (逆同期)
        vmRegs.foreach { regs =>
          for i <- 0 until regCount do
            regs(i) = NativeRuntime.syncFromNative(nativeRegs.asSlice(i * NativeRuntime.VALUE_LAYOUT.byteSize()))
        }
        
        // 戻り値のタグが 6 (Int) ならその値を返す
        val tag = resultValue.get(ValueLayout.JAVA_LONG, 0)
        if (tag == 6) resultValue.get(ValueLayout.ADDRESS, 8).address()
        else 0L
      } catch {
        case e: Throwable =>
          // 終了コード 123 系統などの例外が発生した場合は適切に処理
          throw new RuntimeException(s"Panama Execution Error: ${e.getMessage}")
      }
    }

  private def runViaProcess(ir: String, entryName: String): Long =
    val wrapper = ir + s"""
define i32 @main() {
  %v_ptr = alloca %Value
  %v = call %Value @$entryName(ptr null)
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
      case e: RuntimeException => throw e
      case e: Exception => throw new RuntimeException(s"JIT Process execution failed: ${e.getMessage}")
    } finally {
      tmpFile.delete()
    }

  private def using[T <: AutoCloseable, R](resource: T)(block: T => R): R =
    try { block(resource) } finally { resource.close() }
