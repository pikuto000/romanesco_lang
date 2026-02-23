// ==========================================
// SpeculativeExecutor.scala
// プロファイリングと JIT を動的に切り替える実行エンジン
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.ArrayBuffer

import romanesco.Utils.Debug

class SpeculativeExecutor(
  val hotThreshold: Int = 5,
  val vm: ProfilingVM = new ProfilingVM(),
  val jit: LLVMJIT = new LLVMJIT()
):
  private var jitCompiled = false
  private var jitEntryName: String = ""

  def execute(code: Array[Op]): Value =
    val entryPC = 0
    val prof = vm.profileData.get(code, entryPC)

    if jitCompiled then
       try {
         val result = jit.run(code, entryName = jitEntryName, profile = Some(vm.profileData), vmRegs = Some(vm.currentRegs))
         Value.Atom(result.toInt)
       } catch {
         case e: Exception => 
           Debug.logger.warn(s"[SpeculativeExecutor] JIT Execution Error: ${e.getMessage}. Falling back to VM.")
           vm.runWithProfiling(code)
       }
    else if prof.count >= hotThreshold && !jitCompiled then
      Debug.logger.info(s"[SpeculativeExecutor] Hotspot detected (count=${prof.count}). Compiling...")
      try {
        jitEntryName = "romanesco_jit_entry_" + System.nanoTime()
        val result = jit.run(code, entryName = jitEntryName, profile = Some(vm.profileData), vmRegs = Some(vm.currentRegs))
        jitCompiled = true
        Value.Atom(result.toInt)
      } catch {
        case e: Exception =>
          Debug.logger.warn(s"[SpeculativeExecutor] JIT Compilation/Execution Error: ${e.getMessage}. Falling back to VM.")
          vm.runWithProfiling(code)
      }
    else
      vm.runWithProfiling(code)
