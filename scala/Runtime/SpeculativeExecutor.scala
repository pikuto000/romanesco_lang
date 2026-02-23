// ==========================================
// SpeculativeExecutor.scala
// プロファイリングと JIT を動的に切り替える実行エンジン
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.ArrayBuffer

class SpeculativeExecutor(
  val hotThreshold: Int = 5,
  val vm: ProfilingVM = new ProfilingVM(),
  val jit: LLVMJIT = new LLVMJIT()
):
  private var jitCompiled = false
  private var jitEntryName: String = ""

  def execute(code: Array[Op]): Value =
    val entryPC = 0
    val prof = vm.profileData.get(entryPC)

    if jitCompiled then
       try {
         // OSR 対応: vm.regs を直接渡す
         val result = jit.run(code, entryName = jitEntryName, profile = Some(vm.profileData), vmRegs = Some(vm.currentRegs))
         Value.Atom(result.toInt)
       } catch {
         case e: Exception if e.getMessage.contains("Nonzero exit value:") =>
            val pattern = "Nonzero exit value: (\\d+)".r
            pattern.findFirstMatchIn(e.getMessage) match {
              case Some(m) =>
                val deoptPc = m.group(1).toInt - 123
                println(s"[SpeculativeExecutor] JIT Deoptimization at PC $deoptPc! Resuming in VM.")
                vm.runWithProfiling(code, startPc = deoptPc)
              case None => vm.runWithProfiling(code)
            }
         case e: Exception => vm.runWithProfiling(code)
       }
    else if prof.count >= hotThreshold && !jitCompiled then
      println(s"[SpeculativeExecutor] Hotspot detected (count=${prof.count}). Compiling...")
      try {
        jitEntryName = "romanesco_jit_entry_" + System.nanoTime()
        val result = jit.run(code, entryName = jitEntryName, profile = Some(vm.profileData), vmRegs = Some(vm.currentRegs))
        jitCompiled = true
        println(s"[SpeculativeExecutor] JIT execution successful. Result: $result")
        Value.Atom(result.toInt)
      } catch {
        case e: RuntimeException if e.getMessage.contains("Nonzero exit value:") =>
          val pattern = "Nonzero exit value: (\\d+)".r
          pattern.findFirstMatchIn(e.getMessage) match {
            case Some(m) =>
              val exitCode = m.group(1).toInt
              if (exitCode >= 123) {
                val deoptPc = exitCode - 123
                println(s"[SpeculativeExecutor] JIT Deoptimization at PC $deoptPc! Recovering state...")
                val regs = recoverStateFromLog(e.getMessage)
                vm.runWithProfiling(code, regsInput = Some(regs), startPc = deoptPc)
              } else {
                vm.runWithProfiling(code)
              }
            case None => vm.runWithProfiling(code)
          }
        case e: Exception =>
          println(s"[SpeculativeExecutor] JIT Error: ${e.getMessage}. Falling back to VM.")
          vm.runWithProfiling(code)
      }
    else
      vm.runWithProfiling(code)

  private def recoverStateFromLog(log: String): ArrayBuffer[Value] =
    val regsRes = ArrayBuffer.fill[Value](32)(Value.Unit)
    val lines = log.split("\n")
    var inDump = false
    val regPattern = "REG (\\d+) TAG (\\d+) VAL (0x[0-9a-fA-F]+|0|nil)".r

    for line <- lines do
      if (line.contains("DEOPT_STATE_START")) inDump = true
      else if (line.contains("DEOPT_STATE_END")) inDump = false
      else if (inDump) {
        regPattern.findFirstMatchIn(line) match {
          case Some(m) =>
            val idx = m.group(1).toInt
            val tag = m.group(2).toLong
            val hexVal = m.group(3)
            val longVal = if (hexVal.startsWith("0x")) java.lang.Long.parseUnsignedLong(hexVal.substring(2), 16)
                          else if (hexVal == "nil") 0L else hexVal.toLong
            
            val value = tag match
              case 6 => Value.Atom(longVal.toInt) // Int
              case 5 => Value.Unit
              case 2 => Value.Atom("<Pair>")
              case _ => Value.Unit
            
            if (idx < regsRes.length) regsRes(idx) = value
          case None => ()
        }
      }
    regsRes
