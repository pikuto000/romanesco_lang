// ==========================================
// SpeculativeJITTest.scala
// 投機的最適化と Deoptimization の検証
// ==========================================

package romanesco.Runtime.LLVM_Test

import romanesco.Runtime._

@main def SpeculativeJITTest(): Unit =
  println("=== Speculative JIT Engine Test ===")

  val jit = new LLVMJIT()
  val pvm = new ProfilingVM()

  // --- シナリオ 1: 整数加算の最適化 ---
  // (x + x) を計算するコード
  // pc 0: LoadConst(0, ?)
  // pc 1: Add(1, 0, 0)
  // pc 2: Return(1)
  def getOps(v: Value) = Array(
    Op.LoadConst(0, v),
    Op.Add(1, 0, 0),
    Op.Return(1)
  )

  println("\n>>> Step 1: Profiling with ProfilingVM (Int input)")
  val opsInt = getOps(Value.Atom(100))
  pvm.runWithProfiling(opsInt)
  println(s"  Profile Data:\n${pvm.profileData}")

  println("\n>>> Step 2: Speculative JIT (Should be optimized)")
  // プロファイル情報を渡して JIT
  val res1 = jit.run(opsInt, profile = Some(pvm.profileData))
  println(s"  Result: $res1 (Expected: 200)")
  assert(res1 == 200)

  // --- シナリオ 2: 投機失敗 (Deoptimization) ---
  println("\n>>> Step 3: Speculative JIT with WRONG type (Trigger Deopt)")
  // 同じプロファイル（Intを期待）を使いつつ、実際には Pair を渡すコードを JIT
  val opsPair = getOps(Value.PairVal(Value.Atom(1), Value.Atom(2)))

  try {
    jit.run(opsPair, profile = Some(pvm.profileData))
    println("  ! Error: Should have deoptimized but finished normally.")
  } catch {
    case e: RuntimeException
        if e.getMessage
          .contains("123") || e.getMessage.contains("Deoptimization") =>
      println(
        "  ✓ Success: Deoptimization triggered correctly (Exit code 123 detected)"
      )
    case e: Exception =>
      println(s"  ! Unexpected Error: ${e.getMessage}")
  }

  // --- シナリオ 3: シームレスな引き継ぎ (Deopt -> VM Resume) ---
  println("\n>>> Step 4: Seamless Handover (JIT Deopt -> VM Resume)")
  val executor = new SpeculativeExecutor(hotThreshold = 1)

  // 1回目: 実行してホットスポットにする (Int)
  executor.execute(opsInt)

  // 2回目: 型を変えて実行 (Trigger Deopt and Resume)
  // JITが走り、Addの直前でDeoptし、VMが残りを引き継ぐ
  println("  Executing with Pair (Expect Deopt & Handover)...")
  val resHandover = executor.execute(opsPair)
  println(s"  Final Result from Handover: $resHandover")

  println("\nSpeculative JIT tests completed.")
