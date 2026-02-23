// ==========================================
// UntypedJITTest.scala
// 型無し環境における JIT コンパイルと実行の検証
// ==========================================

package romanesco.Runtime.LLVM_Test

import romanesco.Runtime._
import romanesco.Utils.Debug

@main def UntypedJITTest(): Unit =
  Debug.logger.switch(true)
  Debug.logger.setLevel(Debug.logger.Level.INFO)
  Debug.logger.info("=== Untyped JIT Engine Test ===")

  val jit = new LLVMJIT()
  val pvm = new ProfilingVM()

  // --- シナリオ 1: 整数加算の JIT ---
  def getOps(v: Value) = Array(
    Op.LoadConst(0, v),
    Op.Add(1, 0, 0),
    Op.Return(1)
  )

  Debug.logger.info("\n>>> Step 1: Profiling with ProfilingVM (Int input)")
  val opsInt = getOps(Value.Atom(100))
  pvm.runWithProfiling(opsInt)
  Debug.logger.log(s"  Profile Data:\n${pvm.profileData}")

  Debug.logger.info("\n>>> Step 2: Native JIT Execution")
  val res1 = jit.run(opsInt, profile = Some(pvm.profileData))
  Debug.logger.info(s"  Result: $res1 (Expected: 200)")
  assert(res1.toString.toLong == 200)

  // --- シナリオ 2: 型の区別がない計算 (Untyped behavior) ---
  Debug.logger.info("\n>>> Step 3: JIT with Pair input (Generic handling)")
  val opsPair = Array(
    Op.LoadConst(0, Value.Atom(1)),
    Op.LoadConst(1, Value.Atom(2)),
    Op.MakePair(2, 0, 1), // reg 2 = (1, 2)
    Op.Add(3, 2, 2),      // reg 3 = (1, 2) + (1, 2)
    Op.Return(3)
  )

  val resPair = jit.run(opsPair, profile = Some(pvm.profileData))
  Debug.logger.info(s"  Result with Pair: $resPair")
  assert(resPair.toString.toLong == 2)

  // --- シナリオ 3: 高負荷後の JIT 切り替え ---
  Debug.logger.info("\n>>> Step 4: Automatic JIT Transition")
  val executor = new SpeculativeExecutor(hotThreshold = 3)

  for (i <- 1 to 2) executor.execute(opsInt)
  
  Debug.logger.info("  Executing 3rd time (Should trigger JIT)...")
  val resFinal = executor.execute(opsInt)
  Debug.logger.info(s"  Final Result: $resFinal")
  assert(resFinal.toString.contains("200"))

  // --- シナリオ 4: 拡張プロファイリングの検証 ---
  Debug.logger.info("\n>>> Test 5: Extended Profiling (Branch, Call, Value)")
  val pvm2 = new ProfilingVM()
  
  val fBody = Array(Op.Add(1, 0, 0), Op.Return(1))
  val opsExt = Array(
    Op.LoadConst(0, Value.Atom(1)),
    Op.MakeInl(1, 0),
    Op.MakeClosure(2, fBody, Array(), 1),
    Op.Case(3, 1, 
      Array(Op.Call(4, 2, Array(0)), Op.Return(4)),
      Array(Op.Add(4, 0, 0), Op.Return(4))
    ),
    Op.Return(3)
  )

  pvm2.runWithProfiling(opsExt)
  val profile = pvm2.profileData.toString
  Debug.logger.info(s"  Ext Profile Data:\n$profile")
  
  assert(profile.contains("branches") && profile.contains("0 -> 1"))
  assert(profile.contains("calls"))
  assert(profile.contains("values"))

  Debug.logger.info("\nAll extended profiling tests passed!")

