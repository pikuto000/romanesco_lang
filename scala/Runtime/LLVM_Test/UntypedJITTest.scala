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

  // --- シナリオ 5: ビット幅最適化の検証 ---
  Debug.logger.info("\n>>> Test 6: Bit-width Optimization & Propagation")
  // 100 + 100 = 200 (i8 は 127 までなので、本来 i16 以上が必要)
  // 解析が正しければ、Add(dst, l, r) で幅が広がり、正しく 200 が得られる
  val opsWidth = Array(
    Op.LoadConst(0, Value.Atom(100)), // i8 (100)
    Op.LoadConst(1, Value.Atom(100)), // i8 (100)
    Op.Add(2, 0, 1),                 // should be inferred as i16 or i64 (100+100=200)
    Op.Return(2)
  )

  val resWidth = jit.run(opsWidth, profile = None)
  Debug.logger.info(s"  Result (100+100): $resWidth")
  assert(resWidth.toString.toLong == 200)

  // 127 + 1 = 128 (i8 の境界)
  val opsBoundary = Array(
    Op.LoadConst(0, Value.Atom(127)),
    Op.LoadConst(1, Value.Atom(1)),
    Op.Add(2, 0, 1),
    Op.Return(2)
  )
  val resBoundary = jit.run(opsBoundary, profile = None)
  Debug.logger.info(s"  Result (127+1): $resBoundary")
  assert(resBoundary.toString.toLong == 128)

  // --- シナリオ 6: 任意ビット幅 (iN) の検証 ---
  Debug.logger.info("\n>>> Test 7: Arbitrary Bit-width (iN) Precision")
  // 3 + 3 = 6
  // 3 は i3 (011)、その加算結果は i4 (0110) と推論されるはず
  val opsExact = Array(
    Op.LoadConst(0, Value.Atom(3)),
    Op.LoadConst(1, Value.Atom(3)),
    Op.Add(2, 0, 1),
    Op.Return(2)
  )
  val resExact = jit.run(opsExact, profile = None)
  Debug.logger.info(s"  Result (3+3): $resExact")
  assert(resExact.toString.toLong == 6)

  // 15 + 1 = 16 (i5 の境界)
  val opsExactBound = Array(
    Op.LoadConst(0, Value.Atom(15)),
    Op.LoadConst(1, Value.Atom(1)),
    Op.Add(2, 0, 1),
    Op.Return(2)
  )
  val resExactBound = jit.run(opsExactBound, profile = None)
  Debug.logger.info(s"  Result (15+1): $resExactBound")
  assert(resExactBound.toString.toLong == 16)

  Debug.logger.info("\nAll arbitrary bit-width (iN) tests passed!")

