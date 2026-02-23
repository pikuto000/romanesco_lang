// ==========================================
// UntypedJITTest.scala
// 型無し環境における JIT コンパイルと実行の検証
// ==========================================

package romanesco.Runtime.LLVM_Test

import romanesco.Runtime._

@main def UntypedJITTest(): Unit =
  println("=== Untyped JIT Engine Test ===")

  val jit = new LLVMJIT()
  val pvm = new ProfilingVM()

  // --- シナリオ 1: 整数加算の JIT ---
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
  println(s"  Profile Data (Execution count only):\n${pvm.profileData}")

  println("\n>>> Step 2: Native JIT Execution")
  // プロファイル情報を渡すが、型最適化は行われない
  val res1 = jit.run(opsInt, profile = Some(pvm.profileData))
  println(s"  Result: $res1 (Expected: 200)")
  assert(res1.toString.toLong == 200)

    // --- シナリオ 2: 型の区別がない計算 (Untyped behavior) ---
    println("\n>>> Step 3: JIT with Pair input (Generic handling)")
    // 以前は LoadConst で直接 Pair を渡そうとしていましたが、
    // LLVMCodeGen の LoadConst は Atom のみに対応しているため、
    // 命令を組み合わせて Pair を構築するように変更します。
    val opsPair = Array(
      Op.LoadConst(0, Value.Atom(1)),
      Op.LoadConst(1, Value.Atom(2)),
      Op.MakePair(2, 0, 1), // reg 2 = (1, 2)
      Op.Add(3, 2, 2),      // reg 3 = (1, 2) + (1, 2)
      Op.Return(3)
    )
  
    // 型無しランタイムでは そのまま実行され、rt_get_int が Pair から値を抽出する
    val resPair = jit.run(opsPair, profile = Some(pvm.profileData))
    println(s"  Result with Pair: $resPair")
    println("  (Note: rt_get_int extracted the first element 1 from each pair, so 1 + 1 = 2 is expected)")
    assert(resPair.toString.toLong == 2)
  

  // --- シナリオ 3: 高負荷後の JIT 切り替え ---
  println("\n>>> Step 4: Automatic JIT Transition")
  val executor = new SpeculativeExecutor(hotThreshold = 3)

  // 数回実行してホットにする
  for (i <- 1 to 2) executor.execute(opsInt)

  println("  Executing 3rd time (Should trigger JIT)...")
  val resFinal = executor.execute(opsInt)
  println(s"  Final Result: $resFinal")
  assert(resFinal.toString.contains("200"))

  println("\nUntyped JIT tests completed successfully.")
