// ==========================================
// AdvancedJITTest.scala
// 再帰、和型、深層呼び出しの JIT 検証
// ==========================================

package romanesco.Runtime.LLVM_Test

import romanesco.Runtime._

@main def AdvancedJITTest(): Unit =
  println("=== Romanesco Advanced JIT Test Suite ===")
  val executor = new SpeculativeExecutor(hotThreshold = 3)

  // --- 1. ウォームアップと JIT 移行 ---
  println("\n>>> Test 1: Simple JIT Loop Warmup")
  val loopOps = Array(
    Op.LoadConst(0, Value.Atom(100)),
    Op.LoadConst(1, Value.Atom(200)),
    Op.Add(2, 0, 1),
    Op.Return(2)
  )

  for i <- 1 to 5 do
    val res = executor.execute(loopOps)
    println(s"    Run $i result: $res")

  // --- 2. 和型 (Sum Types / Option) ---
  // let x = Inl(10) in case x of Inl(v) -> v + 1 | Inr(v) -> v - 1
  println("\n>>> Test 2: Sum Types (Case) JIT")
  val opsSum = Array(
    Op.LoadConst(0, Value.Atom(10)),
    Op.MakeInl(1, 0), // regs[1] = Inl(10)
    Op.Case(
      2,
      1,
      inlBranch = Array(
        Op.LoadConst(3, Value.Atom(1)),
        Op.Add(2, 2, 3) // regs[2] に結果を書き込む
      ),
      inrBranch = Array(
        Op.LoadConst(3, Value.Atom(1)),
        Op.Sub(2, 2, 3)
      )
    ),
    Op.Return(2)
  )

  // 1回目 (VMで実行)
  val resSum1 = executor.execute(opsSum)
  println(s"  VM Run Result: $resSum1 (Expected: 11)")

  // ホットスポット化して JIT 実行
  for _ <- 1 to 3 do executor.execute(opsSum)
  val resSum2 = executor.execute(opsSum)
  println(s"  JIT Run Result: $resSum2 (Expected: 11)")
  assert(resSum2 == Value.Atom(11))

  // --- 3. 複合データ構造 (Nested Pairs & Sums) ---
  println("\n>>> Test 3: Complex Data Structures")
  val opsComplex = Array(
    Op.LoadConst(0, Value.Atom(42)),
    Op.MakeInr(1, 0), // Inr(42)
    Op.LoadConst(2, Value.Atom(7)),
    Op.MakePair(3, 1, 2), // (Inr(42), 7)
    Op.Proj1(4, 3), // Proj1((Inr(42), 7)) -> Inr(42)
    Op.Case(5, 4, inlBranch = Array.empty, inrBranch = Array.empty),
    Op.Return(5)
  )
  val resComplex = executor.execute(opsComplex)
  println(s"  Complex Result: $resComplex (Expected: 42)")
  assert(resComplex == Value.Atom(42))

  println("\nAll advanced JIT tests passed!")
