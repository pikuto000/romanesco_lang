// ==========================================
// OmniVerificationTest.scala
// 全論理体系を動員した網羅的検証テスト (Logger付き)
// ==========================================

package romanesco.Runtime

import romanesco.Types._
import romanesco.Utils.Debug

@main def OmniVerificationTest(): Unit =
  // Debug.logger.switch(true)
  Debug.logger.setLevel(Debug.logger.Level.DEBUG)
  Debug.logger.setMaxDepth(20)

  val optimizer = new ResourceOptimizer(new OmniResourceLogic())
  var passed = 0
  var failed = 0

  def verify(name: String, ops: Array[Op], expectSuccess: Boolean): Unit =
    println(s"\n>>> Test Case: $name")
    optimizer.verify(ops) match
      case Right(_) if expectSuccess =>
        passed += 1
        println("  ✓ Result: SUCCESS (Expected SUCCESS)")
      case Left(err) if !expectSuccess =>
        passed += 1
        println(s"  ✓ Result: FAILURE (Expected FAILURE) - Error: $err")
      case Right(_) =>
        failed += 1
        println("  ✗ Result: SUCCESS (Expected FAILURE)")
      case Left(err) =>
        failed += 1
        println(s"  ✗ Result: FAILURE (Expected SUCCESS) - Error: $err")

  println("=== Omni-Logic Verification Comprehensive Suite ===")

  // 1. 正常系: 算術演算
  verify(
    "算術演算 (HoTT中立ステップ)",
    Array(
      Op.LoadConst(0, Value.Atom(10)),
      Op.LoadConst(1, Value.Atom(20)),
      Op.Add(2, 0, 1),
      Op.Return(2)
    ),
    true
  )

  // 2. 正常系: ペア操作
  verify(
    "ペア操作 (線形性)",
    Array(
      Op.LoadConst(0, Value.Atom(1)),
      Op.LoadConst(1, Value.Atom(2)),
      Op.MakePair(2, 0, 1),
      Op.Proj1(3, 2), // r0 を reg 3 に、reg 2 は r1 が書き戻される
      Op.Free(3), // r0 を解放
      Op.Free(2), // r1 も解放 (これを忘れるとリーク！)
      Op.Return(3) // 空のレジスタを返しても良い(Unit的な扱い)
    ),
    true
  )

  // 3. 異常系: メモリリーク
  verify(
    "メモリリーク",
    Array(
      Op.LoadConst(0, Value.Atom(100)),
      Op.LoadConst(1, Value.Atom(200)),
      Op.Return(0) // r1 is leaked
    ),
    false
  )

  // 4. 異常系: 二重解放
  verify(
    "二重解放",
    Array(
      Op.LoadConst(0, Value.Atom(10)),
      Op.Free(0),
      Op.Free(0),
      Op.Return(0)
    ),
    false
  )

  // 5. 借用
  verify(
    "正常な借用",
    Array(
      Op.LoadConst(0, Value.Atom(10)),
      Op.Borrow(1, 0),
      Op.Return(0) // Borrow doesn't consume, so this is safe
    ),
    true
  )

  println(
    s"\nFinal Omni-Verification Results: $passed Passed, $failed Failed / ${passed + failed} Tests"
  )
  if failed > 0 then sys.exit(1)
