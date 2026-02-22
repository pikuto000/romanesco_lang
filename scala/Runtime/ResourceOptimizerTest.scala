// ==========================================
// ResourceOptimizerTest.scala
// リソース最適化器のテスト
// ==========================================

package romanesco.Runtime

@main def ResourceOptimizerTest(): Unit =
  val optimizer = new ResourceOptimizer()
  var passed = 0
  var failed = 0

  def test(name: String)(body: => Unit): Unit =
    try
      body
      passed += 1
      println(s"  ✓ $name")
    catch
      case e: Throwable =>
        failed += 1
        println(s"  ✗ $name: ${e.getMessage}")

  def assertEquals[T](actual: T, expected: T): Unit =
    if actual != expected then
      throw AssertionError(s"期待: $expected, 実際: $actual")

  def assertTrue(cond: Boolean, msg: String = ""): Unit =
    if !cond then throw AssertionError(s"条件不成立: $msg")

  println("=== ResourceOptimizerTest ===")

  // --- optimize ---

  test("ゴミなし: コード変更なし") {
    val code = Array[Op](
      Op.LoadConst(0, Value.Atom(42)),
      Op.Return(0)
    )
    val optimized = optimizer.optimize(code)
    assertEquals(optimized.length, code.length)
  }

  test("MakePair + Proj1: Free命令が挿入される") {
    // r0 = 1, r1 = 2, r2 = (r0, r1), r3 = r2._1 (r0)
    // Garbage: id=1 (reg 1), id=2 (reg 2)
    val code = Array[Op](
      Op.LoadConst(0, Value.Atom(1)),
      Op.LoadConst(1, Value.Atom(2)),
      Op.MakePair(2, 0, 1),
      Op.Proj1(3, 2),
      Op.Return(3)
    )
    val optimized = optimizer.optimize(code)
    val freeOps = optimized.collect { case Op.Free(reg) => reg }
    // reg 2 (Proj1でsndIdが書き戻されたレジスタ) が解放される
    assertEquals(freeOps.toSet, Set(2))

    val returnIdx = optimized.lastIndexWhere {
      case Op.Return(_) => true
      case _            => false
    }
    assertTrue(returnIdx > 0, "Returnが存在する")
  }

  test("未使用変数: Free命令が挿入される") {
    val code = Array[Op](
      Op.LoadConst(0, Value.Atom(1)),
      Op.LoadConst(1, Value.Atom(2)),
      Op.Return(0)
    )
    val optimized = optimizer.optimize(code)
    val freeOps = optimized.collect { case Op.Free(reg) => reg }
    assertEquals(freeOps.toSet, Set(1))
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
