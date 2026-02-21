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
      Op.PushConst(Value.Atom(42)),
      Op.Return
    )
    val optimized = optimizer.optimize(code)
    assertEquals(optimized.length, code.length)
  }

  test("MakePair + Proj1: Free命令が挿入される") {
    val code = Array[Op](
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.Proj1,                      // r0を取り出し
      Op.Return
    )
    val optimized = optimizer.optimize(code)
    // r1とr2がゴミ → Free(1)とFree(2)が挿入されるはず
    val freeOps = optimized.collect { case Op.Free(rid) => rid }
    assertEquals(freeOps.toSet, Set(1, 2))
    // Free命令はReturn直前に挿入
    val returnIdx = optimized.lastIndexWhere {
      case Op.Return => true
      case _ => false
    }
    assertTrue(returnIdx > 0, "Returnが存在する")
    // Free命令はReturn直前
    for i <- (returnIdx - freeOps.length) until returnIdx do
      optimized(i) match
        case Op.Free(_) => ()
        case other => throw AssertionError(s"Return直前にFree以外: $other")
  }

  test("MakePair + Proj2: Free命令が挿入される") {
    val code = Array[Op](
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2
      Op.Proj2,                      // r1を取り出し
      Op.Return
    )
    val optimized = optimizer.optimize(code)
    val freeOps = optimized.collect { case Op.Free(rid) => rid }
    assertEquals(freeOps.toSet, Set(0, 2))
  }

  test("ネストペアのProj1.Proj2: 4つのゴミリソースがfreeされる") {
    val code = Array[Op](
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.PushConst(Value.Atom(3)),  // r3
      Op.MakePair,                   // r4 = Pair(r2, r3)
      Op.Proj1,                      // r2を取り出し
      Op.Proj2,                      // r1を取り出し
      Op.Return
    )
    val optimized = optimizer.optimize(code)
    val freeOps = optimized.collect { case Op.Free(rid) => rid }
    assertEquals(freeOps.toSet, Set(0, 2, 3, 4))
  }

  test("Popでゴミ: Free命令が挿入される") {
    val code = Array[Op](
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.Pop,                        // r1を捨てる
      Op.Return
    )
    val optimized = optimizer.optimize(code)
    val freeOps = optimized.collect { case Op.Free(rid) => rid }
    assertEquals(freeOps.toSet, Set(1))
  }

  // --- buildLinearContext ---

  test("buildLinearContext: Atom→ r ↦ Atom") {
    val analyzer = new ResourceAnalyzer()
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.Pop,
      Op.Return
    ))
    val ctx = optimizer.buildLinearContext(result)
    assertEquals(ctx.size, 1)
    // g1 : r1 ↦ Atom
    val (name, prop) = ctx.head
    assertEquals(name, "g1")
  }

  test("buildLinearContext: Shell→ r ↦ Shell(children)") {
    val analyzer = new ResourceAnalyzer()
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2
      Op.Proj1,                      // r0を取り出し、r2はShell
      Op.Return
    ))
    val ctx = optimizer.buildLinearContext(result)
    assertEquals(ctx.size, 2) // r1 (Atom) と r2 (Shell)
  }

  // --- buildFreeRules ---

  test("buildFreeRules: Atom用ルールが生成される") {
    val analyzer = new ResourceAnalyzer()
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
      Op.Pop,
      Op.Return
    ))
    val rules = optimizer.buildFreeRules(result)
    assertTrue(rules.nonEmpty, "ルールが生成される")
    assertTrue(rules.exists(_.name == "free-r1"), "free-r1ルールが存在する")
  }

  // --- verify ---

  test("verify: ゴミなし → Right(Nil)") {
    val code = Array[Op](
      Op.PushConst(Value.Atom(42)),
      Op.Return
    )
    val result = optimizer.verify(code)
    assertEquals(result, Right(Nil))
  }

  test("verify: 単純なゴミ → 検証結果を返す") {
    val code = Array[Op](
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
      Op.Pop,
      Op.Return
    )
    val result = optimizer.verify(code)
    println(s"    verify結果: $result")
    result match
      case Right(freeOrder) =>
        assertEquals(freeOrder, List(1))
      case Left(msg) =>
        throw AssertionError(s"検証失敗: $msg")
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
