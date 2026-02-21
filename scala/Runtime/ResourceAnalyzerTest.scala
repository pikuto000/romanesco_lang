// ==========================================
// ResourceAnalyzerTest.scala
// リソース解析のテスト
// ==========================================

package romanesco.Runtime

@main def ResourceAnalyzerTest(): Unit =
  val analyzer = new ResourceAnalyzer()
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

  println("=== ResourceAnalyzerTest ===")

  // --- 基本 ---

  test("PushConst + Return: 1リソース生成、ゴミなし") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(42)),
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(0))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  test("Unit: freeの必要なし") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Unit),
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(0))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  // --- ペア ---

  test("MakePair + Return: ペアを返す、ゴミなし") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
      Op.MakePair,
      Op.Return
    ))
    // r0=Atom, r1=Atom, r2=Pair(r0,r1) → r2を返す、r0,r1はr2の子なので生存
    assertEquals(result.returnedResource, Some(2))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  test("MakePair + Proj1: ペアの殻と第2要素がゴミ") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.Proj1,                      // r0を取り出し、r2はShell(r1)
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(0))
    // r1 (取り出されなかった方) と r2 (殻) がゴミ
    assertEquals(result.garbageResources, Set(1, 2))
  }

  test("MakePair + Proj2: ペアの殻と第1要素がゴミ") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.Proj2,                      // r1を取り出し
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(1))
    assertEquals(result.garbageResources, Set(0, 2))
  }

  // --- ネストしたペア ---

  test("pair(pair(1,2), 3)のProj1: 外側ペアの殻とr3がゴミ") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.PushConst(Value.Atom(3)),  // r3
      Op.MakePair,                   // r4 = Pair(r2, r3)
      Op.Proj1,                      // r2を取り出し
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(2))
    // r2は生存（返り値）、r0とr1はr2の子で生存
    // r3とr4がゴミ
    assertEquals(result.garbageResources, Set(3, 4))
  }

  test("pair(pair(1,2), 3)のProj1.Proj2: r0, r2, r3, r4がゴミ") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.PushConst(Value.Atom(3)),  // r3
      Op.MakePair,                   // r4 = Pair(r2, r3)
      Op.Proj1,                      // r2を取り出し（r4はShell(r3)）
      Op.Proj2,                      // r1を取り出し（r2はShell(r0)）
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(1))
    // r1のみ生存。r0, r2(殻), r3, r4(殻) がゴミ
    assertEquals(result.garbageResources, Set(0, 2, 3, 4))
  }

  // --- 余積 ---

  test("MakeInl + Return: ゴミなし") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.MakeInl,                    // r1 = Inl(r0)
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(1))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  // --- Pop ---

  test("Pop: 捨てた値がゴミになる") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.Pop,                        // r1を捨てる
      Op.Return
    ))
    assertEquals(result.returnedResource, Some(0))
    assertEquals(result.garbageResources, Set(1))
  }

  // --- ゴミ詳細 ---

  test("garbageDetails: リソースの種別が正しい") {
    val result = analyzer.analyze(Array(
      Op.PushConst(Value.Atom(1)),  // r0
      Op.PushConst(Value.Atom(2)),  // r1
      Op.MakePair,                   // r2 = Pair(r0, r1)
      Op.Proj1,                      // r0を取り出し
      Op.Return
    ))
    val details = result.garbageDetails
    assertEquals(details.size, 2)
    val r1 = details.find(_.id == 1).get
    val r2 = details.find(_.id == 2).get
    assertEquals(r1.kind, ResourceKind.Atom)
    assertEquals(r2.kind, ResourceKind.Shell)
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
