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
        e.printStackTrace()

  def assertEquals[T](actual: T, expected: T): Unit =
    if actual != expected then
      throw AssertionError(s"期待: $expected, 実際: $actual")

  println("=== ResourceAnalyzerTest (Register Machine) ===")

  // --- 基本 ---

  test("LoadConst + Return: 1リソース生成、ゴミなし") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(42)),
        Op.Return(0)
      )
    )
    // id=0 -> reg 0
    assertEquals(result.returnedResource, Some(0))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  test("Unit: freeの必要なし") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Unit),
        Op.Return(0)
      )
    )
    assertEquals(result.returnedResource, Some(0))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  // --- ペア ---

  test("MakePair + Return: ペアを返す、ゴミなし") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.MakePair(2, 0, 1),
        Op.Return(2)
      )
    )
    // r0(id=0), r1(id=1), r2(id=2)=Pair(0,1)
    // Return id=2.
    // id=0, id=1 are children of id=2, so they are live.
    assertEquals(result.returnedResource, Some(2))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  test("MakePair + Proj1: ペアの殻と第2要素がゴミ") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)), // id=0
        Op.LoadConst(1, Value.Atom(2)), // id=1
        Op.MakePair(2, 0, 1), // id=2 = Pair(0, 1)
        Op.Proj1(3, 2), // reg 3 = id=0. id=2 becomes Shell(1)
        Op.Return(3) // return id=0
      )
    )
    assertEquals(result.returnedResource, Some(0))
    // id=0 is live.
    // id=2 (Shell) is garbage.
    // id=1 is child of id=2, so garbage.
    assertEquals(result.garbageResources, Set(1, 2))
  }

  test("MakePair + Proj2: ペアの殻と第1要素がゴミ") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.MakePair(2, 0, 1),
        Op.Proj2(3, 2),
        Op.Return(3)
      )
    )
    // return id=1 (from reg 3 <- reg 2._2)
    // id=0, id=2 are garbage.
    assertEquals(result.returnedResource, Some(1))
    assertEquals(result.garbageResources, Set(0, 2))
  }

  // --- ネストしたペア ---

  test("pair(pair(1,2), 3)のProj1: 外側ペアの殻とr3がゴミ") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)), // id=0
        Op.LoadConst(1, Value.Atom(2)), // id=1
        Op.MakePair(2, 0, 1), // id=2 = Pair(0,1)
        Op.LoadConst(3, Value.Atom(3)), // id=3
        Op.MakePair(4, 2, 3), // id=4 = Pair(2,3)
        Op.Proj1(5, 4), // reg 5 = id=2. id=4 becomes Shell(3)
        Op.Return(5) // return id=2
      )
    )
    assertEquals(result.returnedResource, Some(2))
    // id=2 is live. id=0,1 are children of id=2 -> live.
    // id=4 (Shell) is garbage.
    // id=3 is child of id=4 -> garbage.
    assertEquals(result.garbageResources, Set(3, 4))
  }

  test("pair(pair(1,2), 3)のProj1.Proj2: r0, r2, r3, r4がゴミ") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)), // id=0
        Op.LoadConst(1, Value.Atom(2)), // id=1
        Op.MakePair(2, 0, 1), // id=2 = Pair(0,1)
        Op.LoadConst(3, Value.Atom(3)), // id=3
        Op.MakePair(4, 2, 3), // id=4 = Pair(2,3)
        Op.Proj1(5, 4), // reg 5 = id=2. id=4 -> Shell(3)
        Op.Proj2(6, 5), // reg 6 = id=1. id=2 -> Shell(0)
        Op.Return(6) // return id=1
      )
    )
    assertEquals(result.returnedResource, Some(1))
    // id=1 live.
    // Garbage:
    // id=0 (child of id=2)
    // id=2 (Shell, garbage)
    // id=3 (child of id=4)
    // id=4 (Shell, garbage)
    assertEquals(result.garbageResources, Set(0, 2, 3, 4))
  }

  // --- 余積 ---

  test("MakeInl + Return: ゴミなし") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)), // id=0
        Op.MakeInl(1, 0), // id=1 = Inl(0)
        Op.Return(1)
      )
    )
    assertEquals(result.returnedResource, Some(1))
    assertEquals(result.garbageResources, Set.empty[Int])
  }

  // --- 上書き (Pop相当) ---

  test("上書き: 古い値がゴミになる") {
    // reg 0 = 1
    // reg 0 = 2 (1 is overwritten)
    // return reg 0
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)), // id=0
        Op.LoadConst(0, Value.Atom(2)), // id=1. reg 0 refers to id=1 now.
        Op.Return(0)
      )
    )
    assertEquals(result.returnedResource, Some(1))
    // id=0 is garbage
    assertEquals(result.garbageResources, Set(0))
  }

  // --- ゴミ詳細 ---

  test("garbageDetails: リソースの種別が正しい") {
    val result = analyzer.analyze(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.MakePair(2, 0, 1),
        Op.Proj1(3, 2),
        Op.Return(3)
      )
    )
    // id=1, id=2 are garbage
    val details = result.garbageDetails
    assertEquals(details.size, 2)
    val r1 = details.find(_.id == 1).get
    val r2 = details.find(_.id == 2).get
    assertEquals(r1.kind, ResourceKind.Atom)
    assertEquals(r2.kind, ResourceKind.Shell)
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
