// ==========================================
// VMTest.scala
// バイトコードインタプリタのテスト
// ==========================================

package romanesco.Runtime

@main def VMTest(): Unit =
  val vm = new VM()
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

  def assertEquals(actual: Value, expected: Value): Unit =
    if actual != expected then
      throw AssertionError(s"期待: $expected, 実際: $actual")

  println("=== VMTest ===")

  // --- 基本命令 ---

  test("PushConst + Return: 定数を返す") {
    val result = vm.run(Array(
      Op.PushConst(Value.Atom(42)),
      Op.Return
    ))
    assertEquals(result, Value.Atom(42))
  }

  test("PushConst: リテラルを返す") {
    val result = vm.run(Array(
      Op.PushConst(Value.Atom("hello"))
    ))
    assertEquals(result, Value.Atom("hello"))
  }

  test("空コード: Unitを返す") {
    val result = vm.run(Array.empty)
    assertEquals(result, Value.Unit)
  }

  // --- ペア ---

  test("MakePair + Proj1") {
    val result = vm.run(Array(
      Op.PushConst(Value.Atom("a")),
      Op.PushConst(Value.Atom("b")),
      Op.MakePair,
      Op.Proj1
    ))
    assertEquals(result, Value.Atom("a"))
  }

  test("MakePair + Proj2") {
    val result = vm.run(Array(
      Op.PushConst(Value.Atom("a")),
      Op.PushConst(Value.Atom("b")),
      Op.MakePair,
      Op.Proj2
    ))
    assertEquals(result, Value.Atom("b"))
  }

  test("ネストしたペア: pair(pair(1,2), 3)のProj1.Proj2") {
    val result = vm.run(Array(
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
      Op.MakePair,
      Op.PushConst(Value.Atom(3)),
      Op.MakePair,
      Op.Proj1,
      Op.Proj2
    ))
    assertEquals(result, Value.Atom(2))
  }

  // --- クロージャ ---

  test("恒等関数: (λx. x)(42) = 42") {
    val idBody = Array[Op](Op.PushVar(0), Op.Return)
    val result = vm.run(Array(
      Op.MakeClosure(idBody, 1),
      Op.PushConst(Value.Atom(42)),
      Op.Apply
    ))
    assertEquals(result, Value.Atom(42))
  }

  test("定数関数: (λx. 99)(42) = 99") {
    val constBody = Array[Op](Op.PushConst(Value.Atom(99)), Op.Return)
    val result = vm.run(Array(
      Op.MakeClosure(constBody, 1),
      Op.PushConst(Value.Atom(42)),
      Op.Apply
    ))
    assertEquals(result, Value.Atom(99))
  }

  test("クロージャのenv捕捉: 外側の変数を参照") {
    // λx. (λy. x)(0) を構築
    // 内側のλy. xは、envからxを参照する
    val innerBody = Array[Op](Op.PushVar(0), Op.Return) // env[0] = x
    val outerBody = Array[Op](
      Op.MakeClosure(innerBody, 1), // env = [x]
      Op.PushConst(Value.Atom(0)),
      Op.Apply,
      Op.Return
    )
    val result = vm.run(Array(
      Op.MakeClosure(outerBody, 1),
      Op.PushConst(Value.Atom("captured")),
      Op.Apply
    ))
    assertEquals(result, Value.Atom("captured"))
  }

  // --- 余積とCase ---

  test("Case(inl(x), f, g) = f(x)") {
    val fBody = Array[Op](Op.PushVar(0), Op.Return) // f = id
    val gBody = Array[Op](Op.PushConst(Value.Atom("wrong")), Op.Return)
    val result = vm.run(Array(
      Op.PushConst(Value.Atom("left_val")),
      Op.MakeInl,
      Op.MakeClosure(fBody, 1),
      Op.MakeClosure(gBody, 1),
      Op.Case(Array.empty, Array.empty)
    ))
    assertEquals(result, Value.Atom("left_val"))
  }

  test("Case(inr(y), f, g) = g(y)") {
    val fBody = Array[Op](Op.PushConst(Value.Atom("wrong")), Op.Return)
    val gBody = Array[Op](Op.PushVar(0), Op.Return) // g = id
    val result = vm.run(Array(
      Op.PushConst(Value.Atom("right_val")),
      Op.MakeInr,
      Op.MakeClosure(fBody, 1),
      Op.MakeClosure(gBody, 1),
      Op.Case(Array.empty, Array.empty)
    ))
    assertEquals(result, Value.Atom("right_val"))
  }

  // --- 部分適用 ---

  test("部分適用: (λx y. pair(x,y)) a b = pair(a,b)") {
    // body: locals = [...env, x, y], pair(x, y)
    val body = Array[Op](
      Op.PushVar(0), // x
      Op.PushVar(1), // y
      Op.MakePair,
      Op.Return
    )
    val result = vm.run(Array(
      Op.MakeClosure(body, 2),
      Op.PushConst(Value.Atom("a")),
      Op.Apply,  // 部分適用 → closure/1
      Op.PushConst(Value.Atom("b")),
      Op.Apply   // 完全適用
    ))
    assertEquals(result, Value.PairVal(Value.Atom("a"), Value.Atom("b")))
  }

  // --- エラーケース ---

  test("関数でない値へのApplyはエラー") {
    try
      vm.run(Array(
        Op.PushConst(Value.Atom(1)),
        Op.PushConst(Value.Atom(2)),
        Op.Apply
      ))
      throw AssertionError("例外が発生すべき")
    catch
      case _: VMError => () // 期待通り
  }

  test("Proj1: ペアでない値はエラー") {
    try
      vm.run(Array(
        Op.PushConst(Value.Atom(1)),
        Op.Proj1
      ))
      throw AssertionError("例外が発生すべき")
    catch
      case _: VMError => () // 期待通り
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
