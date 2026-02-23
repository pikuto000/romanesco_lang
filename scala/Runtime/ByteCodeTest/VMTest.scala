// ==========================================
// VMTest.scala
// バイトコードインタプリタのテスト
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.ArrayBuffer

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
        e.printStackTrace()

  def assertEquals(actual: Value, expected: Value): Unit =
    if actual != expected then
      throw AssertionError(s"期待: $expected, 実際: $actual")

  println("=== VMTest (Register Machine) ===")

  // --- 基本命令 ---

  test("LoadConst + Return: 定数を返す") {
    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom(42)),
        Op.Return(0)
      )
    )
    assertEquals(result, Value.Atom(42))
  }

  test("LoadConst: リテラルを返す") {
    // Returnなしの場合、Unitが返る仕様だが、最後のセットされた値が返るわけではない。
    // VMの実装では retVal の初期値 Unit が返る。
    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom("hello"))
      )
    )
    assertEquals(result, Value.Unit)
  }

  test("LoadConst + Return: リテラル") {
    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom("hello")),
        Op.Return(0)
      )
    )
    assertEquals(result, Value.Atom("hello"))
  }

  // --- ペア ---

  test("MakePair + Proj1") {
    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom("a")),
        Op.LoadConst(1, Value.Atom("b")),
        Op.MakePair(2, 0, 1),
        Op.Proj1(3, 2),
        Op.Return(3)
      )
    )
    assertEquals(result, Value.Atom("a"))
  }

  test("MakePair + Proj2") {
    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom("a")),
        Op.LoadConst(1, Value.Atom("b")),
        Op.MakePair(2, 0, 1),
        Op.Proj2(3, 2),
        Op.Return(3)
      )
    )
    assertEquals(result, Value.Atom("b"))
  }

  test("ネストしたペア: pair(pair(1,2), 3)のProj1.Proj2") {
    // r0=1, r1=2
    // r2=(r0,r1)
    // r3=3
    // r4=(r2,r3)
    // r5=r4._1 -> r2
    // r6=r5._2 -> r1(2)
    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.MakePair(2, 0, 1),
        Op.LoadConst(3, Value.Atom(3)),
        Op.MakePair(4, 2, 3),
        Op.Proj1(5, 4),
        Op.Proj2(6, 5),
        Op.Return(6)
      )
    )
    assertEquals(result, Value.Atom(2))
  }

  // --- クロージャ ---

  test("恒等関数: (λx. x)(42) = 42") {
    // idBody: arg at regs[0] (env is empty)
    val idBody = Array[Op](Op.Return(0))
    val result = vm.run(
      Array(
        Op.MakeClosure(0, idBody, Array.empty[Int], 1),
        Op.LoadConst(1, Value.Atom(42)),
        Op.Call(2, 0, Array(1)),
        Op.Return(2)
      )
    )
    assertEquals(result, Value.Atom(42))
  }

  test("定数関数: (λx. 99)(42) = 99") {
    val constBody = Array[Op](Op.LoadConst(0, Value.Atom(99)), Op.Return(0))
    val result = vm.run(
      Array(
        Op.MakeClosure(0, constBody, Array.empty[Int], 1),
        Op.LoadConst(1, Value.Atom(42)),
        Op.Call(2, 0, Array(1)),
        Op.Return(2)
      )
    )
    assertEquals(result, Value.Atom(99))
  }

  test("クロージャのenv捕捉: 外側の変数を参照") {
    // outer: (λx. (λy. x)) (arg=0)
    // x is at regs[0] (env is empty)
    // inner: λy. x
    // inner captures env=[x]. inner args start at regs[1] (y)
    // x is at regs[0] inside inner.

    val innerBody = Array[Op](Op.Return(0))

    val outerBody = Array[Op](
      // MakeClosure captures x (reg 0)
      Op.MakeClosure(1, innerBody, Array(0), 1),
      Op.Return(1)
    )

    // main:
    // r0 = outerClosure
    // r1 = "captured"
    // r2 = outerClosure("captured") -> returns innerClosure
    // r3 = innerClosure(unit) -> should return "captured"
    val result = vm.run(
      Array(
        Op.MakeClosure(0, outerBody, Array.empty[Int], 1),
        Op.LoadConst(1, Value.Atom("captured")),
        Op.Call(2, 0, Array(1)),
        Op.LoadConst(3, Value.Unit),
        Op.Call(4, 2, Array(3)),
        Op.Return(4)
      )
    )
    assertEquals(result, Value.Atom("captured"))
  }

  // --- 余積とCase ---

  test("Case(inl(x), f, g) = result of f(x)") {
    // inlBranch receives x at the end of regs.
    // here main regs has: 0:str, 1:inl
    // inlBranch regs copy: 0:str, 1:inl, 2:x(str)
    val inlBranch = Array[Op](Op.Return(2))
    val inrBranch =
      Array[Op](Op.LoadConst(0, Value.Atom("wrong")), Op.Return(0))

    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom("left_val")),
        Op.MakeInl(1, 0),
        Op.Case(2, 1, inlBranch, inrBranch),
        Op.Return(2)
      )
    )
    assertEquals(result, Value.Atom("left_val"))
  }

  test("Case(inr(y), f, g) = result of g(y)") {
    val inlBranch =
      Array[Op](Op.LoadConst(0, Value.Atom("wrong")), Op.Return(0))
    val inrBranch = Array[Op](Op.Return(2))

    val result = vm.run(
      Array(
        Op.LoadConst(0, Value.Atom("right_val")),
        Op.MakeInr(1, 0),
        Op.Case(2, 1, inlBranch, inrBranch),
        Op.Return(2)
      )
    )
    assertEquals(result, Value.Atom("right_val"))
  }

  // --- エラーケース ---

  test("関数でない値へのCallはエラー") {
    try
      vm.run(
        Array(
          Op.LoadConst(0, Value.Atom(1)),
          Op.LoadConst(1, Value.Atom(2)),
          Op.Call(2, 0, Array(1)), // 1 is not a function
          Op.Return(2)
        )
      )
      throw AssertionError("例外が発生すべき")
    catch case _: VMError => () // 期待通り
  }

  test("Proj1: ペアでない値はエラー") {
    try
      vm.run(
        Array(
          Op.LoadConst(0, Value.Atom(1)),
          Op.Proj1(1, 0)
        )
      )
      throw AssertionError("例外が発生すべき")
    catch case _: VMError => () // 期待通り
  }

  // --- 無限レジスタ ---

  test("無限レジスタ有効: 大きなインデックスも使用可能") {
    val infiniteVM = new VM(infiniteRegisters = true)
    val result = infiniteVM.run(
      Array(
        Op.LoadConst(100, Value.Atom("far")),
        Op.Return(100)
      )
    )
    assertEquals(result, Value.Atom("far"))
  }

  test("無限レジスタ無効(デフォルト): 範囲外アクセスはエラー") {
    val fixedVM = new VM(initialRegSize = 32)
    try
      fixedVM.run(
        Array(
          Op.LoadConst(32, Value.Atom("overflow")),
          Op.Return(32)
        )
      )
      throw AssertionError("例外が発生すべき")
    catch case _: VMError => () // 期待通り
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
