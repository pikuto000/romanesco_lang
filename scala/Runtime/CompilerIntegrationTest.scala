// ==========================================
// CompilerIntegrationTest.scala
// コンパイルから検証、実行までの統合テスト
// ==========================================

package romanesco.Runtime

import romanesco.Types._
import romanesco.Parser.rParser

@main def CompilerIntegrationTest(): Unit =
  val compiler = new BytecodeCompiler()
  val optimizer = new ResourceOptimizer()
  val vm = new VM()

  var passed = 0
  var failed = 0

  def test(
      name: String
  )(tree: Tree[(String, Vector[String])], expected: Value): Unit =
    try
      println(s"Testing: $name")
      // 1. コンパイル
      val rawOps = compiler.compile(tree)
      println(s"  Compiled to ${rawOps.length} ops")

      // 2. 最適化 (Free挿入)
      val optimizedOps = optimizer.optimize(rawOps)
      println(s"  Optimized to ${optimizedOps.length} ops")

      // 3. 検証 (形式的証明)
      optimizer.verify(optimizedOps) match
        case Left(err) =>
          println(s"  ✗ Verification failed: $err")
          failed += 1
          return
        case Right(_) =>
          println("  ✓ Memory safety verified via linear logic")

      // 4. 実行 (VM)
      val result = vm.run(optimizedOps)
      if result == expected then
        passed += 1
        println(s"  ✓ Execution result matched: $result")
      else
        failed += 1
        println(
          s"  ✗ Execution result mismatch: expected $expected, but got $result"
        )

    catch
      case e: Throwable =>
        failed += 1
        println(s"  ✗ Error: ${e.getMessage}")
        e.printStackTrace()

  println("=== Compiler Integration Test ===")

  // 手動で ParseTree を構築

  // Test 1: (10 + 20) * 3
  val tree1 = Tree.V(
    ("Mul", Vector.empty),
    Vector(
      Tree.V(
        ("Add", Vector.empty),
        Vector(
          Tree.V(("Literal", Vector("10")), Vector.empty),
          Tree.V(("Literal", Vector("20")), Vector.empty)
        )
      ),
      Tree.V(("Literal", Vector("3")), Vector.empty)
    )
  )
  test("算術演算")(tree1, Value.Atom(90L))

  // Test 2: Pair(1, 2).Proj1
  val tree2 = Tree.V(
    ("Proj1", Vector.empty),
    Vector(
      Tree.V(
        ("Pair", Vector.empty),
        Vector(
          Tree.V(("Literal", Vector("1")), Vector.empty),
          Tree.V(("Literal", Vector("2")), Vector.empty)
        )
      )
    )
  )
  test("ペア操作と自動解放")(tree2, Value.Atom(1))

  // Test 3: Let x = 42 in x
  val tree3 = Tree.V(
    ("Let", Vector("x")),
    Vector(
      Tree.V(("Literal", Vector("42")), Vector.empty),
      Tree.V(("Var", Vector("x")), Vector.empty)
    )
  )
  test("変数とLetバインド")(tree3, Value.Atom(42))

  println(s"結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
