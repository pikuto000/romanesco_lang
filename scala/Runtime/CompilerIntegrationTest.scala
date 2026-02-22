// ==========================================
// CompilerIntegrationTest.scala
// コンパイルから検証、実行までの統合テスト
// ==========================================

package romanesco.Runtime

import romanesco.Types._
import romanesco.Parser.rParser

@main def CompilerIntegrationTest(): Unit =
  val compiler = new BytecodeCompiler()
  val optimizer = new ResourceOptimizer() // Default: LinearHoTTLogic
  val vm = new VM()
  
  var passed = 0
  var failed = 0

  def testTree(name: String)(tree: Tree[(String, Vector[String])], expected: Value): Unit =
    try
      println(s"Testing: $name (Policy: ${optimizer.logic.name})")
      val rawOps = compiler.compile(tree)
      val optimizedOps = optimizer.optimize(rawOps)
      
      optimizer.verify(optimizedOps) match
        case Left(err) => 
          println(s"  ✗ Verification failed: $err")
          failed += 1
          return
        case Right(_) => 
          println("  ✓ Memory safety verified via linear logic")

      val result = vm.run(optimizedOps)
      if result == expected then
        passed += 1
        println(s"  ✓ Execution result matched: $result")
      else
        failed += 1
        println(s"  ✗ Execution result mismatch: expected $expected, but got $result")
    catch
      case e: Throwable =>
        failed += 1
        println(s"  ✗ Error: ${e.getMessage}")

  def testOps(name: String)(ops: Array[Op], expected: Option[Value]): Unit =
    try
      println(s"Testing (Ops): $name")
      optimizer.verify(ops) match
        case Left(err) =>
          if expected.isEmpty then
            passed += 1
            println(s"  ✓ Correctly rejected: $err")
          else
            failed += 1
            println(s"  ✗ Should have passed, but verification failed: $err")
        case Right(_) =>
          if expected.isEmpty then
            failed += 1
            println(s"  ✗ Should have failed, but verification passed")
          else
            val result = vm.run(ops)
            if result == expected.get then
              passed += 1
              println(s"  ✓ Execution result matched: $result")
            else
              failed += 1
              println(s"  ✗ Execution result mismatch: expected ${expected.get}, but got $result")
    catch
      case e: Throwable =>
        failed += 1
        println(s"  ✗ Error: ${e.getMessage}")

  println("=== Compiler Integration Test ===")

  // Test 1: (10 + 20) * 3
  val tree1 = Tree.V(("Mul", Vector.empty), Vector(
    Tree.V(("Add", Vector.empty), Vector(
      Tree.V(("Literal", Vector("10")), Vector.empty),
      Tree.V(("Literal", Vector("20")), Vector.empty)
    )),
    Tree.V(("Literal", Vector("3")), Vector.empty)
  ))
  testTree("算術演算") (tree1, Value.Atom(90L))

  // Test 2: Pair(1, 2).Proj1
  val tree2 = Tree.V(("Proj1", Vector.empty), Vector(
    Tree.V(("Pair", Vector.empty), Vector(
      Tree.V(("Literal", Vector("1")), Vector.empty),
      Tree.V(("Literal", Vector("2")), Vector.empty)
    ))
  ))
  testTree("ペア操作と自動解放") (tree2, Value.Atom(1))

  // Test 3: Let x = 42 in x
  val tree3 = Tree.V(("Let", Vector("x")), Vector(
    Tree.V(("Literal", Vector("42")), Vector.empty),
    Tree.V(("Var", Vector("x")), Vector.empty)
  ))
  testTree("変数とLetバインド") (tree3, Value.Atom(42))

  println("\n--- Borrowing Tests ---")

  testOps("借用: 破壊せずに読み取り") (
    Array(
      Op.LoadConst(0, Value.Atom(10)),
      Op.Borrow(1, 0),
      Op.Free(1),      // 借用証を破棄（参照カウントが0に戻る）
      Op.Add(2, 0, 0), // 10 + 10 (xを消費)
      Op.Return(2)
    ),
    Some(Value.Atom(20L))
  )

  testOps("借用失敗: 借用中に解放") (
    Array(
      Op.LoadConst(0, Value.Atom(10)),
      Op.Borrow(1, 0),
      Op.Free(0), // Error: r0 is borrowed by r1
      Op.Return(1)
    ),
    None // Expect failure
  )

  println(s"結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
