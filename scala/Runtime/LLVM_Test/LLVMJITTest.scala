// ==========================================
// LLVMJITTest.scala
// LLVM JIT 実行の検証
// ==========================================

package romanesco.Runtime.LLVM_Test

import romanesco.Runtime._

@main def LLVMJITTest(): Unit =
  println("=== LLVM JIT Engine Test ===")
  val jit = new LLVMJIT()

  // テスト 1: 算術演算 (123 + 456 = 579)
  val ops1 = Array(
    Op.LoadConst(0, Value.Atom(123)),
    Op.LoadConst(1, Value.Atom(456)),
    Op.Add(2, 0, 1),
    Op.Return(2)
  )
  println(">>> Test 1: Simple Arithmetic")
  val res1 = jit.run(ops1)
  println(s"  Expected: 579, Got: $res1")
  assert(res1 == 579)

  // テスト 2: 複雑な算術 (10 * 10 - 5 = 95)
  val ops2 = Array(
    Op.LoadConst(0, Value.Atom(10)),
    Op.LoadConst(1, Value.Atom(10)),
    Op.Mul(2, 0, 1),
    Op.LoadConst(3, Value.Atom(5)),
    Op.Sub(4, 2, 3),
    Op.Return(4)
  )
  println(">>> Test 2: Complex Arithmetic")
  val res2 = jit.run(ops2)
  println(s"  Expected: 95, Got: $res2")
  assert(res2 == 95)

  // テスト 3: クロージャ (λx. x + 7)(3) = 10
  val body3 = Array(
    Op.LoadConst(1, Value.Atom(7)),
    Op.Add(2, 0, 1),
    Op.Return(2)
  )
  val ops3 = Array(
    Op.MakeClosure(0, body3, Array.empty, 1),
    Op.LoadConst(1, Value.Atom(3)),
    Op.Call(2, 0, Array(1)),
    Op.Return(2)
  )
  println(">>> Test 3: Closure JIT")
  val res3 = jit.run(ops3)
  println(s"  Expected: 10, Got: $res3")
  assert(res3 == 10)

  // テスト 4: ペアと射影 (let p = (100, 200) in proj1(p)) = 100
  val ops4 = Array(
    Op.LoadConst(0, Value.Atom(100)),
    Op.LoadConst(1, Value.Atom(200)),
    Op.MakePair(2, 0, 1),
    Op.Proj1(3, 2),
    Op.Return(3)
  )
  println(">>> Test 4: Pair and Proj1 JIT")
  val res4 = jit.run(ops4)
  println(s"  Expected: 100, Got: $res4")
  assert(res4 == 100)

  println("\nAll JIT tests passed!")
