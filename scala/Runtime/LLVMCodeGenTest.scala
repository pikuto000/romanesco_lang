// ==========================================
// LLVMCodeGenTest.scala
// LLVM IRコード生成のテスト
// ==========================================

package romanesco.Runtime

@main def LLVMCodeGenTest(): Unit =
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

  def assertContains(ir: String, expected: String): Unit =
    if !ir.contains(expected) then
      throw AssertionError(s"IRに「$expected」が含まれていない\n--- 生成IR ---\n$ir")

  def assertNotContains(ir: String, unexpected: String): Unit =
    if ir.contains(unexpected) then
      throw AssertionError(s"IRに「$unexpected」が含まれるべきでない")

  println("=== LLVMCodeGenTest ===")

  // --- 基本 ---

  test("モジュール構造: %Value型とランタイム宣言が含まれる") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.IntVal(42)), Op.Return))
    assertContains(ir, "%Value = type { i8, i8* }")
    assertContains(ir, "declare %Value @rt_make_int(i64)")
    assertContains(ir, "define %Value @main()")
  }

  test("PushConst(IntVal) + Return: rt_make_intの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.IntVal(42)), Op.Return))
    assertContains(ir, "@rt_make_int(i64 42)")
    assertContains(ir, "ret %Value")
  }

  test("PushConst(Literal): 文字列リテラルとrt_make_literal") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.Literal("hello")), Op.Return))
    assertContains(ir, "c\"hello\\00\"")
    assertContains(ir, "@rt_make_literal")
  }

  test("PushConst(Unit): rt_make_unit呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.Unit), Op.Return))
    assertContains(ir, "@rt_make_unit()")
  }

  // --- ペア ---

  test("MakePair: rt_make_pairの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.IntVal(1)),
      Op.PushConst(Value.IntVal(2)),
      Op.MakePair,
      Op.Return
    ))
    assertContains(ir, "@rt_make_int(i64 1)")
    assertContains(ir, "@rt_make_int(i64 2)")
    assertContains(ir, "@rt_make_pair")
  }

  test("Proj1: rt_proj1の呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.IntVal(1)),
      Op.PushConst(Value.IntVal(2)),
      Op.MakePair,
      Op.Proj1,
      Op.Return
    ))
    assertContains(ir, "@rt_proj1")
  }

  test("Proj2: rt_proj2の呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.IntVal(1)),
      Op.PushConst(Value.IntVal(2)),
      Op.MakePair,
      Op.Proj2,
      Op.Return
    ))
    assertContains(ir, "@rt_proj2")
  }

  // --- 余積 ---

  test("MakeInl: rt_make_inlの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.IntVal(1)),
      Op.MakeInl,
      Op.Return
    ))
    assertContains(ir, "@rt_make_inl")
  }

  test("MakeInr: rt_make_inrの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.IntVal(1)),
      Op.MakeInr,
      Op.Return
    ))
    assertContains(ir, "@rt_make_inr")
  }

  // --- クロージャ ---

  test("MakeClosure: 別関数定義 + rt_make_closure") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.PushVar(0), Op.Return)
    val ir = gen.generate(Array(
      Op.MakeClosure(idBody, 1),
      Op.Return
    ))
    assertContains(ir, "@rt_make_closure")
    assertContains(ir, "@rt_alloc_env")
    // クロージャ関数が別途定義されている
    assertContains(ir, "define %Value @__closure_0(i8* %env, %Value %arg)")
  }

  test("MakeClosure + Apply: rt_apply呼び出し") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.PushVar(0), Op.Return)
    val ir = gen.generate(Array(
      Op.MakeClosure(idBody, 1),
      Op.PushConst(Value.IntVal(42)),
      Op.Apply,
      Op.Return
    ))
    assertContains(ir, "@rt_apply")
  }

  // --- Case ---

  test("Case: 分岐構造（br + phi）が生成される") {
    val gen = new LLVMCodeGen()
    val fBody = Array[Op](Op.PushVar(0), Op.Return)
    val gBody = Array[Op](Op.PushVar(0), Op.Return)
    val ir = gen.generate(Array(
      Op.PushConst(Value.IntVal(1)),
      Op.MakeInl,
      Op.MakeClosure(fBody, 1),
      Op.MakeClosure(gBody, 1),
      Op.Case(Array.empty, Array.empty),
      Op.Return
    ))
    assertContains(ir, "@rt_get_tag")
    assertContains(ir, "icmp eq i8")
    assertContains(ir, "br i1")
    assertContains(ir, "phi %Value")
  }

  // --- IR出力の確認 ---

  test("生成IRの表示（デバッグ用）") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.PushVar(0), Op.Return)
    val ir = gen.generate(Array(
      Op.PushConst(Value.Literal("a")),
      Op.PushConst(Value.Literal("b")),
      Op.MakePair,
      Op.MakeClosure(idBody, 1),
      Op.PushConst(Value.IntVal(99)),
      Op.Apply,
      Op.MakePair,
      Op.Proj1,
      Op.Return
    ))
    println("\n--- 生成されたLLVM IR ---")
    println(ir)
    println("--- ここまで ---")
  }

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
