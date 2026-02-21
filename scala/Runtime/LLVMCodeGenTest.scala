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
    val ir = gen.generate(Array(Op.PushConst(Value.Atom(42)), Op.Return))
    assertContains(ir, "%Value = type { i8, ptr }")
    assertContains(ir, "declare %Value @rt_make_int(i64)")
    assertContains(ir, "define %Value @main()")
  }

  test("PushConst(IntVal) + Return: rt_make_intの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.Atom(42)), Op.Return))
    assertContains(ir, "@rt_make_int(i64 42)")
    assertContains(ir, "ret %Value")
  }

  test("PushConst(Literal): 文字列リテラルとrt_make_literal") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.Atom("hello")), Op.Return))
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
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
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
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
      Op.MakePair,
      Op.Proj1,
      Op.Return
    ))
    assertContains(ir, "@rt_proj1")
  }

  test("Proj2: rt_proj2の呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.Atom(1)),
      Op.PushConst(Value.Atom(2)),
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
      Op.PushConst(Value.Atom(1)),
      Op.MakeInl,
      Op.Return
    ))
    assertContains(ir, "@rt_make_inl")
  }

  test("MakeInr: rt_make_inrの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.PushConst(Value.Atom(1)),
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
    assertContains(ir, "define %Value @__closure_0(ptr %env, %Value %arg)")
  }

  test("MakeClosure + Apply: rt_apply呼び出し") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.PushVar(0), Op.Return)
    val ir = gen.generate(Array(
      Op.MakeClosure(idBody, 1),
      Op.PushConst(Value.Atom(42)),
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
      Op.PushConst(Value.Atom(1)),
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

  // --- 埋め込みモード ---

  test("embedRuntime: ランタイム関数の定義が含まれる") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.Atom(42)), Op.Return), embedRuntime = true)
    assertContains(ir, "define %Value @rt_make_int(i64 %n)")
    assertContains(ir, "define %Value @rt_make_pair(%Value %a, %Value %b)")
    assertContains(ir, "define %Value @rt_apply(%Value %fn, %Value %arg)")
    assertContains(ir, "declare ptr @malloc(i64)")
    // declareではなくdefineになっている
    assertNotContains(ir, "declare %Value @rt_make_int")
  }

  test("embedRuntime: %Closure, %Pair型が定義されている") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.PushConst(Value.Atom(1)), Op.Return), embedRuntime = true)
    assertContains(ir, "%Closure = type")
    assertContains(ir, "%Pair = type")
  }

  // --- IR出力の確認 ---

  test("embedRuntime: 完全なIR出力（デバッグ用）") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.PushVar(0), Op.Return)
    val ir = gen.generate(Array(
      Op.PushConst(Value.Atom(42)),
      Op.MakeClosure(idBody, 1),
      Op.PushConst(Value.Atom(42)),
      Op.Apply,
      Op.Return
    ), embedRuntime = true)
    println("\n--- 埋め込みモードLLVM IR ---")
    println(ir)
    println("--- ここまで ---")
  }

  // --- lli実行テスト ---

  def runWithLli(code: Array[Op], expectedExitCode: Int): Unit =
    val gen = new LLVMCodeGen()
    // mainは%Valueを返すが、lliはi32を期待する。ラッパーを生成
    val ir = gen.generate(code, entryName = "romanesco_main", embedRuntime = true)
    // i32を返すmainラッパーを追加（IntValのpayloadを取り出してexit codeに）
    val wrapper = ir + """
define i32 @main() {
  %v = call %Value @romanesco_main()
  %tag = extractvalue %Value %v, 0
  %is_int = icmp eq i8 %tag, 0
  br i1 %is_int, label %int_case, label %default_case
int_case:
  %ptr = extractvalue %Value %v, 1
  %n = load i64, ptr %ptr
  %r = trunc i64 %n to i32
  ret i32 %r
default_case:
  ret i32 0
}
"""
    // 一時ファイルに書き出し
    val tmpFile = java.io.File.createTempFile("romanesco_", ".ll")
    tmpFile.deleteOnExit()
    val writer = new java.io.PrintWriter(tmpFile)
    writer.print(wrapper)
    writer.close()

    val process = Runtime.getRuntime().exec(Array("lli", tmpFile.getAbsolutePath()))
    val exitCode = process.waitFor()
    if exitCode != expectedExitCode then
      throw AssertionError(s"lli exit code: 期待=$expectedExitCode, 実際=$exitCode")

  // lliが存在するか確認
  val lliAvailable = try
    val p = Runtime.getRuntime().exec(Array("lli", "--version"))
    p.waitFor() == 0
  catch
    case _: Throwable => false

  if lliAvailable then
    println("\n--- lli実行テスト ---")

    test("lli: 整数定数42を返す") {
      runWithLli(Array(Op.PushConst(Value.Atom(42)), Op.Return), 42)
    }

    test("lli: pair(10, 20)のproj1 → 10") {
      runWithLli(Array(
        Op.PushConst(Value.Atom(10)),
        Op.PushConst(Value.Atom(20)),
        Op.MakePair,
        Op.Proj1,
        Op.Return
      ), 10)
    }

    test("lli: pair(10, 20)のproj2 → 20") {
      runWithLli(Array(
        Op.PushConst(Value.Atom(10)),
        Op.PushConst(Value.Atom(20)),
        Op.MakePair,
        Op.Proj2,
        Op.Return
      ), 20)
    }

    test("lli: 恒等関数 (λx.x)(7) → 7") {
      val idBody = Array[Op](Op.PushVar(0), Op.Return)
      runWithLli(Array(
        Op.MakeClosure(idBody, 1),
        Op.PushConst(Value.Atom(7)),
        Op.Apply,
        Op.Return
      ), 7)
    }
  else
    println("\n--- lli実行テスト (スキップ: lliが見つからない) ---")

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
