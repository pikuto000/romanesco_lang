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
        e.printStackTrace()

  def assertContains(ir: String, expected: String): Unit =
    if !ir.contains(expected) then
      throw AssertionError(s"IRに「$expected」が含まれていない\n--- 生成IR ---\n$ir")

  def assertNotContains(ir: String, unexpected: String): Unit =
    if ir.contains(unexpected) then
      throw AssertionError(s"IRに「$unexpected」が含まれるべきでない")

  println("=== LLVMCodeGenTest (Register Machine) ===")

  // --- 基本 ---

  test("モジュール構造: %Value型とランタイム宣言が含まれる") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.LoadConst(0, Value.Atom(42)), Op.Return(0)))
    assertContains(ir, "%Value = type { i8, ptr }")
    assertContains(ir, "declare %Value @rt_make_int(i64)")
    assertContains(ir, "define %Value @main()")
    assertContains(ir, "alloca %Value") // レジスタ確保
  }

  test("LoadConst(IntVal) + Return: rt_make_intの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(Op.LoadConst(0, Value.Atom(42)), Op.Return(0)))
    assertContains(ir, "@rt_make_int(i64 42)")
    assertContains(ir, "ret %Value")
  }

  test("LoadConst(Literal): 文字列リテラルとrt_make_literal") {
    val gen = new LLVMCodeGen()
    val ir =
      gen.generate(Array(Op.LoadConst(0, Value.Atom("hello")), Op.Return(0)))
    assertContains(ir, "c\"hello\\00\"")
    assertContains(ir, "@rt_make_literal")
  }

  // --- ペア ---

  test("MakePair: rt_make_pairの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.MakePair(2, 0, 1),
        Op.Return(2)
      )
    )
    assertContains(ir, "@rt_make_int(i64 1)")
    assertContains(ir, "@rt_make_int(i64 2)")
    assertContains(ir, "@rt_make_pair")
  }

  test("Proj1: rt_proj1の呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.LoadConst(1, Value.Atom(2)),
        Op.MakePair(2, 0, 1),
        Op.Proj1(3, 2),
        Op.Return(3)
      )
    )
    assertContains(ir, "@rt_proj1")
  }

  // --- クロージャ ---

  test("MakeClosure: 別関数定義 + rt_make_closure") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.Return(0))
    val ir = gen.generate(
      Array(
        Op.MakeClosure(0, idBody, Array.empty[Int], 1),
        Op.Return(0)
      )
    )
    assertContains(ir, "@rt_make_closure")
    assertContains(ir, "@rt_alloc_env")
    // クロージャ関数が別途定義されている
    assertContains(
      ir,
      "define %Value @__closure_0(ptr %env, ptr %args, i32 %num_args)"
    )
  }

  test("MakeClosure + Call: rt_call呼び出し") {
    val gen = new LLVMCodeGen()
    val idBody = Array[Op](Op.Return(0))
    val ir = gen.generate(
      Array(
        Op.MakeClosure(0, idBody, Array.empty[Int], 1),
        Op.LoadConst(1, Value.Atom(42)),
        Op.Call(2, 0, Array(1)),
        Op.Return(2)
      )
    )
    assertContains(ir, "@rt_call")
  }

  // --- Case ---

  test("Case: 分岐構造生成") {
    val gen = new LLVMCodeGen()
    val inlBody = Array[Op](Op.Return(0))
    val inrBody = Array[Op](Op.Return(0))
    val ir = gen.generate(
      Array(
        Op.LoadConst(0, Value.Atom(1)),
        Op.MakeInl(1, 0),
        Op.Case(2, 1, inlBody, inrBody),
        Op.Return(2)
      )
    )
    assertContains(ir, "br i1")
    assertContains(ir, "phi %Value")
    assertContains(ir, "@__closure_0_inl") // 分岐関数
    assertContains(ir, "@__closure_1_inr") // 分岐関数
  }

  test("Free: rt_free_valueの呼び出し") {
    val gen = new LLVMCodeGen()
    val ir = gen.generate(Array(
      Op.LoadConst(0, Value.Atom(1)),
      Op.Free(0),
      Op.Return(0)
    ))
    assertContains(ir, "call void @rt_free_value")
  }

  // --- lli実行テスト ---

  def runWithLli(code: Array[Op], expectedExitCode: Int): Unit =
    val gen = new LLVMCodeGen()
    // mainは%Valueを返すが、lliはi32を期待する。ラッパーを生成
    val ir =
      gen.generate(code, entryName = "romanesco_main", embedRuntime = true)
    // i32を返すmainラッパーを追加（IntValのpayloadを取り出してexit codeに）
    val wrapper = ir + """
  define i32 @main() {
    %v = call %Value @romanesco_main()
    %tag = extractvalue %Value %v, 0
    %is_int = icmp eq i8 %tag, 6
    br i1 %is_int, label %int_case, label %default_case
  int_case:
    %ptr = extractvalue %Value %v, 1
    %n = ptrtoint ptr %ptr to i64
    %r = trunc i64 %n to i32
    ret i32 %r
  default_case:
    ret i32 999
  }
  """ // 一時ファイルに書き出し
    val tmpFile = java.io.File.createTempFile("romanesco_", ".ll")
    tmpFile.deleteOnExit()
    val writer = new java.io.PrintWriter(tmpFile)
    writer.print(wrapper)
    writer.close()

    try
      val process =
        Runtime.getRuntime().exec(Array("lli", tmpFile.getAbsolutePath()))
      val exitCode = process.waitFor()
      if exitCode != expectedExitCode then
        throw AssertionError(
          s"lli exit code: 期待=$expectedExitCode, 実際=$exitCode"
        )
    catch
      case e: Throwable =>
        println("\n--- lli execution failed. Generated IR: ---")
        println(wrapper)
        println("-------------------------------------------")
        throw e

  // lliが存在するか確認
  val lliAvailable = try
    val p = Runtime.getRuntime().exec(Array("lli", "--version"))
    p.waitFor() == 0
  catch case _: Throwable => false

  if lliAvailable then
    println("\n--- lli実行テスト ---")

    test("lli: 整数定数42を返す") {
      runWithLli(Array(Op.LoadConst(0, Value.Atom(42)), Op.Return(0)), 42)
    }

    test("lli: pair(10, 20)のproj1 → 10") {
      runWithLli(
        Array(
          Op.LoadConst(0, Value.Atom(10)),
          Op.LoadConst(1, Value.Atom(20)),
          Op.MakePair(2, 0, 1),
          Op.Proj1(3, 2),
          Op.Return(3)
        ),
        10
      )
    }

    test("lli: 恒等関数 (λx.x)(7) → 7") {
      val idBody = Array[Op](Op.Return(0)) // 0は環境になければ引数0 (x)
      runWithLli(
        Array(
          Op.MakeClosure(0, idBody, Array.empty[Int], 1),
          Op.LoadConst(1, Value.Atom(7)),
          Op.Call(2, 0, Array(1)),
          Op.Return(2)
        ),
        7
      )
    }

    test("lli: 環境キャプチャ (λx. (λy. x))(10) -> (λy. 10)(20) -> 10") {
      // inner: return x (x is at env[0] = regs[0])
      val innerBody = Array[Op](Op.Return(0))

      // outer: return closure(inner) (captures x)
      // x is at args[0] -> regs[0]
      val outerBody = Array[Op](
        Op.MakeClosure(1, innerBody, Array(0), 1),
        Op.Return(1)
      )

      runWithLli(
        Array(
          Op.MakeClosure(0, outerBody, Array.empty[Int], 1),
          Op.LoadConst(1, Value.Atom(10)),
          Op.Call(2, 0, Array(1)), // returns inner closure
          Op.LoadConst(3, Value.Atom(20)), // dummy arg y
          Op.Call(4, 2, Array(3)), // call inner
          Op.Return(4) // should be 10
        ),
        10
      )
    }

    test("lli: 算術演算 (10 + 20) * 3 = 90") {
      runWithLli(
        Array(
          Op.LoadConst(0, Value.Atom(10)),
          Op.LoadConst(1, Value.Atom(20)),
          Op.Add(2, 0, 1),
          Op.LoadConst(3, Value.Atom(3)),
          Op.Mul(4, 2, 3),
          Op.Return(4)
        ),
        90
      )
    }

    test("ビット幅推論: 小さな値の加算は i8 で行われる") {
      val gen = new LLVMCodeGen()
      val ir = gen.generate(
        Array(
          Op.LoadConst(0, Value.Atom(10)),
          Op.LoadConst(1, Value.Atom(20)),
          Op.Add(2, 0, 1),
          Op.Return(2)
        )
      )
      assertContains(ir, "add i8")
      assertContains(ir, "trunc i64")
      assertContains(ir, "sext i8")
    }
  else println("\n--- lli実行テスト (スキップ: lliが見つからない) ---")

  println(s"\n結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
