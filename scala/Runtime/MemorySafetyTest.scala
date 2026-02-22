// ==========================================
// MemorySafetyTest.scala
// 静的解析による自動解放の統合テスト
// ==========================================

package romanesco.Runtime

@main def MemorySafetyTest(): Unit =
  val optimizer = new ResourceOptimizer()
  val codegen = new LLVMCodeGen()
  
  var passed = 0
  var failed = 0

  def test(name: String)(code: Array[Op], expected: Int): Unit =
    try
      println(s"Testing: $name")
      // 1. 最適化 (Free挿入)
      val optimized = optimizer.optimize(code)
      println(s"  Optimized code length: ${optimized.length} (original: ${code.length})")
      
      // 2. IR生成
      val ir = codegen.generate(optimized, entryName = "romanesco_main", embedRuntime = true)
      
      // 3. lli実行
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
"""
      val tmpFile = java.io.File.createTempFile("romanesco_mem_", ".ll")
      tmpFile.deleteOnExit()
      val writer = new java.io.PrintWriter(tmpFile)
      writer.print(wrapper)
      writer.close()

      val process = Runtime.getRuntime().exec(Array("lli", tmpFile.getAbsolutePath()))
      val exitCode = process.waitFor()
      
      if exitCode == expected then
        passed += 1
        println(s"  ✓ $name: Passed (exit code $exitCode)")
      else
        failed += 1
        println(s"  ✗ $name: Failed (expected $expected, but got $exitCode)")
        
    catch
      case e: Throwable =>
        failed += 1
        println(s"  ✗ $name: Error - ${e.getMessage}")
        e.printStackTrace()

  println("=== MemorySafetyTest (Deterministic Free) ===")

  test("単純なペアの解放") (
    Array(
      Op.LoadConst(0, Value.Atom(10)),
      Op.LoadConst(1, Value.Atom(20)),
      Op.MakePair(2, 0, 1),
      Op.Proj1(3, 2),
      Op.Return(3)
    ),
    10
  )

  test("ネストしたペアの再帰的解放") (
    Array(
      Op.LoadConst(0, Value.Atom(1)),
      Op.LoadConst(1, Value.Atom(2)),
      Op.MakePair(2, 0, 1),
      Op.LoadConst(3, Value.Atom(3)),
      Op.MakePair(4, 2, 3), // pair(pair(1,2), 3)
      Op.Proj2(5, 4), // 3を取り出し
      Op.Return(5) 
    ),
    3
  )

  test("クロージャとその環境の解放") (
    Array(
      Op.LoadConst(0, Value.Atom(100)),
      // x=100 をキャプチャする恒等関数
      Op.MakeClosure(1, Array(Op.Return(0)), Array(0), 1),
      Op.LoadConst(2, Value.Unit),
      Op.Call(3, 1, Array(2)), 
      Op.Return(3)
    ),
    100
  )

  test("レジスタ上書き時の即時解放") (
    Array(
      Op.LoadConst(0, Value.Atom(10)), // id=0
      Op.LoadConst(0, Value.Atom(20)), // id=1, ここで id=0 が解放されるべき
      Op.Return(0)
    ),
    20
  )

  println(s"結果: $passed 成功, $failed 失敗 / ${passed + failed} テスト")
  if failed > 0 then sys.exit(1)
