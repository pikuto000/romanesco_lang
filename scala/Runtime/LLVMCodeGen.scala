// ==========================================
// LLVMCodeGen.scala
// Op（バイトコード）→ LLVM IRテキスト生成
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/** LLVM IR生成時のエラー */
class CodeGenError(msg: String) extends RuntimeException(msg)

/** SSAレジスタ名 */
case class Reg(id: Int):
  override def toString: String = s"%$id"

/** LLVM IRコード生成器 */
class LLVMCodeGen:
  private var regCounter = 0
  private var labelCounter = 0
  private var closureCounter = 0
  private val functions = new ArrayBuffer[String]()    // 生成された関数定義
  private val stringLiterals = new ArrayBuffer[(String, String)]() // (名前, 値)

  private def freshReg(): Reg =
    val r = Reg(regCounter)
    regCounter += 1
    r

  private def freshLabel(prefix: String): String =
    val l = s"${prefix}_$labelCounter"
    labelCounter += 1
    l

  private def freshClosureName(): String =
    val n = s"@__closure_$closureCounter"
    closureCounter += 1
    n

  private def registerStringLiteral(s: String): String =
    // 既存のリテラルを探す
    stringLiterals.find(_._2 == s) match
      case Some((name, _)) => name
      case None =>
        val name = s"@.str.${stringLiterals.size}"
        stringLiterals += ((name, s))
        name

  /** バイトコード列からLLVM IRモジュール全体を生成 */
  def generate(code: Array[Op], entryName: String = "main"): String =
    regCounter = 0
    labelCounter = 0
    closureCounter = 0
    functions.clear()
    stringLiterals.clear()

    // エントリ関数をコンパイル
    val entryBody = compileFunction(code, entryName, Nil)
    functions += entryBody

    buildModule()

  /** Op列を1つのLLVM IR関数にコンパイル */
  private def compileFunction(
      code: Array[Op],
      name: String,
      params: List[String]
  ): String =
    val savedReg = regCounter
    regCounter = 0

    val lines = new ArrayBuffer[String]()
    val stack = new ArrayBuffer[String]() // SSAレジスタ名のスタック

    // パラメータを仮想スタック用の変数テーブルに入れる
    val locals = new ArrayBuffer[String]()
    params.foreach(p => locals += p)

    def emit(line: String): Unit = lines += s"  $line"

    def pushStack(reg: String): Unit = stack += reg

    def popStack(): String =
      if stack.isEmpty then throw CodeGenError("コンパイル時スタックが空")
      stack.remove(stack.size - 1)

    for op <- code do
      op match
        case Op.PushConst(Value.IntVal(n)) =>
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_int(i64 $n)")
          pushStack(r.toString)

        case Op.PushConst(Value.Literal(s)) =>
          val strName = registerStringLiteral(s)
          val strLen = s.length
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_literal(i8* getelementptr inbounds ([${strLen + 1} x i8], [${strLen + 1} x i8]* $strName, i64 0, i64 0))")
          pushStack(r.toString)

        case Op.PushConst(Value.Unit) =>
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_unit()")
          pushStack(r.toString)

        case Op.PushConst(v) =>
          throw CodeGenError(s"未対応の定数型: $v")

        case Op.PushVar(index) =>
          if index >= locals.size then
            throw CodeGenError(s"ローカル変数インデックス範囲外: $index")
          pushStack(locals(index))

        case Op.StoreVar(index) =>
          val v = popStack()
          while locals.size <= index do locals += ""
          locals(index) = v

        case Op.MakeClosure(body, arity) =>
          val closureName = freshClosureName()
          // クロージャのbodyを別関数としてコンパイル
          // 引数: env (i8*), arg (%Value)
          val closureParams = locals.toList ++ List("%arg")
          val closureBody = compileClosureFunction(body, closureName, locals.toList, arity)
          functions += closureBody
          // 環境の生成
          val envSize = locals.size
          val envR = freshReg()
          emit(s"$envR = call i8* @rt_alloc_env(i32 $envSize)")
          // 環境に現在のローカルを保存
          for (local, i) <- locals.zipWithIndex do
            emit(s"call void @rt_env_store(i8* $envR, i32 $i, %Value $local)")
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_closure(i8* bitcast (%Value (i8*, %Value)* $closureName to i8*), i8* $envR, i32 $arity)")
          pushStack(r.toString)

        case Op.Apply =>
          val arg = popStack()
          val fn = popStack()
          val r = freshReg()
          emit(s"$r = call %Value @rt_apply(%Value $fn, %Value $arg)")
          pushStack(r.toString)

        case Op.Return =>
          if stack.nonEmpty then
            val v = popStack()
            emit(s"ret %Value $v")
          else
            emit(s"ret %Value zeroinitializer")

        case Op.MakePair =>
          val snd = popStack()
          val fst = popStack()
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_pair(%Value $fst, %Value $snd)")
          pushStack(r.toString)

        case Op.Proj1 =>
          val p = popStack()
          val r = freshReg()
          emit(s"$r = call %Value @rt_proj1(%Value $p)")
          pushStack(r.toString)

        case Op.Proj2 =>
          val p = popStack()
          val r = freshReg()
          emit(s"$r = call %Value @rt_proj2(%Value $p)")
          pushStack(r.toString)

        case Op.MakeInl =>
          val v = popStack()
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_inl(%Value $v)")
          pushStack(r.toString)

        case Op.MakeInr =>
          val v = popStack()
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_inr(%Value $v)")
          pushStack(r.toString)

        case Op.Case(inlBranch, inrBranch) =>
          val gFn = popStack()
          val fFn = popStack()
          val scrutinee = popStack()

          val tagR = freshReg()
          emit(s"$tagR = call i8 @rt_get_tag(%Value $scrutinee)")
          val innerR = freshReg()
          emit(s"$innerR = call %Value @rt_get_inner(%Value $scrutinee)")

          val inlLabel = freshLabel("case_inl")
          val inrLabel = freshLabel("case_inr")
          val endLabel = freshLabel("case_end")

          // tag == 4 → Inl, tag == 5 → Inr
          val cmpR = freshReg()
          emit(s"$cmpR = icmp eq i8 $tagR, 4")
          emit(s"br i1 $cmpR, label %$inlLabel, label %$inrLabel")

          // Inlブランチ
          lines += s"$inlLabel:"
          val inlResult = freshReg()
          emit(s"$inlResult = call %Value @rt_apply(%Value $fFn, %Value $innerR)")
          emit(s"br label %$endLabel")

          // Inrブランチ
          lines += s"$inrLabel:"
          val inrResult = freshReg()
          emit(s"$inrResult = call %Value @rt_apply(%Value $gFn, %Value $innerR)")
          emit(s"br label %$endLabel")

          // 合流
          lines += s"$endLabel:"
          val phiR = freshReg()
          emit(s"$phiR = phi %Value [ $inlResult, %$inlLabel ], [ $inrResult, %$inrLabel ]")
          pushStack(phiR.toString)

        case Op.Pop =>
          popStack()

    // 暗黙のreturn（Returnが無い場合）
    if code.isEmpty || code.last != Op.Return then
      if stack.nonEmpty then
        val v = popStack()
        emit(s"ret %Value $v")
      else
        emit(s"ret %Value zeroinitializer")

    regCounter = savedReg + regCounter

    val paramStr = if params.isEmpty then "" else params.map(p => s"%Value $p").mkString(", ")
    val header = if name == "main" || name.startsWith("@") then
      val cleanName = if name.startsWith("@") then name.drop(1) else name
      s"define %Value @$cleanName($paramStr) {"
    else
      s"define %Value @$name($paramStr) {"

    s"$header\nentry:\n${lines.mkString("\n")}\n}"

  /** クロージャ用関数をコンパイル（引数: env pointer, arg） */
  private def compileClosureFunction(
      body: Array[Op],
      name: String,
      capturedLocals: List[String],
      arity: Int
  ): String =
    val savedReg = regCounter
    regCounter = 0

    val lines = new ArrayBuffer[String]()
    val stack = new ArrayBuffer[String]()
    val locals = new ArrayBuffer[String]()

    def emit(line: String): Unit = lines += s"  $line"

    // 環境からキャプチャされた変数を復元
    for i <- capturedLocals.indices do
      val r = freshReg()
      emit(s"$r = call %Value @rt_env_load(i8* %env, i32 $i)")
      locals += r.toString

    // 引数を追加
    locals += "%arg"

    // body をコンパイル（再帰的にcompileFunctionを使わず直接展開）
    for op <- body do
      op match
        case Op.PushConst(Value.IntVal(n)) =>
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_int(i64 $n)")
          stack += r.toString

        case Op.PushConst(Value.Literal(s)) =>
          val strName = registerStringLiteral(s)
          val strLen = s.length
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_literal(i8* getelementptr inbounds ([${strLen + 1} x i8], [${strLen + 1} x i8]* $strName, i64 0, i64 0))")
          stack += r.toString

        case Op.PushConst(Value.Unit) =>
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_unit()")
          stack += r.toString

        case Op.PushVar(index) =>
          if index >= locals.size then
            throw CodeGenError(s"クロージャ内変数インデックス範囲外: $index")
          stack += locals(index)

        case Op.MakePair =>
          val snd = stack.remove(stack.size - 1)
          val fst = stack.remove(stack.size - 1)
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_pair(%Value $fst, %Value $snd)")
          stack += r.toString

        case Op.Proj1 =>
          val p = stack.remove(stack.size - 1)
          val r = freshReg()
          emit(s"$r = call %Value @rt_proj1(%Value $p)")
          stack += r.toString

        case Op.Proj2 =>
          val p = stack.remove(stack.size - 1)
          val r = freshReg()
          emit(s"$r = call %Value @rt_proj2(%Value $p)")
          stack += r.toString

        case Op.Apply =>
          val arg = stack.remove(stack.size - 1)
          val fn = stack.remove(stack.size - 1)
          val r = freshReg()
          emit(s"$r = call %Value @rt_apply(%Value $fn, %Value $arg)")
          stack += r.toString

        case Op.Return =>
          if stack.nonEmpty then
            val v = stack.remove(stack.size - 1)
            emit(s"ret %Value $v")
          else
            emit(s"ret %Value zeroinitializer")

        case _ =>
          throw CodeGenError(s"クロージャ内で未対応の命令: $op")

    // 暗黙のreturn
    if body.isEmpty || body.last != Op.Return then
      if stack.nonEmpty then
        val v = stack.remove(stack.size - 1)
        emit(s"ret %Value $v")
      else
        emit(s"ret %Value zeroinitializer")

    regCounter = savedReg + regCounter

    val cleanName = if name.startsWith("@") then name.drop(1) else name
    s"define %Value @$cleanName(i8* %env, %Value %arg) {\nentry:\n${lines.mkString("\n")}\n}"

  /** モジュール全体を組み立て */
  private def buildModule(): String =
    val sb = new StringBuilder()

    sb ++= "; ==========================================\n"
    sb ++= "; Romanesco LLVM IR (自動生成)\n"
    sb ++= "; ==========================================\n\n"

    // 値型の定義
    sb ++= "; 値型: {tag: i8, payload: i8*}\n"
    sb ++= "%Value = type { i8, i8* }\n\n"

    // 文字列リテラル
    if stringLiterals.nonEmpty then
      sb ++= "; 文字列リテラル\n"
      for (name, value) <- stringLiterals do
        val escaped = value.flatMap {
          case '\\' => "\\\\"
          case '"'  => "\\22"
          case c if c < 32 => f"\\${c.toInt}%02X"
          case c    => c.toString
        }
        sb ++= s"$name = private unnamed_addr constant [${value.length + 1} x i8] c\"$escaped\\00\"\n"
      sb ++= "\n"

    // ランタイム関数の宣言
    sb ++= runtimeDeclarations
    sb ++= "\n"

    // 生成された関数
    for fn <- functions do
      sb ++= fn
      sb ++= "\n\n"

    sb.toString

  /** ランタイムヘルパー関数の宣言 */
  private val runtimeDeclarations: String =
    """; ランタイムヘルパー関数（外部リンク）
declare %Value @rt_make_int(i64)
declare %Value @rt_make_literal(i8*)
declare %Value @rt_make_unit()
declare %Value @rt_make_pair(%Value, %Value)
declare %Value @rt_proj1(%Value)
declare %Value @rt_proj2(%Value)
declare %Value @rt_make_inl(%Value)
declare %Value @rt_make_inr(%Value)
declare i8 @rt_get_tag(%Value)
declare %Value @rt_get_inner(%Value)
declare %Value @rt_make_closure(i8*, i8*, i32)
declare %Value @rt_apply(%Value, %Value)
declare i8* @rt_alloc_env(i32)
declare void @rt_env_store(i8*, i32, %Value)
declare %Value @rt_env_load(i8*, i32)
"""
