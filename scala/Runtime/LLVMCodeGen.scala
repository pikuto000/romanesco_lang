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

  /** バイトコード列からLLVM IRモジュール全体を生成
    * @param embedRuntime trueの場合、ランタイム関数の実装をIRに埋め込む（lli実行可能）
    */
  def generate(code: Array[Op], entryName: String = "main", embedRuntime: Boolean = false): String =
    regCounter = 0
    labelCounter = 0
    closureCounter = 0
    functions.clear()
    stringLiterals.clear()

    // エントリ関数をコンパイル
    val entryBody = compileFunction(code, entryName, Nil)
    functions += entryBody

    buildModule(embedRuntime)

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
          emit(s"$r = call %Value @rt_make_literal(ptr $strName)")
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
          val closureBody = compileClosureFunction(body, closureName, locals.toList, arity)
          functions += closureBody
          val envSize = locals.size
          val envR = freshReg()
          emit(s"$envR = call ptr @rt_alloc_env(i32 $envSize)")
          for (local, i) <- locals.zipWithIndex do
            emit(s"call void @rt_env_store(ptr $envR, i32 $i, %Value $local)")
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_closure(ptr $closureName, ptr $envR, i32 $arity)")
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
      emit(s"$r = call %Value @rt_env_load(ptr %env, i32 $i)")
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
          val r = freshReg()
          emit(s"$r = call %Value @rt_make_literal(ptr $strName)")
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
    s"define %Value @$cleanName(ptr %env, %Value %arg) {\nentry:\n${lines.mkString("\n")}\n}"

  /** モジュール全体を組み立て */
  private def buildModule(embedRuntime: Boolean = false): String =
    val sb = new StringBuilder()

    sb ++= "; ==========================================\n"
    sb ++= "; Romanesco LLVM IR (自動生成)\n"
    sb ++= "; ==========================================\n\n"

    if embedRuntime then
      // ランタイム実装を埋め込み（%Value型定義含む）
      sb ++= runtimeImplementation
      sb ++= "\n"
    else
      // 値型の定義
      sb ++= "; 値型: {tag: i8, payload: ptr}\n"
      sb ++= "%Value = type { i8, ptr }\n\n"
      // ランタイム関数の宣言のみ
      sb ++= runtimeDeclarations
      sb ++= "\n"

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

    // 生成された関数
    for fn <- functions do
      sb ++= fn
      sb ++= "\n\n"

    sb.toString

  /** ランタイムヘルパー関数の宣言 */
  private val runtimeDeclarations: String =
    """; ランタイムヘルパー関数（外部リンク）
declare %Value @rt_make_int(i64)
declare %Value @rt_make_literal(ptr)
declare %Value @rt_make_unit()
declare %Value @rt_make_pair(%Value, %Value)
declare %Value @rt_proj1(%Value)
declare %Value @rt_proj2(%Value)
declare %Value @rt_make_inl(%Value)
declare %Value @rt_make_inr(%Value)
declare i8 @rt_get_tag(%Value)
declare %Value @rt_get_inner(%Value)
declare %Value @rt_make_closure(ptr, ptr, i32)
declare %Value @rt_apply(%Value, %Value)
declare ptr @rt_alloc_env(i32)
declare void @rt_env_store(ptr, i32, %Value)
declare %Value @rt_env_load(ptr, i32)
"""

  /** ランタイム関数の実装（埋め込みモード用） */
  private val runtimeImplementation: String =
    """; ==========================================
; Romanesco ランタイムライブラリ（埋め込み）
; ==========================================

%Value = type { i8, ptr }
%Closure = type { ptr, ptr, i32 }
%Pair = type { %Value, %Value }

declare ptr @malloc(i64)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

define %Value @rt_make_int(i64 %n) {
  %ptr = call ptr @malloc(i64 8)
  store i64 %n, ptr %ptr
  %v1 = insertvalue %Value undef, i8 0, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define %Value @rt_make_literal(ptr %s) {
  %v1 = insertvalue %Value undef, i8 1, 0
  %v2 = insertvalue %Value %v1, ptr %s, 1
  ret %Value %v2
}

define %Value @rt_make_unit() {
  %v1 = insertvalue %Value undef, i8 6, 0
  %v2 = insertvalue %Value %v1, ptr null, 1
  ret %Value %v2
}

define %Value @rt_make_pair(%Value %a, %Value %b) {
  %ptr = call ptr @malloc(i64 24)
  %p1 = getelementptr %Pair, ptr %ptr, i32 0, i32 0
  store %Value %a, ptr %p1
  %p2 = getelementptr %Pair, ptr %ptr, i32 0, i32 1
  store %Value %b, ptr %p2
  %v1 = insertvalue %Value undef, i8 3, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define %Value @rt_proj1(%Value %p) {
  %ptr = extractvalue %Value %p, 1
  %vptr = getelementptr %Pair, ptr %ptr, i32 0, i32 0
  %v = load %Value, ptr %vptr
  ret %Value %v
}

define %Value @rt_proj2(%Value %p) {
  %ptr = extractvalue %Value %p, 1
  %vptr = getelementptr %Pair, ptr %ptr, i32 0, i32 1
  %v = load %Value, ptr %vptr
  ret %Value %v
}

define %Value @rt_make_inl(%Value %inner) {
  %ptr = call ptr @malloc(i64 16)
  store %Value %inner, ptr %ptr
  %v1 = insertvalue %Value undef, i8 4, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define %Value @rt_make_inr(%Value %inner) {
  %ptr = call ptr @malloc(i64 16)
  store %Value %inner, ptr %ptr
  %v1 = insertvalue %Value undef, i8 5, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define i8 @rt_get_tag(%Value %v) {
  %tag = extractvalue %Value %v, 0
  ret i8 %tag
}

define %Value @rt_get_inner(%Value %v) {
  %ptr = extractvalue %Value %v, 1
  %inner = load %Value, ptr %ptr
  ret %Value %inner
}

define %Value @rt_make_closure(ptr %func, ptr %env, i32 %arity) {
  %ptr = call ptr @malloc(i64 24)
  %f = getelementptr %Closure, ptr %ptr, i32 0, i32 0
  store ptr %func, ptr %f
  %e = getelementptr %Closure, ptr %ptr, i32 0, i32 1
  store ptr %env, ptr %e
  %a = getelementptr %Closure, ptr %ptr, i32 0, i32 2
  store i32 %arity, ptr %a
  %v1 = insertvalue %Value undef, i8 2, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define %Value @rt_apply(%Value %fn, %Value %arg) {
entry:
  %ptr = extractvalue %Value %fn, 1
  %fpp = getelementptr %Closure, ptr %ptr, i32 0, i32 0
  %funcPtr = load ptr, ptr %fpp
  %epp = getelementptr %Closure, ptr %ptr, i32 0, i32 1
  %env = load ptr, ptr %epp
  %app = getelementptr %Closure, ptr %ptr, i32 0, i32 2
  %arity = load i32, ptr %app
  %is_one = icmp eq i32 %arity, 1
  br i1 %is_one, label %direct_call, label %partial_apply
direct_call:
  %result = call %Value %funcPtr(ptr %env, %Value %arg)
  ret %Value %result
partial_apply:
  %env_size = load i32, ptr %env
  %new_size = add i32 %env_size, 1
  %new_env = call ptr @rt_alloc_env(i32 %new_size)
  %src_data = getelementptr i8, ptr %env, i32 4
  %dst_data = getelementptr i8, ptr %new_env, i32 4
  %copy_bytes = mul i32 %env_size, 16
  %copy_i64 = zext i32 %copy_bytes to i64
  call void @llvm.memcpy.p0.p0.i64(ptr %dst_data, ptr %src_data, i64 %copy_i64, i1 false)
  call void @rt_env_store(ptr %new_env, i32 %env_size, %Value %arg)
  %new_arity = sub i32 %arity, 1
  %new_closure = call %Value @rt_make_closure(ptr %funcPtr, ptr %new_env, i32 %new_arity)
  ret %Value %new_closure
}

define ptr @rt_alloc_env(i32 %n) {
  %val_size = mul i32 %n, 16
  %total_i32 = add i32 %val_size, 4
  %total = zext i32 %total_i32 to i64
  %ptr = call ptr @malloc(i64 %total)
  store i32 %n, ptr %ptr
  ret ptr %ptr
}

define void @rt_env_store(ptr %env, i32 %idx, %Value %val) {
  %data = getelementptr i8, ptr %env, i32 4
  %vptr = getelementptr %Value, ptr %data, i32 %idx
  store %Value %val, ptr %vptr
  ret void
}

define %Value @rt_env_load(ptr %env, i32 %idx) {
  %data = getelementptr i8, ptr %env, i32 4
  %vptr = getelementptr %Value, ptr %data, i32 %idx
  %v = load %Value, ptr %vptr
  ret %Value %v
}
"""
