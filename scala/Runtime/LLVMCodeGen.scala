// ==========================================
// LLVMCodeGen.scala
// Op（バイトコード）→ LLVM IRテキスト生成（レジスタマシン対応）
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/** LLVM IR生成時のエラー */
class CodeGenError(msg: String) extends RuntimeException(msg)

/** SSAレジスタ名 */
case class Reg(id: Int):
  override def toString: String = s"%v$id"

/** LLVM IRコード生成器
  * レジスタマシンに対応。
  * 関数は共通シグネチャ `define %Value @func(ptr %env, ptr %args, i32 %num_args)` を持つ。
  */
class LLVMCodeGen:
  private var regCounter = 0
  private var labelCounter = 0
  private var closureCounter = 0
  private val functions = new ArrayBuffer[String]()    // 生成された関数定義
  private val stringLiterals = new ArrayBuffer[(String, String)]() // (名前, 値)
  private val rangeAnalyzer = new RangeAnalyzer()

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

    // 範囲解析を実行
    val analysis = rangeAnalyzer.analyze(code)

    // エントリ関数をコンパイル
    val entryBody = compileMainFunction(code, entryName, analysis)
    functions += entryBody

    buildModule(embedRuntime)

  /** 最大レジスタインデックスを探索 */
  private def findMaxReg(code: Array[Op]): Int =
    var max = -1
    def update(r: Int): Unit = if (r > max) max = r
    
    // 再帰的に探索 (ただしクロージャ内部は別関数なので探索しない)
    def scan(ops: Array[Op]): Unit =
      for op <- ops do
        op match
          case Op.Move(d, s) => update(d); update(s)
          case Op.LoadConst(d, _) => update(d)
          case Op.MakeClosure(d, _, caps, _) => update(d); capturesMax(caps)
          case Op.Call(d, f, args) => update(d); update(f); args.foreach(update)
          case Op.Return(s) => update(s)
          case Op.MakePair(d, f, s) => update(d); update(f); update(s)
          case Op.Proj1(d, s) => update(d); update(s)
          case Op.Proj2(d, s) => update(d); update(s)
          case Op.MakeInl(d, s) => update(d); update(s)
          case Op.MakeInr(d, s) => update(d); update(s)
          case Op.Case(d, s, _, _) => update(d); update(s)
          case Op.Add(d, l, r) => update(d); update(l); update(r)
          case Op.Sub(d, l, r) => update(d); update(l); update(r)
          case Op.Mul(d, l, r) => update(d); update(l); update(r)
          case Op.Free(r) => update(r)
    
    def capturesMax(caps: Array[Int]): Unit =
        caps.foreach(update)

    scan(code)
    max

  /** メイン関数（エントリポイント）のコンパイル */
  private def compileMainFunction(code: Array[Op], name: String, analysis: RangeAnalysisResult): String =
    val savedReg = regCounter
    regCounter = 0
    val lines = new ArrayBuffer[String]()
    val allocas = new ArrayBuffer[String]()
    
    def emit(line: String): Unit = lines += s"  $line"

    // レジスタ領域の確保 (配列として確保)
    val maxReg = findMaxReg(code)
    emit(s"%regs_base = alloca %Value, i32 ${maxReg + 1}")

    // 初期化 (Unit)
    for i <- 0 to maxReg do
      val tmp = freshReg()
      emit(s"$tmp = call %Value @rt_make_unit()")
      val slot = freshReg()
      emit(s"$slot = getelementptr %Value, ptr %regs_base, i32 $i")
      emit(s"store %Value $tmp, ptr $slot")

    // 命令生成 (useArrayRegs = true)
    compileOps(code, lines, allocas, analysis, 0, useArrayRegs = true)

    // 終了処理 (Returnがない場合のデフォルト)
    if code.isEmpty || !code.last.isInstanceOf[Op.Return] then
      val tmp = freshReg()
      emit(s"$tmp = call %Value @rt_make_unit()")
      emit(s"ret %Value $tmp")

    regCounter = savedReg + regCounter
    
    val cleanName = if name.startsWith("@") then name.drop(1) else name
    val body = (allocas ++ lines).mkString("\n")
    s"define %Value @$cleanName() {\nentry:\n$body\n}"

  /** クロージャ関数のコンパイル */
  private def compileClosureFunction(
      code: Array[Op],
      name: String,
      arity: Int,
      payloadDst: Option[Int] = None
  ): String =
    val savedReg = regCounter
    regCounter = 0
    val lines = new ArrayBuffer[String]()
    val allocas = new ArrayBuffer[String]()

    // クロージャ内部用の範囲解析
    val analysis = rangeAnalyzer.analyze(code)

    def emit(line: String): Unit = lines += s"  $line"

    // シグネチャ: (ptr %env, ptr %args, i32 %num_args)
    // レジスタ領域確保
    val maxReg = findMaxReg(code)
    
    emit(s"%regs_base = alloca %Value, i32 ${maxReg + 1}")
    
    // ランタイムで初期化
    emit(s"call void @rt_setup_regs(ptr %regs_base, i32 ${maxReg + 1}, ptr %env, ptr %args, i32 %num_args)")

    // Caseの場合、引数0をpayloadDstに配置
    payloadDst match
      case Some(dstIdx) =>
        if dstIdx <= maxReg then
          val arg0Ptr = freshReg()
          emit(s"$arg0Ptr = getelementptr %Value, ptr %args, i32 0")
          val arg0Val = freshReg()
          emit(s"$arg0Val = load %Value, ptr $arg0Ptr")
          val dstPtr = freshReg()
          emit(s"$dstPtr = getelementptr %Value, ptr %regs_base, i32 $dstIdx")
          emit(s"store %Value $arg0Val, ptr $dstPtr")
      case None => ()

    // 命令生成
    compileOps(code, lines, allocas, analysis, 0, useArrayRegs = true)

    // 終了処理
    if code.isEmpty || !code.last.isInstanceOf[Op.Return] then
      val tmp = freshReg()
      emit(s"$tmp = call %Value @rt_make_unit()")
      emit(s"ret %Value $tmp")

    regCounter = savedReg + regCounter
    
    val cleanName = if name.startsWith("@") then name.drop(1) else name
    val body = (allocas ++ lines).mkString("\n")
    s"define %Value @$cleanName(ptr %env, ptr %args, i32 %num_args) {\nentry:\n$body\n}"


  /** 命令列のコンパイル
    * useArrayRegs: trueなら %regs_base 配列を使用、falseなら %reg_ptr_N 変数を使用
    */
  private def compileOps(
      code: Array[Op],
      lines: ArrayBuffer[String],
      allocas: ArrayBuffer[String],
      analysis: RangeAnalysisResult,
      depth: Int,
      useArrayRegs: Boolean = false
  ): Unit =
    
    def emit(line: String): Unit = lines += s"  $line"
    def emitAlloca(line: String): Unit = allocas += s"  $line"
    
    def getRegPtr(idx: Int): String =
      if useArrayRegs then
        val r = freshReg()
        emit(s"$r = getelementptr %Value, ptr %regs_base, i32 $idx")
        r.toString
      else
        s"%reg_ptr_$idx"

    def consumeReg(idx: Int): String =
      val ptr = getRegPtr(idx)
      val v = freshReg()
      emit(s"$v = load %Value, ptr $ptr")
      val unit = freshReg()
      emit(s"$unit = call %Value @rt_make_unit()")
      emit(s"store %Value $unit, ptr $ptr")
      v.toString

    for op <- code do
      op match
        case Op.Move(dst, src) =>
          val v = consumeReg(src)
          val dstPtr = getRegPtr(dst)
          emit(s"store %Value $v, ptr $dstPtr")

        case Op.LoadConst(dst, Value.Atom(n: Int)) =>
          val dstPtr = getRegPtr(dst)
          val tmp = freshReg()
          emit(s"$tmp = call %Value @rt_make_int(i64 $n)")
          emit(s"store %Value $tmp, ptr $dstPtr")

        case Op.LoadConst(dst, Value.Atom(n: Long)) =>
          val dstPtr = getRegPtr(dst)
          val tmp = freshReg()
          emit(s"$tmp = call %Value @rt_make_int(i64 $n)")
          emit(s"store %Value $tmp, ptr $dstPtr")

        case Op.LoadConst(dst, Value.Atom(s: String)) =>
          val dstPtr = getRegPtr(dst)
          val strName = registerStringLiteral(s)
          val tmp = freshReg()
          emit(s"$tmp = call %Value @rt_make_literal(ptr $strName)")
          emit(s"store %Value $tmp, ptr $dstPtr")

        case Op.LoadConst(dst, Value.Unit) =>
          val dstPtr = getRegPtr(dst)
          val tmp = freshReg()
          emit(s"$tmp = call %Value @rt_make_unit()")
          emit(s"store %Value $tmp, ptr $dstPtr")
          
        case Op.LoadConst(dst, v) =>
           throw CodeGenError(s"未対応の定数: $v")

        case Op.MakeClosure(dst, body, captures, arity) =>
          val closureName = freshClosureName()
          val closureBody = compileClosureFunction(body, closureName, arity)
          functions += closureBody
          
          val envR = freshReg()
          emit(s"$envR = call ptr @rt_alloc_env(i32 ${captures.length})")
          
          for (regIdx, i) <- captures.zipWithIndex do
             val valR = consumeReg(regIdx)
             emit(s"call void @rt_env_store(ptr $envR, i32 $i, %Value $valR)")

          val tmp = freshReg()
          val dstPtr = getRegPtr(dst)
          emit(s"$tmp = call %Value @rt_make_closure(ptr $closureName, ptr $envR, i32 $arity)")
          emit(s"store %Value $tmp, ptr $dstPtr")

        case Op.Call(dst, funcIdx, argIdxs) =>
           val funcVal = consumeReg(funcIdx)
           val argsLen = argIdxs.length
           val argsArray = freshReg()
           emitAlloca(s"$argsArray = alloca %Value, i32 $argsLen")
           
           for (argIdx, i) <- argIdxs.zipWithIndex do
             val v = consumeReg(argIdx)
             val slot = freshReg()
             emit(s"$slot = getelementptr %Value, ptr $argsArray, i32 $i")
             emit(s"store %Value $v, ptr $slot")
           
           val res = freshReg()
           emit(s"$res = call %Value @rt_call(%Value $funcVal, ptr $argsArray, i32 $argsLen)")
           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $res, ptr $dstPtr")

        case Op.Return(src) =>
           val v = consumeReg(src)
           emit(s"ret %Value $v")

        case Op.MakePair(dst, fst, snd) =>
           val f = consumeReg(fst)
           val s = consumeReg(snd)
           val res = freshReg()
           emit(s"$res = call %Value @rt_make_pair(%Value $f, %Value $s)")
           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $res, ptr $dstPtr")

        case Op.Proj1(dst, src) =>
           val v = consumeReg(src)
           val f = freshReg()
           emit(s"$f = call %Value @rt_proj1(%Value $v)")
           val s = freshReg()
           emit(s"$s = call %Value @rt_proj2(%Value $v)")
           val ptr = freshReg()
           emit(s"$ptr = extractvalue %Value $v, 1")
           emit(s"call void @free(ptr $ptr)")
           
           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $f, ptr $dstPtr")
           val srcPtr = getRegPtr(src)
           emit(s"store %Value $s, ptr $srcPtr")

        case Op.Proj2(dst, src) =>
           val v = consumeReg(src)
           val f = freshReg()
           emit(s"$f = call %Value @rt_proj1(%Value $v)")
           val s = freshReg()
           emit(s"$s = call %Value @rt_proj2(%Value $v)")
           val ptr = freshReg()
           emit(s"$ptr = extractvalue %Value $v, 1")
           emit(s"call void @free(ptr $ptr)")

           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $s, ptr $dstPtr")
           val srcPtr = getRegPtr(src)
           emit(s"store %Value $f, ptr $srcPtr")
           
        case Op.MakeInl(dst, src) =>
           val v = consumeReg(src)
           val res = freshReg()
           emit(s"$res = call %Value @rt_make_inl(%Value $v)")
           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $res, ptr $dstPtr")

        case Op.MakeInr(dst, src) =>
           val v = consumeReg(src)
           val res = freshReg()
           emit(s"$res = call %Value @rt_make_inr(%Value $v)")
           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $res, ptr $dstPtr")
           
        case Op.Case(dst, scrutinee, inlBranch, inrBranch) =>
           val v = consumeReg(scrutinee)
           
           val tag = freshReg()
           emit(s"$tag = call i8 @rt_get_tag(%Value $v)")
           val inner = freshReg()
           emit(s"$inner = call %Value @rt_get_inner(%Value $v)")
           
           val labelInl = freshLabel("case_inl")
           val labelInr = freshLabel("case_inr")
           val labelEnd = freshLabel("case_end")
           
           val isInl = freshReg()
           emit(s"$isInl = icmp eq i8 $tag, 3") // 3 = Inl
           emit(s"br i1 $isInl, label %$labelInl, label %$labelInr")
           
           val maxReg = findMaxReg(code)
           val envR = freshReg()
           emit(s"$envR = call ptr @rt_alloc_env(i32 ${maxReg + 1})")
           for i <- 0 to maxReg do
             val r = getRegPtr(i)
             val valR = freshReg()
             emit(s"$valR = load %Value, ptr $r")
             emit(s"call void @rt_env_store(ptr $envR, i32 $i, %Value $valR)")
           
           // Inl Branch
           lines += s"$labelInl:"
           val inlFunc = freshClosureName() + "_inl"
           val inlBody = compileClosureFunction(inlBranch, inlFunc, 1, payloadDst = Some(dst))
           functions += inlBody
           val argsArr = freshReg()
           emitAlloca(s"$argsArr = alloca %Value, i32 1")
           val slot0 = freshReg()
           emit(s"$slot0 = getelementptr %Value, ptr $argsArr, i32 0")
           emit(s"store %Value $inner, ptr $slot0")
           val resInl = freshReg()
           emit(s"$resInl = call %Value $inlFunc(ptr $envR, ptr $argsArr, i32 1)")
           emit(s"br label %$labelEnd")
           
           // Inr Branch
           lines += s"$labelInr:"
           val inrFunc = freshClosureName() + "_inr"
           val inrBody = compileClosureFunction(inrBranch, inrFunc, 1, payloadDst = Some(dst))
           functions += inrBody
           val argsArr2 = freshReg()
           emitAlloca(s"$argsArr2 = alloca %Value, i32 1")
           val slot0_2 = freshReg()
           emit(s"$slot0_2 = getelementptr %Value, ptr $argsArr2, i32 0")
           emit(s"store %Value $inner, ptr $slot0_2")
           val resInr = freshReg()
           emit(s"$resInr = call %Value $inrFunc(ptr $envR, ptr $argsArr2, i32 1)")
           emit(s"br label %$labelEnd")
           
           // End
           lines += s"$labelEnd:"
           val resPhi = freshReg()
           emit(s"$resPhi = phi %Value [ $resInl, %$labelInl ], [ $resInr, %$labelInr ]")
           val dstPtr = getRegPtr(dst)
           emit(s"store %Value $resPhi, ptr $dstPtr")

        case Op.Add(dst, lhs, rhs) => compileBinOpLinear(dst, lhs, rhs, "add", analysis, lines, useArrayRegs)
        case Op.Sub(dst, lhs, rhs) => compileBinOpLinear(dst, lhs, rhs, "sub", analysis, lines, useArrayRegs)
        case Op.Mul(dst, lhs, rhs) => compileBinOpLinear(dst, lhs, rhs, "mul", analysis, lines, useArrayRegs)

        case Op.Free(reg) =>
           val v = consumeReg(reg)
           emit(s"call void @rt_free_value(%Value $v)")
    end for

  private def compileBinOpLinear(
      dst: Int, lhs: Int, rhs: Int, opName: String,
      analysis: RangeAnalysisResult,
      lines: ArrayBuffer[String],
      useArrayRegs: Boolean
  ): Unit =
    def emit(line: String): Unit = lines += s"  $line"
    def getRegPtr(idx: Int): String =
      if useArrayRegs then
        val r = freshReg()
        emit(s"$r = getelementptr %Value, ptr %regs_base, i32 $idx")
        r.toString
      else s"%reg_ptr_$idx"
    
    val lPtr = getRegPtr(lhs)
    val rPtr = getRegPtr(rhs)
    val lVal = freshReg()
    emit(s"$lVal = load %Value, ptr $lPtr")
    val rVal = freshReg()
    emit(s"$rVal = load %Value, ptr $rPtr")
    
    val unit = freshReg()
    emit(s"$unit = call %Value @rt_make_unit()")
    emit(s"store %Value $unit, ptr $lPtr")
    emit(s"store %Value $unit, ptr $rPtr")
    
    val lInt = freshReg()
    emit(s"$lInt = call i64 @rt_get_int(%Value $lVal)")
    val rInt = freshReg()
    emit(s"$rInt = call i64 @rt_get_int(%Value $rVal)")
    
    val width = analysis.bitWidth(dst)
    val iW = s"i$width"
    val lTrunc = freshReg()
    emit(s"$lTrunc = trunc i64 $lInt to $iW")
    val rTrunc = freshReg()
    emit(s"$rTrunc = trunc i64 $rInt to $iW")
    val resTrunc = freshReg()
    emit(s"$resTrunc = $opName $iW $lTrunc, $rTrunc")
    val resInt = freshReg()
    emit(s"$resInt = sext $iW $resTrunc to i64")
    val resVal = freshReg()
    emit(s"$resVal = call %Value @rt_make_int(i64 $resInt)")
    emit(s"store %Value $resVal, ptr ${getRegPtr(dst)}")

  /** モジュール全体を組み立て */
  private def buildModule(embedRuntime: Boolean = false): String =
    val sb = new StringBuilder()
    sb ++= "; Romanesco LLVM IR (Register Machine)\n\n"
    
    if embedRuntime then
      sb ++= runtimeImplementation
    else
      sb ++= "%Value = type { i8, ptr }\n"
      sb ++= runtimeDeclarations

    if stringLiterals.nonEmpty then
      for (name, value) <- stringLiterals do
        val escaped = value.flatMap {
          case '\\' => "\\\\"
          case '"'  => "\\22"
          case c if c < 32 => f"\\${c.toInt}%02X"
          case c    => c.toString
        }
        sb ++= s"$name = private unnamed_addr constant [${value.length + 1} x i8] c\"$escaped\\00\"\n"
    
    for fn <- functions do
      sb ++= fn
      sb ++= "\n\n"
      
    sb.toString

  /** ランタイム宣言 */
  private val runtimeDeclarations: String =
    """declare %Value @rt_make_int(i64)
declare i64 @rt_get_int(%Value)
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
declare %Value @rt_call(%Value, ptr, i32)
declare void @rt_free_value(%Value)
declare ptr @rt_alloc_env(i32)
declare void @rt_env_store(ptr, i32, %Value)
declare void @rt_setup_regs(ptr, i32, ptr, ptr, i32)
declare i32 @rt_env_get_size(ptr)
"""

  /** ランタイム実装（埋め込み用） */
  private val runtimeImplementation: String =
    """
%Value = type { i8, ptr }
%Closure = type { ptr, ptr, i32 }
%Pair = type { %Value, %Value }

declare ptr @malloc(i64)
declare void @free(ptr)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

; Tagged Integer 実装 (Tag 6 = Inline Int)
define %Value @rt_make_int(i64 %n) {
  %v1 = insertvalue %Value zeroinitializer, i8 6, 0
  %ptr = inttoptr i64 %n to ptr
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define i64 @rt_get_int(%Value %v) {
  %tag = extractvalue %Value %v, 0
  %is_inline = icmp eq i8 %tag, 6
  br i1 %is_inline, label %inline_case, label %heap_case
inline_case:
  %ptr = extractvalue %Value %v, 1
  %n = ptrtoint ptr %ptr to i64
  ret i64 %n
heap_case:
  ret i64 0
}

define %Value @rt_make_literal(ptr %s) {
  %v1 = insertvalue %Value zeroinitializer, i8 0, 0
  %v2 = insertvalue %Value %v1, ptr %s, 1
  ret %Value %v2
}

define %Value @rt_make_unit() {
  %v1 = insertvalue %Value zeroinitializer, i8 5, 0
  %v2 = insertvalue %Value %v1, ptr null, 1
  ret %Value %v2
}

define %Value @rt_make_pair(%Value %a, %Value %b) {
  %ptr = call ptr @malloc(i64 32)
  %p1 = getelementptr %Pair, ptr %ptr, i32 0, i32 0
  store %Value %a, ptr %p1
  %p2 = getelementptr %Pair, ptr %ptr, i32 0, i32 1
  store %Value %b, ptr %p2
  %v1 = insertvalue %Value zeroinitializer, i8 2, 0
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
  %v1 = insertvalue %Value zeroinitializer, i8 3, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define %Value @rt_make_inr(%Value %inner) {
  %ptr = call ptr @malloc(i64 16)
  store %Value %inner, ptr %ptr
  %v1 = insertvalue %Value zeroinitializer, i8 4, 0
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
  %v1 = insertvalue %Value zeroinitializer, i8 1, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

define %Value @rt_call(%Value %fn, ptr %args, i32 %num_args) {
entry:
  %ptr = extractvalue %Value %fn, 1
  %fpp = getelementptr %Closure, ptr %ptr, i32 0, i32 0
  %funcPtr = load ptr, ptr %fpp
  %epp = getelementptr %Closure, ptr %ptr, i32 0, i32 1
  %env = load ptr, ptr %epp
  %result = call %Value %funcPtr(ptr %env, ptr %args, i32 %num_args)
  ret %Value %result
}

define ptr @rt_alloc_env(i32 %size) {
  %val_size = mul i32 %size, 16
  %total = add i32 %val_size, 8
  %total_i64 = zext i32 %total to i64
  %env = call ptr @malloc(i64 %total_i64)
  store i32 %size, ptr %env
  ret ptr %env
}

define void @rt_env_store(ptr %env, i32 %idx, %Value %val) {
  %data = getelementptr i8, ptr %env, i32 8
  %vptr = getelementptr %Value, ptr %data, i32 %idx
  store %Value %val, ptr %vptr
  ret void
}

define void @rt_setup_regs(ptr %regs_base, i32 %regs_count, ptr %env, ptr %args, i32 %num_args) {
entry:
  %i_ptr = alloca i32
  %j_ptr = alloca i32
  %env_size = load i32, ptr %env
  %env_data = getelementptr i8, ptr %env, i32 8
  %c1 = call i32 @rt_umin(i32 %regs_count, i32 %env_size)
  store i32 0, ptr %i_ptr
  br label %env_loop_cond
env_loop_cond:
  %i = load i32, ptr %i_ptr
  %cmp1 = icmp ult i32 %i, %c1
  br i1 %cmp1, label %env_loop_body, label %env_loop_end
env_loop_body:
  %src_ptr = getelementptr %Value, ptr %env_data, i32 %i
  %dst_ptr = getelementptr %Value, ptr %regs_base, i32 %i
  %val = load %Value, ptr %src_ptr
  store %Value %val, ptr %dst_ptr
  %next_i = add i32 %i, 1
  store i32 %next_i, ptr %i_ptr
  br label %env_loop_cond
env_loop_end:
  %cond = icmp uge i32 %env_size, %regs_count
  br i1 %cond, label %done, label %copy_args_prep
copy_args_prep:
  %max_args = sub i32 %regs_count, %env_size
  %c2 = call i32 @rt_umin(i32 %num_args, i32 %max_args)
  store i32 0, ptr %j_ptr
  br label %arg_loop_cond
arg_loop_cond:
  %j = load i32, ptr %j_ptr
  %cmp2 = icmp ult i32 %j, %c2
  br i1 %cmp2, label %arg_loop_body, label %done
arg_loop_body:
  %arg_src_ptr = getelementptr %Value, ptr %args, i32 %j
  %idx_in_regs = add i32 %env_size, %j
  %arg_dst_ptr = getelementptr %Value, ptr %regs_base, i32 %idx_in_regs
  %arg_val = load %Value, ptr %arg_src_ptr
  store %Value %arg_val, ptr %arg_dst_ptr
  %next_j = add i32 %j, 1
  store i32 %next_j, ptr %j_ptr
  br label %arg_loop_cond
done:
  ret void
}

define void @rt_free_value(%Value %v) {
entry:
  %tag = extractvalue %Value %v, 0
  %ptr = extractvalue %Value %v, 1
  switch i8 %tag, label %done [
    i8 1, label %free_closure
    i8 2, label %free_pair
    i8 3, label %free_union
    i8 4, label %free_union
  ]
free_pair:
  %p_ptr = getelementptr %Pair, ptr %ptr, i32 0, i32 0
  %v1 = load %Value, ptr %p_ptr
  call void @rt_free_value(%Value %v1)
  %p_ptr2 = getelementptr %Pair, ptr %ptr, i32 0, i32 1
  %v2 = load %Value, ptr %p_ptr2
  call void @rt_free_value(%Value %v2)
  call void @free(ptr %ptr)
  br label %done
free_union:
  %u_val = load %Value, ptr %ptr
  call void @rt_free_value(%Value %u_val)
  call void @free(ptr %ptr)
  br label %done
free_closure:
  %e_ptr_ptr = getelementptr %Closure, ptr %ptr, i32 0, i32 1
  %e_ptr = load ptr, ptr %e_ptr_ptr
  call void @rt_free_env(ptr %e_ptr)
  call void @free(ptr %ptr)
  br label %done
done:
  ret void
}

define void @rt_free_env(ptr %env) {
entry:
  %isNull = icmp eq ptr %env, null
  br i1 %isNull, label %exit, label %start
start:
  %size = load i32, ptr %env
  %i_ptr = alloca i32
  store i32 0, ptr %i_ptr
  br label %cond
cond:
  %i = load i32, ptr %i_ptr
  %cmp = icmp ult i32 %i, %size
  br i1 %cmp, label %body, label %end
body:
  %data = getelementptr i8, ptr %env, i32 8
  %vptr = getelementptr %Value, ptr %data, i32 %i
  %v = load %Value, ptr %vptr
  call void @rt_free_value(%Value %v)
  %next = add i32 %i, 1
  store i32 %next, ptr %i_ptr
  br label %cond
end:
  call void @free(ptr %env)
  br label %exit
exit:
  ret void
}

define i32 @rt_umin(i32 %a, i32 %b) {
  %cond = icmp ult i32 %a, %b
  %res = select i1 %cond, i32 %a, i32 %b
  ret i32 %res
}

define i32 @rt_env_get_size(ptr %env) {
  %s = load i32, ptr %env
  ret i32 %s
}
"""
