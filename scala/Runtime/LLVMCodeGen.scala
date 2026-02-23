// ==========================================
// LLVMCodeGen.scala
// 高度なクロージャ・再帰対応版 LLVM 生成器 (安定版)
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class CodeGenError(msg: String) extends RuntimeException(msg)

case class Reg(id: Int):
  override def toString: String = s"%v$id"

class LLVMCodeGen:
  private var regCounter = 0
  private var closureCounter = 0
  private val functions = new ArrayBuffer[String]()
  private val rangeAnalyzer = new RangeAnalyzer()
  private var currentProfile: Option[ProfileData] = None

  private def buildTargetHeader(): String =
    val isWin = System.getProperty("os.name").toLowerCase.contains("win")
    if (isWin) "target triple = \"x86_64-pc-windows-msvc\"\n" else ""

  private def freshReg(): Reg =
    val r = Reg(regCounter)
    regCounter += 1
    r

  private def freshClosureName(): String =
    val n = s"@__closure_$closureCounter"
    closureCounter += 1
    n

  def generate(code: Array[Op], entryName: String = "main", embedRuntime: Boolean = true, profile: Option[ProfileData] = None): String =
    regCounter = 0
    closureCounter = 0
    functions.clear()
    currentProfile = profile
    val mainBody = compileFunction(code, entryName, isMain = true)
    functions += mainBody
    buildModule(mainBody, embedRuntime)

  private def compileFunction(code: Array[Op], name: String, isMain: Boolean = false, payloadDst: Option[Int] = None): String =
    val oldRegCounter = regCounter
    regCounter = 0
    val analysis = rangeAnalyzer.analyze(code)
    val maxReg = findMaxReg(code)
    val lines = new ArrayBuffer[String]()
    val allocas = new ArrayBuffer[String]()
    def emit(line: String): Unit = lines += s"  $line"
    def emitAlloca(line: String): Unit = allocas += s"  $line"
    def getRegPtr(idx: Int): String = { val r = freshReg(); emit(s"$r = getelementptr %Value, ptr %regs_base, i32 $idx"); r.toString }

    if (isMain) {
      // Main では phi で受け取った regs_base を使うためここでは emit しない
      for i <- 0 to maxReg do emit(s"call void @rt_make_unit(ptr ${getRegPtr(i)})")
    } else {
      emit(s"%regs_base = alloca %Value, i32 ${maxReg + 1}")
      emit(s"call void @rt_setup_regs(ptr %regs_base, i32 ${maxReg + 1}, ptr %env, ptr %args, i32 %num_args)")
    }

    def consumeReg(idx: Int): String = {
      val ptr = getRegPtr(idx)
      val v = freshReg()
      emit(s"$v = load %Value, ptr $ptr")
      emit(s"call void @rt_make_unit(ptr $ptr)")
      v.toString
    }

    for (op, pc) <- code.zipWithIndex do
      op match
        case Op.LoadConst(dst, Value.Atom(n: Int)) => emit(s"call void @rt_make_int(ptr ${getRegPtr(dst)}, i64 $n)")
        case Op.Move(dst, src) => emit(s"store %Value ${consumeReg(src)}, ptr ${getRegPtr(dst)}")
        case Op.Add(dst, l, r) => 
          // 投機的最適化: 型が Int (6) で安定していればガードを挿入
          currentProfile.flatMap(_.get(pc).dominantTag(l)) match
            case Some(6L) if currentProfile.get.get(pc).dominantTag(r).contains(6L) =>
              emit(s"; Speculative optimization for Add at PC $pc")
              val lv = freshReg(); emit(s"$lv = load %Value, ptr ${getRegPtr(l)}")
              val rv = freshReg(); emit(s"$rv = load %Value, ptr ${getRegPtr(r)}")
              emit(s"call void @rt_guard_int(%Value $lv, i32 $pc, ptr %regs_base, i32 ${maxReg + 1})")
              emit(s"call void @rt_guard_int(%Value $rv, i32 $pc, ptr %regs_base, i32 ${maxReg + 1})")
              // ガードを通過 = Int 確定なので、直接 i64 加算を行う
              val li = freshReg(); emit(s"$li = extractvalue %Value $lv, 1")
              val ri = freshReg(); emit(s"$ri = extractvalue %Value $rv, 1")
              val lin = freshReg(); emit(s"$lin = ptrtoint ptr $li to i64")
              val rin = freshReg(); emit(s"$rin = ptrtoint ptr $ri to i64")
              val res = freshReg(); emit(s"$res = add i64 $lin, $rin")
              emit(s"call void @rt_make_int(ptr ${getRegPtr(dst)}, i64 $res)")
              emit(s"call void @rt_make_unit(ptr ${getRegPtr(l)})")
              emit(s"call void @rt_make_unit(ptr ${getRegPtr(r)})")
            case _ =>
              compileBinOp(dst, l, r, "add", analysis, lines, getRegPtr, consumeReg)
        case Op.Sub(dst, l, r) => compileBinOp(dst, l, r, "sub", analysis, lines, getRegPtr, consumeReg)
        case Op.Mul(dst, l, r) => compileBinOp(dst, l, r, "mul", analysis, lines, getRegPtr, consumeReg)
        case Op.MakeClosure(dst, body, captures, arity) =>
          val closureName = freshClosureName()
          functions += compileFunction(body, closureName)
          val envR = freshReg(); emit(s"$envR = call ptr @rt_alloc_env(i32 ${captures.length})")
          for (srcIdx, i) <- captures.zipWithIndex do
            val v = freshReg(); emit(s"$v = load %Value, ptr ${getRegPtr(srcIdx)}")
            emit(s"call void @rt_env_store(ptr $envR, i32 $i, %Value $v)")
          emit(s"call void @rt_make_closure(ptr ${getRegPtr(dst)}, ptr $closureName, ptr $envR, i32 $arity)")
        case Op.Call(dst, fIdx, args) =>
          val fVal = freshReg(); emit(s"$fVal = load %Value, ptr ${getRegPtr(fIdx)}")
          val aArr = freshReg(); emitAlloca(s"$aArr = alloca %Value, i32 ${args.length}")
          for (aIdx, i) <- args.zipWithIndex do
            val v = consumeReg(aIdx)
            val s = freshReg(); emit(s"$s = getelementptr %Value, ptr $aArr, i32 $i")
            emit(s"store %Value $v, ptr $s")
          val res = freshReg(); emit(s"$res = call %Value @rt_call(%Value $fVal, ptr $aArr, i32 ${args.length})")
          emit(s"store %Value $res, ptr ${getRegPtr(dst)}")
        case Op.Return(src) => emit(s"ret %Value ${consumeReg(src)}")
        case Op.Borrow(dst, src) =>
          val v = freshReg()
          emit(s"$v = load %Value, ptr ${getRegPtr(src)}")
          emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
        case Op.Free(reg) =>
          emit(s"call void @rt_make_unit(ptr ${getRegPtr(reg)})")
        case Op.MakePair(dst, f, s) =>
          val fVal = consumeReg(f)
          val sVal = consumeReg(s)
          emit(s"call void @rt_make_pair(ptr ${getRegPtr(dst)}, %Value $fVal, %Value $sVal)")
        case Op.Proj1(dst, src) =>
          val sPtr = getRegPtr(src)
          val sVal = freshReg(); emit(s"$sVal = load %Value, ptr $sPtr")
          val res = freshReg(); emit(s"$res = call %Value @rt_proj1(ptr $sPtr, %Value $sVal)")
          emit(s"store %Value $res, ptr ${getRegPtr(dst)}")
        case Op.Proj2(dst, src) =>
          val sPtr = getRegPtr(src)
          val sVal = freshReg(); emit(s"$sVal = load %Value, ptr $sPtr")
          val res = freshReg(); emit(s"$res = call %Value @rt_proj2(ptr $sPtr, %Value $sVal)")
          emit(s"store %Value $res, ptr ${getRegPtr(dst)}")
        case Op.MakeInl(dst, src) =>
          val v = consumeReg(src)
          emit(s"call void @rt_make_sum(ptr ${getRegPtr(dst)}, %Value $v, i64 3)")
        case Op.MakeInr(dst, src) =>
          val v = consumeReg(src)
          emit(s"call void @rt_make_sum(ptr ${getRegPtr(dst)}, %Value $v, i64 4)")
        case Op.Case(dst, scrut, inl, inr) =>
          val sVal = consumeReg(scrut)
          val tag = freshReg(); emit(s"$tag = extractvalue %Value $sVal, 0")
          val valPtr = freshReg(); emit(s"$valPtr = extractvalue %Value $sVal, 1")
          val innerVal = freshReg(); emit(s"$innerVal = load %Value, ptr $valPtr")
          
          val inlLab = s"case_inl_$pc"
          val inrLab = s"case_inr_$pc"
          val endLab = s"case_end_$pc"
          
          val isInl = freshReg(); emit(s"$isInl = icmp eq i64 $tag, 3")
          emit(s"br i1 $isInl, label %$inlLab, label %$inrLab")
          
          emit(s"$inlLab:")
          emit(s"store %Value $innerVal, ptr ${getRegPtr(dst)}")
          // 簡易的にブランチ内もインライン展開（本来は別関数が望ましい）
          compileInline(inl, lines, getRegPtr, consumeReg)
          emit(s"br label %$endLab")
          
          emit(s"$inrLab:")
          emit(s"store %Value $innerVal, ptr ${getRegPtr(dst)}")
          compileInline(inr, lines, getRegPtr, consumeReg)
          emit(s"br label %$endLab")
          
          emit(s"$endLab:")
        case _ => ()

    regCounter = oldRegCounter
    val cleanName = if (name.startsWith("@")) name.drop(1) else name
    val retDefault = "  ret %Value { i64 5, ptr null }"
    val bodyStr = (allocas ++ lines).mkString("\n")
    
    val currentIsWin = System.getProperty("os.name").toLowerCase.contains("win")
    val currentDllexp = if (currentIsWin && isMain) "dllexport " else ""

    if (isMain) 
      s"""define $currentDllexp%Value @$cleanName(ptr %external_regs) {
entry:
  %is_null = icmp eq ptr %external_regs, null
  br i1 %is_null, label %alloc_stack, label %use_external

alloc_stack:
  %regs_stack = alloca %Value, i32 ${maxReg + 1}
  br label %body

use_external:
  br label %body

body:
  %regs_base = phi ptr [ %regs_stack, %alloc_stack ], [ %external_regs, %use_external ]
$bodyStr
$retDefault
}"""
    else s"define %Value @$cleanName(ptr %env, ptr %args, i32 %num_args) {\nentry:\n$bodyStr\n$retDefault\n}"

  private def compileInline(code: Array[Op], lines: ArrayBuffer[String], getRegPtr: Int => String, consumeReg: Int => String): Unit =
    // インライン展開用の簡易コンパイラ（再帰的に現在の emit を使用）
    // 本来は compileFunction を呼び出すべきだが、ラベルの競合を避ける必要がある
    () // 簡略化のため空実装。実際には block 単位の管理が必要。

  private def compileBinOp(dst: Int, lhs: Int, rhs: Int, op: String, analysis: RangeAnalysisResult, lines: ArrayBuffer[String], getRegPtr: Int => String, consumeReg: Int => String): Unit =
    def emit(line: String): Unit = lines += s"  $line"
    val lv = freshReg(); emit(s"$lv = call i64 @rt_get_int(ptr ${getRegPtr(lhs)})")
    val rv = freshReg(); emit(s"$rv = call i64 @rt_get_int(ptr ${getRegPtr(rhs)})")
    val res = freshReg(); emit(s"$res = $op i64 $lv, $rv")
    emit(s"call void @rt_make_unit(ptr ${getRegPtr(lhs)})")
    emit(s"call void @rt_make_unit(ptr ${getRegPtr(rhs)})")
    emit(s"call void @rt_make_int(ptr ${getRegPtr(dst)}, i64 $res)")

  private def findMaxReg(code: Array[Op]): Int =
    var max = -1
    def update(r: Int): Unit = if (r > max) max = r
    code.foreach {
      case Op.Move(d, s) => update(d); update(s)
      case Op.LoadConst(d, _) => update(d)
      case Op.Return(s) => update(s)
      case Op.MakePair(d, f, s) => update(d); update(f); update(s)
      case Op.Add(d, l, r) => update(d); update(l); update(r)
      case Op.Sub(d, l, r) => update(d); update(l); update(r)
      case Op.Mul(d, l, r) => update(d); update(l); update(r)
      case Op.MakeClosure(d, _, caps, _) => update(d); caps.foreach(update)
      case Op.Call(d, f, args) => update(d); update(f); args.foreach(update)
      case _ => ()
    }
    max

  private def buildModule(mainFunc: String, embedRuntime: Boolean): String =
    val sb = new StringBuilder()
    sb ++= buildTargetHeader()
    sb ++= "; Romanesco Pure Native LLVM IR Module\n%Value = type { i64, ptr }\n%Closure = type { ptr, ptr, i32 }\n"
    if (embedRuntime) sb ++= runtimeImplementation else sb ++= runtimeDeclarations
    for fn <- functions do sb ++= "\n" + fn + "\n"
    sb.toString

  private val runtimeDeclarations = """
declare void @rt_make_int(ptr, i64)
declare i64 @rt_get_int(ptr)
declare void @rt_make_unit(ptr)
declare void @rt_print_int(i64)
declare ptr @rt_alloc_env(i32)
declare void @rt_env_store(ptr, i32, %Value)
declare void @rt_make_closure(ptr, ptr, ptr, i32)
declare void @rt_make_pair(ptr, %Value, %Value)
declare void @rt_make_sum(ptr, %Value, i64)
declare %Value @rt_proj1(ptr, %Value)
declare %Value @rt_proj2(ptr, %Value)
declare %Value @rt_call(%Value, ptr, i32)
declare void @rt_setup_regs(ptr, i32, ptr, ptr, i32)
declare void @rt_guard_int(%Value, i32, ptr, i32)
declare void @rt_dump_regs(ptr, i32)
"""

  private val runtimeImplementation = """
declare ptr @malloc(i64)
declare void @free(ptr)
declare i32 @printf(ptr, ...)
declare void @exit(i32)
@.str_int_fmt = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
@.str_deopt_msg = private unnamed_addr constant [35 x i8] c"Deoptimization at PC %d triggered\0A\00"
@.str_reg_dump = private unnamed_addr constant [24 x i8] c"REG %d TAG %lld VAL %p\0A\00"
@.str_deopt_start = private unnamed_addr constant [19 x i8] c"DEOPT_STATE_START\0A\00"
@.str_deopt_end = private unnamed_addr constant [17 x i8] c"DEOPT_STATE_END\0A\00"

define void @rt_print_int(i64 %n) {
  call i32 (ptr, ...) @printf(ptr @.str_int_fmt, i64 %n)
  ret void
}
define void @rt_dump_regs(ptr %regs, i32 %count) {
  call i32 (ptr, ...) @printf(ptr @.str_deopt_start)
  %i_ptr = alloca i32
  store i32 0, ptr %i_ptr
  br label %loop
loop:
  %i = load i32, ptr %i_ptr
  %cond = icmp ult i32 %i, %count
  br i1 %cond, label %body, label %done
body:
  %ptr = getelementptr %Value, ptr %regs, i32 %i
  %tag_ptr = getelementptr %Value, ptr %ptr, i32 0, i32 0
  %tag = load i64, ptr %tag_ptr
  %val_ptr = getelementptr %Value, ptr %ptr, i32 0, i32 1
  %val = load ptr, ptr %val_ptr
  call i32 (ptr, ...) @printf(ptr @.str_reg_dump, i32 %i, i64 %tag, ptr %val)
  %next = add i32 %i, 1
  store i32 %next, ptr %i_ptr
  br label %loop
done:
  call i32 (ptr, ...) @printf(ptr @.str_deopt_end)
  ret void
}
define void @rt_guard_int(%Value %v, i32 %pc, ptr %regs, i32 %count) {
  %tag = extractvalue %Value %v, 0
  %is_int = icmp eq i64 %tag, 6
  br i1 %is_int, label %ok, label %deopt
ok:
  ret void
deopt:
  call void @rt_dump_regs(ptr %regs, i32 %count)
  call i32 (ptr, ...) @printf(ptr @.str_deopt_msg, i32 %pc)
  %exit_code = add i32 123, %pc
  call void @exit(i32 %exit_code)
  unreachable
}
define void @rt_make_int(ptr %out, i64 %n) {
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 6, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  %ptr = inttoptr i64 %n to ptr
  store ptr %ptr, ptr %p2
  ret void
}
define i64 @rt_get_int(ptr %v) {
  %p1 = getelementptr %Value, ptr %v, i32 0, i32 0
  %tag = load i64, ptr %p1
  %is_int = icmp eq i64 %tag, 6
  br i1 %is_int, label %int_val, label %other_val
int_val:
  %p2 = getelementptr %Value, ptr %v, i32 0, i32 1
  %ptr = load ptr, ptr %p2
  %n = ptrtoint ptr %ptr to i64
  ret i64 %n
other_val:
  %is_pair = icmp eq i64 %tag, 2
  br i1 %is_pair, label %pair_val, label %fail
pair_val:
  ; ペアの場合は便宜上1番目の要素の整数値を返す(デバッグ用)
  %p3 = getelementptr %Value, ptr %v, i32 0, i32 1
  %ptr2 = load ptr, ptr %p3
  %v1 = load %Value, ptr %ptr2
  %v1_ptr = alloca %Value
  store %Value %v1, ptr %v1_ptr
  %n2 = call i64 @rt_get_int(ptr %v1_ptr)
  ret i64 %n2
fail:
  ret i64 0
}
define void @rt_make_unit(ptr %out) {
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 5, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr null, ptr %p2
  ret void
}
define void @rt_make_pair(ptr %out, %Value %v1, %Value %v2) {
  %mem = call ptr @malloc(i64 32)
  store %Value %v1, ptr %mem
  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
  store %Value %v2, ptr %v2_ptr
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 2, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %p2
  ret void
}
define void @rt_make_sum(ptr %out, %Value %v, i64 %tag) {
  %mem = call ptr @malloc(i64 16)
  store %Value %v, ptr %mem
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 %tag, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %p2
  ret void
}
define %Value @rt_proj1(ptr %src_ptr, %Value %pair) {
  %mem = extractvalue %Value %pair, 1
  %v1 = load %Value, ptr %mem
  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
  %v2 = load %Value, ptr %v2_ptr
  ; Proj1 はペアを消費し、v2 を src に戻す (VMの仕様に合わせる)
  store %Value %v2, ptr %src_ptr
  ret %Value %v1
}
define %Value @rt_proj2(ptr %src_ptr, %Value %pair) {
  %mem = extractvalue %Value %pair, 1
  %v1 = load %Value, ptr %mem
  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
  %v2 = load %Value, ptr %v2_ptr
  ; Proj2 はペアを消費し、v1 を src に戻す
  store %Value %v1, ptr %src_ptr
  ret %Value %v2
}
define ptr @rt_alloc_env(i32 %size) {
  %mem = call ptr @malloc(i64 1024)
  store i32 %size, ptr %mem
  ret ptr %mem
}
define void @rt_env_store(ptr %env, i32 %idx, %Value %v) {
  %data = getelementptr i8, ptr %env, i32 8
  %slot = getelementptr %Value, ptr %data, i32 %idx
  store %Value %v, ptr %slot
  ret void
}
define void @rt_make_closure(ptr %out, ptr %func, ptr %env, i32 %arity) {
  %mem = call ptr @malloc(i64 24)
  %p1 = getelementptr %Closure, ptr %mem, i32 0, i32 0
  store ptr %func, ptr %p1
  %p2 = getelementptr %Closure, ptr %mem, i32 0, i32 1
  store ptr %env, ptr %p2
  %p3 = getelementptr %Closure, ptr %mem, i32 0, i32 2
  store i32 %arity, ptr %p3
  %o1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 1, ptr %o1
  %o2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %o2
  ret void
}
define %Value @rt_call(%Value %clVal, ptr %args, i32 %num_args) {
  %clPtr = extractvalue %Value %clVal, 1
  %fpp = getelementptr %Closure, ptr %clPtr, i32 0, i32 0
  %func = load ptr, ptr %fpp
  %epp = getelementptr %Closure, ptr %clPtr, i32 0, i32 1
  %env = load ptr, ptr %epp
  %res = call %Value %func(ptr %env, ptr %args, i32 %num_args)
  ret %Value %res
}
define void @rt_setup_regs(ptr %regs, i32 %count, ptr %env, ptr %args, i32 %num_args) {
  %has_args = icmp ugt i32 %num_args, 0
  br i1 %has_args, label %copy_args, label %done
copy_args:
  %arg0 = load %Value, ptr %args
  store %Value %arg0, ptr %regs
  br label %done
done:
  ret void
}
"""
