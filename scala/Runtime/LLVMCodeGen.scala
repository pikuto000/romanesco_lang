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

  private def freshReg(): Reg =
    val r = Reg(regCounter)
    regCounter += 1
    r

  private def freshClosureName(): String =
    val n = s"@__closure_$closureCounter"
    closureCounter += 1
    n

  def generate(code: Array[Op], entryName: String = "main", embedRuntime: Boolean = true): String =
    regCounter = 0
    closureCounter = 0
    functions.clear()
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

    emit(s"%regs_base = alloca %Value, i32 ${maxReg + 1}")
    if (isMain) {
      for i <- 0 to maxReg do emit(s"call void @rt_make_unit(ptr ${getRegPtr(i)})")
    } else {
      emit(s"call void @rt_setup_regs(ptr %regs_base, i32 ${maxReg + 1}, ptr %env, ptr %args, i32 %num_args)")
    }

    def consumeReg(idx: Int): String = {
      val ptr = getRegPtr(idx)
      val v = freshReg()
      emit(s"$v = load %Value, ptr $ptr")
      emit(s"call void @rt_make_unit(ptr $ptr)")
      v.toString
    }

    for op <- code do
      op match
        case Op.LoadConst(dst, Value.Atom(n: Int)) => emit(s"call void @rt_make_int(ptr ${getRegPtr(dst)}, i64 $n)")
        case Op.Move(dst, src) => emit(s"store %Value ${consumeReg(src)}, ptr ${getRegPtr(dst)}")
        case Op.Add(dst, l, r) => compileBinOp(dst, l, r, "add", analysis, lines, getRegPtr, consumeReg)
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
        case _ => ()

    regCounter = oldRegCounter
    val cleanName = if (name.startsWith("@")) name.drop(1) else name
    val retDefault = "  ret %Value { i64 5, ptr null }"
    val bodyStr = (allocas ++ lines).mkString("\n")
    if (isMain) s"define %Value @$cleanName() {\nentry:\n$bodyStr\n$retDefault\n}"
    else s"define %Value @$cleanName(ptr %env, ptr %args, i32 %num_args) {\nentry:\n$bodyStr\n$retDefault\n}"

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
declare %Value @rt_call(%Value, ptr, i32)
declare void @rt_setup_regs(ptr, i32, ptr, ptr, i32)
"""

  private val runtimeImplementation = """
declare ptr @malloc(i64)
declare void @free(ptr)
declare i32 @printf(ptr, ...)
@.str_int_fmt = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
define void @rt_print_int(i64 %n) {
  call i32 (ptr, ...) @printf(ptr @.str_int_fmt, i64 %n)
  ret void
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
  %p2 = getelementptr %Value, ptr %v, i32 0, i32 1
  %ptr = load ptr, ptr %p2
  %n = ptrtoint ptr %ptr to i64
  ret i64 %n
}
define void @rt_make_unit(ptr %out) {
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 5, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr null, ptr %p2
  ret void
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
