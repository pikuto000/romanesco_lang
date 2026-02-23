// ==========================================
// LLVMCodeGen.scala
// 完全自律型 LLVM 生成器 (Bug Fix: MaxReg)
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class CodeGenError(msg: String) extends RuntimeException(msg)

case class Reg(id: Int):
  override def toString: String = s"%v$id"

class LLVMCodeGen:
  private var regCounter = 0
  private val functions = new ArrayBuffer[String]()
  private val rangeAnalyzer = new RangeAnalyzer()

  private def freshReg(): Reg =
    val r = Reg(regCounter)
    regCounter += 1
    r

  def generate(code: Array[Op], entryName: String = "main", embedRuntime: Boolean = true): String =
    regCounter = 0
    functions.clear()
    val analysis = rangeAnalyzer.analyze(code)
    val maxReg = findMaxReg(code)
    
    val lines = new ArrayBuffer[String]()
    val allocas = new ArrayBuffer[String]()
    def emit(line: String): Unit = lines += s"  $line"
    def emitAlloca(line: String): Unit = allocas += s"  $line"

    def getRegPtr(idx: Int): String =
      val r = freshReg()
      emit(s"$r = getelementptr %Value, ptr %regs_base, i32 $idx")
      r.toString

    emit(s"%regs_base = alloca %Value, i32 ${maxReg + 1}")
    for i <- 0 to maxReg do
      emit(s"call void @rt_make_unit(ptr ${getRegPtr(i)})")

    def consumeReg(idx: Int): String =
      val ptr = getRegPtr(idx)
      val v = freshReg()
      emit(s"$v = load %Value, ptr $ptr")
      emit(s"call void @rt_make_unit(ptr $ptr)")
      v.toString

    for op <- code do
      op match
        case Op.LoadConst(dst, Value.Atom(n: Int)) =>
          emit(s"call void @rt_make_int(ptr ${getRegPtr(dst)}, i64 $n)")
        case Op.Move(dst, src) =>
          val v = consumeReg(src)
          emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
        case Op.Add(dst, l, r) => compileBinOp(dst, l, r, "add", analysis, lines, getRegPtr, consumeReg)
        case Op.Sub(dst, l, r) => compileBinOp(dst, l, r, "sub", analysis, lines, getRegPtr, consumeReg)
        case Op.Mul(dst, l, r) => compileBinOp(dst, l, r, "mul", analysis, lines, getRegPtr, consumeReg)
        case Op.MakePair(dst, f, s) =>
          emit(s"call void @rt_make_pair(ptr ${getRegPtr(dst)}, ptr ${getRegPtr(f)}, ptr ${getRegPtr(s)})")
        case Op.Proj1(dst, src) =>
          emit(s"call void @rt_proj1(ptr ${getRegPtr(dst)}, ptr ${getRegPtr(src)})")
        case Op.Return(src) =>
          val v = consumeReg(src)
          emit(s"ret %Value $v")
        case _ => ()

    val cleanName = if entryName.startsWith("@") then entryName.drop(1) else entryName
    val mainFunc = s"define %Value @$cleanName() {\nentry:\n${(allocas ++ lines).mkString("\n")}\n  %r_tmp = alloca %Value\n  call void @rt_make_unit(ptr %r_tmp)\n  %rv = load %Value, ptr %r_tmp\n  ret %Value %rv\n}"
    buildModule(mainFunc, embedRuntime)

  private def compileBinOp(dst: Int, lhs: Int, rhs: Int, op: String, 
                           analysis: RangeAnalysisResult, lines: ArrayBuffer[String],
                           getRegPtr: Int => String, consumeReg: Int => String): Unit =
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
      case Op.Proj1(d, s) => update(d); update(s)
      case Op.Proj2(d, s) => update(d); update(s)
      case Op.Add(d, l, r) => update(d); update(l); update(r)
      case Op.Sub(d, l, r) => update(d); update(l); update(r)
      case Op.Mul(d, l, r) => update(d); update(l); update(r)
      case Op.Free(r) => update(r)
      case Op.Borrow(d, s) => update(d); update(s)
      case _ => ()
    }
    max

  private def buildModule(mainFunc: String, embedRuntime: Boolean): String =
    val sb = new StringBuilder()
    sb ++= "; Romanesco Pure LLVM IR Module\n"
    sb ++= "%Value = type { i64, ptr }\n"
    sb ++= "%Pair = type { %Value, %Value }\n\n"
    sb ++= """
declare ptr @malloc(i64)
declare void @free(ptr)
declare i32 @printf(ptr, ...)
@.str_int_format = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
define void @rt_print_int(i64 %n) {
  call i32 (ptr, ...) @printf(ptr @.str_int_format, i64 %n)
  ret void
}
define void @rt_make_int(ptr %out, i64 %n) {
  %p_tag = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 6, ptr %p_tag
  %p_ptr = getelementptr %Value, ptr %out, i32 0, i32 1
  %val_ptr = inttoptr i64 %n to ptr
  store ptr %val_ptr, ptr %p_ptr
  ret void
}
define i64 @rt_get_int(ptr %v) {
  %p_ptr = getelementptr %Value, ptr %v, i32 0, i32 1
  %ptr = load ptr, ptr %p_ptr
  %n = ptrtoint ptr %ptr to i64
  ret i64 %n
}
define void @rt_make_unit(ptr %out) {
  %p_tag = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 5, ptr %p_tag
  %p_ptr = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr null, ptr %p_ptr
  ret void
}
define void @rt_make_pair(ptr %out, ptr %a, ptr %b) {
  %mem = call ptr @malloc(i64 32)
  %v_a = load %Value, ptr %a
  %v_b = load %Value, ptr %b
  store %Value %v_a, ptr %mem
  %p2 = getelementptr %Value, ptr %mem, i32 1
  store %Value %v_b, ptr %p2
  %p_tag = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 2, ptr %p_tag
  %p_ptr = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %p_ptr
  ret void
}
define void @rt_proj1(ptr %out, ptr %p) {
  %mem_ptr_ptr = getelementptr %Value, ptr %p, i32 0, i32 1
  %mem_ptr = load ptr, ptr %mem_ptr_ptr
  %val = load %Value, ptr %mem_ptr
  store %Value %val, ptr %out
  ret void
}
define void @rt_free_value(ptr %v) {
  %p_tag = getelementptr %Value, ptr %v, i32 0, i32 0
  %tag = load i64, ptr %p_tag
  %is_heap = icmp eq i64 %tag, 2
  br i1 %is_heap, label %do_free, label %done
do_free:
  %p_ptr = getelementptr %Value, ptr %v, i32 0, i32 1
  %ptr = load ptr, ptr %p_ptr
  call void @free(ptr %ptr)
  ret void
done:
  ret void
}
"""
    sb ++= "\n" + mainFunc + "\n"
    sb.toString
