// ==========================================
// LLVMCodeGen.scala
// 高速・高機能版 LLVM 生成器 (フル機能・最適化統合)
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

  def generate(code: Array[Op], entryName: String = "main", embedRuntime: Boolean = false): String =
    regCounter = 0
    functions.clear()
    val analysis = rangeAnalyzer.analyze(code)
    val maxReg = findMaxReg(code)
    
    val lines = new ArrayBuffer[String]()
    val allocas = new ArrayBuffer[String]()
    def emit(line: String): Unit = lines += s"  $line"
    def emitAlloca(line: String): Unit = allocas += s"  $line"

    // --- メイン関数生成 ---
    emit(s"%regs_base = alloca %Value, i32 ${maxReg + 1}")
    for i <- 0 to maxReg do
      val slot = freshReg()
      emit(s"$slot = getelementptr %Value, ptr %regs_base, i32 $i")
      emit(s"store %Value { i8 5, ptr null }, ptr $slot")

    def getRegPtr(idx: Int): String =
      val r = freshReg()
      emit(s"$r = getelementptr %Value, ptr %regs_base, i32 $idx")
      r.toString

    def consumeReg(idx: Int): String =
      val ptr = getRegPtr(idx)
      val v = freshReg()
      emit(s"$v = load %Value, ptr $ptr")
      emit(s"store %Value { i8 5, ptr null }, ptr $ptr")
      v.toString

    for op <- code do
      op match
        case Op.LoadConst(dst, Value.Atom(n: Int)) =>
          val tmp = freshReg()
          emit(s"$tmp = call %Value @rt_make_int(i64 $n)")
          emit(s"store %Value $tmp, ptr ${getRegPtr(dst)}")
        case Op.Move(dst, src) =>
          emit(s"store %Value ${consumeReg(src)}, ptr ${getRegPtr(dst)}")
        case Op.Borrow(dst, src) =>
          // 借用は単なるコピーだが、線形論理的には「消費しない」読み取り
          val ptr = getRegPtr(src)
          val v = freshReg()
          emit(s"$v = load %Value, ptr $ptr")
          emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
        case Op.Add(dst, l, r) => compileBinOpLinear(dst, l, r, "add", analysis, lines, getRegPtr, consumeReg)
        case Op.Sub(dst, l, r) => compileBinOpLinear(dst, l, r, "sub", analysis, lines, getRegPtr, consumeReg)
        case Op.Mul(dst, l, r) => compileBinOpLinear(dst, l, r, "mul", analysis, lines, getRegPtr, consumeReg)
        case Op.MakePair(dst, f, s) =>
          val fv = consumeReg(f); val sv = consumeReg(s)
          if (!analysis.escapes.contains(dst)) {
            val ptr = freshReg(); emitAlloca(s"$ptr = alloca %Pair")
            val p1 = freshReg(); emit(s"$p1 = getelementptr %Pair, ptr $ptr, i32 0, i32 0"); emit(s"store %Value $fv, ptr $p1")
            val p2 = freshReg(); emit(s"$p2 = getelementptr %Pair, ptr $ptr, i32 0, i32 1"); emit(s"store %Value $sv, ptr $p2")
            val v = freshReg(); emit(s"$v = insertvalue %Value { i8 2, ptr null }, ptr $ptr, 1")
            emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
          } else {
            val v = freshReg(); emit(s"$v = call %Value @rt_make_pair(%Value $fv, %Value $sv)")
            emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
          }
        case Op.Proj1(dst, src) =>
          val v = freshReg(); emit(s"$v = call %Value @rt_proj1(%Value ${consumeReg(src)})")
          emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
        case Op.Proj2(dst, src) =>
          val v = freshReg(); emit(s"$v = call %Value @rt_proj2(%Value ${consumeReg(src)})")
          emit(s"store %Value $v, ptr ${getRegPtr(dst)}")
        case Op.Free(reg) =>
          emit(s"call void @rt_free_value(%Value ${consumeReg(reg)})")
        case Op.Return(src) =>
          emit(s"ret %Value ${consumeReg(src)}")
        case _ => ()

    val cleanName = if entryName.startsWith("@") then entryName.drop(1) else entryName
    val mainFunc = s"define %Value @$cleanName() {\nentry:\n${(allocas ++ lines).mkString("\n")}\n  ret %Value { i8 5, ptr null }\n}"
    
    buildModule(mainFunc, embedRuntime)

  private def compileBinOpLinear(dst: Int, lhs: Int, rhs: Int, opName: String, 
                                 analysis: RangeAnalysisResult, lines: ArrayBuffer[String],
                                 getRegPtr: Int => String, consumeReg: Int => String): Unit =
    def emit(line: String): Unit = lines += s"  $line"
    val lv = freshReg(); emit(s"$lv = call i64 @rt_get_int(%Value ${consumeReg(lhs)})")
    val rv = freshReg(); emit(s"$rv = call i64 @rt_get_int(%Value ${consumeReg(rhs)})")
    val res = freshReg(); emit(s"$res = $opName i64 $lv, $rv")
    val rv2 = freshReg(); emit(s"$rv2 = call %Value @rt_make_int(i64 $res)")
    emit(s"store %Value $rv2, ptr ${getRegPtr(dst)}")

  private def findMaxReg(code: Array[Op]): Int =
    var max = -1
    def update(r: Int): Unit = if (r > max) max = r
    code.foreach {
      case Op.Move(d, s) => update(d); update(s)
      case Op.Borrow(d, s) => update(d); update(s)
      case Op.LoadConst(d, _) => update(d)
      case Op.Return(s) => update(s)
      case Op.MakePair(d, f, s) => update(d); update(f); update(s)
      case Op.Proj1(d, s) => update(d); update(s)
      case Op.Proj2(d, s) => update(d); update(s)
      case Op.Add(d, l, r) => update(d); update(l); update(r)
      case Op.Sub(d, l, r) => update(d); update(l); update(r)
      case Op.Mul(d, l, r) => update(d); update(l); update(r)
      case Op.Free(r) => update(r)
      case _ => ()
    }
    max

  private def buildModule(mainFunc: String, embedRuntime: Boolean): String =
    val sb = new StringBuilder()
    sb ++= "; Romanesco Optimized LLVM\n"
    sb ++= "%Value = type { i8, ptr }\n"
    sb ++= "%Pair = type { %Value, %Value }\n"
    if (embedRuntime) sb ++= """
declare ptr @malloc(i64)
declare void @free(ptr)
define %Value @rt_make_int(i64 %n) {
  %p = inttoptr i64 %n to ptr
  %v = insertvalue %Value { i8 6, ptr null }, ptr %p, 1
  ret %Value %v
}
define i64 @rt_get_int(%Value %v) {
  %p = extractvalue %Value %v, 1
  %n = ptrtoint ptr %p to i64
  ret i64 %n
}
define %Value @rt_make_pair(%Value %a, %Value %b) {
  %ptr = call ptr @malloc(i64 32)
  store %Value %a, ptr %ptr
  %p2 = getelementptr %Value, ptr %ptr, i32 1
  store %Value %b, ptr %p2
  %v = insertvalue %Value { i8 2, ptr null }, ptr %ptr, 1
  ret %Value %v
}
define %Value @rt_proj1(%Value %p) {
  %ptr = extractvalue %Value %p, 1
  %v = load %Value, ptr %ptr
  ret %Value %v
}
define %Value @rt_proj2(%Value %p) {
  %ptr = extractvalue %Value %p, 1
  %p2 = getelementptr %Value, ptr %ptr, i32 1
  %v = load %Value, ptr %p2
  ret %Value %v
}
define void @rt_free_value(%Value %v) {
  %tag = extractvalue %Value %v, 0
  %is_heap = icmp ule i8 %tag, 4
  br i1 %is_heap, label %do_free, label %done
do_free:
  %ptr = extractvalue %Value %v, 1
  call void @free(ptr %ptr)
  ret void
done:
  ret void
}
""" else sb ++= """
declare %Value @rt_make_int(i64)
declare i64 @rt_get_int(%Value)
declare %Value @rt_make_pair(%Value, %Value)
declare %Value @rt_proj1(%Value)
declare %Value @rt_proj2(%Value)
declare void @rt_free_value(%Value)
"""
    sb ++= "\n" + mainFunc + "\n"
    sb.toString
