// ==========================================
// ProfilingVM.scala
// 統計情報を収集しながら実行するインタープリタ
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.ArrayBuffer

class ProfilingVM(
  val profileData: ProfileData = new ProfileData(),
  val maxDepth: Int = 1000,
  initialRegSize: Int = 32
) extends VM(maxDepth = maxDepth, initialRegSize = initialRegSize):

  /** プロファイリングを行いながらバイトコードを実行する */
  def runWithProfiling(code: Array[Op], regs: Option[ArrayBuffer[Value]] = None, startPc: Int = 0): Value =
    execProfiling(code, regs.getOrElse(initialRegs()), 0, startPc)

  private def initialRegs(): ArrayBuffer[Value] =
    val regs = new ArrayBuffer[Value](32)
    for _ <- 0 until 32 do regs += Value.Unit
    regs

  /** 実行ごとに型情報をプロファイリングデータに記録する */
  private def recordProfile(pc: Int, op: Op, regs: ArrayBuffer[Value]): Unit =
    profileData.record(pc)
    val prof = profileData.get(pc)
    op match
      case Op.Add(_, l, r) =>
        prof.recordType(l, tagOf(regs(l)))
        prof.recordType(r, tagOf(regs(r)))
      case Op.Sub(_, l, r) =>
        prof.recordType(l, tagOf(regs(l)))
        prof.recordType(r, tagOf(regs(r)))
      case Op.Mul(_, l, r) =>
        prof.recordType(l, tagOf(regs(l)))
        prof.recordType(r, tagOf(regs(r)))
      case Op.Call(_, f, args) =>
        prof.recordType(f, tagOf(regs(f)))
        args.foreach(a => prof.recordType(a, tagOf(regs(a))))
      case Op.Proj1(_, s) =>
        prof.recordType(s, tagOf(regs(s)))
      case Op.Proj2(_, s) =>
        prof.recordType(s, tagOf(regs(s)))
      case _ => ()

  private def tagOf(v: Value): Long = v match
    case Value.Atom(_: Int) | Value.Atom(_: Long) => 6 // Int
    case Value.PairVal(_, _) => 2 // Pair
    case Value.Closure(_, _, _) => 1 // Closure
    case Value.Unit => 5 // Unit
    case Value.InlVal(_) | Value.InrVal(_) => 3 // Sum
    case Value.Atom(_) => 0 // Other

  private def execProfiling(code: Array[Op], regs: ArrayBuffer[Value], depth: Int, startPc: Int = 0): Value =
    if depth > maxDepth then throw VMError("スタックオーバーフロー")
    
    var pc = startPc
    var retVal: Value = Value.Unit

    def getReg(idx: Int): Value = regs(idx)
    def setReg(idx: Int, v: Value): Unit = regs(idx) = v
    def consumeReg(idx: Int): Value = { val v = regs(idx); regs(idx) = Value.Unit; v }

    while pc < code.length do
      val op = code(pc)
      // リスタート直後はプロファイルをとらない（既にJITが走った証拠）
      if (pc > startPc) recordProfile(pc, op, regs)
      
      op match
        case Op.Move(dst, src) =>
          setReg(dst, consumeReg(src))
          pc += 1
        case Op.LoadConst(dst, v) =>
          setReg(dst, v)
          pc += 1
        case Op.Add(dst, lhs, rhs) =>
          val (lv, rv) = (getReg(lhs), getReg(rhs)) // 型不一致でDeoptした可能性があるので、ここでもう一度チェック
          val res = (lv, rv) match
            case (Value.Atom(a: Int), Value.Atom(b: Int)) =>
              consumeReg(lhs); consumeReg(rhs)
              a.toLong + b.toLong
            case (Value.Atom(a: Long), Value.Atom(b: Long)) =>
              consumeReg(lhs); consumeReg(rhs)
              a + b
            case _ => 0L
          setReg(dst, Value.Atom(res))
          pc += 1
        case Op.Mul(dst, lhs, rhs) =>
          val (lv, rv) = (consumeReg(lhs), consumeReg(rhs))
          val res = (lv, rv) match
            case (Value.Atom(a: Int), Value.Atom(b: Int)) => a.toLong * b.toLong
            case (Value.Atom(a: Long), Value.Atom(b: Long)) => a * b
            case _ => 0L
          setReg(dst, Value.Atom(res))
          pc += 1
        case Op.Return(src) =>
          return consumeReg(src)
        case _ => 
          pc += 1
    retVal
