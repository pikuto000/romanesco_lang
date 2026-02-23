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
  private var regs: ArrayBuffer[Value] = initialRegs()
  def currentRegs: ArrayBuffer[Value] = regs

  /** プロファイリングを行いながらバイトコードを実行する */
  def runWithProfiling(code: Array[Op], regsInput: Option[ArrayBuffer[Value]] = None, startPc: Int = 0): Value =
    regsInput.foreach(r => regs = r)
    execProfiling(code, regs, 0, startPc)

  private def initialRegs(): ArrayBuffer[Value] =
    val r = new ArrayBuffer[Value](32)
    for _ <- 0 until 32 do r += Value.Unit
    r

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

  private def execProfiling(code: Array[Op], initialRegsForExec: ArrayBuffer[Value], depth: Int, startPc: Int = 0): Value =
    if depth > maxDepth then throw VMError("スタックオーバーフロー")
    
    var pc = startPc
    var retVal: Value = Value.Unit

    def getReg(idx: Int): Value = initialRegsForExec(idx)
    def setReg(idx: Int, v: Value): Unit = initialRegsForExec(idx) = v
    def consumeReg(idx: Int): Value = { val v = initialRegsForExec(idx); initialRegsForExec(idx) = Value.Unit; v }

    while pc < code.length do
      val op = code(pc)
      // リスタート直後はプロファイルをとらない（既にJITが走った証拠）
      if (pc > startPc) recordProfile(pc, op, initialRegsForExec)
      
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
        case Op.MakePair(dst, f, s) =>
          setReg(dst, Value.PairVal(consumeReg(f), consumeReg(s)))
          pc += 1
        case Op.Proj1(dst, src) =>
          consumeReg(src) match
            case Value.PairVal(f, s) =>
              setReg(dst, f)
              setReg(src, s)
            case v => throw VMError(s"Proj1: not a pair: $v")
          pc += 1
        case Op.Proj2(dst, src) =>
          consumeReg(src) match
            case Value.PairVal(f, s) =>
              setReg(dst, s)
              setReg(src, f)
            case v => throw VMError(s"Proj2: not a pair: $v")
          pc += 1
        case Op.MakeInl(dst, src) =>
          setReg(dst, Value.InlVal(consumeReg(src)))
          pc += 1
        case Op.MakeInr(dst, src) =>
          setReg(dst, Value.InrVal(consumeReg(src)))
          pc += 1
        case Op.Case(dst, scrut, inl, inr) =>
          val scrutVal = consumeReg(scrut)
          val (branch, innerV) = scrutVal match
            case Value.InlVal(v) => (inl, v)
            case Value.InrVal(v) => (inr, v)
            case _ => throw VMError(s"Case: invalid scrutinee $scrutVal")
          
          setReg(dst, innerV)
          execProfiling(branch, initialRegsForExec, depth + 1)
          pc += 1
        case Op.Return(src) =>
          return consumeReg(src)
        case _ => 
          pc += 1
    retVal
