// ==========================================
// VM.scala
// レジスタベースのバイトコードインタプリタ (Linear Logic Mode)
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.ArrayBuffer

class VMError(msg: String) extends RuntimeException(msg)

class VM(
  maxDepth: Int = 1000,
  infiniteRegisters: Boolean = false,
  initialRegSize: Int = 32
):
  
  def run(code: Array[Op]): Value =
    exec(code, initialRegs(), 0)

  private def initialRegs(): ArrayBuffer[Value] =
    val regs = new ArrayBuffer[Value](initialRegSize)
    for _ <- 0 until initialRegSize do regs += Value.Unit
    regs

  private def exec(code: Array[Op], regs: ArrayBuffer[Value], depth: Int): Value =
    if depth > maxDepth then throw VMError("スタックオーバーフロー")
    
    var pc = 0
    var retVal: Value = Value.Unit

    def getReg(idx: Int): Value =
      if idx < 0 then throw VMError(s"負のレジスタインデックス: $idx")
      if idx >= regs.length then
        if infiniteRegisters then Value.Unit
        else throw VMError(s"レジスタ読み取り範囲外: $idx (上限: ${regs.length})")
      else regs(idx)

    def setReg(idx: Int, v: Value): Unit =
      if idx < 0 then throw VMError(s"負のレジスタインデックス: $idx")
      if idx >= regs.length then
        if infiniteRegisters then
          while regs.length <= idx do regs += Value.Unit
        else throw VMError(s"レジスタ読み取り範囲外: $idx (上限: ${regs.length})")
      regs(idx) = v

    def consumeReg(idx: Int): Value =
      val v = getReg(idx)
      setReg(idx, Value.Unit)
      v

    while pc < code.length do
      val op = code(pc)
      op match
        case Op.Move(dst, src) =>
          setReg(dst, consumeReg(src))
          pc += 1

        case Op.LoadConst(dst, v) =>
          setReg(dst, v)
          pc += 1

        case Op.MakeClosure(dst, body, captures, arity) =>
          val env = captures.map(idx => consumeReg(idx))
          setReg(dst, Value.Closure(body, env, arity))
          pc += 1

        case Op.Call(dst, funcIdx, argIdxs) =>
          val func = consumeReg(funcIdx)
          val args = argIdxs.map(idx => consumeReg(idx))
          func match
            case Value.Closure(body, env, arity) =>
              val newRegs = ArrayBuffer.from(env ++ args)
              if !infiniteRegisters then
                while newRegs.length < initialRegSize do newRegs += Value.Unit
              val res = exec(body, newRegs, depth + 1)
              setReg(dst, res)
              retVal = res
            case _ =>
              throw VMError(s"関数でない値へのCall: $func")
          pc += 1
          
        case Op.Return(src) =>
          return consumeReg(src)

        case Op.MakePair(dst, fst, snd) =>
          setReg(dst, Value.PairVal(consumeReg(fst), consumeReg(snd)))
          pc += 1

        case Op.Proj1(dst, src) =>
          consumeReg(src) match
            case Value.PairVal(f, s) => 
              setReg(dst, f)
              setReg(src, s) // ペアを分解して第2要素を元のレジスタに戻す
            case v => throw VMError(s"Proj1: ペアでない値: $v")
          pc += 1
          
        case Op.Proj2(dst, src) =>
          consumeReg(src) match
            case Value.PairVal(f, s) => 
              setReg(dst, s)
              setReg(src, f) // 第1要素を戻す
            case v => throw VMError(s"Proj2: ペアでない値: $v")
          pc += 1

        case Op.MakeInl(dst, src) =>
          setReg(dst, Value.InlVal(consumeReg(src)))
          pc += 1

        case Op.MakeInr(dst, src) =>
          setReg(dst, Value.InrVal(consumeReg(src)))
          pc += 1

        case Op.Case(dst, scrutinee, inlBranch, inrBranch) =>
           val res = consumeReg(scrutinee) match
             case Value.InlVal(v) =>
               val newRegs = regs.clone()
               setRegAt(newRegs, dst, v)
               exec(inlBranch, newRegs, depth + 1)
             case Value.InrVal(v) =>
               val newRegs = regs.clone()
               setRegAt(newRegs, dst, v)
               exec(inrBranch, newRegs, depth + 1)
             case v =>
               throw VMError(s"Case: inl/inrでない値: $v")
           setReg(dst, res)
           retVal = res
           pc += 1

        case Op.Free(reg) =>
           setReg(reg, Value.Unit)
           pc += 1

        case Op.Add(dst, lhs, rhs) =>
          val (lv, rv) = (getReg(lhs), getReg(rhs))
          consumeReg(lhs); consumeReg(rhs)
          val res = (lv, rv) match
            case (Value.Atom(a: Int), Value.Atom(b: Int)) => a.toLong + b.toLong
            case (Value.Atom(a: Long), Value.Atom(b: Long)) => a + b
            case (Value.Atom(a: Int), Value.Atom(b: Long)) => a.toLong + b
            case (Value.Atom(a: Long), Value.Atom(b: Int)) => a + b.toLong
            case _ => throw VMError(s"数値でない値の加算")
          setReg(dst, Value.Atom(res))
          pc += 1

        case Op.Sub(dst, lhs, rhs) =>
          val (lv, rv) = (getReg(lhs), getReg(rhs))
          consumeReg(lhs); consumeReg(rhs)
          val res = (lv, rv) match
            case (Value.Atom(a: Int), Value.Atom(b: Int)) => a.toLong - b.toLong
            case (Value.Atom(a: Long), Value.Atom(b: Long)) => a - b
            case (Value.Atom(a: Int), Value.Atom(b: Long)) => a.toLong - b
            case (Value.Atom(a: Long), Value.Atom(b: Int)) => a - b.toLong
            case _ => throw VMError(s"数値でない値の減算")
          setReg(dst, Value.Atom(res))
          pc += 1

        case Op.Mul(dst, lhs, rhs) =>
          val (lv, rv) = (getReg(lhs), getReg(rhs))
          consumeReg(lhs); consumeReg(rhs)
          val res = (lv, rv) match
            case (Value.Atom(a: Int), Value.Atom(b: Int)) => a.toLong * b.toLong
            case (Value.Atom(a: Long), Value.Atom(b: Long)) => a * b
            case (Value.Atom(a: Int), Value.Atom(b: Long)) => a.toLong * b
            case (Value.Atom(a: Long), Value.Atom(b: Int)) => a * b.toLong
            case _ => throw VMError(s"数値でない値の乗算")
          setReg(dst, Value.Atom(res))
          pc += 1

        case Op.Borrow(dst, src) =>
          setReg(dst, getReg(src))
          pc += 1
    
    retVal

  private def setRegAt(regs: ArrayBuffer[Value], idx: Int, v: Value): Unit =
    if idx >= regs.length then
      if infiniteRegisters then while regs.length <= idx do regs += Value.Unit
    regs(idx) = v
