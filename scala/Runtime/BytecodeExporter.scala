// ==========================================
// BytecodeExporter.scala
// .rbc (Romanesco ByteCode) 形式のバイナリエクスポーター
// ==========================================

package romanesco.Runtime

import java.io._
import java.nio._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class BytecodeExporter:
  private val constants = mutable.ArrayBuffer[Value]()
  private val codeBlocks = mutable.ArrayBuffer[Array[Op]]()
  private val blockMap = mutable.Map[Array[Op], Int]()
  private val constMap = mutable.Map[Value, Int]()

  /**
   * バイトコードをバイナリ形式 (.rbc) でファイルに出力する
   */
  def save(ops: Array[Op], path: String): Unit =
    constants.clear()
    codeBlocks.clear()
    blockMap.clear()
    constMap.clear()

    // 1. 全てのコードブロックと定数を収集
    collectBlocksAndConstants(ops)

    // 2. バイナリ構築 (Little Endian)
    val bos = new FileOutputStream(path)
    val out = new DataOutputStream(bos)

    try {
      // Header: "RBC\x01" + Version(1)
      out.write(Array[Byte]('R', 'B', 'C', 1))
      writeIntLE(out, 1)

      // Constant Pool
      writeIntLE(out, constants.size)
      for (v <- constants) {
        v match {
          case Value.Unit =>
            out.writeByte(5) // Tag: Unit
          case Value.Atom(n: Int) =>
            out.writeByte(6) // Tag: Bits (Legacy Int)
            writeLongLE(out, n.toLong)
          case Value.Atom(n: Long) =>
            out.writeByte(6)
            writeLongLE(out, n)
          case Value.Bits(n, _) =>
            out.writeByte(6)
            writeLongLE(out, n)
          case _ =>
            // サポート外の型はとりあえず Unit
            out.writeByte(5)
        }
      }

      // Code Blocks
      writeIntLE(out, codeBlocks.size)
      for (block <- codeBlocks) {
        writeIntLE(out, block.length)
        for (op <- block) {
          writeOp(out, op)
        }
      }
    } finally {
      out.close()
      bos.close()
    }

  private def collectBlocksAndConstants(ops: Array[Op]): Unit =
    if (blockMap.contains(ops)) return
    val id = codeBlocks.size
    codeBlocks += ops
    blockMap(ops) = id

    for op <- ops do
      op match
        case Op.LoadConst(_, v) => getConstId(v)
        case Op.MakeClosure(_, body, _, _) => collectBlocksAndConstants(body)
        case Op.Case(_, _, inl, inr) => 
          collectBlocksAndConstants(inl)
          collectBlocksAndConstants(inr)
        case _ => ()

  private def getConstId(v: Value): Int =
    constMap.getOrElseUpdate(v, {
      val id = constants.size
      constants += v
      id
    })

  private def writeOp(out: DataOutputStream, op: Op): Unit =
    op match
      case Op.Move(dst, src) =>
        out.writeByte(0x01); writeIntLE(out, dst); writeIntLE(out, src)
      case Op.LoadConst(dst, v) =>
        out.writeByte(0x02); writeIntLE(out, dst); writeIntLE(out, constMap(v))
      case Op.MakeClosure(dst, body, caps, arity) =>
        out.writeByte(0x03); writeIntLE(out, dst); writeIntLE(out, blockMap(body))
        writeIntLE(out, caps.length)
        for c <- caps do writeIntLE(out, c)
        writeIntLE(out, arity)
      case Op.Call(dst, func, args) =>
        out.writeByte(0x04); writeIntLE(out, dst); writeIntLE(out, func)
        writeIntLE(out, args.length)
        for a <- args do writeIntLE(out, a)
      case Op.Return(src) =>
        out.writeByte(0x05); writeIntLE(out, src)
      case Op.MakePair(dst, f, s) =>
        out.writeByte(0x06); writeIntLE(out, dst); writeIntLE(out, f); writeIntLE(out, s)
      case Op.Proj1(dst, src) =>
        out.writeByte(0x07); writeIntLE(out, dst); writeIntLE(out, src)
      case Op.Proj2(dst, src) =>
        out.writeByte(0x08); writeIntLE(out, dst); writeIntLE(out, src)
      case Op.MakeInl(dst, src) =>
        out.writeByte(0x09); writeIntLE(out, dst); writeIntLE(out, src)
      case Op.MakeInr(dst, src) =>
        out.writeByte(0x0A); writeIntLE(out, dst); writeIntLE(out, src)
      case Op.Case(dst, scr, inl, inr) =>
        out.writeByte(0x0B); writeIntLE(out, dst); writeIntLE(out, scr)
        writeIntLE(out, blockMap(inl)); writeIntLE(out, blockMap(inr))
      case Op.Add(dst, l, r) =>
        out.writeByte(0x0C); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.Sub(dst, l, r) =>
        out.writeByte(0x0D); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.Mul(dst, l, r) =>
        out.writeByte(0x0E); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.Borrow(dst, src) =>
        out.writeByte(0x0F); writeIntLE(out, dst); writeIntLE(out, src)
      case Op.Free(reg) =>
        out.writeByte(0x10); writeIntLE(out, reg)

      case Op.IBin(dst, l, r, op_type, w) =>
        out.writeByte(0x11); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
        out.writeByte(op_type.ordinal); writeInt16LE(out, w)
      case Op.ICmp(dst, l, r, pred, w) =>
        out.writeByte(0x12); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
        out.writeByte(pred.ordinal); writeInt16LE(out, w)
      case Op.LoadBits(dst, v, w) =>
        out.writeByte(0x13); writeIntLE(out, dst); writeLongLE(out, v); writeInt16LE(out, w)
      case Op.LoadWide(dst, limbs, w) =>
        out.writeByte(0x14); writeIntLE(out, dst); writeInt16LE(out, w)
        writeIntLE(out, limbs.length)
        for l <- limbs do writeLongLE(out, l)
      case Op.SExt(dst, src, f, t) =>
        out.writeByte(0x15); writeIntLE(out, dst); writeIntLE(out, src); writeInt16LE(out, f); writeInt16LE(out, t)
      case Op.ZExt(dst, src, f, t) =>
        out.writeByte(0x16); writeIntLE(out, dst); writeIntLE(out, src); writeInt16LE(out, f); writeInt16LE(out, t)
      case Op.Trunc(dst, src, f, t) =>
        out.writeByte(0x17); writeIntLE(out, dst); writeIntLE(out, src); writeInt16LE(out, f); writeInt16LE(out, t)
      case Op.Itof(dst, src, w, s) =>
        out.writeByte(0x18); writeIntLE(out, dst); writeIntLE(out, src); writeInt16LE(out, w); out.writeByte(if (s) 1 else 0)
      case Op.Ftoi(dst, src, w, s) =>
        out.writeByte(0x19); writeIntLE(out, dst); writeIntLE(out, src); writeInt16LE(out, w); out.writeByte(if (s) 1 else 0)
      case Op.FAdd(dst, l, r) =>
        out.writeByte(0x20); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.FSub(dst, l, r) =>
        out.writeByte(0x21); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.FMul(dst, l, r) =>
        out.writeByte(0x22); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.FDiv(dst, l, r) =>
        out.writeByte(0x23); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.FRem(dst, l, r) =>
        out.writeByte(0x24); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r)
      case Op.FCmp(dst, l, r, pred) =>
        out.writeByte(0x25); writeIntLE(out, dst); writeIntLE(out, l); writeIntLE(out, r); out.writeByte(pred.ordinal)

  private def writeIntLE(out: DataOutputStream, v: Int): Unit =
    out.writeByte(v & 0xFF)
    out.writeByte((v >> 8) & 0xFF)
    out.writeByte((v >> 16) & 0xFF)
    out.writeByte((v >> 24) & 0xFF)

  private def writeInt16LE(out: DataOutputStream, v: Int): Unit =
    out.writeByte(v & 0xFF)
    out.writeByte((v >> 8) & 0xFF)

  private def writeLongLE(out: DataOutputStream, v: Long): Unit =
    for i <- 0 until 8 do
      out.writeByte(((v >> (i * 8)) & 0xFF).toInt)
