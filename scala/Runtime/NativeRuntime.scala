// ==========================================
// NativeRuntime.scala
// Project Panama (FFM API) 用の定数とユーティリティ
// ==========================================

package romanesco.Runtime

import java.lang.foreign._
import scala.collection.mutable.ArrayBuffer

object NativeRuntime:
  /** %Value = { i64, ptr } の構造体レイアウト */
  val VALUE_LAYOUT: StructLayout = MemoryLayout.structLayout(
    ValueLayout.JAVA_LONG.withName("tag"),
    ValueLayout.ADDRESS.withName("data")
  ).withByteAlignment(8)

  /** レジスタファイルの配列レイアウト */
  def regsLayout(count: Int): SequenceLayout = 
    MemoryLayout.sequenceLayout(count, VALUE_LAYOUT)

  /** Scala の Value から Native への同期 */
  def syncToNative(v: Value, segment: MemorySegment): Unit =
    v match
      case Value.Atom(n: Int) =>
        segment.set(ValueLayout.JAVA_LONG, 0, 6L) // Tag: Int
        segment.set(ValueLayout.ADDRESS, 8, MemorySegment.ofAddress(n.toLong))
      case Value.Atom(n: Long) =>
        segment.set(ValueLayout.JAVA_LONG, 0, 6L)
        segment.set(ValueLayout.ADDRESS, 8, MemorySegment.ofAddress(n))
      case Value.Unit =>
        segment.set(ValueLayout.JAVA_LONG, 0, 5L) // Tag: Unit
        segment.set(ValueLayout.ADDRESS, 8, MemorySegment.NULL)
      case _ =>
        // 他の型はポインタが絡むため現在は簡易実装
        segment.set(ValueLayout.JAVA_LONG, 0, 0L)
        segment.set(ValueLayout.ADDRESS, 8, MemorySegment.NULL)

  /** Native から Scala の Value への同期 */
  def syncFromNative(segment: MemorySegment): Value =
    val tag = segment.get(ValueLayout.JAVA_LONG, 0)
    val addr = segment.get(ValueLayout.ADDRESS, 8)
    tag match
      case 6 => Value.Atom(addr.address().toInt)
      case 5 => Value.Unit
      case 2 => Value.Atom("<Pair>")
      case 1 => Value.Atom("<Closure>")
      case _ => Value.Unit
