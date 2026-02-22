// ==========================================
// RangeAnalyzer.scala
// 各レジスタの値の範囲（Range）を解析し、ビット幅を推論する
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

/** 値の範囲を表す */
case class ValueRange(min: Long, max: Long):
  def isUnknown: Boolean = min == Long.MinValue && max == Long.MaxValue
  
  def +(other: ValueRange): ValueRange =
    if isUnknown || other.isUnknown then ValueRange.unknown
    else ValueRange(min + other.min, max + other.max)

  def -(other: ValueRange): ValueRange =
    if isUnknown || other.isUnknown then ValueRange.unknown
    else ValueRange(min - other.max, max - other.min)

  def *(other: ValueRange): ValueRange =
    if isUnknown || other.isUnknown then ValueRange.unknown
    else
      val products = List(min * other.min, min * other.max, max * other.min, max * other.max)
      ValueRange(products.min, products.max)

  /** 必要なビット幅を返す (8, 16, 32, 64) */
  def bitWidth: Int =
    if min >= -128 && max <= 127 then 8
    else if min >= -32768 && max <= 32767 then 16
    else if min >= -2147483648L && max <= 2147483647L then 32
    else 64

object ValueRange:
  val unknown = ValueRange(Long.MinValue, Long.MaxValue)
  def const(v: Long) = ValueRange(v, v)

/** 範囲解析の結果 */
case class RangeAnalysisResult(
    registerRanges: Map[Int, ValueRange]
):
  def bitWidth(reg: Int): Int = registerRanges.getOrElse(reg, ValueRange.unknown).bitWidth

class RangeAnalyzer:
  /** コードを解析して各レジスタの範囲を推定する */
  def analyze(code: Array[Op]): RangeAnalysisResult =
    val ranges = mutable.Map[Int, ValueRange]()
    
    for op <- code do
      op match
        case Op.LoadConst(dst, Value.Atom(n: Int)) =>
          ranges(dst) = ValueRange.const(n.toLong)
        case Op.LoadConst(dst, Value.Atom(n: Long)) =>
          ranges(dst) = ValueRange.const(n)
        case Op.Move(dst, src) =>
          ranges(dst) = ranges.getOrElse(src, ValueRange.unknown)
        case Op.Add(dst, lhs, rhs) =>
          ranges(dst) = ranges.getOrElse(lhs, ValueRange.unknown) + ranges.getOrElse(rhs, ValueRange.unknown)
        case Op.Sub(dst, lhs, rhs) =>
          ranges(dst) = ranges.getOrElse(lhs, ValueRange.unknown) - ranges.getOrElse(rhs, ValueRange.unknown)
        case Op.Mul(dst, lhs, rhs) =>
          ranges(dst) = ranges.getOrElse(lhs, ValueRange.unknown) * ranges.getOrElse(rhs, ValueRange.unknown)
        case _ =>
          // その他の命令（Call等）の結果は未知とする
          // dst を持つ命令の場合は unknown をセット
          op match
            case Op.MakeClosure(dst, _, _, _) => ranges(dst) = ValueRange.unknown
            case Op.Call(dst, _, _) => ranges(dst) = ValueRange.unknown
            case Op.MakePair(dst, _, _) => ranges(dst) = ValueRange.unknown
            case Op.Proj1(dst, _) => ranges(dst) = ValueRange.unknown
            case Op.Proj2(dst, _) => ranges(dst) = ValueRange.unknown
            case Op.MakeInl(dst, _) => ranges(dst) = ValueRange.unknown
            case Op.MakeInr(dst, _) => ranges(dst) = ValueRange.unknown
            case Op.Case(dst, _, _, _) => ranges(dst) = ValueRange.unknown
            case _ => ()

    RangeAnalysisResult(ranges.toMap)
