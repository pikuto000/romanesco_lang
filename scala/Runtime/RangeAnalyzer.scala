// ==========================================
// RangeAnalyzer.scala
// 範囲解析 & エスケープ解析
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

case class RangeAnalysisResult(
    bitWidths: Map[Int, Int],
    escapes: Set[Int] // エスケープする（スタックに置けない）レジスタの集合
) {
  def bitWidth(reg: Int): Int = bitWidths.getOrElse(reg, 64)
  def doesEscape(reg: Int): Boolean = escapes.contains(reg)
}

class RangeAnalyzer:
  def analyze(code: Array[Op], profile: Option[ProfileData] = None): RangeAnalysisResult =
    val widths = mutable.Map[Int, Int]()
    // 初期状態: 全て 64bit
    for i <- 0 to 31 do widths(i) = 64

    // ヘルパー: 値から必要な最小ビット幅（iN）を正確に判定
    def requiredBits(n: Long): Int =
      if (n == 0) 1
      else if (n == -1) 1
      else {
        val magnitude = if (n < 0) -n - 1 else n
        val bits = 64 - java.lang.Long.numberOfLeadingZeros(magnitude) + 1
        Math.min(64, bits)
      }

    // プロファイル情報から、各レジスタで観測された最大ビット幅を事前に抽出
    profile.foreach { p =>
      for {
        ((_, _), prof) <- p.getAll
        regIdx <- 0 to 31
        value <- prof.dominantValue(regIdx)
      } {
        value match
          case Value.Atom(n: Int)  => widths(regIdx) = Math.max(widths(regIdx), requiredBits(n.toLong))
          case Value.Atom(n: Long) => widths(regIdx) = Math.max(widths(regIdx), requiredBits(n))
          case _ => ()
      }
    }

    // 前向きスキャンで依存関係に基づきビット幅を更新
    for (op, pc) <- code.zipWithIndex do
      op match
        case Op.LoadConst(dst, Value.Atom(n: Int)) => 
          widths(dst) = requiredBits(n.toLong)
        case Op.LoadConst(dst, Value.Atom(n: Long)) => 
          widths(dst) = requiredBits(n)
        case Op.Move(dst, src) => 
          widths(dst) = widths(src)
        case Op.Add(dst, l, r) =>
          widths(dst) = Math.min(64, Math.max(widths(l), widths(r)) + 1)
        case Op.Sub(dst, l, r) =>
          widths(dst) = Math.max(widths(l), widths(r))
        case Op.Mul(dst, l, r) =>
          widths(dst) = Math.min(64, widths(l) + widths(r))
        case _ => ()

    // エスケープ解析 (既存のロジックを維持)
    val finalEscapes = mutable.Set[Int]()
    val worklist = mutable.Queue[Int]()
    code.foreach {
      case Op.Return(r) => worklist.enqueue(r)
      case Op.MakeClosure(d, _, caps, _) => worklist.enqueue(d); caps.foreach(worklist.enqueue)
      case _ => ()
    }
    while (worklist.nonEmpty) {
      val r = worklist.dequeue()
      if (!finalEscapes.contains(r)) {
        finalEscapes += r
        for op <- code do
          op match
            case Op.MakePair(d, f, s) if d == r => worklist.enqueue(f); worklist.enqueue(s)
            case Op.Move(d, s) if d == r => worklist.enqueue(s)
            case _ => ()
      }
    }

    RangeAnalysisResult(widths.toMap, finalEscapes.toSet)
