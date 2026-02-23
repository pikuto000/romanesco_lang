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
  def analyze(code: Array[Op]): RangeAnalysisResult =
    val widths = mutable.Map[Int, Int]()
    val escapes = mutable.Set[Int]()
    
    // 簡易的なエスケープ解析 (逆向き伝播)
    def markEscape(reg: Int): Unit =
      if (!escapes.contains(reg)) {
        escapes += reg
        // このレジスタが依存しているリソースもエスケープさせる
        // (本来はデータフローを追う必要があるが、ここでは保守的に全てマーク)
      }

    // 前向きスキャン
    for op <- code do
      op match
        case Op.Return(src) => markEscape(src)
        case Op.MakeClosure(_, _, caps, _) => caps.foreach(markEscape)
        case Op.Call(_, f, args) => markEscape(f); args.foreach(markEscape)
        // ペアへの格納は、ペア自体がエスケープするなら中身もエスケープ
        case Op.MakePair(dst, fst, snd) =>
          // 保守的に、一旦 MakePair は全てエスケープ候補とする（後のパスで精査）
          // ただし、もし dst が Return されないならスタックに置ける
        case _ => ()

    // 2パス目: 依存関係の解消
    // 実際には Return されるもの、または Global に出るものだけを Escape とする
    // 今回は「Return されるレジスタ」から辿れるものだけを Escapes とする
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
        // r を生成した命令を探して、その入力もエスケープさせる
        for op <- code do
          op match
            case Op.MakePair(d, f, s) if d == r => 
              // ペアの中身は、ペア自体がエスケープするならエスケープする
              worklist.enqueue(f); worklist.enqueue(s)
            case Op.Move(d, s) if d == r => worklist.enqueue(s)
            case Op.Proj1(d, s) if d == r => 
              // Proj1 で取り出した要素がエスケープしても、元のペア s がエスケープするとは限らない
              // (ただし、今回は簡潔さのため、保守的に s もエスケープ対象から外してみる)
              () 
            case Op.Proj2(d, s) if d == r => ()
            case _ => ()
      }
    }

    RangeAnalysisResult(widths.toMap, finalEscapes.toSet)
