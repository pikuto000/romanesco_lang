// ==========================================
// LemmaManager.scala
// 補題（Lemma）の保存・読み込み管理
// ==========================================

package romanesco.Solver.core

import java.io.{File, PrintWriter}
import scala.io.Source
import romanesco.Solver.TestParser

object LemmaManager {

  /** 補題をテキストファイルに保存します。 フォーマット: name\tlhs\trhs\tuniv1,univ2,...
    */
  def saveLemmas(file: String, lemmas: List[CatRule]): Unit = {
    val writer = new PrintWriter(new File(file))
    try {
      lemmas.foreach { lemma =>
        val univs = lemma.universals.map(_.toString).mkString(",")
        writer.println(s"${lemma.name}\t${lemma.lhs}\t${lemma.rhs}\t$univs")
      }
    } finally {
      writer.close()
    }
  }

  /** 補題をテキストファイルから読み込みます。
    */
  def loadLemmas(file: String): List[CatRule] = {
    if (!new File(file).exists()) return Nil

    val source = Source.fromFile(file)
    try {
      source
        .getLines()
        .filter(_.trim.nonEmpty)
        .flatMap { line =>
          val parts = line.split("\t", -1)
          if (parts.length >= 3) {
            val name = parts(0)
            val lhs = TestParser.parse(parts(1), Set.empty)
            val rhs = TestParser.parse(parts(2), Set.empty)
            val universals = if (parts.length > 3 && parts(3).nonEmpty) {
              parts(3)
                .split(",")
                .map(s => TestParser.parse(s, Set.empty))
                .toList
            } else Nil
            Some(CatRule(name, lhs, rhs, universals))
          } else None
        }
        .toList
    } catch {
      case e: Exception =>
        println(s"Error loading lemmas from $file: ${e.getMessage}")
        Nil
    } finally {
      source.close()
    }
  }
}
