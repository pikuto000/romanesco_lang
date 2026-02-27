// ==========================================
// LemmaManager.scala
// 補題（Lemma）の保存・読み込み管理
// ==========================================

package romanesco.Solver.core

import java.io.{File, PrintWriter}
import scala.io.Source
import romanesco.Solver.TestParser

object LemmaManager {

  /** 補題をテキストファイルに保存します。
    * フォーマット: name\tlhs\trhs\tuniv1,univ2,...
    * '#' で始まる行はコメントとして扱われます。
    */
  def saveLemmas(file: String, lemmas: List[CatRule]): Unit = {
    val writer = new PrintWriter(new File(file))
    try {
      writer.println("# Romanesco 補題ファイル")
      writer.println("# 形式: 名前<TAB>左辺<TAB>右辺<TAB>全称変数(カンマ区切り)")
      writer.println()
      lemmas.foreach { lemma =>
        val univs = lemma.universals.map(_.toString).mkString(",")
        writer.println(s"${lemma.name}\t${lemma.lhs}\t${lemma.rhs}\t$univs")
      }
    } finally {
      writer.close()
    }
  }

  /** 補題をテキストファイルから読み込みます。
    * '#' で始まる行はコメントとしてスキップされます。
    */
  def loadLemmas(file: String): List[CatRule] = {
    if (!new File(file).exists()) return Nil

    val source = Source.fromFile(file)
    try {
      source
        .getLines()
        .map(_.trim)
        .filter(line => line.nonEmpty && !line.startsWith("#"))
        .flatMap { line =>
          val parts = line.split("\t", -1)
          if (parts.length >= 3) {
            try {
              val name = parts(0)
              val lhs = TestParser.parse(parts(1), Set.empty)
              val rhs = TestParser.parse(parts(2), Set.empty)
              val universals = if (parts.length > 3 && parts(3).nonEmpty) {
                parts(3)
                  .split(",")
                  .map(s => TestParser.parse(s.trim, Set.empty))
                  .toList
              } else Nil
              Some(CatRule(name, lhs, rhs, universals))
            } catch {
              case e: Exception =>
                println(s"Warning: 補題の解析に失敗 (行: $line): ${e.getMessage}")
                None
            }
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
