// ==========================================
// ProfileData.scala
// 実行時の統計情報と型プロファイルの記録
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

/** 命令ごとの型情報統計 */
case class OpProfile(
  var count: Long = 0,
  val typeCounts: mutable.Map[Int, mutable.Map[Long, Long]] = mutable.Map() // regIdx -> (tag -> count)
):
  def recordType(reg: Int, tag: Long): Unit =
    val regStats = typeCounts.getOrElseUpdate(reg, mutable.Map())
    regStats(tag) = regStats.getOrElse(tag, 0L) + 1

  /** 最も頻出するタグを取得 */
  def dominantTag(reg: Int): Option[Long] =
    typeCounts.get(reg).flatMap { stats =>
      if stats.isEmpty then None
      else Some(stats.maxBy(_._2)._1)
    }

class ProfileData:
  private val profiles = mutable.Map[Int, OpProfile]() // pc -> profile

  def get(pc: Int): OpProfile =
    profiles.getOrElseUpdate(pc, OpProfile())

  def record(pc: Int): Unit =
    get(pc).count += 1

  def isHot(pc: Int, threshold: Int = 100): Boolean =
    get(pc).count >= threshold

  override def toString: String =
    profiles.map { case (pc, p) => s"$pc: count=${p.count}, types=${p.typeCounts}" }.mkString("\n")
