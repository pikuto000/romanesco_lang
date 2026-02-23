// ==========================================
// ProfileData.scala
// 実行時の統計情報と型プロファイルの記録
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

/** 命令の実行回数などの統計 */
case class OpProfile(
    var count: Long = 0
)
// 型情報の記録を削除

class ProfileData:
  private val profiles = mutable.Map[Int, OpProfile]() // pc -> profile

  def get(pc: Int): OpProfile =
    profiles.getOrElseUpdate(pc, OpProfile())

  def record(pc: Int): Unit =
    get(pc).count += 1

  def isHot(pc: Int, threshold: Int = 100): Boolean =
    get(pc).count >= threshold

  override def toString: String =
    profiles.map { case (pc, p) => s"$pc: count=${p.count}" }.mkString("\n")
