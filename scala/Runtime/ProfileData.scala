// ==========================================
// ProfileData.scala
// 実行時の統計情報と型プロファイルの記録
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

/** 命令の実行回数や動的な振る舞いの統計 */
case class OpProfile(
    var count: Long = 0,
    // 分岐の統計: branchIndex -> 実行回数 (0: inl/left, 1: inr/right)
    val branchCounts: mutable.Map[Int, Long] = mutable.Map(),
    // 呼び出し先の統計: bodyのハッシュ値 -> 実行回数
    val callTargets: mutable.Map[Int, Long] = mutable.Map(),
    // レジスタ値の統計: regIdx -> (Value -> count)
    val valuePatterns: mutable.Map[Int, mutable.Map[Value, Long]] = mutable.Map()
):
  def recordBranch(idx: Int): Unit =
    branchCounts(idx) = branchCounts.getOrElse(idx, 0L) + 1

  def recordCallTarget(body: Array[Op]): Unit =
    val id = System.identityHashCode(body)
    callTargets(id) = callTargets.getOrElse(id, 0L) + 1

  def recordValue(regIdx: Int, v: Value): Unit =
    val stats = valuePatterns.getOrElseUpdate(regIdx, mutable.Map())
    v match
      case Value.Atom(_) | Value.Unit => 
        stats(v) = stats.getOrElse(v, 0L) + 1
      case Value.Closure(_, _, arity) => 
        // クロージャは arity と ID で記録
        val tag = Value.Atom(s"<closure/$arity>")
        stats(tag) = stats.getOrElse(tag, 0L) + 1
      case _ =>
        val tag = Value.Atom(s"<${v.getClass.getSimpleName}>")
        stats(tag) = stats.getOrElse(tag, 0L) + 1

  /** 最も頻出する分岐インデックスを取得 */
  def dominantBranch: Option[Int] =
    if branchCounts.isEmpty then None else Some(branchCounts.maxBy(_._2)._1)

  /** 最も頻出する呼び出し先（bodyのハッシュ）を取得 */
  def dominantCallTarget: Option[Int] =
    if callTargets.isEmpty then None else Some(callTargets.maxBy(_._2)._1)

  /** 特定のレジスタで支配的な定数値を取得 */
  def dominantValue(regIdx: Int): Option[Value] =
    valuePatterns.get(regIdx).flatMap { stats =>
      if stats.isEmpty then None else Some(stats.maxBy(_._2)._1)
    }

class ProfileData:
  // (codeBlockId, pc) -> profile
  private val profiles = mutable.Map[(Int, Int), OpProfile]()

  def getAll: Map[(Int, Int), OpProfile] = profiles.toMap

  def loadFrom(other: ProfileData): Unit =
    profiles ++= other.getAll

  private def getCodeId(code: Array[Op]): Int = System.identityHashCode(code)

  def get(code: Array[Op], pc: Int): OpProfile =
    profiles.getOrElseUpdate((getCodeId(code), pc), OpProfile())

  def record(code: Array[Op], pc: Int): Unit =
    get(code, pc).count += 1

  def isHot(code: Array[Op], pc: Int, threshold: Int = 100): Boolean =
    get(code, pc).count >= threshold

  override def toString: String =
    profiles.map { case ((id, pc), p) =>
      val branches = if p.branchCounts.isEmpty then "" else s", branches=${p.branchCounts}"
      val calls = if p.callTargets.isEmpty then "" else s", calls=${p.callTargets}"
      val values = if p.valuePatterns.isEmpty then "" else s", values=${p.valuePatterns}"
      s"Block($id) PC($pc): count=${p.count}$branches$calls$values"
    }.mkString("\n")
