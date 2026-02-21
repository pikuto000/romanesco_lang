// ==========================================
// ResourceAnalyzer.scala
// バイトコードの抽象実行によるリソース追跡
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

/** リソースの種別 */
enum ResourceKind:
  case Atom       // 不透明な定数（mallocされたpayload）
  case PairStruct // ペア構造体
  case InlStruct  // 余積（左）構造体
  case InrStruct  // 余積（右）構造体
  case Shell      // 子を抽出された後の空殻
  case UnitRes    // Unitはpayload=null、freeの必要なし

/** リソース情報 */
case class Resource(
    id: Int,
    kind: ResourceKind,
    children: Set[Int] = Set.empty,  // 直接所有する子リソースID
    allocSite: Int = -1              // 生成された命令のインデックス
)

/** 抽象実行の状態 */
case class ResourceState(
    resources: Map[Int, Resource] = Map.empty,  // 全リソース
    stack: List[Int] = Nil,                     // スタック上のリソースID
    nextId: Int = 0
):
  def alloc(kind: ResourceKind, children: Set[Int], site: Int): (ResourceState, Int) =
    val rid = nextId
    val r = Resource(rid, kind, children, site)
    (copy(resources = resources + (rid -> r), nextId = nextId + 1), rid)

  def push(rid: Int): ResourceState =
    copy(stack = rid :: stack)

  def pop(): (ResourceState, Int) =
    stack match
      case head :: tail => (copy(stack = tail), head)
      case Nil => throw RuntimeException("ResourceAnalyzer: スタックが空")

  /** 親リソースから子を抽出（所有権移動） */
  def extractChild(parentId: Int, childId: Int): ResourceState =
    resources.get(parentId) match
      case Some(r) =>
        val newChildren = r.children - childId
        val updated = r.copy(
          children = newChildren,
          kind = ResourceKind.Shell
        )
        copy(resources = resources + (parentId -> updated))
      case None => this

/** 解析結果 */
case class AnalysisResult(
    finalState: ResourceState,
    returnedResource: Option[Int],        // 返り値のリソースID
    garbageResources: Set[Int]            // freeが必要なリソース（返り値以外の全生存リソース）
):
  /** ゴミリソースの詳細情報 */
  def garbageDetails: List[Resource] =
    garbageResources.toList.flatMap(rid => finalState.resources.get(rid)).sortBy(_.id)

/** バイトコード列を抽象実行し、リソースの生成・所有権を追跡する */
class ResourceAnalyzer:

  /** 直線的なバイトコードを解析（分岐・クロージャ呼び出しは未対応） */
  def analyze(code: Array[Op]): AnalysisResult =
    var state = ResourceState()

    for (op, idx) <- code.zipWithIndex do
      op match
        case Op.PushConst(Value.Atom(_)) =>
          val (s, rid) = state.alloc(ResourceKind.Atom, Set.empty, idx)
          state = s.push(rid)

        case Op.PushConst(Value.Unit) =>
          val (s, rid) = state.alloc(ResourceKind.UnitRes, Set.empty, idx)
          state = s.push(rid)

        case Op.MakePair =>
          val (s1, sndId) = state.pop()
          val (s2, fstId) = s1.pop()
          val (s3, pairId) = s2.alloc(ResourceKind.PairStruct, Set(fstId, sndId), idx)
          state = s3.push(pairId)

        case Op.Proj1 =>
          val (s1, pairId) = state.pop()
          state.resources.get(pairId) match
            case Some(r) if r.children.size >= 1 =>
              // ペアの第1子を取り出す（ID順で小さい方が第1要素）
              val fstId = r.children.min
              state = s1.extractChild(pairId, fstId).push(fstId)
            case _ =>
              throw RuntimeException(s"ResourceAnalyzer: Proj1の対象がペアでない (r$pairId)")

        case Op.Proj2 =>
          val (s1, pairId) = state.pop()
          state.resources.get(pairId) match
            case Some(r) if r.children.size >= 1 =>
              val sndId = r.children.max
              state = s1.extractChild(pairId, sndId).push(sndId)
            case _ =>
              throw RuntimeException(s"ResourceAnalyzer: Proj2の対象がペアでない (r$pairId)")

        case Op.MakeInl =>
          val (s1, innerId) = state.pop()
          val (s2, inlId) = s1.alloc(ResourceKind.InlStruct, Set(innerId), idx)
          state = s2.push(inlId)

        case Op.MakeInr =>
          val (s1, innerId) = state.pop()
          val (s2, inrId) = s1.alloc(ResourceKind.InrStruct, Set(innerId), idx)
          state = s2.push(inrId)

        case Op.Return =>
          () // Returnは状態を変更しない（後で処理）

        case Op.Pop =>
          val (s1, _) = state.pop()
          state = s1

        case Op.PushConst(_) =>
          // Closure等の定数 — プロトタイプでは未対応
          val (s, rid) = state.alloc(ResourceKind.Atom, Set.empty, idx)
          state = s.push(rid)

        case _ =>
          // MakeClosure, Apply, Case, StoreVar, PushVar等 — プロトタイプでは未対応
          ()

    // 返り値の特定
    val returnedResource = state.stack.headOption

    // 全リソースのうち、返り値とその推移的子孫以外がゴミ
    val liveResources = returnedResource match
      case Some(rid) => transitiveOwned(state, rid) + rid
      case None => Set.empty[Int]

    // freeの必要がないリソース（UnitRes）を除外
    val allResources = state.resources.keySet
    val garbage = (allResources -- liveResources).filter { rid =>
      state.resources.get(rid).exists(_.kind != ResourceKind.UnitRes)
    }

    AnalysisResult(state, returnedResource, garbage)

  /** リソースIDから推移的に所有する全子孫を返す */
  private def transitiveOwned(state: ResourceState, rid: Int): Set[Int] =
    state.resources.get(rid) match
      case Some(r) =>
        r.children ++ r.children.flatMap(c => transitiveOwned(state, c))
      case None => Set.empty
