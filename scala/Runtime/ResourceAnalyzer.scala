// ==========================================
// ResourceAnalyzer.scala
// バイトコードの抽象実行によるリソース追跡 (Linear Mode)
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

/** リソースの種別 */
enum ResourceKind:
  case Atom
  case PairStruct
  case InlStruct
  case InrStruct
  case Shell
  case UnitRes

/** リソース情報 */
case class Resource(
    id: Int,
    kind: ResourceKind,
    children: Set[Int] = Set.empty,
    initialChildren: Set[Int] = Set.empty,
    allocSite: Int = -1
)

/** 抽象実行の状態 */
case class ResourceState(
    resources: Map[Int, Resource] = Map.empty,
    regs: Map[Int, Int] = Map.empty,
    nextId: Int = 0
):
  def alloc(kind: ResourceKind, children: Set[Int], site: Int): (ResourceState, Int) =
    val rid = nextId
    val r = Resource(rid, kind, children, children, site)
    (copy(resources = resources + (rid -> r), nextId = nextId + 1), rid)

  def setReg(reg: Int, rid: Int): ResourceState = copy(regs = regs + (reg -> rid))
  def clearReg(reg: Int): ResourceState = copy(regs = regs - reg)
  def getReg(reg: Int): Option[Int] = regs.get(reg)

  /** 親リソースから子を抽出（所有権移動） */
  def extractChild(parentId: Int, childId: Int): ResourceState =
    resources.get(parentId) match
      case Some(r) =>
        val newChildren = r.children - childId
        val updated = r.copy(
          children = newChildren
        )
        copy(resources = resources + (parentId -> updated))
      case None => this

/** 解析結果 */
case class AnalysisResult(
    finalState: ResourceState,
    returnedResource: Option[Int],
    garbageResources: Set[Int],
    lostAt: Map[Int, Set[Int]],
    regsAt: Map[Int, Map[Int, Int]], 
    finalRegs: Map[Int, Int] 
):
  def garbageDetails: List[Resource] =
    garbageResources.toList.flatMap(rid => finalState.resources.get(rid)).sortBy(_.id)

class ResourceAnalyzer:

  def analyze(code: Array[Op]): AnalysisResult =
    var state = ResourceState()
    val lostAt = mutable.Map[Int, Set[Int]]()
    val regsAt = mutable.Map[Int, Map[Int, Int]]()

    for (op, idx) <- code.zipWithIndex do
      regsAt(idx) = state.regs
      val reachableBefore = allReachable(state)
      
      state = op match
        case Op.LoadConst(dst, Value.Atom(_)) =>
          val (s, rid) = state.alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)

        case Op.LoadConst(dst, Value.Unit) =>
          val (s, rid) = state.alloc(ResourceKind.UnitRes, Set.empty, idx)
          s.setReg(dst, rid)

        case Op.Move(dst, src) =>
          val s = state.getReg(src)
          val state2 = state.clearReg(src)
          s match
            case Some(rid) => state2.setReg(dst, rid)
            case None => state2

        case Op.MakePair(dst, fst, snd) =>
          val fstId = state.getReg(fst).getOrElse(-1)
          val sndId = state.getReg(snd).getOrElse(-1)
          val children = Set(fstId, sndId).filter(_ >= 0)
          val state2 = state.clearReg(fst).clearReg(snd)
          val (s, rid) = state2.alloc(ResourceKind.PairStruct, children, idx)
          s.setReg(dst, rid)

        case Op.Proj1(dst, src) =>
          val res = for {
            pairId <- state.getReg(src)
            pairRes <- state.resources.get(pairId)
            if pairRes.initialChildren.size >= 2
          } yield (pairRes.initialChildren.min, pairRes.initialChildren.max)
          
          val state2 = state.clearReg(src)
          res match
            case Some((fstId, sndId)) => 
              state2.setReg(dst, fstId).setReg(src, sndId)
            case None => state2

        case Op.Proj2(dst, src) =>
          val res = for {
            pairId <- state.getReg(src)
            pairRes <- state.resources.get(pairId)
            if pairRes.initialChildren.size >= 2
          } yield (pairRes.initialChildren.min, pairRes.initialChildren.max)
          
          val state2 = state.clearReg(src)
          res match
            case Some((fstId, sndId)) => 
              state2.setReg(dst, sndId).setReg(src, fstId)
            case None => state2

        case Op.Add(dst, lhs, rhs) =>
          val children = state.getReg(lhs).toSet ++ state.getReg(rhs).toSet
          var s2 = state.clearReg(lhs).clearReg(rhs)
          val (s3, rid) = s2.alloc(ResourceKind.Atom, children, idx)
          s3.setReg(dst, rid)

        case Op.Sub(dst, lhs, rhs) =>
          val children = state.getReg(lhs).toSet ++ state.getReg(rhs).toSet
          var s2 = state.clearReg(lhs).clearReg(rhs)
          val (s3, rid) = s2.alloc(ResourceKind.Atom, children, idx)
          s3.setReg(dst, rid)

        case Op.Mul(dst, lhs, rhs) =>
          val children = state.getReg(lhs).toSet ++ state.getReg(rhs).toSet
          var s2 = state.clearReg(lhs).clearReg(rhs)
          val (s3, rid) = s2.alloc(ResourceKind.Atom, children, idx)
          s3.setReg(dst, rid)

        case Op.Call(dst, f, args) =>
          val children = state.getReg(f).toSet ++ args.flatMap(state.getReg)
          var s2 = state.clearReg(f)
          args.foreach(a => s2 = s2.clearReg(a))
          val (s3, rid) = s2.alloc(ResourceKind.Atom, children, idx)
          s3.setReg(dst, rid)

        case Op.MakeInl(dst, src) =>
          val innerId = state.getReg(src).getOrElse(-1)
          val state2 = state.clearReg(src)
          val children = if innerId >= 0 then Set(innerId) else Set.empty[Int]
          val (s, rid) = state2.alloc(ResourceKind.InlStruct, children, idx)
          s.setReg(dst, rid)

        case Op.MakeInr(dst, src) =>
          val innerId = state.getReg(src).getOrElse(-1)
          val state2 = state.clearReg(src)
          val children = if innerId >= 0 then Set(innerId) else Set.empty[Int]
          val (s, rid) = state2.alloc(ResourceKind.InrStruct, children, idx)
          s.setReg(dst, rid)
        
        case Op.MakeClosure(dst, _, captures, _) =>
          val children = captures.flatMap(state.getReg).toSet
          var s2 = state
          captures.foreach(c => s2 = s2.clearReg(c))
          val (s3, rid) = s2.alloc(ResourceKind.Atom, children, idx)
          s3.setReg(dst, rid)

        case Op.Case(dst, scrutinee, _, _) =>
          val inner = for {
            pId <- state.getReg(scrutinee)
            pRes <- state.resources.get(pId)
            if pRes.initialChildren.nonEmpty
          } yield pRes.initialChildren.head
          
          val s2 = state.clearReg(scrutinee)
          inner match
            case Some(id) => s2.setReg(dst, id)
            case None => s2

        case Op.Return(src) =>
          // Return means EVERY register is cleared (lost to this function scope)
          state.copy(regs = Map.empty)

        case _ => state

      val reachableAfter = allReachable(state)
      val lost = (reachableBefore -- reachableAfter).filter { rid =>
        state.resources.get(rid).exists(_.kind != ResourceKind.UnitRes)
      }
      if lost.nonEmpty then lostAt(idx) = lost

    val totalLost = lostAt.values.flatten.toSet
    AnalysisResult(state, None, totalLost, lostAt.toMap, regsAt.toMap, state.regs)

  private def allReachable(state: ResourceState): Set[Int] =
    state.regs.values.toSet.flatMap(rid => transitiveOwned(state, rid) + rid)

  private def transitiveOwned(state: ResourceState, rid: Int): Set[Int] =
    state.resources.get(rid) match
      case Some(r) => r.children ++ r.children.flatMap(c => transitiveOwned(state, c))
      case None => Set.empty
