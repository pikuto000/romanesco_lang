// ==========================================
// ResourceAnalyzer.scala
// 究極の簡潔版リソース追跡
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

enum ResourceKind:
  case Atom, PairStruct, InlStruct, InrStruct, Shell, UnitRes

case class Resource(id: Int, kind: ResourceKind, children: Set[Int] = Set.empty, initialChildren: Set[Int] = Set.empty, allocSite: Int = -1)

case class ResourceState(resources: Map[Int, Resource] = Map.empty, regs: Map[Int, Int] = Map.empty, nextId: Int = 0):
  def alloc(kind: ResourceKind, children: Set[Int], site: Int): (ResourceState, Int) =
    val rid = nextId
    val r = Resource(rid, kind, children, children, site)
    (copy(resources = resources + (rid -> r), nextId = nextId + 1), rid)
  def setReg(reg: Int, rid: Int): ResourceState = copy(regs = regs + (reg -> rid))
  def clearReg(reg: Int): ResourceState = copy(regs = regs - reg)
  def getReg(reg: Int): Option[Int] = regs.get(reg)

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
    val regsAt = mutable.Map[Int, Map[Int, Int]]()
    val garbage = mutable.Set[Int]()

    val lostAt = mutable.Map[Int, Set[Int]]()

    for (op, idx) <- code.zipWithIndex do
      regsAt(idx) = state.regs
      state = op match
        case Op.LoadConst(dst, _) =>
          val (s, rid) = state.alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.LoadBits(dst, _, _) =>
          val (s, rid) = state.alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.LoadWide(dst, _, _) =>
          val (s, rid) = state.alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.Free(reg) => state.clearReg(reg)
        case Op.Move(dst, src) =>
          val rid = state.getReg(src)
          val s2 = state.clearReg(src)
          rid.map(s2.setReg(dst, _)).getOrElse(s2)
        case Op.MakePair(dst, fst, snd) =>
          val fId = state.getReg(fst).getOrElse(-1)
          val sId = state.getReg(snd).getOrElse(-1)
          val (s, rid) = state.clearReg(fst).clearReg(snd).alloc(ResourceKind.PairStruct, Set(fId, sId).filter(_>=0), idx)
          s.setReg(dst, rid)
        case Op.Proj1(dst, src) =>
          val res = for { pId <- state.getReg(src); r <- state.resources.get(pId) if r.initialChildren.size >= 2 } yield (r.initialChildren.toList.sorted)
          val s2 = state.clearReg(src)
          res.map(ids => s2.setReg(dst, ids(0)).setReg(src, ids(1))).getOrElse(s2)
        case Op.Proj2(dst, src) =>
          val res = for { pId <- state.getReg(src); r <- state.resources.get(pId) if r.initialChildren.size >= 2 } yield (r.initialChildren.toList.sorted)
          val s2 = state.clearReg(src)
          res.map(ids => s2.setReg(dst, ids(1)).setReg(src, ids(0))).getOrElse(s2)
        case Op.Add(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.Sub(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.Mul(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.IBin(dst, l, r, _, _) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.ICmp(dst, l, r, _, _) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.FAdd(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.FSub(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.FMul(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.FDiv(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.FRem(dst, l, r) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.FCmp(dst, l, r, _) =>
          val (s, rid) = state.clearReg(l).clearReg(r).alloc(ResourceKind.Atom, Set.empty, idx)
          s.setReg(dst, rid)
        case Op.SExt(dst, src, _, _) =>
          val rid = state.getReg(src)
          val s2 = state.clearReg(src)
          rid.map(s2.setReg(dst, _)).getOrElse(s2)
        case Op.ZExt(dst, src, _, _) =>
          val rid = state.getReg(src)
          val s2 = state.clearReg(src)
          rid.map(s2.setReg(dst, _)).getOrElse(s2)
        case Op.Trunc(dst, src, _, _) =>
          val rid = state.getReg(src)
          val s2 = state.clearReg(src)
          rid.map(s2.setReg(dst, _)).getOrElse(s2)
        case Op.Itof(dst, src, _, _) =>
          val rid = state.getReg(src)
          val s2 = state.clearReg(src)
          rid.map(s2.setReg(dst, _)).getOrElse(s2)
        case Op.Ftoi(dst, src, _, _) =>
          val rid = state.getReg(src)
          val s2 = state.clearReg(src)
          rid.map(s2.setReg(dst, _)).getOrElse(s2)
        case Op.Borrow(dst, src) => state.getReg(src).map(state.setReg(dst, _)).getOrElse(state)
        case Op.Return(src) =>
          val returnedRid = state.getReg(src)
          val others = state.regs.filter(_._1 != src).values.toSet
          val trueLeaks = returnedRid match {
            case Some(rid) => others - rid
            case None => others
          }
          garbage ++= trueLeaks
          lostAt(idx) = trueLeaks
          state.copy(regs = Map.empty)
        case _ => state

    AnalysisResult(state, None, garbage.toSet, lostAt.toMap, regsAt.toMap, state.regs)

  private def allReachable(state: ResourceState): Set[Int] =
    state.regs.values.toSet.flatMap(rid => transitiveOwned(state, rid) + rid)
  private def transitiveOwned(state: ResourceState, rid: Int): Set[Int] =
    state.resources.get(rid).map(r => r.children ++ r.children.flatMap(transitiveOwned(state, _))).getOrElse(Set.empty)
