// ==========================================
// ResourceOptimizer.scala
// リソース解析に基づくFree命令の挿入と検証 (Linear Mode)
// ==========================================

package romanesco.Runtime

import romanesco.Solver.core.{Expr, CatRule, Goal, ProverConfig, Prover}
import scala.collection.mutable
import scala.util.boundary

/** リソース最適化器
  * 解析パスでゴミリソースを特定し、Free命令を挿入する。
  */
class ResourceOptimizer:

  private val analyzer = new ResourceAnalyzer()

  /** バイトコードにFree命令を挿入して返す */
  def optimize(code: Array[Op]): Array[Op] =
    val result = analyzer.analyze(code)
    if result.lostAt.isEmpty then return code

    val newCode = new mutable.ArrayBuffer[Op]()

    for (op, idx) <- code.zipWithIndex do
      val lostRids = result.lostAt.getOrElse(idx, Set.empty)
      val regsBefore = result.regsAt(idx)

      // 命令の実行前に解放すべきもの
      op match
        case Op.LoadConst(dst, _) =>
          regsBefore.get(dst).foreach(rid => if lostRids.contains(rid) then newCode += Op.Free(dst))
        case Op.Move(dst, _) =>
          regsBefore.get(dst).foreach(rid => if lostRids.contains(rid) then newCode += Op.Free(dst))
        case Op.Return(src) =>
          // Return時、src以外の全レジスタのリソースを解放
          for (reg, rid) <- regsBefore if reg != src && lostRids.contains(rid) do
            newCode += Op.Free(reg)
        case _ => ()

      newCode += op
    
    newCode.toArray

  def verify(code: Array[Op]): Either[String, Unit] = boundary:
    val analysis = analyzer.analyze(code)
    val state = analysis.finalState
    
    var linearContext = mutable.Set[Int]()
    val currentRegs = mutable.Map[Int, Int]()

    for (op, idx) <- code.zipWithIndex do
      op match
        case Op.LoadConst(dst, _) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          analysis.finalState.resources.values.find(_.allocSite == idx) match
            case Some(res) => linearContext.add(res.id); currentRegs(dst) = res.id
            case None => ()

        case Op.Move(dst, src) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          currentRegs.remove(src).foreach(rid => currentRegs(dst) = rid)

        case Op.Add(dst, l, r) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          val lRid = currentRegs.remove(l); val rRid = currentRegs.remove(r)
          analysis.finalState.resources.values.find(_.allocSite == idx).foreach { res =>
            lRid.foreach(linearContext.remove); rRid.foreach(linearContext.remove); linearContext.add(res.id); currentRegs(dst) = res.id
          }

        case Op.Sub(dst, l, r) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          val lRid = currentRegs.remove(l); val rRid = currentRegs.remove(r)
          analysis.finalState.resources.values.find(_.allocSite == idx).foreach { res =>
            lRid.foreach(linearContext.remove); rRid.foreach(linearContext.remove); linearContext.add(res.id); currentRegs(dst) = res.id
          }

        case Op.Mul(dst, l, r) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          val lRid = currentRegs.remove(l); val rRid = currentRegs.remove(r)
          analysis.finalState.resources.values.find(_.allocSite == idx).foreach { res =>
            lRid.foreach(linearContext.remove); rRid.foreach(linearContext.remove); linearContext.add(res.id); currentRegs(dst) = res.id
          }

        case Op.MakePair(dst, lhs, rhs) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          val lRid = currentRegs.remove(lhs); val rRid = currentRegs.remove(rhs)
          analysis.finalState.resources.values.find(_.allocSite == idx).foreach { res =>
            lRid.foreach(linearContext.remove); rRid.foreach(linearContext.remove); linearContext.add(res.id); currentRegs(dst) = res.id
          }

        case Op.Proj1(dst, src) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          currentRegs.remove(src) match
            case Some(pairRid) =>
              analysis.finalState.resources.get(pairRid).foreach { p =>
                linearContext.remove(pairRid)
                val fstRid = p.initialChildren.min; val sndRid = p.initialChildren.max
                linearContext.add(fstRid); linearContext.add(sndRid)
                currentRegs(dst) = fstRid; currentRegs(src) = sndRid
              }
            case None => ()

        case Op.Proj2(dst, src) =>
          if currentRegs.contains(dst) then return Left(s"Overwrite detected without Free: reg $dst at op $idx")
          currentRegs.remove(src) match
            case Some(pairRid) =>
              analysis.finalState.resources.get(pairRid).foreach { p =>
                linearContext.remove(pairRid)
                val fstRid = p.initialChildren.min; val sndRid = p.initialChildren.max
                linearContext.add(fstRid); linearContext.add(sndRid)
                currentRegs(dst) = sndRid; currentRegs(src) = fstRid
              }
            case None => ()

        case Op.Free(reg) =>
          currentRegs.remove(reg).foreach { rid =>
            if !linearContext.contains(rid) then boundary.break(Left(s"Double Free: r$rid at op $idx"))
            (transitiveOwnedIds(analysis.finalState, rid) + rid).foreach(linearContext.remove)
          }

        case Op.Return(src) =>
          val retRid = currentRegs.remove(src)
          retRid.foreach(rid => (transitiveOwnedIds(analysis.finalState, rid) + rid).foreach(linearContext.remove))
          if linearContext.nonEmpty then
            boundary.break(Left(s"Memory Leak: Unfreed resources: ${linearContext.map(id => s"r$id").mkString(", ")}"))

        case _ => ()
    
    Right(())

  private def transitiveOwnedIds(state: ResourceState, rid: Int): Set[Int] =
    state.resources.get(rid) match
      case Some(r) => r.children ++ r.children.flatMap(c => transitiveOwnedIds(state, c))
      case None => Set.empty
