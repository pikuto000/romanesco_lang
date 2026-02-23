// ==========================================
// RegisterAllocator.scala
// レジスタ使用数の最適化（再割り当てによる最小化）
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

class RegisterAllocator:
  /** 
   * バイトコード内のレジスタ番号を、生存期間に基づき再割り当てして最小化する。
   * initialMapping: すでに割り当てが固定されているレジスタ（引数やキャプチャ等）
   */
  def allocate(code: Array[Op], initialMapping: Map[Int, Int] = Map.empty): Array[Op] =
    val lastUse = computeLastUses(code)
    val mapping = mutable.Map[Int, Int]() ++ initialMapping
    val freePool = mutable.PriorityQueue.empty(using Ordering[Int].reverse)
    
    // 最初に使用可能な最小レジスタ番号を決定
    var nextNew = if (mapping.isEmpty) 0 else mapping.values.max + 1
    
    // 穴あき部分があれば freePool に入れる
    val usedValues = mapping.values.toSet
    for (i <- 0 until nextNew) {
      if (!usedValues.contains(i)) freePool.enqueue(i)
    }

    def getMapped(old: Int): Int = mapping.getOrElse(old, old)

    def defineReg(old: Int): Int =
      if (mapping.contains(old)) mapping(old)
      else {
        val n = if (freePool.nonEmpty) freePool.dequeue() else { val r = nextNew; nextNew += 1; r }
        mapping(old) = n
        n
      }

    def releaseIfLast(old: Int, pc: Int): Unit =
      if (lastUse.get(old).contains(pc)) {
        mapping.get(old).foreach { n =>
          if (!initialMapping.contains(old)) {
            mapping.remove(old)
            freePool.enqueue(n)
          }
        }
      }

    val result = new mutable.ArrayBuffer[Op]()
    code.zipWithIndex.foreach { (op, pc) =>
      val newOp = op match
        case Op.Move(dst, src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          val d = defineReg(dst)
          if (d == s) None else Some(Op.Move(d, s))

        case Op.LoadConst(dst, v) =>
          Some(Op.LoadConst(defineReg(dst), v))

        case Op.Add(dst, l, r) =>
          val nl = getMapped(l); val nr = getMapped(r)
          releaseIfLast(l, pc); releaseIfLast(r, pc)
          Some(Op.Add(defineReg(dst), nl, nr))

        case Op.Sub(dst, l, r) =>
          val nl = getMapped(l); val nr = getMapped(r)
          releaseIfLast(l, pc); releaseIfLast(r, pc)
          Some(Op.Sub(defineReg(dst), nl, nr))

        case Op.Mul(dst, l, r) =>
          val nl = getMapped(l); val nr = getMapped(r)
          releaseIfLast(l, pc); releaseIfLast(r, pc)
          Some(Op.Mul(defineReg(dst), nl, nr))

        case Op.MakePair(dst, f, s) =>
          val nf = getMapped(f); val ns = getMapped(s)
          releaseIfLast(f, pc); releaseIfLast(s, pc)
          Some(Op.MakePair(defineReg(dst), nf, ns))

        case Op.Proj1(dst, src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          Some(Op.Proj1(defineReg(dst), s))

        case Op.Proj2(dst, src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          Some(Op.Proj2(defineReg(dst), s))

        case Op.MakeInl(dst, src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          Some(Op.MakeInl(defineReg(dst), s))

        case Op.MakeInr(dst, src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          Some(Op.MakeInr(defineReg(dst), s))

        case Op.Case(dst, scrut, inl, inr) =>
          val s = getMapped(scrut)
          releaseIfLast(scrut, pc)
          val currentMapping = mapping.toMap
          Some(Op.Case(defineReg(dst), s, allocate(inl, currentMapping), allocate(inr, currentMapping)))

        case Op.Call(dst, f, args) =>
          val nf = getMapped(f)
          val nargs = args.map(getMapped)
          releaseIfLast(f, pc)
          args.foreach(releaseIfLast(_, pc))
          Some(Op.Call(defineReg(dst), nf, nargs))

        case Op.MakeClosure(dst, body, caps, arity) =>
          val ncaps = caps.map(getMapped)
          val bodyMapping = mutable.Map[Int, Int]()
          caps.zipWithIndex.foreach { (old, i) => bodyMapping(old) = i }
          caps.foreach(releaseIfLast(_, pc))
          Some(Op.MakeClosure(defineReg(dst), allocate(body, bodyMapping.toMap), ncaps, arity))

        case Op.Return(src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          Some(Op.Return(s))

        case Op.Borrow(dst, src) =>
          val s = getMapped(src)
          releaseIfLast(src, pc)
          Some(Op.Borrow(defineReg(dst), s))

        case Op.Free(reg) =>
          val r = getMapped(reg)
          releaseIfLast(reg, pc)
          Some(Op.Free(r))
      
      newOp.foreach(result += _)
    }
    result.toArray

  private def computeLastUses(code: Array[Op]): Map[Int, Int] =
    val lastUse = mutable.Map[Int, Int]()
    code.zipWithIndex.foreach { (op, pc) =>
      op match
        case Op.Move(_, src) => lastUse(src) = pc
        case Op.Add(_, l, r) => lastUse(l) = pc; lastUse(r) = pc
        case Op.Sub(_, l, r) => lastUse(l) = pc; lastUse(r) = pc
        case Op.Mul(_, l, r) => lastUse(l) = pc; lastUse(r) = pc
        case Op.Call(_, f, args) => lastUse(f) = pc; args.foreach(lastUse(_) = pc)
        case Op.Proj1(_, src) => lastUse(src) = pc
        case Op.Proj2(_, src) => lastUse(src) = pc
        case Op.MakePair(_, f, s) => lastUse(f) = pc; lastUse(s) = pc
        case Op.MakeInl(_, src) => lastUse(src) = pc
        case Op.MakeInr(_, src) => lastUse(src) = pc
        case Op.Case(_, scrut, _, _) => lastUse(scrut) = pc
        case Op.Return(src) => lastUse(src) = pc
        case Op.MakeClosure(_, _, caps, _) => caps.foreach(lastUse(_) = pc)
        case Op.Borrow(_, src) => lastUse(src) = pc
        case Op.Free(reg) => lastUse(reg) = pc
        case _ => ()
    }
    lastUse.toMap
