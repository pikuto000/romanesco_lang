// ==========================================
// BytecodeOptimizer.scala
// 命令数削減のための最適化パス
// ==========================================

package romanesco.Runtime

import scala.collection.mutable

class BytecodeOptimizer:
  /** 
   * バイトコードを最適化して命令数を削減する。
   * 複数のパス（定数畳み込み、DCE等）を収束するまで繰り返す。
   */
  def optimize(code: Array[Op]): Array[Op] =
    var currentCode = code
    var changed = true
    var fuel = 10 // 無限ループ防止

    while (changed && fuel > 0) {
      val (code1, c1) = foldConstants(currentCode)
      val (code2, c2) = eliminateDeadCode(code1)
      val (code3, c3) = simplifyMoves(code2)
      currentCode = code3
      changed = c1 || c2 || c3
      fuel -= 1
    }
    currentCode

  /** 定数同士の演算を事前に計算する */
  private def foldConstants(code: Array[Op]): (Array[Op], Boolean) =
    val constants = mutable.Map[Int, Value]()
    var changed = false
    
    val result = code.flatMap {
      case Op.LoadConst(d, v) =>
        constants(d) = v
        Some(Op.LoadConst(d, v))
      case Op.Add(d, l, r) if constants.contains(l) && constants.contains(r) =>
        (constants(l), constants(r)) match
          case (Value.Atom(a: Int), Value.Atom(b: Int)) =>
            val res = Value.Atom(a + b)
            constants(d) = res
            changed = true
            Some(Op.LoadConst(d, res))
          case _ => Some(Op.Add(d, l, r))
      case Op.Mul(d, l, r) if constants.contains(l) && constants.contains(r) =>
        (constants(l), constants(r)) match
          case (Value.Atom(a: Int), Value.Atom(b: Int)) =>
            val res = Value.Atom(a * b)
            constants(d) = res
            changed = true
            Some(Op.LoadConst(d, res))
          case _ => Some(Op.Mul(d, l, r))
      // 他の命令も同様に処理可能
      case op => Some(op)
    }
    (result, changed)

  /** 一度も使われない計算を削除する */
  private def eliminateDeadCode(code: Array[Op]): (Array[Op], Boolean) =
    val used = mutable.Set[Int]()
    // 最後に使われるレジスタや副作用のある命令をマーク
    code.foreach {
      case Op.Return(s) => used += s
      case Op.Add(_, l, r) => used += l; used += r
      case Op.Sub(_, l, r) => used += l; used += r
      case Op.Mul(_, l, r) => used += l; used += r
      case Op.Move(_, s) => used += s
      case Op.Borrow(_, s) => used += s
      case Op.Proj1(_, s) => used += s
      case Op.Proj2(_, s) => used += s
      case Op.MakePair(_, f, s) => used += f; used += s
      case Op.MakeInl(_, s) => used += s
      case Op.MakeInr(_, s) => used += s
      case Op.Case(_, s, _, _) => used += s
      case Op.Call(_, f, args) => used += f; used ++= args
      case Op.MakeClosure(_, _, caps, _) => used ++= caps
      case Op.Free(r) => used += r
      case _ => ()
    }

    var changed = false
    val result = code.filter {
      case Op.LoadConst(d, _) if !used.contains(d) => changed = true; false
      case Op.Add(d, _, _) if !used.contains(d) => changed = true; false
      case Op.Sub(d, _, _) if !used.contains(d) => changed = true; false
      case Op.Mul(d, _, _) if !used.contains(d) => changed = true; false
      case Op.Move(d, _) if !used.contains(d) => changed = true; false
      // Side-effecting ops (Call, MakeClosure etc) are kept
      case _ => true
    }
    (result, changed)

  /** 移動命令の簡略化 */
  private def simplifyMoves(code: Array[Op]): (Array[Op], Boolean) =
    var changed = false
    // 例: LoadConst(0, v) -> Move(1, 0) => LoadConst(1, v)
    // 今回は RegisterAllocator がある程度行うため、ここでは最小限の実装
    (code, changed)
