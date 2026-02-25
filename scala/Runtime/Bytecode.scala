// ==========================================
// Bytecode.scala
// バイトコード命令セットと値型の定義（レジスタマシン版）
// ==========================================

package romanesco.Runtime

/** 整数演算の種類 */
enum IBinOp:
  case Add, Sub, Mul
  case SDiv, UDiv
  case SRem, URem
  case And, Or, Xor
  case Shl, LShr, Ashr

/** 整数比較の述語 */
enum ICmpPred:
  case Eq, Ne
  case Slt, Sle, Sgt, Sge
  case Ult, Ule, Ugt, Uge

/** 浮動小数点比較の述語 */
enum FCmpPred:
  case Oeq, One, Olt, Ole, Ogt, Oge, Ord, Uno

/** バイトコード命令
  * レジスタは Int で指定されるインデックス。
  */
enum Op:
  case Move(dst: Int, src: Int)                     // dst = src
  case LoadConst(dst: Int, value: Value)            // dst = value
  case MakeClosure(dst: Int, body: Array[Op], captures: Array[Int], arity: Int) // dst = closure(body, captures)
  case Call(dst: Int, func: Int, args: Array[Int])  // dst = func(args...)
  case Return(src: Int)                             // return src
  case MakePair(dst: Int, fst: Int, snd: Int)       // dst = (fst, snd)
  case Proj1(dst: Int, src: Int)                    // dst = src._1
  case Proj2(dst: Int, src: Int)                    // dst = src._2
  case MakeInl(dst: Int, src: Int)                  // dst = inl(src)
  case MakeInr(dst: Int, src: Int)                  // dst = inr(src)
  case Case(dst: Int, scrutinee: Int, inlBranch: Array[Op], inrBranch: Array[Op]) // dst = case scrutinee of ...
  case Add(dst: Int, lhs: Int, rhs: Int)            // dst = lhs + rhs (Legacy)
  case Sub(dst: Int, lhs: Int, rhs: Int)            // dst = lhs - rhs (Legacy)
  case Mul(dst: Int, lhs: Int, rhs: Int)            // dst = lhs * rhs (Legacy)
  case Borrow(dst: Int, src: Int)                   // dst = reference to src (src is not consumed)
  case Free(reg: Int)                               // free value in register 'reg'

  // --- New Zig-compatible instructions ---
  case IBin(dst: Int, lhs: Int, rhs: Int, op: IBinOp, width: Int)
  case ICmp(dst: Int, lhs: Int, rhs: Int, pred: ICmpPred, width: Int)
  case LoadBits(dst: Int, value: Long, width: Int)
  case LoadWide(dst: Int, limbs: Array[Long], width: Int)
  case SExt(dst: Int, src: Int, from: Int, to: Int)
  case ZExt(dst: Int, src: Int, from: Int, to: Int)
  case Trunc(dst: Int, src: Int, from: Int, to: Int)
  case Itof(dst: Int, src: Int, width: Int, signed: Boolean)
  case Ftoi(dst: Int, src: Int, width: Int, signed: Boolean)
  case FAdd(dst: Int, lhs: Int, rhs: Int)
  case FSub(dst: Int, lhs: Int, rhs: Int)
  case FMul(dst: Int, lhs: Int, rhs: Int)
  case FDiv(dst: Int, lhs: Int, rhs: Int)
  case FRem(dst: Int, lhs: Int, rhs: Int)
  case FCmp(dst: Int, lhs: Int, rhs: Int, pred: FCmpPred)

/** VM上の値 */
enum Value:
  case Atom(value: Any)                                   // 不透明な定数（整数、文字列等）
  case Closure(body: Array[Op], env: Array[Value], arity: Int) // クロージャ
  case PairVal(fst: Value, snd: Value)                    // 積型
  case InlVal(v: Value)                                   // 余積（左）
  case InrVal(v: Value)                                   // 余積（右）
  case Unit                                               // 単位型（⊤ / Terminal）
  case Bits(value: Long, width: Int)                      // 固定長ビット列
  case Wide(limbs: Array[Long], width: Int)               // 任意精度ビット列

  override def toString: String = this match
    case Atom(v)       => v.toString
    case Closure(_, _, a) => s"<closure/$a>"
    case PairVal(a, b) => s"($a, $b)"
    case InlVal(v)     => s"inl($v)"
    case InrVal(v)     => s"inr($v)"
    case Unit          => "()"
    case Bits(v, w)    => s"${v}:i$w"
    case Wide(_, w)    => s"<bigint:i$w>"

/** コールフレーム */
case class Frame(
    returnAddr: Int,          // 呼び出し元の命令位置
    code: Array[Op],          // 現在のコード
    regs: Array[Value]        // レジスタファイル
)
