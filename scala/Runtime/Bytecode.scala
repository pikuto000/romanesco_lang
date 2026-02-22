// ==========================================
// Bytecode.scala
// バイトコード命令セットと値型の定義（レジスタマシン版）
// ==========================================

package romanesco.Runtime

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
  case Add(dst: Int, lhs: Int, rhs: Int)            // dst = lhs + rhs
  case Sub(dst: Int, lhs: Int, rhs: Int)            // dst = lhs - rhs
  case Mul(dst: Int, lhs: Int, rhs: Int)            // dst = lhs * rhs
  case Borrow(dst: Int, src: Int)                   // dst = reference to src (src is not consumed)
  case Free(reg: Int)                               // free value in register 'reg'

/** VM上の値 */
enum Value:
  case Atom(value: Any)                                   // 不透明な定数（整数、文字列等）
  case Closure(body: Array[Op], env: Array[Value], arity: Int) // クロージャ
  case PairVal(fst: Value, snd: Value)                    // 積型
  case InlVal(v: Value)                                   // 余積（左）
  case InrVal(v: Value)                                   // 余積（右）
  case Unit                                               // 単位型（⊤ / Terminal）

  override def toString: String = this match
    case Atom(v)       => v.toString
    case Closure(_, _, a) => s"<closure/$a>"
    case PairVal(a, b) => s"($a, $b)"
    case InlVal(v)     => s"inl($v)"
    case InrVal(v)     => s"inr($v)"
    case Unit          => "()"

/** コールフレーム */
case class Frame(
    returnAddr: Int,          // 呼び出し元の命令位置
    code: Array[Op],          // 現在のコード
    regs: Array[Value]        // レジスタファイル
)
