// ==========================================
// Bytecode.scala
// バイトコード命令セットと値型の定義
// ==========================================

package romanesco.Runtime

/** バイトコード命令 */
enum Op:
  case PushConst(value: Value)       // 定数をスタックに積む
  case PushVar(index: Int)           // ローカル変数を積む
  case StoreVar(index: Int)          // スタックトップをローカル変数に格納
  case MakeClosure(body: Array[Op], arity: Int) // クロージャ生成
  case Apply                         // f(arg) を評価
  case Return                        // 関数から戻る
  case MakePair                      // pair(a, b) を生成
  case Proj1                         // 第一射影
  case Proj2                         // 第二射影
  case MakeInl                       // 余積（左）
  case MakeInr                       // 余積（右）
  case Case(inlBranch: Array[Op], inrBranch: Array[Op]) // 場合分け
  case Pop                           // スタックトップを破棄
  case Free(rid: Int)                // リソースIDを指定して深いfree

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
    code: Array[Op],          // 呼び出し元のコード
    locals: Array[Value],     // ローカル変数
    stackBase: Int            // フレーム開始時のスタック位置
)
