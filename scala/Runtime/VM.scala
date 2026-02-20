// ==========================================
// VM.scala
// スタックベースのバイトコードインタプリタ
// ==========================================

package romanesco.Runtime

import scala.collection.mutable.ArrayBuffer

class VMError(msg: String) extends RuntimeException(msg)

class VM(maxStackSize: Int = 1024, maxFrameDepth: Int = 256):
  private val stack = new ArrayBuffer[Value](64)
  private val frames = new ArrayBuffer[Frame](16)

  private def push(v: Value): Unit =
    if stack.size >= maxStackSize then
      throw VMError(s"スタックオーバーフロー (上限: $maxStackSize)")
    stack += v

  private def pop(): Value =
    if stack.isEmpty then
      throw VMError("空スタックからのpop")
    stack.remove(stack.size - 1)

  private def peek(): Value =
    if stack.isEmpty then
      throw VMError("空スタックのpeek")
    stack.last

  /** バイトコード列を実行し、結果を返す */
  def run(code: Array[Op]): Value =
    stack.clear()
    frames.clear()
    exec(code, Array.empty[Value])
    if stack.isEmpty then Value.Unit
    else pop()

  /** 指定コードをローカル変数環境で実行 */
  private def exec(code: Array[Op], locals: Array[Value]): Unit =
    var pc = 0
    var currentLocals = locals

    while pc < code.length do
      val op = code(pc)
      op match
        case Op.PushConst(v) =>
          push(v)
          pc += 1

        case Op.PushVar(idx) =>
          if idx < 0 || idx >= currentLocals.length then
            throw VMError(s"変数インデックス範囲外: $idx (locals=${currentLocals.length})")
          push(currentLocals(idx))
          pc += 1

        case Op.StoreVar(idx) =>
          val v = pop()
          if idx < 0 || idx >= currentLocals.length then
            throw VMError(s"変数インデックス範囲外: $idx (locals=${currentLocals.length})")
          currentLocals(idx) = v
          pc += 1

        case Op.MakeClosure(body, arity) =>
          // 現在の環境をキャプチャ
          val env = currentLocals.clone()
          push(Value.Closure(body, env, arity))
          pc += 1

        case Op.Apply =>
          val arg = pop()
          val fn = pop()
          fn match
            case Value.Closure(body, env, arity) =>
              if arity == 1 then
                // 引数をenvの末尾に追加して実行
                val newLocals = env :+ arg
                if frames.size >= maxFrameDepth then
                  throw VMError(s"フレーム深さ上限超過 (上限: $maxFrameDepth)")
                // 現在のフレームを保存
                frames += Frame(pc + 1, code, currentLocals, stack.size)
                exec(body, newLocals)
                // フレーム復帰
                val frame = frames.remove(frames.size - 1)
                currentLocals = frame.locals
                pc = frame.returnAddr
                // exec内でスタックに結果が積まれている
              else if arity > 1 then
                // 部分適用: arityを1減らしたクロージャを返す
                val newEnv = env :+ arg
                push(Value.Closure(body, newEnv, arity - 1))
                pc += 1
              else
                throw VMError(s"arity 0 のクロージャへの適用")
            case _ =>
              throw VMError(s"関数でない値への適用: $fn")

        case Op.Return =>
          // exec呼び出しから戻る
          return

        case Op.MakePair =>
          val snd = pop()
          val fst = pop()
          push(Value.PairVal(fst, snd))
          pc += 1

        case Op.Proj1 =>
          pop() match
            case Value.PairVal(fst, _) => push(fst)
            case v => throw VMError(s"Proj1: ペアでない値: $v")
          pc += 1

        case Op.Proj2 =>
          pop() match
            case Value.PairVal(_, snd) => push(snd)
            case v => throw VMError(s"Proj2: ペアでない値: $v")
          pc += 1

        case Op.MakeInl =>
          push(Value.InlVal(pop()))
          pc += 1

        case Op.MakeInr =>
          push(Value.InrVal(pop()))
          pc += 1

        case Op.Case(inlBranch, inrBranch) =>
          val gFn = pop()
          val fFn = pop()
          val scrutinee = pop()
          scrutinee match
            case Value.InlVal(v) =>
              // fを適用
              push(fFn)
              push(v)
              push(Value.Unit) // Applyの結果用にダミー不要、直接Apply
              // Apply命令を模倣
              stack.remove(stack.size - 1) // ダミー削除
              fFn match
                case Value.Closure(body, env, 1) =>
                  val newLocals = env :+ v
                  exec(body, newLocals)
                case _ =>
                  // inlBranchを直接実行（fFnがクロージャでない場合）
                  exec(inlBranch, currentLocals :+ v)
            case Value.InrVal(v) =>
              gFn match
                case Value.Closure(body, env, 1) =>
                  val newLocals = env :+ v
                  exec(body, newLocals)
                case _ =>
                  exec(inrBranch, currentLocals :+ v)
            case v =>
              throw VMError(s"Case: inl/inrでない値: $v")
          pc += 1

        case Op.Pop =>
          pop()
          pc += 1
