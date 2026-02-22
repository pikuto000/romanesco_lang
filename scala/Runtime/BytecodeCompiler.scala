// ==========================================
// BytecodeCompiler.scala
// ParseTree -> レジスタマシンバイトコード (Op)
// ==========================================

package romanesco.Runtime

import romanesco.Types._
import romanesco.Parser.rParser

import scala.collection.mutable

class CompileError(msg: String) extends RuntimeException(msg)

class BytecodeCompiler:
  private var regCounter = 0
  private val env = mutable.Map[String, Int]() // 変数名 -> レジスタ番号

  private def freshReg(): Int =
    val r = regCounter
    regCounter += 1
    r

  /** ParseTree をコンパイルして Op 配列を返す */
  def compile(tree: Tree[(String, Vector[String])]): Array[Op] =
    regCounter = 0
    env.clear()
    val ops = mutable.ArrayBuffer[Op]()
    val finalReg = compileRecursive(tree, ops)
    ops += Op.Return(finalReg)
    ops.toArray

  private def compileRecursive(
      tree: Tree[(String, Vector[String])],
      ops: mutable.ArrayBuffer[Op]
  ): Int = tree match
    case Tree.E() =>
      val r = freshReg()
      ops += Op.LoadConst(r, Value.Unit)
      r

    case Tree.V((op, args), branches) =>
      op match
        case "ParseRoot" =>
          // 複数の式がある場合は最後を結果とする
          var lastReg = -1
          for branch <- branches do
            lastReg = compileRecursive(branch, ops)
          if lastReg == -1 then
            lastReg = freshReg()
            ops += Op.LoadConst(lastReg, Value.Unit)
          lastReg

        case "Const" | "Literal" =>
          val r = freshReg()
          val value = args.headOption.getOrElse("0")
          val v = try Value.Atom(value.toInt) catch case _: Exception => Value.Atom(value)
          ops += Op.LoadConst(r, v)
          r

        case "Var" =>
          val name = args.head
          env.getOrElse(name, throw CompileError(s"未定義の変数: $name"))

        case "Add" => compileBinOp(ops, branches, Op.Add.apply)
        case "Sub" => compileBinOp(ops, branches, Op.Sub.apply)
        case "Mul" => compileBinOp(ops, branches, Op.Mul.apply)

        case "Pair" =>
          if branches.size != 2 then throw CompileError("Pairは2つの引数が必要です")
          val r1 = compileRecursive(branches(0), ops)
          val r2 = compileRecursive(branches(1), ops)
          val dst = freshReg()
          ops += Op.MakePair(dst, r1, r2)
          dst

        case "Proj1" =>
          val r = compileRecursive(branches(0), ops)
          val dst = freshReg()
          ops += Op.Proj1(dst, r)
          dst

        case "Proj2" =>
          val r = compileRecursive(branches(0), ops)
          val dst = freshReg()
          ops += Op.Proj2(dst, r)
          dst

        case "Let" =>
          // (Let varName valueExpr bodyExpr)
          val varName = args.head
          val valReg = compileRecursive(branches(0), ops)
          val oldMapping = env.get(varName)
          env(varName) = valReg
          val resultReg = compileRecursive(branches(1), ops)
          // 本来はスコープを抜けるときに env を戻すべきだが簡易化
          resultReg

        case "Lambda" =>
          // (Lambda argName bodyExpr)
          val argName = args.head
          val savedCounter = regCounter
          val savedEnv = env.toMap
          
          // 引数レジスタを予約（環境の直後に配置される想定）
          // FIXME: クロージャの環境キャプチャと引数配置の規約を合わせる
          val argReg = freshReg()
          env(argName) = argReg
          
          val bodyOps = mutable.ArrayBuffer[Op]()
          val bodyRes = compileRecursive(branches(0), bodyOps)
          bodyOps += Op.Return(bodyRes)
          
          regCounter = savedCounter
          // env を戻す（キャプチャ対象の特定に必要）
          val captures = savedEnv.values.toArray
          
          val dst = freshReg()
          ops += Op.MakeClosure(dst, bodyOps.toArray, captures, 1)
          dst

        case "EOF" =>
          val r = freshReg()
          ops += Op.LoadConst(r, Value.Unit)
          r

        case _ =>
          // 未知の演算子はとりあえず Unit
          val r = freshReg()
          ops += Op.LoadConst(r, Value.Unit)
          r

  private def compileBinOp(
      ops: mutable.ArrayBuffer[Op],
      branches: Vector[Tree[(String, Vector[String])]],
      constructor: (Int, Int, Int) => Op
  ): Int =
    if branches.size != 2 then throw CompileError("2項演算には2つの引数が必要です")
    val r1 = compileRecursive(branches(0), ops)
    val r2 = compileRecursive(branches(1), ops)
    val dst = freshReg()
    ops += constructor(dst, r1, r2)
    dst
