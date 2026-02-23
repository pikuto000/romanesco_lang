// ==========================================
// BytecodeCompiler.scala
// 所有権推論・自動メモリ管理（Optimizer統合型）コンパイラ
// ==========================================

package romanesco.Runtime

import romanesco.Types._
import romanesco.Parser.rParser
import scala.collection.mutable

class CompileError(msg: String) extends RuntimeException(msg)

class BytecodeCompiler:
  private var regCounter = 0
  private val env = mutable.Map[String, (Int, Int)]() 
  private val optimizer = new ResourceOptimizer(new OmniResourceLogic())

  private def freshReg(): Int =
    val r = regCounter
    regCounter += 1
    r

  private def countUses(tree: Tree[(String, Vector[String])], counts: mutable.Map[String, Int]): Unit = tree match
    case Tree.V(("Var", args), _) =>
      val name = args.head
      counts(name) = counts.getOrElse(name, 0) + 1
    case Tree.V(_, branches) =>
      branches.foreach(countUses(_, counts))
    case _ => ()

  /** ParseTree をコンパイルし、さらに最適化（自動Free挿入）を適用して返す */
  def compile(tree: Tree[(String, Vector[String])]): Array[Op] =
    regCounter = 0
    env.clear()
    val ops = mutable.ArrayBuffer[Op]()
    val finalReg = compileRecursive(tree, ops)
    ops += Op.Return(finalReg)
    
    // 生成された「生の」バイトコードに対して、
    // ResourceOptimizer を適用し、必要な Free 命令を自動挿入する。
    // これこそが「メモリ管理の完全自動化」である。
    val optimizedOps = optimizer.optimize(ops.toArray)
    optimizedOps

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
          val (reg, uses) = env.getOrElse(name, throw CompileError(s"未定義の変数: $name"))
          
          if (uses > 1) {
            val dst = freshReg()
            ops += Op.Borrow(dst, reg)
            env(name) = (reg, uses - 1)
            dst
          } else {
            val dst = freshReg()
            ops += Op.Move(dst, reg)
            env(name) = (reg, 0)
            dst
          }

        case "Add" => compileBinOp(ops, branches, Op.Add.apply)
        case "Sub" => compileBinOp(ops, branches, Op.Sub.apply)
        case "Mul" => compileBinOp(ops, branches, Op.Mul.apply)

        case "Pair" =>
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
          val varName = args.head
          val bodyUses = mutable.Map[String, Int]()
          countUses(branches(1), bodyUses)
          
          val valReg = compileRecursive(branches(0), ops)
          
          val oldMapping = env.get(varName)
          env(varName) = (valReg, bodyUses.getOrElse(varName, 0))
          
          val resultReg = compileRecursive(branches(1), ops)
          
          oldMapping match
            case Some(m) => env(varName) = m
            case None => env.remove(varName)
            
          resultReg

        case "Lambda" =>
          val argName = args.head
          val savedCounter = regCounter
          val savedEnv = env.toMap
          
          val bodyUses = mutable.Map[String, Int]()
          countUses(branches(0), bodyUses)
          
          val argReg = freshReg()
          env(argName) = (argReg, bodyUses.getOrElse(argName, 0))
          
          val bodyOps = mutable.ArrayBuffer[Op]()
          val bodyRes = compileRecursive(branches(0), bodyOps)
          bodyOps += Op.Return(bodyRes)
          
          // 関数内部も個別に最適化する
          val optimizedBody = optimizer.optimize(bodyOps.toArray)
          
          regCounter = savedCounter
          env.clear()
          env ++= savedEnv
          
          val dst = freshReg()
          ops += Op.MakeClosure(dst, optimizedBody, savedEnv.values.map(_._1).toArray, 1)
          dst

        case _ =>
          val r = freshReg()
          ops += Op.LoadConst(r, Value.Unit)
          r

  private def compileBinOp(
      ops: mutable.ArrayBuffer[Op],
      branches: Vector[Tree[(String, Vector[String])]],
      constructor: (Int, Int, Int) => Op
  ): Int =
    val r1 = compileRecursive(branches(0), ops)
    val r2 = compileRecursive(branches(1), ops)
    val dst = freshReg()
    ops += constructor(dst, r1, r2)
    dst
