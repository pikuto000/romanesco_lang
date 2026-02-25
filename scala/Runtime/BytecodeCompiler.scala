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
  private val registerAllocator = new RegisterAllocator()
  private val bytecodeOptimizer = new BytecodeOptimizer()
  private val rangeAnalyzer = new RangeAnalyzer()

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
    
    finalizeCode(ops.toArray, Map.empty)

  private def finalizeCode(code: Array[Op], initialMapping: Map[Int, Int]): Array[Op] =
    // 1. バイトコードレベルの命令削減（定数畳み込み等）
    val optimized = bytecodeOptimizer.optimize(code)

    // 2. 範囲解析を行い、演算をビット幅付きの命令(IBin)に変換する
    val rangeResult = rangeAnalyzer.analyze(optimized)
    val typed = optimized.map {
      case Op.Add(d, l, r) => Op.IBin(d, l, r, IBinOp.Add, rangeResult.bitWidth(d))
      case Op.Sub(d, l, r) => Op.IBin(d, l, r, IBinOp.Sub, rangeResult.bitWidth(d))
      case Op.Mul(d, l, r) => Op.IBin(d, l, r, IBinOp.Mul, rangeResult.bitWidth(d))
      case other => other
    }
    
    // 3. ResourceOptimizer を適用し、必要な Free 命令を自動挿入する。
    val withFreeOps = optimizer.optimize(typed)
    
    // 4. 最後にレジスタの再割り当てを行い、使用レジスタ数を最小化する。
    val finalOps = registerAllocator.allocate(withFreeOps, initialMapping)
    finalOps

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

        case "Call" =>
          val funcReg = compileRecursive(branches(0), ops)
          val argRegs = branches.tail.map(compileRecursive(_, ops))
          val dst = freshReg()
          ops += Op.Call(dst, funcReg, argRegs.toArray)
          dst

        case "Case" =>
          val scrutReg = compileRecursive(branches(0), ops)
          
          val inlOps = mutable.ArrayBuffer[Op]()
          val inlRes = compileRecursive(branches(1), inlOps)
          inlOps += Op.Return(inlRes)
          
          val inrOps = mutable.ArrayBuffer[Op]()
          val inrRes = compileRecursive(branches(2), inrOps)
          inrOps += Op.Return(inrRes)
          
          val dst = freshReg()
          // Caseの各ブランチも個別に最適化・型付けする
          ops += Op.Case(dst, scrutReg, finalizeCode(inlOps.toArray, Map.empty), finalizeCode(inrOps.toArray, Map.empty))
          dst

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
          
          val captures = savedEnv.values.map(_._1).toArray
          
          regCounter = savedCounter
          env.clear()
          env ++= savedEnv
          
          val dst = freshReg()
          // 引数レジスタも初期マッピングに含める
          val initialMapping = mutable.Map[Int, Int]()
          captures.zipWithIndex.foreach { (old, i) => initialMapping(old) = i }
          initialMapping(argReg) = captures.length
          
          // 関数内部も一括処理
          ops += Op.MakeClosure(dst, finalizeCode(bodyOps.toArray, initialMapping.toMap), captures, 1)
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
