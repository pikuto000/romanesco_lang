// ==========================================
// ResourceOptimizer.scala
// Omni-Logic Verification Engine (Refined Symbolic Refutation)
// ==========================================

package romanesco.Runtime

import romanesco.Solver.core._
import scala.collection.mutable
import scala.util.boundary

class ResourceOptimizer(val logic: ResourceLogic = new OmniResourceLogic()):

  private val analyzer = new ResourceAnalyzer()

  def optimize(code: Array[Op]): Array[Op] =
    val result = analyzer.analyze(code)
    if result.lostAt.isEmpty then return code
    val newCode = new mutable.ArrayBuffer[Op]()
    for (op, idx) <- code.zipWithIndex do
      val lostRids = result.lostAt.getOrElse(idx, Set.empty)
      val regsBefore = result.regsAt(idx)
      
      // 命令の直前に Free を挿入するべきレジスタを特定
      val regsToFree = mutable.Set[Int]()
      
      // その命令で上書きされるレジスタを Free する
      def checkOverwrite(dst: Int) =
        if (regsBefore.contains(dst) && lostRids.contains(regsBefore(dst))) regsToFree += dst

      // その命令の「入力」として使われ、かつ消滅するレジスタを Free する
      def checkInput(src: Int) =
        if (regsBefore.contains(src) && lostRids.contains(regsBefore(src))) regsToFree += src

      op match
        case Op.LoadConst(dst, _) => checkOverwrite(dst)
        case Op.Move(dst, src) => checkOverwrite(dst); checkInput(src)
        case Op.MakePair(dst, fst, snd) => checkOverwrite(dst); checkInput(fst); checkInput(snd)
        case Op.Proj1(dst, src) => checkOverwrite(dst); checkInput(src)
        case Op.Proj2(dst, src) => checkOverwrite(dst); checkInput(src)
        case Op.Return(src) =>
          // Returnの直前に、返り値以外の全レジスタを Free する
          for ((reg, rid) <- regsBefore if reg != src && lostRids.contains(rid)) {
            regsToFree += reg
          }
        case _ => ()
      
      for (reg <- regsToFree.toList.sorted) {
        newCode += Op.Free(reg)
      }
      
      newCode += op
    newCode.toArray

  def verify(code: Array[Op]): Either[String, Unit] = boundary:
    val analysis = analyzer.analyze(code)
    val axioms = logic.buildVerificationRules(code, analysis)

    val config = ProverConfig(
      rules = axioms,
      pluginPacks = List(
        // プログラム検証に必須の最小限のパックのみを使用
        romanesco.Solver.core.ResourceLogicPack, // LinearLogic 等
        romanesco.Solver.core.AdvancedReasoningPack // ForwardReasoning 等
      ),
      maxParallelism = 8,
      maxComplexity = 5000
    )
    val prover = new Prover(config)

    // --- パス 1: 安全性証明 (逆向き探索) ---
    val safetyGoal = Expr.App(Expr.Sym("⊸"), List(
      Expr.App(Expr.Sym("⊗"), List(Expr.App(Expr.Sym("At"), List(Expr.Sym("0"))), Expr.Sym("emp"))),
      Expr.App(Expr.Sym("⊗"), List(Expr.Sym("SafeTermination"), Expr.Sym("emp")))
    ))

    prover.prove(safetyGoal, axioms, maxDepth = code.length * 2 + 10) match
      case Right(_) => 
        println("  ✓ Safety Proof Succeeded: No leaks or double-frees.")
        return Right(())
      case Left(trace) => 
        println("  ! Safety Proof Failed. Analyzing residue...")
        if (trace.residualLinearContext.nonEmpty) {
          val leaks = trace.residualLinearContext.map(_._2).mkString(", ")
          return Left(s"検証失敗: リソースリークまたは不正な終了状態を検出しました。 残存資源: $leaks")
        } else {
          println(s"  Trace Reason: ${trace.reason}")
          // フォールバック: シンボリック反証 (既存のロジック)
        }

    if (analysis.garbageResources.nonEmpty) {
      return Left(s"反証成功: 以下のリソースが解放されずに残っています: ${analysis.garbageResources.toList.sorted.map(id => s"r$id").mkString(", ")}")
    }

    Left("検証失敗: 安全性を証明できませんでした。")
