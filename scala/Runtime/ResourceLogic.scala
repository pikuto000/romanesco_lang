// ==========================================
// ResourceLogic.scala
// Refined Omni-Logic Architecture (Rule-Based)
// ==========================================

package romanesco.Runtime

import romanesco.Solver.core._

trait ResourceLogic:
  def name: String
  def buildVerificationRules(code: Array[Op], analysis: AnalysisResult): List[CatRule]
  def targetGoal: Expr
  def initialGoal(analysis: AnalysisResult): Goal

class OmniResourceLogic extends ResourceLogic:
  def name = "OmniLogic"
  def targetGoal = Expr.Sym("SafeTermination")

  def initialGoal(analysis: AnalysisResult): Goal =
    Goal(
      context = Nil,
      linearContext = List(
        ("init", Expr.App(Expr.Sym("At"), List(Expr.Sym("0"))))
      ),
      target = targetGoal
    )

  def buildVerificationRules(code: Array[Op], analysis: AnalysisResult): List[CatRule] =
    val rules = new scala.collection.mutable.ListBuffer[CatRule]()
    val total = code.length

    for (op, i) <- code.zipWithIndex do
      val cur = Expr.App(Expr.Sym("At"), List(Expr.Sym(i.toString)))
      val nxt = if i == total - 1 then targetGoal else Expr.App(Expr.Sym("At"), List(Expr.Sym((i + 1).toString)))
      val regsBefore = analysis.regsAt.getOrElse(i, Map.empty)
      val regsAfter = if i + 1 < total then analysis.regsAt.getOrElse(i + 1, Map.empty) else Map.empty

      // 1. その命令が実行されるために「最低限必要な」リソース (LHS)
      //    全ての有効なレジスタにあるOwnをLHSに並べる
      // 命令が正当に実行可能かチェック
      val isOpValid = op match
        case Op.Free(r) => regsBefore.contains(r)
        case Op.Return(r) => true 
        case Op.Borrow(_, s) => regsBefore.contains(s)
        case Op.Add(_, l, r) => regsBefore.contains(l) && regsBefore.contains(r)
        case Op.Sub(_, l, r) => regsBefore.contains(l) && regsBefore.contains(r)
        case Op.Mul(_, l, r) => regsBefore.contains(l) && regsBefore.contains(r)
        case Op.MakePair(_, f, s) => regsBefore.contains(f) && regsBefore.contains(s)
        case Op.Proj1(_, s) => regsBefore.contains(s)
        case Op.Proj2(_, s) => regsBefore.contains(s)
        case _ => true

      def wrapAll(regs: Map[Int, Int], base: Expr): Expr =
        val ids = regs.values.toList.sorted // distinct を削除
        val res = ids.foldLeft(base)((acc, id) => Expr.App(Expr.Sym("⊗"), List(acc, Expr.App(Expr.Sym("Own"), List(Expr.Sym(s"r$id"))))))
        Expr.App(Expr.Sym("⊗"), List(res, Expr.Sym("emp")))

      val lhs = wrapAll(regsBefore, cur)
      
      // 不正な命令の場合、RHS を証明不能なシンボルにする
      val rhs = if (!isOpValid) Expr.Sym("INVALID_STATE_ACCESS") 
        else op match
          case Op.Return(_) =>
            val leaks = analysis.garbageResources.toList.sorted
            val base = leaks.foldLeft(nxt)((acc, id) => Expr.App(Expr.Sym("⊗"), List(acc, Expr.App(Expr.Sym("Own"), List(Expr.Sym(s"r$id"))))))
            // リークがない場合のみ、最終的な emp と一致するようにする
            Expr.App(Expr.Sym("⊗"), List(base, Expr.Sym("emp")))
          case _ => 
            wrapAll(regsAfter, nxt)

      rules += CatRule(s"op-$i-${op.getClass.getSimpleName}", lhs, rhs, domain = "memory")
    
    rules.toList
