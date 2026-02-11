// ==========================================
// LinearLogicSearch.scala
// 線形論理・分離論理固有の探索ロジック
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._

trait LinearLogicSearch { self: Prover =>
  import Expr._
  import Unifier._

  /** 線形論理・分離論理の相殺（Cancellation）規則の適用
    * A * C ⊸ B * C  => A ⊸ B
    */
  private[core] def searchCancellation(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    // 目標が A * B または A ⊗ B の場合に、線形文脈との共通項を探す
    goal match {
      case Expr.App(Expr.Sym(op), List(a, b)) if op == Tensor || op == SepAnd =>
        val goalTerms = collectTerms(goal, op)
        val cancelOptions = linearContext.zipWithIndex.flatMap { case ((name, hyp), idx) =>
          val hypTerms = collectTerms(hyp, op)
          val common = goalTerms.intersect(hypTerms)
          if (common.nonEmpty) {
            val remainingGoal = buildTerm(goalTerms.diff(common), op)
            val remainingHyp = buildTerm(hypTerms.diff(common), op)
            val nextLinear = if (remainingHyp == Expr.Sym(Terminal) || remainingHyp == Expr.Sym(LOne)) {
              linearContext.patch(idx, Nil, 1)
            } else {
              linearContext.patch(idx, List((s"$name.c", remainingHyp)), 1)
            }
            
            Some(search(
              remainingGoal,
              rules,
              context,
              nextLinear,
              subst,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              guarded,
              history
            ).map { case (t, s, restL) =>
              (
                ProofTree.Node(applySubst(goal, s), s"cancel[$name]", List(t)),
                s,
                restL
              )
            })
          } else None
        }
        SolveTree.merge(cancelOptions)
      case _ => SolveTree.Failure
    }
  }

  private def collectTerms(e: Expr, op: String): List[Expr] = e match {
    case Expr.App(Expr.Sym(`op`), List(a, b)) => collectTerms(a, op) ++ collectTerms(b, op)
    case other => List(other)
  }

  private def buildTerm(terms: List[Expr], op: String): Expr = {
    if (terms.isEmpty) {
      if (op == Tensor) Expr.Sym(LOne) else Expr.Sym(Terminal)
    } else {
      terms.reduceLeft((acc, t) => Expr.App(Expr.Sym(op), List(acc, t)))
    }
  }

  /** 線形論理のテンソル(⊗)・分離論理の積(*)の分解 */
  private[core] def searchLinearGoal(
      goal: Expr,
      a: Expr,
      b: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    if (config.maxParallelism <= 1)
      logger.log(s"[TENSOR SPLIT] Goal: $goal, Linear Context: $linearContext")
    val indexedLinear = linearContext.zipWithIndex
    val options = (0 to indexedLinear.length).toList.flatMap { n =>
      indexedLinear.combinations(n).toList.map { leftIndexed =>
        val leftIndices = leftIndexed.map(_._2).toSet
        val rightPart =
          indexedLinear.filterNot(x => leftIndices.contains(x._2)).map(_._1)
        val leftPart = leftIndexed.map(_._1)
        if (config.maxParallelism <= 1)
          logger.log(s"  Trying split - Left: $leftPart, Right: $rightPart")
        // Step で包んで公平性を維持しつつ、flatMap 内で search を実行
        search(
          a,
          rules,
          context,
          leftPart,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        ).flatMap { case (tA, s1, restLA) =>
          if (restLA.isEmpty) {
            search(
              b,
              rules,
              context,
              rightPart,
              s1,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              guarded,
              history
            ).flatMap { case (tB, s2, restLB) =>
              if (restLB.isEmpty)
                SolveTree.Success(
                  (
                    ProofTree.Node(
                      applySubst(goal, s2),
                      "tensor-intro",
                      List(tA, tB)
                    ),
                    s2,
                    Nil
                  )
                )
              else SolveTree.Failure
            }
          } else SolveTree.Failure
        }
      }
    }
    // コンテキスト分割の選択肢を並列に探索
    if (options.length > 1 && config.maxParallelism > 1) {
      SolveTree.fromLazyList(SolveTree.Choice(options).solveParallel(config.maxParallelism))
    } else {
      SolveTree.merge(options)
    }
  }

  /** 線形含意(⊸)の適用 */
  private[core] def searchLinearApply(
      name: String,
      goal: Expr,
      a: Expr,
      b: Expr,
      rules: List[CatRule],
      context: Context,
      nextLinear: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val indexedLinear = nextLinear.zipWithIndex
    val options = (0 to indexedLinear.length).toList.flatMap { n =>
      indexedLinear.combinations(n).toList.map { leftIndexed =>
        val leftIndices = leftIndexed.map(_._2).toSet
        val rightPart =
          indexedLinear.filterNot(x => leftIndices.contains(x._2)).map(_._1)
        val lLin = leftIndexed.map(_._1)
        val rLin = rightPart
        search(
          a,
          rules,
          context,
          lLin,
          subst,
          depth + 1,
          limit,
          visited,
          raaCount,
          inductionCount,
          guarded,
          history
        ).flatMap { case (tA, s1, restLA) =>
          if (restLA.isEmpty)
            search(
              goal,
              rules,
              context,
              (s"$name.res", b) :: rLin,
              s1,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              guarded,
              history
            ).map { case (tGoal, s2, restL) =>
              (
                ProofTree.Node(
                  applySubst(goal, s2),
                  s"linear-apply[$name]",
                  List(tGoal, tA)
                ),
                s2,
                restL
              )
            }
          else SolveTree.Failure
        }
      }
    }
    SolveTree.merge(options)
  }

  /** 線形文脈の分解 */
  private[core] def searchLinearDecomposeContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      linearContext: Context,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr])],
      raaCount: Int,
      inductionCount: Int,
      guarded: Boolean,
      history: List[Expr]
  ): SolveTree[(ProofTree, Subst, Context)] = {
    val linear = linearContext.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b))), i) =>
        val newL =
          linearContext.patch(i, List((s"$name.1", a), (s"$name.2", b)), 1)
        List(
          search(
            goal,
            rules,
            context,
            newL,
            subst,
            depth + 1,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          ).map { case (t, s, restL) =>
            (
              ProofTree.Node(
                applySubst(goal, s),
                s"tensor-destruct[$name]",
                List(t)
              ),
              s,
              restL
            )
          }
        )
      case _ => Nil
    }
    SolveTree.merge(linear)
  }
}
