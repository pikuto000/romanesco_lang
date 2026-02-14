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

  def getGoalHooks(
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
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    goal match {
      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        List(
          searchLinearImpliesGoal(
            goal,
            a,
            b,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          ),
          // 追加: フレームルール
          searchFrameRule(
            goal,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          )
        )
      case Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)) =>
        List(
          searchLinearGoal(
            goal,
            a,
            b,
            rules,
            context,
            linearContext,
            subst,
            depth,
            limit,
            visited,
            raaCount,
            inductionCount,
            guarded,
            history
          )
        )
      case Expr.Meta(_) =>
        // Frame Inference Hook
        List(searchFrameInference(goal, linearContext, subst))
      case _ => Nil
    }
  }

  def getContextHooks(
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
  ): List[SolveTree[(ProofTree, Subst, Context)]] = {
    List(
      searchCancellation(
        goal,
        rules,
        context,
        linearContext,
        subst,
        depth,
        limit,
        visited,
        raaCount,
        inductionCount,
        guarded,
        history
      ),
      searchLinearDecomposeContext(
        goal,
        rules,
        context,
        linearContext,
        subst,
        depth,
        limit,
        visited,
        raaCount,
        inductionCount,
        guarded,
        history
      )
    )
  }

  /** フレーム推論: ゴールがメタ変数で、線形文脈が残っている場合、それらを全てメタ変数に割り当てる */
  private def searchFrameInference(
      goal: Expr,
      linearContext: Context,
      subst: Subst
  ): SolveTree[(ProofTree, Subst, Context)] = {
    if (linearContext.isEmpty) return SolveTree.Failure()

    logger.log(s"[Frame Inference] Goal: $goal")
    
    // 戦略0: スマート推論（もしゴールが既知の構造の一部なら）
    // 例: P * ?F の ?F を埋める場合、P以外をフレームとする
    val smartStrategy = goal match {
      case Expr.Meta(_) => 
        // 既存のヒストリや親のゴールから情報を得るのは難しいため、
        // ここでは単純に「最小」と「全量」の戦略を優先する
        SolveTree.Failure()
      case _ => SolveTree.Failure()
    }

    // 戦略1: 最小フレームを優先（リソースの1つからの組み合わせ）
    val minimalFrameStrategies = (1 until linearContext.length).toList.flatMap { n =>
      linearContext.combinations(n).map { selectedResources =>
        val frame = selectedResources
          .map(_._2)
          .reduceLeft((acc, e) => Expr.App(Expr.Sym(SepAnd), List(acc, e)))
        val remaining = linearContext.filterNot(r => selectedResources.exists(_._1 == r._1))

        SolveTree.fromLazyList(unify(frame, goal, subst).map { s =>
          logger.log(s"[Frame] Minimal ($n): $frame")
          (
            ProofTree.Leaf(applySubst(goal, s), s"frame-inference-min-$n"),
            s,
            remaining
          )
        })
      }
    }

    // 戦略2: 全リソースをフレームに（既存の動作）
    val fullFrameStrategy = {
      val resources = linearContext.map(_._2)
      val frame = resources.reduceLeft((acc, e) =>
        Expr.App(Expr.Sym(SepAnd), List(acc, e))
      )

      SolveTree.fromLazyList(unify(frame, goal, subst).map { s =>
        logger.log(s"[Frame] Full: $frame")
        (
          ProofTree.Leaf(applySubst(goal, s), "frame-inference-full"),
          s,
          Nil
        )
      })
    }

    SolveTree.Choice(smartStrategy :: (minimalFrameStrategies :+ fullFrameStrategy))
  }

  /** フレームルールの自動適用 {P} C {Q} から {P * F} C {Q * F} を導出
    */
  private def searchFrameRule(
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
    goal match {
      case App(
            Sym(LImplies),
            List(
              App(Sym(SepAnd), List(p, framePre)),
              App(Sym(SepAnd), List(q, framePost))
            )
          ) =>
        unify(framePre, framePost, subst).headOption match {
          case Some(s1) =>
            val frameHyp = (s"frame_$depth", applySubst(framePre, s1))

            searchLinearImpliesGoal(
              App(Sym(LImplies), List(p, q)),
              p,
              q,
              rules,
              context,
              frameHyp :: linearContext,
              s1,
              depth + 1,
              limit,
              visited,
              raaCount,
              inductionCount,
              guarded,
              history
            ).map { case (innerProof, s2, restL) =>
              (
                ProofTree.Node(
                  applySubst(goal, s2),
                  "frame-rule",
                  List(innerProof)
                ),
                s2,
                restL.filterNot(_._1.startsWith("frame_"))
              )
            }
          case None => SolveTree.Failure()
        }
      case _ => SolveTree.Failure()
    }
  }

  private[core] def searchLinearImpliesGoal(
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
    search(
      b,
      rules,
      context,
      (s"lh$depth", a) :: linearContext,
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
          "linear-implies-intro",
          List(t)
        ),
        s,
        restL
      )
    }
  }

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
    goal match {
      case Expr.App(Expr.Sym(op), List(a, b)) if op == Tensor || op == SepAnd =>
        val goalTerms = collectTerms(goal, op)
        val cancelOptions = linearContext.zipWithIndex.flatMap {
          case ((name, hyp), idx) =>
            val hypTerms = collectTerms(hyp, op)
            val common = goalTerms.intersect(hypTerms)
            if (common.nonEmpty) {
              val remainingGoal = buildTerm(goalTerms.diff(common), op)
              val remainingHyp = buildTerm(hypTerms.diff(common), op)
              val nextLinear =
                if (
                  remainingHyp == Expr.Sym(Terminal) || remainingHyp == Expr
                    .Sym(LPlus)
                ) {
                  linearContext.patch(idx, Nil, 1)
                } else {
                  linearContext.patch(idx, List((s"$name.c", remainingHyp)), 1)
                }

              Some(
                search(
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
                    ProofTree
                      .Node(applySubst(goal, s), s"cancel[$name]", List(t)),
                    s,
                    restL
                  )
                }
              )
            } else None
        }
        SolveTree.merge(cancelOptions)
      case _ => SolveTree.Failure()
    }
  }

  private def collectTerms(e: Expr, op: String): List[Expr] = e match {
    case Expr.App(Expr.Sym(`op`), List(a, b)) =>
      collectTerms(a, op) ++ collectTerms(b, op)
    case other => List(other)
  }

  private def buildTerm(terms: List[Expr], op: String): Expr = {
    if (terms.isEmpty) {
      if (op == Tensor) Expr.Sym(LPlus) else Expr.Sym(Terminal)
    } else {
      terms.reduceLeft((acc, t) => Expr.App(Expr.Sym(op), List(acc, t)))
    }
  }

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
    val indexedLinear = linearContext.zipWithIndex
    val options = (0 to indexedLinear.length).toList.flatMap { n =>
      indexedLinear.combinations(n).toList.map { leftIndexed =>
        val leftIndices = leftIndexed.map(_._2).toSet
        val rightPart =
          indexedLinear.filterNot(x => leftIndices.contains(x._2)).map(_._1)
        val leftPart = leftIndexed.map(_._1)

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
              else SolveTree.Failure()
            }
          } else SolveTree.Failure()
        }
      }
    }
    if (options.length > 1 && config.maxParallelism > 1) {
      SolveTree.Choice(options.map(o => SolveTree.DeepStep(() => o)))
    } else {
      SolveTree.merge(options)
    }
  }

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
          else SolveTree.Failure()
        }
      }
    }
    SolveTree.merge(options)
  }

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

  /** スマートフレーム推論: ゴールの構造を解析して必要なリソースを特定 */
  private def smartFrameInference(
      target: Expr,
      resources: Context,
      subst: Subst
  ): (Context, Context) = {
    val targetTerms = collectSepAndTerms(target)
    resources.partition { case (_, res) =>
      targetTerms.exists(t => unify(t, res, subst).nonEmpty)
    }
  }

  private def collectSepAndTerms(e: Expr): List[Expr] = e match {
    case Expr.App(Expr.Sym(SepAnd | Tensor), List(a, b)) =>
      collectSepAndTerms(a) ++ collectSepAndTerms(b)
    case other => List(other)
  }
}
