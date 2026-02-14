// ==========================================
// LinearLogicSearch.scala
// 線形論理・分離論理固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree

class LinearLogicPlugin extends LogicPlugin {
  override def name: String = "LinearLogic"
  
  import Unifier._

  type Context = List[(String, Expr)]

  override def getGoalHooks(
      exprs: Vector[Expr],
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
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    
    goal match {
      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        val subTree = prover.search(exprs :+ b, context, (s"lh$depth", a) :: linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        findSuccess(subTree).filter(_.linearContext.isEmpty).map { s =>
          Tree.V(SearchNode(exprs :+ b, "linear-implies-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "linear-implies-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }.toVector
      
      case Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)) =>
        // リソース分割の試行
        val indexedLinear = linearContext.zipWithIndex
        (0 to indexedLinear.length).toVector.flatMap { n =>
          indexedLinear.combinations(n).toVector.flatMap { leftIndexed =>
            val leftIndices = leftIndexed.map(_._2).toSet
            val leftPart = leftIndexed.map(_._1)
            val rightPart = indexedLinear.filterNot(x => leftIndices.contains(x._2)).map(_._1)

            val subTreeA = prover.search(exprs :+ a, context, leftPart, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
            findSuccess(subTreeA).toVector.flatMap { sA =>
              val subTreeB = prover.search(exprs :+ b, sA.context, rightPart, sA.subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
              findSuccess(subTreeB).map { sB =>
                Tree.V(SearchNode(exprs :+ goal, "tensor-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, sB.subst), "tensor-intro", List(sA.result.toOption.get.tree, sB.result.toOption.get.tree)))), sB.subst, sB.context, sB.linearContext), Vector(subTreeA, subTreeB))
              }
            }
          }
        }

      case Expr.Meta(_) =>
        // Frame Inference: 残りのリソースをすべてメタ変数に割り当てる
        if (linearContext.nonEmpty) {
          // リソースを正規化（名前でソートし、項を取り出す）
          val resources = linearContext.sortBy(_._1).map(_._2)
          val frame = resources.reduceLeft((acc, e) => Expr.App(Expr.Sym(SepAnd), List(acc, e)))
          unify(goal, frame, subst).map { s =>
            Tree.V(SearchNode(exprs :+ goal, "frame-inference", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "frame-inference"))), s, context, Nil), Vector.empty)
          }.toVector
        } else {
          // リソースが空なら ⊤ (True/Terminal) または 1 (LOne) に割り当てる
          unify(goal, Expr.Sym(LOne), subst).map { s =>
            Tree.V(SearchNode(exprs :+ goal, "frame-inference-empty", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "frame-inference-empty"))), s, context, Nil), Vector.empty)
          }.toVector
        }

      case _ => Vector.empty
    }
  }

  override def getContextHooks(
      exprs: Vector[Expr],
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
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    
    val bangElim = context.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(Bang), List(a))), i) =>
        val newCtx = context.patch(i, List((name, a)), 1)
        val subTree = prover.search(exprs, newCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
        findSuccess(subTree).map { s =>
          Tree.V(SearchNode(exprs, s"linear-bang-elim[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"linear-bang-elim[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }
      case _ => None
    }.toVector

    val tensorDestruct = (context.map((_, false)) ++ linearContext.map((_, true))).zipWithIndex.flatMap {
      case (((name, Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b))), isLinear), i) =>
        if (isLinear) {
          val newL = linearContext.filterNot(_._1 == name) ++ List((s"$name.1", a), (s"$name.2", b))
          val subTree = prover.search(exprs, context, newL, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          findSuccess(subTree).map { s =>
            Tree.V(SearchNode(exprs, s"tensor-destruct[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"tensor-destruct[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        } else {
          val newCtx = context.patch(context.indexWhere(_._1 == name), List((s"$name.1", a), (s"$name.2", b)), 1)
          val subTree = prover.search(exprs, newCtx, linearContext, subst, depth + 1, limit, visited, raaCount, inductionCount, guarded)
          findSuccess(subTree).map { s =>
            Tree.V(SearchNode(exprs, s"tensor-is-ﾃ夕$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"tensor-is-ﾃ夕$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }
      case _ => None
    }.toVector

    bangElim ++ tensorDestruct
  }

  private def findSuccess(tree: Tree[SearchNode]): Option[SearchNode] = tree match {
    case Tree.E() => None
    case Tree.V(node, branches) =>
      if (node.isSuccess) Some(node)
      else branches.view.flatMap(findSuccess).headOption
  }
}
