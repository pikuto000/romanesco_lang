// ==========================================
// LinearLogicSearch.scala
// 線形論理・分離論理固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class LinearLogicPlugin extends LogicPlugin {
  override def name: String = "LinearLogic"
  override def priority: Int = 40
  
  import Unifier._

  type Context = List[(String, Expr)]

  override def getGoalHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    val linearContext = state.linearContext
    
    
    val linearBackchain = linearContext.flatMap { case (name, hyp) =>
      val (premises, conclusion) = decompose(hyp, () => prover.freshMeta(depth))
      unify(conclusion, goal, subst).flatMap { s =>
        val remainingLinear = linearContext.filterNot(_._1 == name)
        
        def solvePremises(ps: List[Expr], currentS: Subst, currentL: List[(String, Expr)], currentCtx: List[(String, Expr)], solved: List[ProofTree]): LazyList[(Subst, List[(String, Expr)], List[(String, Expr)], List[ProofTree])] = ps match {
          case Nil => LazyList((currentS, currentL, currentCtx, solved))
          case p :: tail =>
            val targetP = applySubst(p, currentS)
            // 線形前提の場合、リソースを使い切る必要がある
            val subTree = prover.search(exprs :+ targetP, currentCtx, state.withLinear(currentL), currentS, depth + 1, limit, visited, guarded)
            allSuccesses(subTree).flatMap { res =>
              solvePremises(tail, res.subst, res.linearContext, res.context, solved :+ res.result.toOption.get.tree)
            }
        }

        solvePremises(premises, s, remainingLinear, context, Nil).map { case (finalS, finalL, finalCtx, proofs) =>
          val pTree = ProofTree.Node(applySubst(goal, finalS), s"apply-linear[$name]", proofs)
          Tree.V(SearchNode(exprs, s"apply-linear[$name]", depth, Right(ProofResult(pTree)), finalS, finalCtx, finalL), Vector.empty)
        }
      }
    }.toVector

    goal match {
      case Expr.App(Expr.Sym(LImplies), List(Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)), target)) =>
        // (A * B) ⊸ C  =>  A ⊸ B ⊸ C
        val subGoal = Expr.App(Expr.Sym(LImplies), List(a, Expr.App(Expr.Sym(LImplies), List(b, target))))
        val subTree = prover.search(exprs :+ subGoal, context, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).map { s =>
          Tree.V(SearchNode(exprs :+ subGoal, "tensor-limplies-destruct", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "tensor-limplies-destruct", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }.toVector ++ linearBackchain

      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        val subTree = prover.search(exprs :+ b, context, state.withLinear((s"lh$depth", a) :: linearContext), subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).filter(_.linearContext.isEmpty).map { s =>
          Tree.V(SearchNode(exprs :+ b, "linear-implies-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "linear-implies-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }.toVector ++ linearBackchain
      
      case Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)) =>
        // リソース分割の試行 (より強力なパーティショニング)
        def partition(ctx: List[(String, Expr)]): LazyList[(List[(String, Expr)], List[(String, Expr)])] = {
          if (ctx.isEmpty) LazyList((Nil, Nil))
          else {
            val (name, expr) :: tail = ctx
            partition(tail).flatMap { case (l, r) =>
              LazyList(((name, expr) :: l, r), (l, (name, expr) :: r))
            }
          }
        }

        val results = partition(linearContext).flatMap { case (leftPart, rightPart) =>
          prover.checkDeadline()
          val subTreeA = prover.search(exprs :+ a, context, state.withLinear(leftPart), subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTreeA).flatMap { sA =>
            val subTreeB = prover.search(exprs :+ b, sA.context, state.withLinear(rightPart), sA.subst, depth + 1, limit, visited, guarded)
            allSuccesses(subTreeB).map { sB =>
              Tree.V(SearchNode(exprs :+ goal, "tensor-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, sB.subst), "tensor-intro", List(sA.result.toOption.get.tree, sB.result.toOption.get.tree)))), sB.subst, sB.context, sB.linearContext), Vector(subTreeA, subTreeB))
            }
          }
        }
        results.toVector ++ linearBackchain

      case Expr.Meta(_) =>
        // Frame Inference: 残りのリソースをすべてメタ変数に割り当てる
        val frameResult = if (linearContext.nonEmpty) {
          // emp を除外してリソースを収集
          val resources = linearContext.filterNot(_._2 == Expr.Sym("emp")).sortBy(_._1).map(_._2)
          val frame = if (resources.isEmpty) Expr.Sym("emp")
                      else resources.reduceLeft((acc, e) => Expr.App(Expr.Sym(SepAnd), List(acc, e)))
          
          unify(goal, frame, subst).map { s =>
            Tree.V(SearchNode(exprs :+ goal, "frame-inference", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "frame-inference"))), s, context, Nil), Vector.empty)
          }.toVector
        } else {
          // リソースが空なら emp または ⊤ に割り当てる
          (unify(goal, Expr.Sym("emp"), subst).toVector ++ unify(goal, Expr.Sym(LOne), subst).toVector).map { s =>
            Tree.V(SearchNode(exprs :+ goal, "frame-inference-empty", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "frame-inference-empty"))), s, context, Nil), Vector.empty)
          }
        }
        frameResult ++ linearBackchain

      case _ => linearBackchain
    }
  }

  private def decompose(e: Expr, freshMeta: () => Expr): (List[Expr], Expr) = e match {
    case Expr.App(Expr.Sym(LImplies | Implies), List(a, b)) =>
      val (ps, c) = decompose(b, freshMeta)
      (a :: ps, c)
    case Expr.App(Expr.Sym(Forall), args) if args.length >= 2 =>
      val (vName, body) = args match {
        case List(Expr.Var(v), b) => (v, b)
        case List(Expr.Var(v), _, b) => (v, b)
        case _ => (null, null)
      }
      if (vName != null) {
        val meta = freshMeta()
        decompose(Prover.substVar(body, vName, meta), freshMeta)
      } else (Nil, e)
    case _ => (Nil, e)
  }

  override def getContextHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: List[(String, Expr)],
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    val goal = exprs.last
    val linearContext = state.linearContext
    
    val bangElim = context.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(Bang), List(a))), i) =>
        val newCtx = context.patch(i, List((name, a)), 1)
        val subTree = prover.search(exprs, newCtx, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).map { s =>
          Tree.V(SearchNode(exprs, s"linear-bang-elim[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"linear-bang-elim[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }
      case _ => None
    }.toVector

    val tensorDestruct = (context.map((_, false)) ++ linearContext.map((_, true))).zipWithIndex.flatMap {
      case (((name, Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b))), isLinear), i) =>
        if (isLinear) {
          val newL = linearContext.filterNot(_._1 == name) ++ List((s"$name.1", a), (s"$name.2", b))
          val subTree = prover.search(exprs, context, state.withLinear(newL), subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).map { s =>
            Tree.V(SearchNode(exprs, s"tensor-destruct[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"tensor-destruct[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        } else {
          val newCtx = context.patch(context.indexWhere(_._1 == name), List((s"$name.1", a), (s"$name.2", b)), 1)
          val subTree = prover.search(exprs, newCtx, state, subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).map { s =>
            Tree.V(SearchNode(exprs, s"tensor-is-ﾃ夕$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"tensor-is-ﾃ夕$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
          }
        }
      case _ => None
    }.toVector

    val ptrAssign = linearContext.flatMap {
      case (name, Expr.App(Expr.Sym(PointsTo), List(ptr, oldVal))) =>
        goal match {
          case Expr.App(Expr.Sym("triple"), List(pre, Expr.App(Expr.Sym(":="), List(p, newVal)), post)) if p == ptr =>
            // 残りのリソースをすべて F (Frame) としてまとめる
            val otherLinear = linearContext.filterNot(_._1 == name)
            val frame = if (otherLinear.isEmpty) Expr.Sym("emp") else otherLinear.map(_._2).reduce((a, b) => Expr.App(Expr.Sym(SepAnd), List(a, b)))
            
            // 目標: post が (ptr |-> newVal * frame) を含むことを示す
            val targetPost = Expr.App(Expr.Sym(SepAnd), List(Expr.App(Expr.Sym(PointsTo), List(ptr, newVal)), frame))
            val subGoal = Expr.App(Expr.Sym(LImplies), List(targetPost, post))
            
            val subTree = prover.search(exprs :+ subGoal, context, state.withLinear(Nil), subst, depth + 1, limit, visited, guarded)
            allSuccesses(subTree).map { s =>
              Tree.V(SearchNode(exprs :+ goal, s"ptr-assign[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"ptr-assign[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
            }
          case _ => Nil
        }
      case _ => Nil
    }.toVector

    bangElim ++ tensorDestruct ++ ptrAssign
  }
}
