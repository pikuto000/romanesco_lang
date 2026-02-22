// ==========================================
// LinearLogicSearch.scala
// 線形論理・分離論理固有の探索ロジック (Integrated Forward-Backward Model)
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

  private def isEffectivelyEmpty(ctx: List[(String, Expr)]): Boolean =
    ctx.forall(_._2 == Expr.Sym("emp"))

  private def flatten(e: Expr): List[Expr] = e match {
    case Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)) => flatten(a) ++ flatten(b)
    case _ => List(e)
  }

  private def withFlattened(name: String, e: Expr, ctx: List[(String, Expr)]): List[(String, Expr)] = {
    val items = flatten(e)
    if (items.length == 1 && items.head == e) (name, e) :: ctx
    else {
      items.zipWithIndex.map { case (item, i) => (s"$name.$i", item) } ++ ctx
    }
  }

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

    // --- 1. 前向き推論 (ドミノ倒し) を最優先 ---
    // プログラム実行のように、現在のリソースを使って状態を遷移させるルールを先に適用する
    val fwdResults = prover.asInstanceOf[Prover].config.rules.flatMap { rule =>
      val lhsItems = flatten(rule.lhs).filterNot(_ == Expr.Sym("emp"))
      if (lhsItems.nonEmpty) {
        def consumeAll(remainingLhs: List[Expr], currentCtx: List[(String, Expr)], currentS: Subst): Option[(Subst, List[(String, Expr)])] = remainingLhs match {
          case Nil => Some((currentS, currentCtx))
          case l :: lTail =>
            currentCtx.indices.view.flatMap { ci =>
              unify(applySubst(l, currentS), currentCtx(ci)._2, currentS).flatMap { s =>
                consumeAll(lTail, currentCtx.patch(ci, Nil, 1), s)
              }
            }.headOption
        }

        consumeAll(lhsItems, linearContext, subst).map { case (finalS, residue) =>
          val rhsResult = prover.normalize(applySubst(rule.rhs, finalS))
          // RHS からも emp を除外して追加 (増殖防止)
          val filteredRhs = flatten(rhsResult).filterNot(_ == Expr.Sym("emp"))
          val newL = filteredRhs.zipWithIndex.map { case (item, i) => (s"step[${rule.name}].$i", item) } ++ residue
          
          val subTree = prover.search(exprs, context, state.withLinear(newL), finalS, depth + 1, limit, visited, guarded)
          allSuccesses(subTree).map { res =>
            val pTree = ProofTree.Node(applySubst(goal, res.subst), s"linear-step[${rule.name}]", List(res.result.toOption.get.tree))
            Tree.V(SearchNode(exprs, s"linear-step[${rule.name}]", depth, Right(ProofResult(pTree)), res.subst, res.context, res.linearContext), Vector(subTree))
          }
        }.getOrElse(Nil)
      } else Nil
    }.toVector

    // --- 2. 後ろ向き推論 (バックチェイン) ---
    val allHyps = linearContext.map(h => (h, true)) ++ context.map(h => (h, false))
    val backchainResults = allHyps.flatMap { case ((name, hyp), isLinear) =>
      val (premises, conclusion) = decompose(hyp, () => prover.freshMeta(depth))
      unify(conclusion, goal, subst).flatMap { s =>
        val initialResidue = if (isLinear) linearContext.filterNot(_._1 == name) else linearContext
        def solvePremises(ps: List[Expr], currentS: Subst, currentL: List[(String, Expr)], solved: List[ProofTree]): LazyList[(Subst, List[(String, Expr)], List[ProofTree])] = ps match {
          case Nil => LazyList((currentS, currentL, solved))
          case p :: tail =>
            val targetP = applySubst(p, currentS)
            val subTree = prover.search(exprs :+ targetP, context, state.withLinear(currentL), currentS, depth + 1, limit, visited, guarded)
            allSuccesses(subTree).flatMap { res =>
              solvePremises(tail, res.subst, res.linearContext, solved :+ res.result.toOption.get.tree)
            }
        }
        solvePremises(premises, s, initialResidue, Nil).map { case (finalS, finalL, proofs) =>
          val ruleMark = if (isLinear) "linear" else "persistent"
          val pTree = ProofTree.Node(applySubst(goal, finalS), s"apply-$ruleMark[$name]", proofs)
          Tree.V(SearchNode(exprs, s"apply-$ruleMark[$name]", depth, Right(ProofResult(pTree)), finalS, context, finalL), Vector.empty)
        }
      }
    }.toVector

    // --- 3. ゴールの構造分解 ---
    val logicResults: Vector[Tree[SearchNode]] = goal match {
      case Expr.Sym("emp") =>
        if (isEffectivelyEmpty(linearContext)) {
          Vector(Tree.V(SearchNode(exprs :+ goal, "emp-intro", depth, Right(ProofResult(ProofTree.Leaf(goal, "emp-intro"))), subst, context, Nil), Vector.empty))
        } else Vector.empty

      case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
        val newL = withFlattened(s"lh$depth", a, linearContext)
        val subTree = prover.search(exprs :+ b, context, state.withLinear(newL), subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).filter(s => isEffectivelyEmpty(s.linearContext)).map { s =>
          Tree.V(SearchNode(exprs :+ b, "linear-implies-intro", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), "linear-implies-intro", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }.toVector

      case Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b)) =>
        val subTreeA = prover.search(exprs :+ a, context, state, subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTreeA).flatMap { sA =>
          val subTreeB = prover.search(exprs :+ b, sA.context, state.withLinear(sA.linearContext), sA.subst, depth + 1, limit, visited, guarded)
          allSuccesses(subTreeB).map { sB =>
            Tree.V(SearchNode(exprs :+ goal, "tensor-intro-seq", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, sB.subst), "tensor-intro-seq", List(sA.result.toOption.get.tree, sB.result.toOption.get.tree)))), sB.subst, sB.context, sB.linearContext), Vector(subTreeA, subTreeB))
          }
        }.toVector

      case Expr.Meta(_) =>
        if (linearContext.nonEmpty) {
          val hasLeakedResources = linearContext.exists(h => h._2.headSymbol == "At" || h._2.headSymbol == "Own")
          if (hasLeakedResources) Vector.empty
          else {
            val resources = linearContext.filterNot(_._2 == Expr.Sym("emp")).sortBy(_._1).map(_._2)
            val frame = if (resources.isEmpty) Expr.Sym("emp")
                        else resources.reduceLeft((acc, e) => Expr.App(Expr.Sym(SepAnd), List(acc, e)))
            unify(goal, frame, subst).map { s =>
              Tree.V(SearchNode(exprs :+ goal, "frame-inference", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "frame-inference"))), s, context, Nil), Vector.empty)
            }.toVector
          }
        } else {
          (unify(goal, Expr.Sym("emp"), subst).toVector ++ unify(goal, Expr.Sym(LOne), subst).toVector).map { s =>
            Tree.V(SearchNode(exprs :+ goal, "frame-inference-empty", depth, Right(ProofResult(ProofTree.Leaf(applySubst(goal, s), "frame-inference-empty"))), s, context, Nil), Vector.empty)
          }
        }
      case _ => Vector.empty
    }

    logicResults ++ fwdResults ++ backchainResults
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
    
    // a. 破壊的分解 (A ⊗ B |- A, B)
    val tensorDestruct = linearContext.zipWithIndex.flatMap {
      case ((name, Expr.App(Expr.Sym(Tensor | SepAnd), List(a, b))), i) =>
        val newL = linearContext.filterNot(_._1 == name) ++ List((s"$name.1", a), (s"$name.2", b))
        val subTree = prover.search(exprs, context, state.withLinear(newL), subst, depth + 1, limit, visited, guarded)
        allSuccesses(subTree).map { s =>
          Tree.V(SearchNode(exprs, s"tensor-destruct[$name]", depth, Right(ProofResult(ProofTree.Node(applySubst(goal, s.subst), s"tensor-destruct[$name]", List(s.result.toOption.get.tree)))), s.subst, s.context, s.linearContext), Vector(subTree))
        }
      case _ => None
    }.toVector

    tensorDestruct
  }
}
