// ==========================================
// HoTTSearch.scala
// HoTT (Homotopy Type Theory) 固有の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

//Todo: HIT DSL関連システムの実装
class HoTTPlugin extends LogicPlugin {
  override def name: String = "HoTT"

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
    import Unifier._
    val goal = prover.normalize(applySubst(exprs.last, subst))
    val results = scala.collection.mutable.ArrayBuffer[Tree[SearchNode]]()

    // isSet 直接ゴールマッチ: path(path(A,x,y),p,q) でコンテキストに isSet(A) があれば即解決
    logger.log(s"HoTT goalHook: goal=$goal, headSymbol=${goal.headSymbol}")
    goal match {
      case Expr.App(Expr.Sym(p), List(Expr.App(Expr.Sym(p2), List(a, _, _)), _, _)) if p == Path && p2 == Path =>
        logger.log(s"isSet-elim candidate: a=$a, context=${context.map(_._2)}")
        context.foreach {
          case (name, Expr.App(Expr.Sym("isSet"), List(a2))) =>
            logger.log(s"isSet-elim: trying unify $a with $a2")
            Unifier.unify(a, a2, subst).foreach { s =>
              logger.log(s"isSet-elim SUCCESS: $name")
              val proof = ProofTree.Leaf(applySubst(goal, s), s"isSet-elim[$name]")
              results += Tree.V(
                SearchNode(exprs, s"isSet-elim[$name]", depth, Right(ProofResult(proof)), s, context, state.linearContext),
                Vector.empty
              )
            }
          case _ => ()
        }
      case _ => ()
    }

    results.toVector ++ searchPathInduction(
      exprs,
      rules,
      context,
      state,
      subst,
      depth,
      limit,
      visited,
      guarded,
      prover
    )
  }

  override def getContextHooks(
      exprs: Vector[Expr],
      rules: List[CatRule],
      context: Context,
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface
  ): Vector[Tree[SearchNode]] = {
    import Unifier._
    val goal = exprs.last
    val results = scala.collection.mutable.ArrayBuffer[Tree[SearchNode]]()

    // isProp(A) -> ∀x, y:A. path(A, x, y)
    // isSet(A)  -> ∀x, y:A. ∀p, q:path(A, x, y). path(path(A, x, y), p, q)
    context.foreach {
      case (name, Expr.App(Expr.Sym("isProp"), List(a))) =>
        val rule = CatRule(s"prop_$name", Expr.App(Expr.Sym(Path), List(a, Expr.Var("x"), Expr.Var("y"))), Expr.Sym(True), List(Expr.Var("x"), Expr.Var("y")))
        prover.asInstanceOf[Prover].addDynamicRule(rule)
      case (name, Expr.App(Expr.Sym("isSet"), List(a))) =>
        val innerPath = Expr.App(Expr.Sym(Path), List(a, Expr.Var("x"), Expr.Var("y")))
        // 動的ルール登録（他のプラグインからも利用可能に）
        val rule = CatRule(s"set_$name", Expr.App(Expr.Sym(Path), List(innerPath, Expr.Var("p"), Expr.Var("q"))), Expr.Sym(True), List(Expr.Var("x"), Expr.Var("y"), Expr.Var("p"), Expr.Var("q")))
        prover.asInstanceOf[Prover].addDynamicRule(rule)

        // isSet(A) ゴール加速: ゴールが path(path(A,_,_),_,_) を含む ∀ 量化なら一気に解決
        def stripForalls(e: Expr): Expr = e match {
          case Expr.App(Expr.Sym(Forall), List(_, body)) => stripForalls(body)
          case Expr.App(Expr.Sym(Forall), List(_, _, body)) => stripForalls(body)
          case other => other
        }
        val innerGoal = stripForalls(goal)
        innerGoal match {
          case Expr.App(Expr.Sym(p), List(Expr.App(Expr.Sym(p2), List(a2, _, _)), _, _)) if p == Path && p2 == Path =>
            Unifier.unify(a, a2, subst).foreach { s =>
              val proof = ProofTree.Leaf(applySubst(goal, s), s"isSet-elim[$name]")
              results += Tree.V(
                SearchNode(exprs, s"isSet-elim[$name]", depth, Right(ProofResult(proof)), s, context, state.linearContext),
                Vector.empty
              )
            }
          case _ => ()
        }
      case _ => ()
    }
    results.toVector
  }

  private def searchPathInduction(
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
    if (state.inductionCount >= prover.config.maxInduction) Vector.empty
    else {
      context.indices.flatMap { i =>
        prover.checkDeadline()
        context(i) match {
          case (pName, Expr.App(Expr.Sym(Path), List(_, x, Expr.Var(yName))))
              if x != Expr.Var(yName) =>
            val reflX = Expr.App(Expr.Sym(Refl), List(x))
            val substGoal =
              Prover.substVar(Prover.substVar(goal, yName, x), pName, reflX)
            val nextCtx = context
              .patch(i, Nil, 1)
              .map(h =>
                (
                  h._1,
                  Prover.substVar(Prover.substVar(h._2, yName, x), pName, reflX)
                )
              )
            val subTree = prover.search(
              exprs :+ substGoal,
              nextCtx,
              state.incInduction,
              subst,
              depth + 1,
              limit,
              visited,
              guarded
            )
            allSuccesses(subTree).map { s =>
              val result = Right(
                ProofResult(
                  ProofTree.Node(
                    applySubst(goal, s.subst),
                    s"path-induction[$pName]",
                    List(s.result.toOption.get.tree)
                  )
                )
              )
              Tree.V(
                SearchNode(
                  exprs :+ substGoal,
                  s"path-induction[$pName]",
                  depth,
                  result,
                  s.subst,
                  s.context,
                  s.linearContext
                ),
                Vector(subTree)
              )
            }
          case _ => Vector.empty
        }
      }.toVector
    }
  }
}
