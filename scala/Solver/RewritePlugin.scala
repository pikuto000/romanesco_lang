// ==========================================
// RewritePlugin.scala
// 文脈中の等式を用いた自動書き換えプラグイン
// ==========================================

package romanesco.Solver.core

import romanesco.Solver.core.LogicSymbols._
import romanesco.Types.Tree
import scala.collection.mutable

class RewritePlugin extends LogicPlugin {
  override def name: String = "Rewrite"

  import Unifier._

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
    val results = mutable.ArrayBuffer[Tree[SearchNode]]()

    context.foreach { case (name, hyp) =>
      prover.checkDeadline()
      val instHyp = applySubst(hyp, subst)
      instHyp match {
        case Expr.App(Expr.Sym(Eq | Path), args) if args.length >= 2 =>
          val l = args match { case List(_, x, _) if args.length == 3 => x; case List(x, _) => x; case _ => null }
          val r = args.last
          if (l != null) {
            // ゴールの中で l にマッチする箇所を探して r で置き換える
            def findAndReplace(expr: Expr): List[(Expr, Subst)] = {
              val direct = unify(expr, l, subst).map(s => (applySubst(r, s), s)).toList
              val recursive = expr match {
                case Expr.App(f, as) =>
                  as.indices.flatMap { i =>
                    findAndReplace(as(i)).map { case (newArg, s) =>
                      val nextArgs = as.patch(i, List(newArg), 1).map(applySubst(_, s))
                      (Expr.App(applySubst(f, s), nextArgs), s)
                    }
                  }.toList
                case _ => Nil
              }
              direct ++ recursive
            }

            findAndReplace(goal).take(10).foreach { case (rewritten, nextS) =>
              val subGoal = prover.normalize(rewritten)
              if (subGoal.canonicalize() != goal.canonicalize()) {
                val subTree = prover.search(exprs :+ subGoal, context, state, nextS, depth + 1, limit + 1, visited, guarded) // Bonus +1 depth
                allSuccesses(subTree).foreach { res =>
                  val p = ProofTree.Node(applySubst(goal, res.subst), s"rewrite[$name]", List(res.result.toOption.get.tree))
                  results += Tree.V(SearchNode(exprs :+ subGoal, s"rewrite[$name]", depth, Right(ProofResult(p)), res.subst, res.context, res.linearContext), Vector(subTree))
                }
              }
            }
          }
        case _ => ()
      }
    }
    results.toVector
  }
}
