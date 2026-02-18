// ==========================================
// InductionPlugin.scala
// 帰納法の探索ロジック (Plugin 化・完全Treeベース)
// ==========================================

package romanesco.Solver.core

import romanesco.Utils.Debug.logger
import LogicSymbols._
import romanesco.Types.Tree
import romanesco.Solver.core.Prover

class InductionPlugin extends LogicPlugin {
  override def name: String = "Induction"

  import Unifier._

  type Context = List[(String, Expr)]

  override def getGoalHooks(
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
    val goal = exprs.last
    if (state.inductionCount >= prover.config.maxInduction) Vector.empty
    else {
      goal match {
        case Expr.App(Expr.Sym(Forall), args) if args.length >= 2 =>
          val (vName, body, typeOpt) = args match {
            case List(Expr.Var(v), b) => (v, b, None)
            case List(Expr.Var(v), t, b) => (v, b, Some(t))
            case _ => (null, null, None)
          }
          if (vName == null) return Vector.empty

          logger.log(s"Induction check for variable: $vName (type: $typeOpt)")
          
          val allAlgebras = StandardRules.defaultAlgebras ++ prover.getAlgebras
          val targetAlgebras = allAlgebras.filter { a =>
            val nameMatch = typeOpt.exists(t => t.headSymbol.equalsIgnoreCase(a.name))
            val prefixMatch = vName == a.varPrefix || vName.startsWith(a.varPrefix + "_")
            nameMatch || (typeOpt.isEmpty && prefixMatch)
          }

          targetAlgebras.flatMap { algebra =>
            logger.log(s"Attempting induction on ${algebra.name} for $vName")
            solveInduction(exprs, vName, body, goal, rules, context, state, subst, depth, limit, visited, guarded, prover, algebra)
          }.toVector
        case _ => Vector.empty
      }
    }
  }

  private def solveInduction(
      exprs: Vector[Expr],
      vn: String,
      body: Expr,
      og: Expr,
      rules: List[CatRule],
      context: Context,
      state: LogicState,
      subst: Subst,
      depth: Int,
      limit: Int,
      visited: Set[(Expr, Set[Expr], List[Expr], LogicState)],
      guarded: Boolean,
      prover: ProverInterface,
      algebra: InitialAlgebra
  ): Vector[Tree[SearchNode]] = {
    logger.log(s"Starting induction for ${algebra.name}")
    
    // 点のケースの証明をまず集める必要がある（道のケースで使うため）
    // シンプルにするため、まずは全ケースを独立に扱うが、HITの場合は先行する点の結果に依存する
    
    def solveCases(cs: List[ConstructorDef], s: Subst, st: LogicState, solved: Map[String, ProofTree]): LazyList[(List[ProofTree], Subst, LogicState)] = cs match {
      case Nil => 
        logger.log(s"All cases solved for ${algebra.name}")
        val orderedProofs = algebra.constructors.map(c => solved(c.symbol))
        LazyList((orderedProofs, s, st))
      
      case c :: tail =>
        logger.log(s"Solving case ${c.symbol} for ${algebra.name}")
        c.ctorType match {
          case ConstructorType.Point =>
            // 点のケース
            val ct = if (c.argTypes.isEmpty) Expr.Sym(c.symbol)
                     else Expr.App(Expr.Sym(c.symbol), c.argTypes.zipWithIndex.map {
                       case (ArgType.Recursive, i) => Expr.Var(s"${vn}_$i")
                       case (ArgType.Constant, i) => Expr.Var(s"a_$i")
                     })
            
            val ihs = c.argTypes.zipWithIndex.collect {
              case (ArgType.Recursive, i) => 
                val ih = romanesco.Solver.core.Prover.substVar(body, vn, Expr.Var(s"${vn}_$i"))
                (s"IH_${vn}_$i", ih)
            }

            val caseGoalBase = romanesco.Solver.core.Prover.substVar(body, vn, ct)
            val caseGoal = c.argTypes.zipWithIndex.foldRight[Expr](caseGoalBase) {
              case ((ArgType.Constant, i), acc) => Expr.App(Expr.Sym(Forall), List(Expr.Var(s"a_$i"), acc))
              case ((ArgType.Recursive, i), acc) => Expr.App(Expr.Sym(Forall), List(Expr.Var(s"${vn}_$i"), acc))
            }

            val subTree = prover.search(exprs :+ caseGoal, ihs ++ context, st.incInduction, s, depth + 1, limit + 1, visited, guarded) // Bonus +1 depth
            allSuccesses(subTree).flatMap { res =>
              solveCases(tail, res.subst, st.withLinear(res.linearContext), solved + (c.symbol -> res.result.toOption.get.tree))
            }

          case ConstructorType.Path(from, to) =>
            // 道のケース: transport(λx. body, p, val_from) = val_to
            val p = Expr.Sym(c.symbol) // path constructor
            val pred = Expr.App(Expr.Sym("λ"), List(Expr.Var(vn), body))
            
            // from, to のシンボル名を正規化して solved から取得
            val fromSym = from match { case Expr.Sym(s) => s; case Expr.App(Expr.Sym(s), _) => s; case _ => "" }
            val toSym = to match { case Expr.Sym(s) => s; case Expr.App(Expr.Sym(s), _) => s; case _ => "" }

            val bodyFrom = Prover.substVar(body, vn, from)
            val bodyTo = Prover.substVar(body, vn, to)
            val transportGoal = Expr.App(Expr.Sym(Eq), List(
              Expr.App(Expr.Sym(Transport), List(pred, p, bodyFrom)),
              bodyTo
            ))
            
            val subTree = prover.search(exprs :+ transportGoal, context, st.incInduction, s, depth + 1, limit, visited, guarded)
            allSuccesses(subTree).flatMap { res =>
              solveCases(tail, res.subst, st.withLinear(res.linearContext), solved + (c.symbol -> res.result.toOption.get.tree))
            }
        }
    }

    solveCases(algebra.constructors, subst, state, Map.empty).map { case (ts, fs, fst) =>
      val result = Right(ProofResult(ProofTree.Node(applySubst(og, fs), s"induction[${algebra.name}]", ts)))
      Tree.V(SearchNode(exprs, s"induction[${algebra.name}]", depth, result, fs, context, fst.linearContext), Vector.empty)
    }.toVector
  }
}
