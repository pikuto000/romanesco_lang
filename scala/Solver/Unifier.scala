// ==========================================
// Unifier.scala
// 単一化アルゴリズム（Appラムダ対応）
// ==========================================

package romanesco.Solver.core
import romanesco.Utils.Debug.logger
import LogicSymbols._

object Unifier:
  type Subst = Map[MetaId, Expr]
  def emptySubst: Subst = Map.empty

  def applySubst(e: Expr, s: Subst): Expr = e match
    case Expr.Meta(id) if s.contains(id) =>
      applySubst(s(id), s)
    case Expr.App(f, args) =>
      Rewriter.normalize(Expr.App(applySubst(f, s), args.map(applySubst(_, s))))
    case _ => e

  def applySubstToRule(rule: CatRule, s: Subst): CatRule =
    CatRule(
      rule.name,
      applySubst(rule.lhs, s),
      applySubst(rule.rhs, s),
      rule.universals.map(applySubst(_, s))
    )

  def unify(e1: Expr, e2: Expr, subst: Subst): LazyList[Subst] =
    val r1 = applySubst(e1, subst)
    val r2 = applySubst(e2, subst)

    if r1 == r2 then
      LazyList(subst)
    else
      logger.log(s"Unifying: $r1 with $r2")
      val res = (r1, r2) match
        // --- ラムダ抽象の単一化 (App構造) ---
        case (Expr.Lam(v1, b1), Expr.Lam(v2, b2)) =>
          val newB2 = Prover.substVar(b2, v2, Expr.Var(v1))
          unify(b1, newB2, subst)

        // --- メタ変数同士のアプリケーション ---
        case (Expr.App(Expr.Meta(id1), args1), Expr.App(Expr.Meta(id2), args2)) if id1 == id2 && args1.length == args2.length =>
          args1.zip(args2).foldLeft(LazyList(subst)) { case (sList, (a1, a2)) =>
            sList.flatMap(unify(a1, a2, _))
          }

        // --- 高階パターン単一化 ---
        case (Expr.App(Expr.Meta(id), args), t) if isPattern(args) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else if (!occursCheck(id, t)) solvePattern(id, args, t, subst)
          else LazyList.empty

        case (t, Expr.App(Expr.Meta(id), args)) if isPattern(args) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else if (!occursCheck(id, t)) solvePattern(id, args, t, subst)
          else LazyList.empty

        // --- 基本的なメタ変数の単一化 ---
        case (Expr.Meta(id), t) if !occursCheck(id, t) =>
          LazyList(subst + (id -> t))
        case (t, Expr.Meta(id)) if !occursCheck(id, t) =>
          LazyList(subst + (id -> t))

        case (Expr.Sym(n1), Expr.Sym(n2)) if n1 == n2 => LazyList(subst)
        case (Expr.Var(n1), Expr.Var(n2)) if n1 == n2 => LazyList(subst)

        // --- アプリケーションの単一化 ---
        case (Expr.App(h1, a1), Expr.App(h2, a2)) if a1.length == a2.length =>
          unify(h1, h2, subst).flatMap { s =>
            a1.zip(a2).foldLeft(LazyList(s)) { case (sList, (arg1, arg2)) =>
              sList.flatMap(unify(arg1, arg2, _))
            }
          }

        case _ => LazyList.empty
      
      if (res.nonEmpty) logger.log(s"Unify Success: $r1 = $r2 with ${res.head}")
      res

  private def isPattern(args: List[Expr]): Boolean = {
    val vars = args.collect { case Expr.Var(name) => name }
    vars.length == args.length && vars.distinct.length == vars.length
  }

  private def solvePattern(id: MetaId, args: List[Expr], target: Expr, subst: Subst): LazyList[Subst] = {
    val varNames = args.collect { case Expr.Var(name) => name }
    val lambda = varNames.foldRight(target) { (name, body) =>
      Expr.App(Expr.Sym("λ"), List(Expr.Var(name), body))
    }
    LazyList(subst + (id -> lambda))
  }

  private def occursCheck(metaId: MetaId, expr: Expr): Boolean = expr match
    case Expr.Meta(id)     => id == metaId
    case Expr.App(h, args) =>
      occursCheck(metaId, h) || args.exists(occursCheck(metaId, _))
    case _ => false