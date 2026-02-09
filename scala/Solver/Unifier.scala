// ==========================================
// Unifier.scala
// 単一化アルゴリズム（MetaId対応）
// ==========================================

package romanesco.Solver.core
import romanesco.Utils.Debug.logger

object Unifier:
  type Subst = Map[MetaId, Expr]
  def emptySubst: Subst = Map.empty

  /**
   * 式 e に代入 s を適用します。
   */
  def applySubst(e: Expr, s: Subst): Expr = e match
    case Expr.Meta(id) if s.contains(id) =>
      applySubst(s(id), s)
    case Expr.App(h, args) =>
      Expr.App(applySubst(h, s), args.map(applySubst(_, s)))
    case _ => e

  def applySubstToRule(rule: CatRule, s: Subst): CatRule =
    CatRule(
      rule.name,
      applySubst(rule.lhs, s),
      applySubst(rule.rhs, s),
      rule.universals.map(applySubst(_, s))
    )

  /**
   * 2つの式 e1 と e2 を単一化します。
   */
  def unify(e1: Expr, e2: Expr, subst: Subst): LazyList[Subst] =
    val r1 = applySubst(e1, subst)
    val r2 = applySubst(e2, subst)

    if r1 == r2 then
      LazyList(subst)
    else
      (r1, r2) match
        case (Expr.Meta(id), t) if !occursCheck(id, t) =>
          LazyList(subst + (id -> t))
        case (t, Expr.Meta(id)) if !occursCheck(id, t) =>
          LazyList(subst + (id -> t))

        case (Expr.Sym(n1), Expr.Sym(n2)) if n1 == n2 =>
          LazyList(subst)

        case (Expr.Var(n1), Expr.Var(n2)) if n1 == n2 =>
          LazyList(subst)

        case (Expr.App(h1, a1), Expr.App(h2, a2)) if a1.length == a2.length =>
          unify(h1, h2, subst).flatMap { s =>
            a1.zip(a2).foldLeft(LazyList(s)) { case (sList, (arg1, arg2)) =>
              sList.flatMap(unify(arg1, arg2, _))
            }
          }

        case _ => LazyList.empty

  private def occursCheck(metaId: MetaId, expr: Expr): Boolean = expr match
    case Expr.Meta(id)     => id == metaId
    case Expr.App(h, args) =>
      occursCheck(metaId, h) || args.exists(occursCheck(metaId, _))
    case _ => false
