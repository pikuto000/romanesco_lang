// ==========================================
// Unifier.scala (修正版)
// ==========================================

package romanesco.Solver.core
import romanesco.Utils.Debug.logger

object Unifier:
  type Subst = Map[Int, Expr]
  def emptySubst: Subst = Map.empty

  def applySubst(e: Expr, s: Subst): Expr = e match
    case Expr.Meta(id) if s.contains(id) =>
      logger.log(s"substituting $id with ${s(id)}")
      applySubst(s(id), s)
    case Expr.App(h, args) =>
      logger.log(s"substituting $h with $s")
      Expr.App(applySubst(h, s), args.map(applySubst(_, s)))
    case _ =>
      logger.log(s"not substituting $e")
      e

  def applySubstToRule(rule: CatRule, s: Subst): CatRule =
    CatRule(
      rule.name,
      applySubst(rule.lhs, s),
      applySubst(rule.rhs, s),
      rule.universals.map(applySubst(_, s))
    )

  def unify(e1: Expr, e2: Expr, subst: Subst): Option[Subst] =
    logger.log(s"unifying $e1 with $e2")
    val r1 = applySubst(e1, subst)
    val r2 = applySubst(e2, subst)

    if r1 == r2 then
      logger.log(s"unification succeeded (identical). $subst")
      Some(subst)
    else
      val result = (r1, r2) match
        case (Expr.Meta(id), t) if !occursCheck(id, t) =>
          logger.log(s"unifying meta $id with $t")
          Some(subst + (id -> t))
        case (t, Expr.Meta(id)) if !occursCheck(id, t) =>
          logger.log(s"unifying meta $id with $t")
          Some(subst + (id -> t))

        case (Expr.Sym(n1), Expr.Sym(n2)) if n1 == n2 =>
          Some(subst)

        case (Expr.Var(n1), Expr.Var(n2)) if n1 == n2 =>
          Some(subst)

        case (Expr.App(h1, a1), Expr.App(h2, a2)) if a1.length == a2.length =>
          unify(h1, h2, subst).flatMap { s =>
            a1.zip(a2).foldLeft(Option(s)) { case (sOpt, (arg1, arg2)) =>
              sOpt.flatMap(unify(arg1, arg2, _))
            }
          }

        case _ => None
      
      if result.isDefined then logger.log(s"unification succeeded. ${result.get}")
      else logger.log(s"unification failed. $subst")
      result

  private def occursCheck(metaId: Int, expr: Expr): Boolean = expr match
    case Expr.Meta(id)     => id == metaId
    case Expr.App(h, args) =>
      occursCheck(metaId, h) || args.exists(occursCheck(metaId, _))
    case _ => false
