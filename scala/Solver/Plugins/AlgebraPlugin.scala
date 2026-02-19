package romanesco.Solver.core

import romanesco.Solver.core._
import LogicSymbols._

abstract class AlgebraPlugin(val algebra: InitialAlgebra) extends LogicPlugin {
  override def name: String = s"Algebra[${algebra.name}]"
  override def priority: Int = 30
  def computationRules: List[CatRule]
  override def providedRules: List[CatRule] = computationRules
  override def providedAlgebras: List[InitialAlgebra] = List(algebra)
}

class NatAlgebraPlugin extends AlgebraPlugin(StandardRules.natAlgebra) {
  override def computationRules: List[CatRule] = StandardRules.natPlusRules
  override def normalizeHook(expr: Expr): Option[Expr] = expr match {
    case Expr.App(Expr.Sym("plus"), List(Expr.Sym(z), m)) if z == Zero || z == Initial => Some(m)
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym(s), List(n)), m)) if s == Succ =>
      Some(Expr.App(Expr.Sym(Succ), List(Expr.App(Expr.Sym("plus"), List(n, m)))))
    case Expr.App(Expr.Sym("plus"), List(n, Expr.Sym(z))) if z == Zero || z == Initial => Some(n)
    case Expr.App(Expr.Sym("plus"), List(n, Expr.App(Expr.Sym(s), List(m)))) if s == Succ =>
      Some(Expr.App(Expr.Sym(Succ), List(Expr.App(Expr.Sym("plus"), List(n, m)))))
    case Expr.App(Expr.Sym("plus"), List(Expr.App(Expr.Sym("plus"), List(a, b)), c)) =>
      Some(Expr.App(Expr.Sym("plus"), List(a, Expr.App(Expr.Sym("plus"), List(b, c)))))
    case _ => None
  }
}

class ListAlgebraPlugin extends AlgebraPlugin(StandardRules.listAlgebra) {
  override def computationRules: List[CatRule] = StandardRules.listAppendRules.filter(r => r.name.contains("append") || r.name.contains("reverse") || r.name.contains("list_prop"))
  override def normalizeHook(expr: Expr): Option[Expr] = expr match {
    case Expr.App(Expr.Sym("append"), List(Expr.Sym("nil"), ys)) => Some(ys)
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("cons"), List(x, xs)), ys)) =>
      Some(Expr.App(Expr.Sym("cons"), List(x, Expr.App(Expr.Sym("append"), List(xs, ys)))))
    case Expr.App(Expr.Sym("append"), List(xs, Expr.Sym("nil"))) => Some(xs)
    case Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("append"), List(xs, ys)), zs)) =>
      Some(Expr.App(Expr.Sym("append"), List(xs, Expr.App(Expr.Sym("append"), List(ys, zs)))))

    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("reverse"), List(t)))) => Some(t)
    case Expr.App(Expr.Sym("reverse"), List(Expr.Sym("nil"))) => Some(Expr.Sym("nil"))
    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Some(Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("reverse"), List(xs)), Expr.App(Expr.Sym("cons"), List(x, Expr.Sym("nil"))))))
    case Expr.App(Expr.Sym("reverse"), List(Expr.App(Expr.Sym("append"), List(xs, ys)))) =>
      Some(Expr.App(Expr.Sym("append"), List(Expr.App(Expr.Sym("reverse"), List(ys)), Expr.App(Expr.Sym("reverse"), List(xs)))))

    case Expr.App(Expr.Sym("map"), List(_, Expr.Sym("nil"))) => Some(Expr.Sym("nil"))
    case Expr.App(Expr.Sym("map"), List(f, Expr.App(Expr.Sym("cons"), List(x, xs)))) =>
      Some(Expr.App(Expr.Sym("cons"), List(Expr.App(f, List(x)), Expr.App(Expr.Sym("map"), List(f, xs)))))

    case Expr.App(Expr.Sym("list_prop"), List(Expr.App(Expr.Sym("append"), List(xs, ys)))) =>
      Some(Expr.App(Expr.Sym(And), List(Expr.App(Expr.Sym("list_prop"), List(xs)), Expr.App(Expr.Sym("list_prop"), List(ys)))))
      
    case _ => None
  }
}

class TreeAlgebraPlugin extends AlgebraPlugin(StandardRules.treeAlgebra) {
  override def computationRules: List[CatRule] = StandardRules.listAppendRules.filter(_.name.contains("mirror"))
  override def normalizeHook(expr: Expr): Option[Expr] = expr match {
    case Expr.App(Expr.Sym("mirror"), List(Expr.App(Expr.Sym("mirror"), List(t)))) => Some(t)
    case _ => None
  }
}

class S1AlgebraPlugin extends AlgebraPlugin(StandardRules.s1Algebra) {
  override def computationRules: List[CatRule] = Nil
}

class MaybeAlgebraPlugin extends AlgebraPlugin(StandardRules.maybeAlgebra) {
  override def computationRules: List[CatRule] = Nil
}

class IntervalAlgebraPlugin extends AlgebraPlugin(StandardRules.defaultAlgebras(5)) {
  override def computationRules: List[CatRule] = StandardRules.intervalAlgebra
}

class SuspAlgebraPlugin extends AlgebraPlugin(StandardRules.defaultAlgebras(6)) {
  override def computationRules: List[CatRule] = Nil
}
