package romanesco

import com.microsoft.z3._
import scala.collection.mutable

class Solver extends AutoCloseable {
  import AstExpr._
  import Stmt._

  private val ctx = new Context()
  private val solver = ctx.mkSolver()
  private val vars = mutable.Map[String, com.microsoft.z3.Expr[RealSort]]()
  private val funcs = mutable.Map[String, FuncDecl[RealSort]]()

  private def v(n: String) = vars.getOrElseUpdate(n, ctx.mkRealConst(n))

  private def toZ3(e: AstExpr, env: Map[String, com.microsoft.z3.Expr[RealSort]] = Map.empty): com.microsoft.z3.Expr[RealSort] = e match {
    case Num(n) => ctx.mkReal(n.toString).asInstanceOf[ArithExpr[RealSort]]
    case Var(n) => env.getOrElse(n, v(n))
    case BinOp("=", l, r) => val res = ctx.mkEq(toZ3(l, env), toZ3(r, env)); solver.add(res); toZ3(r, env)
    case BinOp("+", l, r) => ctx.mkAdd(toZ3(l, env).asInstanceOf[ArithExpr[RealSort]], toZ3(r, env).asInstanceOf[ArithExpr[RealSort]])
    case BinOp("-", l, r) => ctx.mkSub(toZ3(l, env).asInstanceOf[ArithExpr[RealSort]], toZ3(r, env).asInstanceOf[ArithExpr[RealSort]])
    case BinOp("*", l, r) => ctx.mkMul(toZ3(l, env).asInstanceOf[ArithExpr[RealSort]], toZ3(r, env).asInstanceOf[ArithExpr[RealSort]])
    case BinOp("/", l, r) => solver.add(ctx.mkNot(ctx.mkEq(toZ3(r, env), ctx.mkReal(0)))); ctx.mkDiv(toZ3(l, env).asInstanceOf[ArithExpr[RealSort]], toZ3(r, env).asInstanceOf[ArithExpr[RealSort]])
    case BinOp(op, l, r) => ctx.mkFuncDecl(op, Array[Sort](ctx.getRealSort, ctx.getRealSort), ctx.getRealSort).apply(toZ3(l, env), toZ3(r, env))
    case Lambda(p, b) =>
      val f = ctx.mkFuncDecl(s"f${Math.abs(e.hashCode)}", Array[Sort](ctx.getRealSort), ctx.getRealSort)
      val x = ctx.mkBound(0, ctx.getRealSort)
      solver.add(ctx.mkForall(Array[Sort](ctx.getRealSort), Array(ctx.mkSymbol(p)), ctx.mkEq(f.apply(x), toZ3(b, env + (p -> x))), 0, null, null, null, null))
      funcs += (f.getName.toString -> f); ctx.mkReal(0)
    case Apply(Var(n), a) => funcs.getOrElse(n, ctx.mkFuncDecl(n, a.map(_ => ctx.getRealSort: Sort).toArray, ctx.getRealSort)).apply(a.map(toZ3(_, env))*)
    case Ambiguous(o) => val t = ctx.mkFreshConst("a", ctx.getRealSort); solver.add(ctx.mkOr(o.map(opt => ctx.mkEq(t, toZ3(opt, env)))*)); t
    case _ => ctx.mkReal(0)
  }

  def add(s: Stmt): Unit = s match {
    case Constraint(Var(n), Lambda(p, b)) =>
      val f = ctx.mkFuncDecl(n, Array[Sort](ctx.getRealSort), ctx.getRealSort)
      val x = ctx.mkBound(0, ctx.getRealSort)
      solver.add(ctx.mkForall(Array[Sort](ctx.getRealSort), Array(ctx.mkSymbol(p)), ctx.mkEq(f.apply(x), toZ3(b, Map(p -> x))), 0, null, null, null, null))
      funcs += (n -> f)
    case Constraint(l, r) => solver.add(ctx.mkEq(toZ3(l), toZ3(r)))
    case Block(ss) => ss.foreach(add)
    case Branch(o) => solver.add(ctx.mkOr(o.map(st => ctx.mkAnd(st match { case Block(ss) => ss.map(convert).toArray case _ => Array(convert(st)) }*)) *))
    case _ =>
  }

  private def convert(s: Stmt): BoolExpr = s match {
    case Constraint(l, r) => ctx.mkEq(toZ3(l), toZ3(r))
    case Block(ss) => ctx.mkAnd(ss.map(convert)*)
    case Branch(o) => ctx.mkOr(o.map(convert)*)
    case _ => ctx.mkTrue()
  }

  def solveAndPrint(): Unit = {
    val status = solver.check()
    println(s"Status: $status")
    if (status == Status.SATISFIABLE) {
      val m = solver.getModel
      vars.keys.toSeq.sorted.foreach(n => println(s"$n = ${m.evaluate(vars(n), true)}"))
      funcs.keys.toSeq.sorted.foreach(n => { val i = m.getFuncInterp(funcs(n)); if (i != null) println(s"fn $n = $i") })
    }
  }

  override def close(): Unit = ctx.close()
}
