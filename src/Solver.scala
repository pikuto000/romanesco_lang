package romanesco
import com.microsoft.z3._
import scala.collection.mutable

class Solver extends AutoCloseable {
  import AstExpr._, Stmt._
  private val ctx = new Context; private val s = ctx.mkSolver()
  private val vs = mutable.Map[String, Expr[RealSort]](); private val fs = mutable.Map[String, FuncDecl[RealSort]]()

  private def v(n: String) = vs.getOrElseUpdate(n, ctx.mkRealConst(n))
  private def b(e: AstExpr, env: Map[String, Expr[RealSort]]): BoolExpr = e match {
    case BinOp("==", l, r) => ctx.mkEq(toZ(l, env), toZ(r, env))
    case BinOp("<", l, r) => ctx.mkLt(toZ(l, env).asInstanceOf[ArithExpr[?]], toZ(r, env).asInstanceOf[ArithExpr[?]])
    case BinOp(">", l, r) => ctx.mkGt(toZ(l, env).asInstanceOf[ArithExpr[?]], toZ(r, env).asInstanceOf[ArithExpr[?]])
    case _ => ctx.mkNot(ctx.mkEq(toZ(e, env), ctx.mkReal(0)))
  }

  private def toZ(e: AstExpr, env: Map[String, Expr[RealSort]] = Map.empty): Expr[RealSort] = e match {
    case Num(n) => ctx.mkReal(n.toString).asInstanceOf[Expr[RealSort]]
    case Var(n) => env.getOrElse(n, v(n))
    case BinOp("=", l, r) => val eq = ctx.mkEq(toZ(l, env), toZ(r, env)); s.add(eq); toZ(r, env)
    case BinOp(o, l, r) => 
      val (lx, rx) = (toZ(l, env).asInstanceOf[ArithExpr[RealSort]], toZ(r, env).asInstanceOf[ArithExpr[RealSort]])
      o match {
        case "+" => ctx.mkAdd(lx, rx); case "-" => ctx.mkSub(lx, rx); case "*" => ctx.mkMul(lx, rx)
        case "/" => s.add(ctx.mkNot(ctx.mkEq(rx, ctx.mkReal(0)))); ctx.mkDiv(lx, rx)
        case _ => ctx.mkFuncDecl(o, Array[Sort](ctx.getRealSort, ctx.getRealSort), ctx.getRealSort).apply(lx, rx).asInstanceOf[Expr[RealSort]]
      }
    case If(c, t, el) => ctx.mkITE(b(c, env), toZ(t, env), toZ(el, env)).asInstanceOf[Expr[RealSort]]
    case Lambda(p, bd) =>
      val f = ctx.mkFuncDecl(s"f${Math.abs(e.hashCode)}", Array[Sort](ctx.getRealSort), ctx.getRealSort)
      val x = ctx.mkBound(0, ctx.getRealSort)
      s.add(ctx.mkForall(Array[Sort](ctx.getRealSort), Array(ctx.mkSymbol(p)), ctx.mkEq(f.apply(x), toZ(bd, env + (p -> x))), 0, null, null, null, null))
      fs += (f.getName.toString -> f); ctx.mkReal(0)
    case Apply(Var(n), a) => fs.getOrElse(n, ctx.mkFuncDecl(n, a.map(_ => ctx.getRealSort: Sort).toArray, ctx.getRealSort)).apply(a.map(toZ(_, env))*)
    case Ambiguous(o) => val t = ctx.mkFreshConst("a", ctx.getRealSort); s.add(ctx.mkOr(o.map(opt => ctx.mkEq(t, toZ(opt, env)))*)); t
    case _ => ctx.mkReal(0)
  }

  def add(st: Stmt): Unit = st match {
    case Constraint(Var(n), Lambda(p, bd)) =>
      val f = ctx.mkFuncDecl(n, Array[Sort](ctx.getRealSort), ctx.getRealSort)
      val x = ctx.mkBound(0, ctx.getRealSort)
      s.add(ctx.mkForall(Array[Sort](ctx.getRealSort), Array(ctx.mkSymbol(p)), ctx.mkEq(f.apply(x), toZ(bd, Map(p -> x))), 0, null, null, null, null))
      fs += (n -> f)
    case Constraint(l, r) => s.add(ctx.mkEq(toZ(l), toZ(r)))
    case Block(ss) => ss.foreach(add)
    case Branch(o) => s.add(ctx.mkOr(o.map(convert)*))
    case _ =>
  }

  private def convert(st: Stmt): BoolExpr = st match {
    case Constraint(l, r) => ctx.mkEq(toZ(l), toZ(r))
    case Block(ss) => ctx.mkAnd(ss.map(convert)*); case Branch(o) => ctx.mkOr(o.map(convert)*); case _ => ctx.mkTrue()
  }

  def solve(): Unit = if (s.check() == Status.SATISFIABLE) {
    val m = s.getModel; vs.keys.toSeq.sorted.foreach(n => println(s"$n = ${m.evaluate(vs(n), true)}"))
    fs.keys.toSeq.sorted.foreach(n => { val i = m.getFuncInterp(fs(n)); if (i != null) println(s"fn $n = $i") })
  }
  def close() = ctx.close()
}
