package romanesco

import com.microsoft.z3._
import scala.collection.mutable

class Solver extends AutoCloseable {
  import AstExpr._
  import Stmt._

  private val ctx = new Context()
  private val solver = ctx.mkSolver()
  private val vars = mutable.Map[String, com.microsoft.z3.Expr[RealSort]]()

  private def getVar(name: String): com.microsoft.z3.Expr[RealSort] = {
    vars.getOrElseUpdate(name, ctx.mkRealConst(name))
  }

  private def toZ3(expr: AstExpr): ArithExpr[RealSort] = expr match {
    case Num(v) => ctx.mkReal(v.toString).asInstanceOf[ArithExpr[RealSort]]
    case Var(n) => getVar(n).asInstanceOf[ArithExpr[RealSort]]
    case BinOp(op, l, r) =>
      op match {
        case "=" =>
          val left = toZ3(l); val right = toZ3(r)
          solver.add(ctx.mkEq(left, right))
          right
        case "<<" => ctx.mkMul(toZ3(l), ctx.mkPower(ctx.mkReal(2), toZ3(r)))
        case ">>" => ctx.mkDiv(toZ3(l), ctx.mkPower(ctx.mkReal(2), toZ3(r)))
        case _ =>
          val left = toZ3(l); val right = toZ3(r)
          op match {
            case "+" => ctx.mkAdd(left, right)
            case "-" => ctx.mkSub(left, right)
            case "*" => ctx.mkMul(left, right)
            case "/" => 
              solver.add(ctx.mkNot(ctx.mkEq(right, ctx.mkReal(0))))
              ctx.mkDiv(left, right)
            case _ => 
              val domain = Array[Sort](ctx.getRealSort, ctx.getRealSort)
              val func = ctx.mkFuncDecl(op, domain, ctx.getRealSort)
              func.apply(left, right).asInstanceOf[ArithExpr[RealSort]]
          }
      }
    case Templated(name, _) =>
      ctx.mkReal(Math.abs(name.hashCode % 1000)).asInstanceOf[ArithExpr[RealSort]]
    case Ambiguous(options) =>
      val tmp = ctx.mkFreshConst("ambig", ctx.getRealSort).asInstanceOf[ArithExpr[RealSort]]
      val constraints = options.map(opt => ctx.mkEq(tmp, toZ3(opt)))
      solver.add(ctx.mkOr(constraints*))
      tmp
    case Raw(_) =>
      ctx.mkReal(0).asInstanceOf[ArithExpr[RealSort]]
    case MacroCall(name, _) =>
      throw new RuntimeException(s"Unexpanded macro call in solver: $name")
  }

  def add(stmt: Stmt): Unit = stmt match {
    case Constraint(l, r) => solver.add(ctx.mkEq(toZ3(l), toZ3(r)))
    case Block(stmts) => stmts.foreach(add)
    case Branch(options) => solver.add(ctx.mkOr(options.map(convertStmtToBoolExpr)*))
    case MacroDef(_, _, _) => 
  }

  private def convertStmtToBoolExpr(stmt: Stmt): BoolExpr = stmt match {
    case Constraint(l, r) => ctx.mkEq(toZ3(l), toZ3(r))
    case Block(stmts) => ctx.mkAnd(stmts.map(convertStmtToBoolExpr)*)
    case Branch(options) => ctx.mkOr(options.map(convertStmtToBoolExpr)*)
    case _ => ctx.mkTrue()
  }

  def solveAndPrint(): Unit = {
    println(s"Constraints added: ${solver.getNumAssertions}")
    val status = solver.check()
    println(s"Status: $status")
    if (status == Status.SATISFIABLE) {
      val model = solver.getModel
      println("--- Model ---")
      vars.keys.toSeq.sorted.foreach { name =>
        val v = vars(name)
        println(s"$name = ${model.evaluate(v, true)}")
      }
    } else {
      println("Unsatisfiable!")
    }
  }

  override def close(): Unit = ctx.close()
}