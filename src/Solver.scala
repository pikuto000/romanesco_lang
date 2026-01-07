package romanesco
import com.microsoft.z3._
import scala.collection.mutable.{Map => MutableMap}

class Solver(val ctx: Context) {
  def this() = this(new Context())
  private val solver = ctx.mkSolver()
  
  private val varMap = MutableMap[String, Expr[?]]()
  private val exprCache = MutableMap[Node, Expr[?]]()

  def getOrCreateReal(tag: HygenicTag): ArithExpr[?] = {
    varMap.getOrElseUpdate(tag.mangledName, ctx.mkRealConst(tag.mangledName)).asInstanceOf[ArithExpr[?]]
  }

  def solve(node: Node): Unit = {
    node.kind match {
      case "Unification" =>
        val lExpr = exprOf(node.children(0))
        val rExpr = exprOf(node.children(1))
        solver.add(ctx.mkEq(lExpr.asInstanceOf[Expr[Sort]], rExpr.asInstanceOf[Expr[Sort]]))
        logger.log(s"[solver] Added Eq constraint: $lExpr == $rExpr")
      
      case "BinaryOp" =>
        val op = node.attributes("op").asInstanceOf[String]
        if (List(">", "<", ">=", "<=", "==").contains(op)) {
          val l = exprOf(node.children(0))
          val r = exprOf(node.children(1))
          val constr = op match {
            case ">"  => ctx.mkGt(l.asInstanceOf[ArithExpr[?]], r.asInstanceOf[ArithExpr[?]])
            case "<"  => ctx.mkLt(l.asInstanceOf[ArithExpr[?]], r.asInstanceOf[ArithExpr[?]])
            case ">=" => ctx.mkGe(l.asInstanceOf[ArithExpr[?]], r.asInstanceOf[ArithExpr[?]])
            case "<=" => ctx.mkLe(l.asInstanceOf[ArithExpr[?]], r.asInstanceOf[ArithExpr[?]])
            case "==" => ctx.mkEq(l.asInstanceOf[Expr[Sort]], r.asInstanceOf[Expr[Sort]])
          }
          solver.add(constr)
          logger.log(s"[solver] Added logical constraint: $l $op $r")
        }

      case "Program" | "Block" =>
        node.children.foreach(solve)
      
      case _ => 
    }
  }

  private def exprOf(node: Node): Expr[?] = {
    exprCache.getOrElseUpdate(node, {
      node.kind match {
        case "Variable" => getOrCreateReal(node.tag)
        case "DecimalLiteral" => 
          val v = node.attributes("value").asInstanceOf[BigDecimal]
          ctx.mkReal(v.toString)
        case "BinaryOp" =>
          val op = node.attributes("op").asInstanceOf[String]
          val l = exprOf(node.children(0)).asInstanceOf[ArithExpr[?]]
          val r = exprOf(node.children(1)).asInstanceOf[ArithExpr[?]]
          op match {
            case "+" => ctx.mkAdd(l, r)
            case "-" => ctx.mkSub(l, r)
            case "*" => ctx.mkMul(l, r)
            case _   => throw new Exception(s"Unsupported arithmetic/logical operator in expr: $op")
          }
        case _ => throw new Exception(s"Unsupported node kind for symbolic expr: ${node.kind}")
      }
    })
  }

  def check(): Unit = {
    logger.log("[solver] Checking satisfiability...")
    val status = solver.check()
    println(s"Solver Status: $status")
    if (status == Status.SATISFIABLE) {
      val model = solver.getModel
      println("--- Deduced Model ---")
      varMap.keys.toSeq.sorted.foreach { name =>
        val res = model.evaluate(varMap(name), true)
        println(s"$name = $res")
      }
      println("----------------------")
    } else if (status == Status.UNSATISFIABLE) {
      println("Error: Constraints are inconsistent (UNSAT).")
    } else {
      println(s"Solver returned: $status")
    }
  }
}