package romanesco
import com.microsoft.z3._
import scala.collection.mutable.{Map => MutableMap}

class Solver(val ctx: Context) {
  def this() = this(new Context())
  private val solver = ctx.mkSolver()
  private val varMap = MutableMap[String, Expr[?]]()

  def getOrCreateReal(tag: HygenicTag): ArithExpr[?] = {
    varMap.getOrElseUpdate(tag.mangledName, ctx.mkRealConst(tag.mangledName)).asInstanceOf[ArithExpr[?]]
  }

  def addEq(left: Expr[?], right: Expr[?]): Unit = {
    solver.add(ctx.mkEq(left.asInstanceOf[Expr[Sort]], right.asInstanceOf[Expr[Sort]]))
  }

  def solve(node: Node): Unit = {
    node.kind match {
      case "Unification" =>
        val lExpr = exprOf(node.children(0))
        val rExpr = exprOf(node.children(1))
        addEq(lExpr, rExpr)
        logger.log(s"[solver] Added constraint: $lExpr == $rExpr")
      
      case "Block" =>
        node.children.foreach(solve)
      
      case _ => // 無視
    }
  }

  private def exprOf(node: Node): Expr[?] = node.kind match {
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
        case _ => throw new Exception(s"Unsupported operator: $op")
      }
    case _ => throw new Exception(s"Unsupported AST node kind for solver: ${node.kind}")
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
    }
  }
}