package romanesco
import com.microsoft.z3._
import scala.collection.mutable.{Map => MutableMap}

class Solver(val ctx: Context) {
  def this() = this(new Context())
  private val solver = ctx.mkSolver()
  
  // Z3オブジェクトのキャッシュ
  private val varMap = MutableMap[String, Expr[?]]()
  private val exprCache = MutableMap[Node, Expr[?]]()

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
      
      case "BinaryOp" =>
        val op = node.attributes("op").asInstanceOf[String]
        if (List(">", "<", ">=", "<=").contains(op)) {
          val l = exprOf(node.children(0)).asInstanceOf[ArithExpr[?]]
          val r = exprOf(node.children(1)).asInstanceOf[ArithExpr[?]]
          val constr = op match {
            case ">" => ctx.mkGt(l, r)
            case "<" => ctx.mkLt(l, r)
            case ">=" => ctx.mkGe(l, r)
            case "<=" => ctx.mkLe(l, r)
          }
          solver.add(constr)
          logger.log(s"[solver] Added constraint: $l $op $r")
        } else {
          // トップレベルの算術式は制約にならないので無視（または警告）
        }

      case "Block" =>
        node.children.foreach(solve)
      
      case _ => // 無視
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
            case _ => throw new Exception(s"Unsupported operator in expression: $op")
          }
        case _ => throw new Exception(s"Unsupported AST node kind for solver: ${node.kind}")
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
    }
  }
}