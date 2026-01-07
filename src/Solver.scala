package romanesco
import com.microsoft.z3._

class Solver {
  private val ctx = new Context()
  private val solver = ctx.mkSolver()
  private val varMap = scala.collection.mutable.Map[String, RealExpr]()

  private def getVar(name: String): RealExpr = {
    varMap.getOrElseUpdate(name, ctx.mkRealConst(name))
  }

  def solve(node: Node): Unit = {
    val expr = buildExpr(node)
    if (expr != null && expr.isInstanceOf[BoolExpr]) {
      solver.add(expr.asInstanceOf[BoolExpr])
    }
  }

  def checkFeasibility(node: Node): Boolean = synchronized {
    solver.push()
    try {
      solve(node)
      val status = solver.check()
      status == Status.SATISFIABLE || status == Status.UNKNOWN
    } catch {
      case _: Exception => true 
    } finally {
      solver.pop()
    }
  }

  def check(): Unit = {
    val status = solver.check()
    println(s"Solver Status: $status")
    if (status == Status.SATISFIABLE) {
      val model = solver.getModel
      println("--- Deduced Model ---")
      varMap.toSeq.sortBy(_._1).foreach { case (name, expr) =>
        val value = model.evaluate(expr, true)
        println(s"$name = $value")
      }
      println("----------------------")
    }
  }

  private def buildExpr(node: Node): Expr[?] = {
    // タグが不透明な場合は論理式を生成しない（壁）
    if (node.isOpaque) return null

    node.kind match {
      case "DecimalLiteral" =>
        ctx.mkReal(node.attributes("value").asInstanceOf[BigDecimal].toString)
      
      case "Variable" =>
        getVar(node.tag.mangledName)

      case "Unification" if node.children.length == 2 =>
        val left = buildExpr(node.children(0))
        val right = buildExpr(node.children(1))
        if (left == null || right == null) null
        else (left, right) match {
          case (l: ArithExpr[?], r: ArithExpr[?]) => ctx.mkEq(l, r)
          case (l: BoolExpr, r: BoolExpr) => ctx.mkEq(l, r)
          case _ => null
        }

      case "BinaryOp" if node.children.length == 2 =>
        val left = buildExpr(node.children(0))
        val right = buildExpr(node.children(1))
        if (left == null || right == null) null
        else {
          val l = left.asInstanceOf[ArithExpr[RealSort]]
          val r = right.asInstanceOf[ArithExpr[RealSort]]
          node.attributes("op") match {
            case "+" => ctx.mkAdd(l, r)
            case "-" => ctx.mkSub(l, r)
            case "*" => ctx.mkMul(l, r)
            case "==" => ctx.mkEq(l, r)
            case ">=" => ctx.mkGe(l, r)
            case "<=" => ctx.mkLe(l, r)
            case ">"  => ctx.mkGt(l, r)
            case "<"  => ctx.mkLt(l, r)
            case _ => null
          }
        }
      
      case "Block" | "Program" =>
        val exprs = node.children.map(buildExpr).collect { case b: BoolExpr => b }
        if (exprs.nonEmpty) ctx.mkAnd(exprs*) else null

      case _ => null
    }
  }
}