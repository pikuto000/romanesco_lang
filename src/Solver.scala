package romanesco
import com.microsoft.z3._
import scala.collection.mutable.{Map => MutableMap}

class Solver(val ctx: Context) extends AutoCloseable {
  def this() = this(new Context())
  
  private val solver = ctx.mkSolver()
  private val varMap = MutableMap[String, Expr[?]]()
  private val exprCache = MutableMap[Node, Expr[?]]()

  def getOrCreateReal(tag: HygenicTag): ArithExpr[?] = {
    varMap.getOrElseUpdate(
      tag.mangledName, 
      ctx.mkRealConst(tag.mangledName)
    ).asInstanceOf[ArithExpr[?]]
  }
  
  // パーサーで使用するメソッドを追加
  def push(): Unit = solver.push()
  def pop(): Unit = solver.pop()
  
  def checkFeasibility(node: Node): Boolean = {
    push()
    try {
      solve(node)
      val status = solver.check()
      status == Status.SATISFIABLE || status == Status.UNKNOWN
    } finally {
      pop()
    }
  }

  def solve(node: Node): Unit = {
    node.kind match {
      case "Unification" => {
        val lExpr = exprOf(node.children(0))
        val rExpr = exprOf(node.children(1))
        solver.add(ctx.mkEq(
          lExpr.asInstanceOf[Expr[Sort]], 
          rExpr.asInstanceOf[Expr[Sort]]
        ))
        logger.debug(s"Added Eq constraint: $lExpr == $rExpr")
      }
      
      case "BinaryOp" => {
        val op = node.requireOp("op")
        if (List(">", "<", ">=", "<=", "==").contains(op)) {
          val l = exprOf(node.children(0))
          val r = exprOf(node.children(1))
          val constr = op match {
            case ">" => ctx.mkGt(
              l.asInstanceOf[ArithExpr[?]], 
              r.asInstanceOf[ArithExpr[?]]
            )
            case "<" => ctx.mkLt(
              l.asInstanceOf[ArithExpr[?]], 
              r.asInstanceOf[ArithExpr[?]]
            )
            case ">=" => ctx.mkGe(
              l.asInstanceOf[ArithExpr[?]], 
              r.asInstanceOf[ArithExpr[?]]
            )
            case "<=" => ctx.mkLe(
              l.asInstanceOf[ArithExpr[?]], 
              r.asInstanceOf[ArithExpr[?]]
            )
            case "==" => ctx.mkEq(
              l.asInstanceOf[Expr[Sort]], 
              r.asInstanceOf[Expr[Sort]]
            )
          }
          solver.add(constr)
          logger.debug(s"Added logical constraint: $l $op $r")
        }
      }

      case "Program" | "Block" => {
        node.children.foreach(solve)
      }
      
      case _ => {
        // 他のノードは無視
      }
    }
  }

  private def exprOf(node: Node): Expr[?] = {
    exprCache.getOrElseUpdate(node, {
      node.kind match {
        case "Variable" => getOrCreateReal(node.tag)
        
        case "DecimalLiteral" => {
          val v = node.requireNum("value")
          ctx.mkReal(v.toString)
        }
        
        case "BinaryOp" => {
          val op = node.requireOp("op")
          val l = exprOf(node.children(0)).asInstanceOf[ArithExpr[?]]
          val r = exprOf(node.children(1)).asInstanceOf[ArithExpr[?]]
          op match {
            case "+" => ctx.mkAdd(l, r)
            case "-" => ctx.mkSub(l, r)
            case "*" => ctx.mkMul(l, r)
            case _ => throw new Exception(
              s"Unsupported arithmetic/logical operator in expr: $op"
            )
          }
        }
        
        case _ => throw new Exception(
          s"Unsupported node kind for symbolic expr: ${node.kind}"
        )
      }
    })
  }

  def check(): Unit = {
    logger.info("Checking satisfiability...")
    val status = solver.check()
    println(s"Solver Status: $status")
    
    status match {
      case Status.SATISFIABLE => {
        val model = solver.getModel
        println("--- Deduced Model ---")
        
        // Z3変数の値を表示
        varMap.keys.toSeq.sorted.foreach { name =>
          val res = model.evaluate(varMap(name), true)
          println(s"$name = $res")
        }
        
        // 定数値も表示（exprCacheから取得）
        val constants = exprCache.collect {
          case (node, expr) if node.kind == "DecimalLiteral" =>
            (node.tag.mangledName, node.requireNum("value"))
        }.toSeq.sortBy(_._1)
        
        constants.foreach { case (name, value) =>
          println(s"$name = $value (constant)")
        }
        
        println("----------------------")
      }
      
      case Status.UNSATISFIABLE => {
        println("Error: Constraints are inconsistent (UNSAT).")
      }
      
      case _ => {
        println(s"Solver returned: $status")
      }
    }
  }
  
  def close(): Unit = {
    exprCache.clear()
    varMap.clear()
    ctx.close()
  }
}
