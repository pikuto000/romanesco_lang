package romanesco
import com.microsoft.z3._
import scala.collection.mutable.{Map => MutableMap}
import romanesco.{AST => RAST}

class Solver {
  private val ctx = new Context()
  private val solver = ctx.mkSolver()
  
  // 変数名（Mangled Name）と Z3 定数のマッピング
  private val varMap = MutableMap[String, Expr[?]]()

  // 実数変数の取得または作成
  def getOrCreateReal(tag: HygenicTag): ArithExpr[?] = {
    varMap.getOrElseUpdate(tag.mangledName, ctx.mkRealConst(tag.mangledName)).asInstanceOf[ArithExpr[?]]
  }

  // 制約の追加 (等式)
  def addEq(left: Expr[?], right: Expr[?]): Unit = {
    solver.add(ctx.mkEq(left.asInstanceOf[Expr[Sort]], right.asInstanceOf[Expr[Sort]]))
  }

  // AST を Z3 の式に変換して制約を追加
  def solve(ast: RAST): Unit = {
    ast match {
      case RAST.Unification(left, right) =>
        val lExpr = exprOf(left)
        val rExpr = exprOf(right)
        addEq(lExpr, rExpr)
        logger.log(s"[solver] Added constraint: $lExpr == $rExpr")
      
      case RAST.Block(stmts) =>
        stmts.foreach(solve)
      
      case _ => // その他は現状無視
    }
  }

  private def exprOf(node: RAST): Expr[?] = node match {
    case RAST.Variable(tag) => getOrCreateReal(tag)
    case RAST.DecimalLiteral(v) => ctx.mkReal(v.toString)
    case RAST.BinaryOp(op, left, right) =>
      val l = exprOf(left).asInstanceOf[ArithExpr[?]]
      val r = exprOf(right).asInstanceOf[ArithExpr[?]]
      op match {
        case "+" => ctx.mkAdd(l, r)
        case "-" => ctx.mkSub(l, r)
        case "*" => ctx.mkMul(l, r)
        case _ => throw new Exception(s"Unsupported operator: $op")
      }
    case _ => throw new Exception(s"Unsupported AST node for solver: $node")
  }

  // 解決と結果の表示
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
