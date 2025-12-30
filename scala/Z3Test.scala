/*package parser
import com.microsoft.z3._

object Z3Test {
  def main(args: Array[String]): Unit = {
    println("Initializing Z3...")
    val ctx = new Context()
    val solver = ctx.mkSolver()

    // x + 5 = 10 を解く
    val x = ctx.mkIntConst("x")
    val five = ctx.mkInt(5)
    val ten = ctx.mkInt(10)

    val constraint = ctx.mkEq(ctx.mkAdd(x, five), ten)
    solver.add(constraint)

    println("Checking satisfaction...")
    if (solver.check() == Status.SATISFIABLE) {
      val model = solver.getModel()
      println(s"Result: SAT")
      println(s"x = ${model.evaluate(x, false)}")
    } else {
      println("Result: UNSAT")
    }
    
    ctx.close()
    println("Z3 test finished.")
  }
}
*/