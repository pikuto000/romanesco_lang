package romanesco
import com.microsoft.z3._

object Reporter {
  private val BOLD = "\u001b[1m"; val GREEN = "\u001b[32m"; val RED = "\u001b[31m"; val CYAN = "\u001b[36m"; val RESET = "\u001b[0m"

  def header(t: String) = println(s"\n$BOLD$CYAN=== $t ===$RESET")
  def info(m: String) = println(s"$GREEN[INFO]$RESET $m")
  def error(m: String) = println(s"$RED[ERROR]$RESET $m")

  def reportSolver(status: Status, vars: Map[String, Expr[RealSort]], funcs: Map[String, FuncDecl[RealSort]], model: Model) = {
    header("Deducted Model")
    println(s"Status: $BOLD${if (status == Status.SATISFIABLE) GREEN else RED}$status$RESET")
    if (status == Status.SATISFIABLE) {
      println(s"\n$BOLD[Variables]$RESET")
      vars.keys.toSeq.sorted.foreach(n => println(f"  $n%-10s = ${model.evaluate(vars(n), true)}"))
      val fs = funcs.keys.toSeq.sorted.flatMap(n => Option(model.getFuncInterp(funcs(n))).map(i => (n, i)))
      if (fs.nonEmpty) { println(s"\n$BOLD[Functions]$RESET"); fs.foreach((n, i) => println(s"  fn $n = $i")) }
    } else error("Logical inconsistency detected.")
    println(s"$CYAN==================$RESET")
  }
}