package romanesco
import com.microsoft.z3._

object Reporter {
  private val B = "\u001b[1m"; val G = "\u001b[32m"; val R = "\u001b[31m"; val C = "\u001b[36m"; val Y = "\u001b[33m"; val RS = "\u001b[0m"
  def header(t: String) = println(s"\n$B$C=== $t ===$RS")
  def error(m: Any) = println(s"$R[ERROR]$RS $m")
  def reportSolver(st: Status, s: romanesco.Solver) = {
    header("Deducted Model"); println(s"Status: $B${if (st == Status.SATISFIABLE) G else R}$st$RS")
    if (st == Status.SATISFIABLE) {
      val m = s.model; s.vs.keys.toSeq.sorted.foreach(n => println(f"  $n%-10s = ${m.evaluate(s.vs(n), true)}"))
      s.fs.keys.toSeq.sorted.flatMap(n => Option(m.getFuncInterp(s.fs(n))).map(i => (n, i))).foreach((n, i) => println(s"  fn $n = $i"))
    } else if (st == Status.UNSATISFIABLE) {
      println(s"\n$B$R[Conflict!]$RS\n$Y Statements conflicting:$RS"); s.getCore().foreach(c => println(s"  ${R}X$RS $c"))
    }
    println(s"$C==================$RS")
  }
}
