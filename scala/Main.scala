package romanesco
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) return Reporter.error("Usage: run <file>")
    MacroExpander.reset()
    val src = scala.io.Source.fromFile(args(0)).mkString
    Lexer.lex(src).map(_.head) match {
      case Right(ts) =>
        SimpleParser.parse(ts) match {
          case SimpleParser.Success(ss, _) =>
            MacroExpander.registerAll(ss)
            val ts2 = MacroExpander.transform(ts)
            SimpleParser.parse(ts2) match {
              case SimpleParser.Success(ss2, next) if next.atEnd =>
                Using.resource(new Solver) { s => 
                  ss2.foreach(s.add); val st = s.check()
                  Reporter.reportSolver(st, s.vs.toMap, s.fs.toMap, if (st == com.microsoft.z3.Status.SATISFIABLE) s.model else null)
                }
              case f => Reporter.error(s"Fail: $f")
            }
          case f => Reporter.error(s"Fail: $f")
        }
      case Left(e) => Reporter.error(e)
    }
  }
}