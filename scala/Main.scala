package romanesco
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) return Reporter.error("Usage: run <file>")
    MacroExpander.reset()
    val src = scala.io.Source.fromFile(args(0)).mkString
    Lexer.lex(src) match {
      case Right(allPaths) =>
        allPaths.foreach { ts =>
          SimpleParser.parse(ts) match {
            case SimpleParser.Success(ss, _) => MacroExpander.registerAll(ss); case _ =>
          }
        }
        val results = allPaths.flatMap { ts =>
          val ts2 = MacroExpander.transform(ts)
          SimpleParser.parse(ts2) match {
            case SimpleParser.Success(ss, next) if next.atEnd => Some(ss); case _ => None
          }
        }.distinct
        if (results.isEmpty) return Reporter.error("All interpretations failed.")
        Using.resource(new romanesco.Solver) { s =>
          if (results.size == 1) results.head.foreach(s.add)
          else s.add(Stmt.Branch(results.map(Stmt.Block.apply)))
          Reporter.reportSolver(s.check(), s)
        }
      case Left(e) => Reporter.error(e)
    }
  }
}