package romanesco
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) return println("Usage: run <file>")
    MacroExpander.reset()
    val src = scala.io.Source.fromFile(args(0)).mkString
    Lexer.lex(src) match {
      case Right(allPaths) =>
        val ts = allPaths.head // デモ用
        SimpleParser.parse(ts) match {
          case SimpleParser.Success(ss, _) =>
            MacroExpander.registerAll(ss)
            val ts2 = MacroExpander.transform(ts)
            println(s"Transformed Tokens: ${ts2.mkString(", ")}")
            SimpleParser.parse(ts2) match {
              case SimpleParser.Success(ss2, _) =>
                println(s"Pass 2 SUCCESS. AST: $ss2")
                Using.resource(new Solver) { s => ss2.foreach(s.add); s.solve() }
              case f => println(s"Pass 2 Fail: $f")
            }
          case f => println(s"Pass 1 Fail: $f")
        }
      case Left(e) => println(e)
    }
  }
}