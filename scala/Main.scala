package romanesco
import scala.util.Using

object Main {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: sbt \"run <file>\"")
      sys.exit(1)
    }

    MacroExpander.reset()
    val source = scala.io.Source.fromFile(args(0)).mkString
    println(s"Source:\n$source\n")

    Lexer.lex(source) match {
      case Left(msg) => println(msg); sys.exit(1)
      case Right(allTokenPaths) =>
        val initialTokens = allTokenPaths.head

        // 1. Discovery Pass
        SimpleParser.parse(initialTokens) match {
          case SimpleParser.Success(rawStmts, _) =>
            println("Pass 1: Discovery Successful.")
            MacroExpander.expandAll(rawStmts) 

            // 2. Metamorphosis
            val transformedTokens = MacroExpander.transformTokens(initialTokens)
            println(s"Metamorphosis Complete. Tokens: ${transformedTokens.size}")
            println(s"--- Transformed Tokens ---\n${transformedTokens.mkString(", ")}\n--------------------------\n")

            // 3. Execution Pass
            SimpleParser.parse(transformedTokens) match {
              case SimpleParser.Success(finalRawStmts, _) =>
                println("Pass 2: Execution Parse Successful.")
                
                val finalStmts = MacroExpander.expandAll(finalRawStmts)
                Using.resource(new Solver()) { solver =>
                  finalStmts.foreach(solver.add)
                  solver.solveAndPrint()
                }

              case f: SimpleParser.NoSuccess =>
                println(s"Pass 2 Failed: ${f.msg}")
            }

          case f: SimpleParser.NoSuccess =>
            println(s"Pass 1 Failed: ${f.msg}")
        }
    }
  }
}
