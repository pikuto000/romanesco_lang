package romanesco
import java.nio.file.{Files, Paths}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: romanesco <input_file> [debug]")
      return
    }

    val sourcePath = args(0)
    val debug = args.length > 1 && args(1) == "debug"
    
    logger.Switch(debug)

    val source = new String(Files.readAllBytes(Paths.get(sourcePath)))
    val lexer = init.lex.setup("firstLexer")
    val solver = new Solver() // 先に Solver を作る
    val parser = init.parse.setup("firstParser", lexer, solver) // Parser に Solver を渡す

    init.parse.literals(parser)
    init.parse.logic(parser)

    val startTime = System.currentTimeMillis()
    val tokenLattice = lexer.tokenize(source).asInstanceOf[Map[Int, Array[parser.lexer.Token]]]
    
    parser.apply(tokenLattice, source) match {
      case parser.Success(results, _) =>
        val parseEndTime = System.currentTimeMillis()
        println(s"parse took ${parseEndTime - startTime} ms")

        val optimizedNodes = init.optimization.apply(results)
        
        println("--- Optimized AST ---")
        optimizedNodes.zipWithIndex.foreach { case (node, idx) => println(s"[$idx] $node") }
        println("---------------------")

        val solveStartTime = System.currentTimeMillis()
        init.semantics.execute(optimizedNodes, solver)
        val solveEndTime = System.currentTimeMillis()
        
        println(s"solve took ${solveEndTime - solveStartTime} ms")
        println(s"totally operation took ${System.currentTimeMillis() - startTime} ms")

      case parser.Failure(msg, next) =>
        println(s"Parse Failed: $msg at ${next.pos}")
      case parser.Error(msg, next) =>
        println(s"Parse Error: $msg at ${next.pos}")
    }
  }
}
