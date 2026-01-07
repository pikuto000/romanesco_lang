package romanesco
import java.time.Instant
import java.time.Duration

object Main {
  def main(args: Array[String]): Unit = {
    val filename = if (args.length >= 1) args(0) else scala.io.StdIn.readLine("input file:")
    val debugflag = if (args.length == 2) args(1) == "debug" else false
    if (debugflag) logger.Switch(true)
    
    val source = scala.io.Source.fromFile(filename)
    val sourceStr = source.mkString
    
    val lexer = init.lex.setup("firstLexer")
    init.lex.keywords(lexer)
    init.lex.literals(lexer)
    init.lex.operators(lexer)

    val parser = init.parse.setup("firstParser", lexer)
    init.parse.literals(parser)
    init.parse.logic(parser)

    val start = Instant.now()
    val lattice = lexer.tokenize(sourceStr)
    if (debugflag) lexer.printStream
    
    val parseResult = parser(lattice.asInstanceOf[Map[Int, Array[parser.lexer.Token]]], sourceStr)
    val parseEnd = Instant.now()
    println(s"parse took ${Duration.between(start, parseEnd).toMillis} ms")
    
    if (debugflag) parser.printStream
    
    if (parseResult.successful) {
      val resRaw = parseResult.get
      val res = init.optimization.apply(resRaw)
      
      if (debugflag) {
        println("--- Optimized AST ---")
        res.zipWithIndex.foreach { case (r, i) => println(s"[$i] $r") }
        println("---------------------")
      }

      val solveStart = Instant.now()
      val z3Ctx = new com.microsoft.z3.Context()
      try {
        val solver = new Solver(z3Ctx)
        init.semantics.execute(res, solver)
      } finally {
        // z3Ctx.close()
      }
      println(s"solve took ${Duration.between(solveStart, Instant.now()).toMillis} ms")
    } else {
      println(s"Skipping execution due to parse failure: $parseResult")
    }

    println(s"totally operation took ${Duration.between(start, Instant.now()).toMillis} ms")
  }
}