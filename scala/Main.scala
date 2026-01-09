package romanesco
import java.time.{Instant, Duration}
import scala.util.{Using, Try}

object Main {
  def main(args: Array[String]): Unit = {
    val result = for {
      config <- parseArgs(args)
      _ = logger.switch(config.debug, if (config.debug) { LogLevel.Debug } else { LogLevel.Info })
      source <- readSource(config.filename)
      _ <- compile(source, config.debug)
    } yield ()
    
    result match {
      case Right(_) => {
        logger.info("Compilation successful")
        sys.exit(0)
      }
      case Left(errors) => {
        errors.foreach(e => println(e.format))
        sys.exit(1)
      }
    }
  }
  
  case class Config(filename: String, debug: Boolean)
  
  private def parseArgs(args: Array[String]): CompilerResult[Config] = {
    val filename = if (args.length >= 1) {
      args(0)
    } else {
      scala.io.StdIn.readLine("input file: ")
    }
    val debug = args.length == 2 && args(1) == "debug"
    Right(Config(filename, debug))
  }
  
  private def readSource(filename: String): CompilerResult[String] = {
    Try {
      val source = scala.io.Source.fromFile(filename)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }.toEither.left.map { e =>
      List(CompilerError.BackendError(
        s"Cannot read file: ${e.getMessage}", 
        "io"
      ))
    }
  }
  
  private def compile(sourceStr: String, debugFlag: Boolean): CompilerResult[Unit] = {
    val start = Instant.now()
    
    val lexer = init.lex.setup("firstLexer")
    val parser = init.parse.setup("firstParser", lexer)
    
    // パーサーの初期化
    init.parse.literals(parser)
    init.parse.logic(parser)
    
    val lattice = lexer.tokenize(sourceStr)
    if (debugFlag) {
      lexer.printStream
    }
    
    val parseResult = parser(lattice.asInstanceOf[Map[Int, Array[parser.lexer.Token]]], sourceStr)
    val parseEnd = Instant.now()
    println(s"parse took ${Duration.between(start, parseEnd).toMillis} ms")
    
    if (debugFlag) {
      parser.printStream
    }
    
    if (!parseResult.successful) {
      return Left(List(CompilerError.ParseError(
        s"Parse failed: $parseResult", 
        None
      )))
    }
    
    val resRaw = parseResult.get
    val res = init.optimization.apply(resRaw)
    
    if (debugFlag) {
      println("--- Optimized AST ---")
      res.zipWithIndex.foreach { case (r, i) => 
        println(s"[$i] $r") 
      }
      println("---------------------")
    }

    val solveStart = Instant.now()
    Try {
      Using.resource(new Solver()) { solver =>
        init.semantics.execute(res, solver)
      }
    }.toEither.left.map { e =>
      List(CompilerError.ConstraintError(
        s"Solver error: ${e.getMessage}",
        None
      ))
    }.map { _ =>
      println(s"solve took ${Duration.between(solveStart, Instant.now()).toMillis} ms")
      println(s"totally operation took ${Duration.between(start, Instant.now()).toMillis} ms")
    }
  }
}
