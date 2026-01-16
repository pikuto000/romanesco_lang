import java.io.File
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

/**
 * 全てのテストファイルを一括実行するテストランナー
 */
@main def runTests(): Unit =
  val testDir = new File("tests")
  if (!testDir.exists() || !testDir.isDirectory) then
    println("Error: tests directory not found.")
    return

  val testFiles = testDir.listFiles()
    .filter(_.getName.endsWith(".romanesco"))
    .sortBy(_.getName)

  println(s"Running ${testFiles.length} tests in Scala...\n")

  for (file <- testFiles) do
    println(s"--- Running ${file.getName} ---")
    
    try
      // メインの run 関数をシミュレート
      val input = Files.readString(file.toPath, StandardCharsets.UTF_8)
      
      import Lexing.*
      import Parsing.*
      import Core.*
      
      // Tokenize
      val tokenizers = buildTokenizers(TokenizerConfig.default)
      val tokenTree = lexAll(input, tokenizers, LexMode.All, false)
      
      // Parse
      val astTree = Parser.parseAll(tokenTree, false)
      val astPaths = astTree.flattenPaths.map(_.toList)
      
      if (astPaths.nonEmpty) then
        val stmts = astPaths.head.head
        // Evaluate
        val (env, result) = Evaluator.evalProgram(stmts)
        
        // Output result
        result match
          case Some(value) => println(s"Result: ${Value.show(value)}")
          case None => 
            // Check for environmental bindings if no direct result
            val userBindings = env.bindings.filterKeys(!Prelude.bindings.contains(_))
            if (userBindings.nonEmpty) then
              userBindings.foreach { case (name, expr) =>
                // Simple display of the evaluated result if available
                // In Scala version, we evaluate once, let's show the result of last expr
                // if it's stored in the environment
              }
            else
              println("Success (no result)")
      else
        println("No valid parse trees found.")
        
    catch
      case e: Exception =>
        println(s"Error during execution: ${e.getMessage}")
    
    println()
