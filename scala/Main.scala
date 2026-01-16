import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

/**
 * romanesco コンパイラ エントリーポイント
 */
@main def run(args: String*): Unit =
  import Lexing.*
  import Parsing.*
  import Solver.Solver
  import Core.*
  
  // 引数チェック
  if args.length < 1 then
    printUsage()
    return
  
  val inputPath = args(0)
  val mode = args.find(a => !Set("prune", "debug", "eval", inputPath).contains(a))
    .flatMap(parseMode)
    .getOrElse(LexMode.All)
  
  val usePruning = args.contains("prune")
  val useDebug = args.contains("debug")
  val useEval = args.contains("eval")
  
  // ファイル読み込み
  val input = try 
    Files.readString(Paths.get(inputPath), StandardCharsets.UTF_8)
  catch
    case e: Exception =>
      System.err.println(s"Failed to read file: $inputPath")
      System.err.println(s"Error: ${e.getMessage}")
      return
  
  // トークナイザー設定
  val config = TokenizerConfig.default
  val tokenizers = buildTokenizers(config)
  
  println(s"=== Romanesco Compiler ===")
  println(s"Input: $inputPath")
  println(s"Mode: $mode")
  println(s"Z3 Pruning: ${if usePruning then "enabled" else "disabled"}")
  println(s"Debug mode: ${if useDebug then "enabled" else "disabled"}")
  println(s"Evaluation: ${if useEval then "enabled" else "disabled"}")
  println(s"Input size: ${input.length} chars")
  println()
  
  // レキシング
  val lexStart = System.currentTimeMillis()
  val rawTokenTree = lexAll(input, tokenizers, mode, useDebug)
  val lexEnd = System.currentTimeMillis()
  
  if useDebug then
    println(rawTokenTree.drawTree)
  
  println("== Lexing (before pruning) ==")
  val rawPaths = rawTokenTree.flattenPaths.map(_.toList)
  println(s"  Found ${rawPaths.length} token sequences")
  println(s"  Time: ${lexEnd - lexStart}ms")
  println()
  
  // Z3による枝刈り
  val tokenTree = if usePruning then
    val pruneStart = System.currentTimeMillis()
    val pruned = Solver.pruneTokenTree(rawTokenTree)
    val pruneEnd = System.currentTimeMillis()
    
    println("== Z3 Constraint Pruning ==")
    val prunedPaths = pruned.flattenPaths.map(_.toList)
    println(s"  Before: ${rawPaths.length} sequences")
    println(s"  After: ${prunedPaths.length} sequences")
    println(s"  Eliminated: ${rawPaths.length - prunedPaths.length} invalid sequences")
    println(s"  Time: ${pruneEnd - pruneStart}ms")
    println()
    
    pruned
  else
    rawTokenTree
  
  printLexingResults(tokenTree, lexEnd - lexStart)
  
  // パーシング
  val parseStart = System.currentTimeMillis()
  val astTree = Parser.parseAll(tokenTree)
  val parseEnd = System.currentTimeMillis()
  
  printParsingResults(astTree, parseEnd - parseStart)
  
  // 評価
  if useEval then
    val astPaths = astTree.flattenPaths.map(_.toList)
    
    if astPaths.nonEmpty then
      val stmts = astPaths.head.head
      
      println("== Evaluation ==")
      val evalStart = System.currentTimeMillis()
      
      try
        val (env, result) = Evaluator.evalProgram(stmts)
        val evalEnd = System.currentTimeMillis()
        
        println(s"  Environment bindings:")
        env.bindings.filterKeys(!Prelude.bindings.contains(_)).foreach { case (name, expr) =>
          println(s"    $name = ${Translator.showExpr(expr)}")
        }
        
        println()
        result match
          case Some(value) =>
            println(s"  Result: ${Value.show(value)}")
          case None =>
            println(s"  No result (statements only)")
        
        println(s"  Time: ${evalEnd - evalStart}ms")
        println()
      catch
        case e: Exception =>
          val evalEnd = System.currentTimeMillis()
          println(s"  Evaluation error: ${e.getMessage}")
          println(s"  Time: ${evalEnd - evalStart}ms")
          println()
          if useDebug then
            e.printStackTrace()
  
  println()
  println(s"Total time: ${System.currentTimeMillis() - lexStart}ms")

// トークナイザー設定
case class TokenizerConfig(
  keywords: Set[String],
  operators: Map[String, Lexing.Token],
  delimiters: Set[String]
)

object TokenizerConfig:
  def default: TokenizerConfig = TokenizerConfig(
    keywords = Set("syntax"),
    
    operators = Map(
      // 論理演算子
      "and" -> Lexing.Token.Op("and"),
      "or"  -> Lexing.Token.Op("or"),
      
      // 比較演算子
      "==" -> Lexing.Token.Op("=="),
      "!=" -> Lexing.Token.Op("!="),
      ">=" -> Lexing.Token.Op(">="),
      "<=" -> Lexing.Token.Op("<="),
      
      // 算術演算子
      "+"  -> Lexing.Token.Op("+"),
      "-"  -> Lexing.Token.Op("-"),
      "*"  -> Lexing.Token.Op("*"),
      "/"  -> Lexing.Token.Op("/"),
      
      // その他
      ">"  -> Lexing.Token.Op(">"),
      "<"  -> Lexing.Token.Op("<"),
      "="  -> Lexing.Token.Op("="),
      "->" -> Lexing.Token.Op("->")
    ),
    
    delimiters = Set("(", ")", "{", "}", "[", "]", ",", ";", ":", "\\", "\n")
  )

def buildTokenizers(config: TokenizerConfig): List[Lexing.Tokenizer] =
  import Lexing.*
  
  val identRegex = "[a-zA-Z_][a-zA-Z_0-9]*".r
  val wsRegex = "([ \\t\\r\\n;]|//.*)+".r
  val numberRegex = "[0-9]+".r
  
  def keysToRegex(keys: Iterable[String]): scala.util.matching.Regex =
    keys.toSeq
      .sortBy(-_.length)
      .map(java.util.regex.Pattern.quote)
      .mkString("|")
      .r
  
  val opRegex = keysToRegex(config.operators.keys)
  
  List(
    lexRegexLongest(wsRegex, Token.WS(_)),
    lexRegexLongest(numberRegex, Token.Number(_)),
    lexRegexLongest(opRegex, s => config.operators(s)),
    lexRegexLongest(identRegex, s => 
      if config.keywords.contains(s) then Token.Keyword(s) 
      else Token.Ident(s)
    ),
    lexDelim(config.delimiters)
  )

def printLexingResults(tokenTree: Undeterminable.tree[Lexing.Token], timeMs: Long): Unit =
  import Lexing.Token
  
  println("== Lexing (final) ==")
  
  val tokenPaths = tokenTree.flattenPaths.map(_.toList)
  
  val displayCount = 5
  tokenPaths.take(displayCount).zipWithIndex.foreach { (tokens, i) =>
    val filtered = tokens.filterNot(_.isInstanceOf[Token.WS])
    println(s"  Path $i: ${filtered.map(_.lexeme).mkString(" ")}")
  }
  
  if tokenPaths.length > displayCount then
    println(s"  ... and ${tokenPaths.length - displayCount} more paths")
  
  println(s"  Time: ${timeMs}ms")
  println(s"  Found ${tokenPaths.length} token sequences")
  println()

def printParsingResults(astTree: Undeterminable.tree[List[Parsing.Stmt]], timeMs: Long): Unit =
  println("== Parsing ==")
  
  val astPaths = astTree.flattenPaths.map(_.toList)
  
  astPaths.zipWithIndex.foreach { (stmts, i) =>
    println(s"  Parse $i:")
    stmts.foreach { st =>
      st.foreach { s =>
        println(s"    ${Parsing.Stmt.show(s)}")
      }
    }
  }
  
  if astPaths.isEmpty then
    println("  No valid parse trees")
  
  println(s"  Time: ${timeMs}ms")
  println(s"  Found ${astPaths.length} parse trees")
  println()

def parseMode(arg: String): Option[Lexing.LexMode] =
  arg match
    case "best" => Some(Lexing.LexMode.BestOnly)
    case "all"  => Some(Lexing.LexMode.All)
    case s if s.startsWith("topN:") =>
      s.drop(5).toIntOption.map(Lexing.LexMode.TopN.apply)
    case _ => None

def printUsage(): Unit =
  System.err.println(
    """Usage:
      |  run <input-file> [options...]
      |
      |Options:
      |  all      - All possible tokenizations (default)
      |  best     - Only the longest tokenization
      |  topN:<n> - Top N tokenizations
      |  prune    - Enable Z3 constraint pruning
      |  debug    - Enable debug output
      |  eval     - Run evaluation phase
      |
      |Examples:
      |  run test.romanesco eval
      |  run test.romanesco debug eval
      |  run test.romanesco prune eval
      |""".stripMargin
  )
