import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.util.boundary

@main def run(args: String*): Unit =
  import Lexing._
  import Parsing._
  
  if args.length < 1 then
    usageAndExit()
    return
  
  lazy val inputPath = args(0)
  lazy val mode = if args.length > 1 then parseMode(args(1)).getOrElse(LexMode.All) else LexMode.All

  lazy val input =
    try Files.readString(Paths.get(inputPath), StandardCharsets.UTF_8)
    catch
      case e: Exception =>
        System.err.println(s"Failed to read file: $inputPath")
        boundary("")

  // Romanesco言語のトークンルール
  lazy val keywords = Set("syntax", "if", "else", "let", "fun", "match", "case")
  
  lazy val opRules =
    Map(
      "=="  -> Token.Op("=="),
      "!="  -> Token.Op("!="),
      ">="  -> Token.Op(">="),
      "<="  -> Token.Op("<="),
      "->"  -> Token.Op("->"),
      "+"   -> Token.Op("+"),
      "-"   -> Token.Op("-"),
      "*"   -> Token.Op("*"),
      "/"   -> Token.Op("/"),
      ">"   -> Token.Op(">"),
      "<"   -> Token.Op("<"),
      "="   -> Token.Op("=")
    )

  lazy val delimRules =
    Map(
      "(" -> Token.Delim("("),
      ")" -> Token.Delim(")"),
      "{" -> Token.Delim("{"),
      "}" -> Token.Delim("}"),
      "[" -> Token.Delim("["),
      "]" -> Token.Delim("]"),
      "," -> Token.Delim(","),
      ";" -> Token.Delim(";"),
      ":" -> Token.Delim(":")
    )

  def keysToRegex(keys: Iterable[String]): scala.util.matching.Regex =
    keys.toSeq.sortBy(-_.length).map(java.util.regex.Pattern.quote).mkString("|").r

  lazy val opRegex = keysToRegex(opRules.keys)
  lazy val opTokenizer = lexRegex(opRegex, s => opRules(s))

  lazy val delimTokenizer = lexDelim(delimRules.keys.toSet)
  
  // 識別子とキーワード
  lazy val identRegex = "[a-zA-Z_][a-zA-Z_0-9]*".r
  lazy val identTokenizer = lexRegexLongest(identRegex, s => 
    if keywords.contains(s) then Token.Keyword(s) else Token.Ident(s)
  )

  lazy val wsRegex = "[ \\t\\n\\r]+".r
  lazy val wsTokenizer = lexRegexLongest(wsRegex, Token.WS(_))

  lazy val numberRegex = "[0-9]+".r
  lazy val numberTokenizer = lexRegexLongest(numberRegex, Token.Number(_))

  lazy val tokenizers = List(
    wsTokenizer,      // 空白を最初に（スキップしやすくするため）
    numberTokenizer,  // 数値
    opTokenizer,      // 演算子（長いものから）
    identTokenizer,   // 識別子とキーワード
    delimTokenizer    // デリミタ
  )

  println(s"=== Romanesco Compiler ===")
  println(s"Input: $inputPath")
  println(s"Mode: $mode")
  println()
  
  // === LEXING ===
  val lexStart = System.currentTimeMillis()
  val tokenTree = lexAll(input, tokenizers, mode)
  val lexEnd = System.currentTimeMillis()
  
  println("== Lexing ==")
  val tokenPaths = tokenTree.flattenPaths.map(_.toList)
  tokenPaths.take(5).zipWithIndex.foreach { (tokens, i) =>
    val filtered = tokens.filterNot(_.isInstanceOf[Token.WS])
    println(s"  Path $i: ${filtered.mkString(" ")}")
  }
  if tokenPaths.length > 5 then
    println(s"  ... and ${tokenPaths.length - 5} more paths")
  println(s"  Time: ${lexEnd - lexStart}ms")
  println(s"  Found ${tokenPaths.length} token sequences")
  println()
  
  // === PARSING ===
  val parseStart = System.currentTimeMillis()
  val astTree = Parser.parseAll(tokenTree)
  val parseEnd = System.currentTimeMillis()
  
  println("== Parsing ==")
  val astPaths = astTree.flattenPaths.map(_.toList)
  astPaths.zipWithIndex.foreach { (stmts, i) =>
    println(s"  Parse $i:")
    stmts.foreach { st =>
      st.foreach { s =>
        println(s"    ${Stmt.show(s)}")
      }
    }
  }
  println(s"  Time: ${parseEnd - parseStart}ms")
  println(s"  Found ${astPaths.length} parse trees")
  println()
  
  println(s"Total time: ${parseEnd - lexStart}ms")

def parseMode(arg: String): Option[Lexing.LexMode] =
  arg match
    case "best" => Some(Lexing.LexMode.BestOnly)
    case "all"  => Some(Lexing.LexMode.All)
    case s if s.startsWith("topN:") =>
      s.drop(5).toIntOption.map(Lexing.LexMode.TopN.apply)
    case _ => None

def usageAndExit(): Unit =
  System.err.println(
    """Usage:
      |  run <input-file> [mode]
      |
      |Modes:
      |  best     - Only the longest tokenization
      |  all      - All possible tokenizations (default)
      |  topN:<n> - Top N tokenizations
      |""".stripMargin
  )
