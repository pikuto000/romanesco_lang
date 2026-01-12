import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import scala.util.boundary
@main def run(args: String*): Unit =
  import Lexing._
  if args.length < 3 then
    usageAndExit()
    return
  lazy val inputPath = args(0)
  lazy val modeArg   = args(1)
  //Unlexするためのファイルパスを指定する
  lazy val targetStringPath = args(2)


  lazy val input =
    try Files.readString(Paths.get(inputPath), StandardCharsets.UTF_8)
    catch
      case e: Exception =>
        System.err.println(s"Failed to read file: $inputPath")
        boundary("")

  lazy val unlexString =
    try Files.readString(Paths.get(targetStringPath), StandardCharsets.UTF_8)
    catch
      case e: Exception =>
        System.err.println(s"Failed to read file: $targetStringPath")
        boundary("")

  lazy val mode =
    parseMode(modeArg).getOrElse {
      System.err.println(s"Invalid mode: $modeArg")
      usageAndExit()
      LexMode.All
    }

  lazy val rules =
    Map(
      ">>"  -> Token.Op(">>"),
      ">"   -> Token.Op(">"),
      "<<"  -> Token.Op("<<"),
      "<"   -> Token.Op("<"),
      "+"   -> Token.Op("+"),
      "-"   -> Token.Op("-"),
      "*"   -> Token.Op("*"),
      "/"   -> Token.Op("/"),
      "->"  -> Token.Op("->"),
      "="   -> Token.Op("="),
      "=="  -> Token.Op("=="),
      "!="  -> Token.Op("!="),
      "=<"  -> Token.Op("=<"),
      ">="  -> Token.Op(">="),
      "&&"  -> Token.Op("&&"),
      "||"  -> Token.Op("||"),
      "!"   -> Token.Op("!")
    )

  lazy val delimRules =
    Map(
      "(" -> Token.Delim("("),
      ")" -> Token.Delim(")"),
      "{" -> Token.Delim("{"),
      "}" -> Token.Delim("}"),
      "[" -> Token.Delim("["),
      "]" -> Token.Delim("]"),
      "<" -> Token.Delim("<"),
      ">" -> Token.Delim(">"),
      "," -> Token.Delim(","),
      ";" -> Token.Delim(";"),
      ":" -> Token.Delim(":")
    )

  // Helper to create regex from map keys
  def keysToRegex(keys: Iterable[String]): scala.util.matching.Regex =
    keys.map(java.util.regex.Pattern.quote).mkString("|").r

  lazy val opRegex = keysToRegex(rules.keys)
  lazy val opTokenizer = lexRegex(opRegex, s => rules(s))

  lazy val delimTokenizer = lexDelim(delimRules.keys.toSet)
  
  // Use regex for identifiers
  lazy val identRegex = "[a-zA-Z_][a-zA-Z_0-9]*".r
  lazy val identTokenizer = lexRegexLongest(identRegex, Token.Ident(_))

  lazy val wsRegex = "[ \t\n\r]+".r
  lazy val wsTokenizer = lexRegexLongest(wsRegex, Token.WS(_))

  lazy val numberRegex = "[0-9]+".r
  lazy val numberTokenizer = lexRegexLongest(numberRegex, Token.Number(_))

  lazy val tokenizers = List(
    opTokenizer,
    identTokenizer,
    wsTokenizer,
    numberTokenizer,
    delimTokenizer
  )

  println(s"== Mode: $mode ==")
  println(s"== Input: $inputPath ==")
  println()

  lazy val results = lexAll(input, tokenizers, mode)
  lazy val TargetTokenResult = lexAll(unlexString, tokenizers, mode)

  results.zipWithIndex.foreach { (tokens, i) =>
    println(s"[$i] $tokens")
  }

  println()
  println("== targetTokenResultMatching (round-trip check) ==")
  //targetTokenResultのトークナイズ結果を使い、resultsの中から逆引きできるか確認する
  TargetTokenResult.zipWithIndex.foreach { (tokens, i) =>
    println(s"[$i] $tokens")
  }
  println()
  
    
  
    
  


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
      |  run <input-file> <mode>
      |
      |Modes:
      |  best
      |  all
      |  topN:<n>
      |""".stripMargin
  )
