package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import scala.util.matching.Regex

// --- Lexical Configuration ---
class LexerTable(val parent: Option[LexerTable] = None) {
  private val delimiters: collection.mutable.Set[Char] = collection.mutable.Set.empty

  def addDelimiter(c: Char): Unit = delimiters += c
  
  def getDelimiters: Set[Char] = {
    val inherited = parent.map(_.getDelimiters).getOrElse(Set.empty)
    inherited ++ delimiters
  }
}

// Mutable wrapper for token reader
class rTokenStream(var reader: scala.util.parsing.input.Reader[rToken]) {
  def first: rToken = reader.first
  def atEnd: Boolean = reader.atEnd
  def consume(): rToken = {
    val t = reader.first
    reader = reader.rest
    t
  }
}

// --- Lexer ---
class rLexer(config: LexerTable) extends RegexParsers with PackratParsers {
  override val skipWhitespace = true
  override protected val whiteSpace: Regex = "(\\s|//.*|(?m)/\\*(\\n|.)*?\\*/)+" .r

  // Dynamic word regex: matches sequences that are NOT whitespace and NOT delimiters
  private def wordRegex: Regex = {
    val delims = config.getDelimiters.mkString
    if (delims.isEmpty) {
      """[^\s]+""".r 
    } else {
      val escaped = Regex.quote(delims)
      s"""[^\\s$escaped]+""".r
    }
  }

  lazy val word: PackratParser[TWord] = wordRegex ^^ TWord.apply

  // Matches a single character delimiter using Regex to benefit from automatic whitespace handling
  lazy val delimiter: PackratParser[TWord] = {
    val delims = config.getDelimiters.mkString
    if (delims.isEmpty) failure("no delimiters")
    else s"[${Regex.quote(delims)}]".r ^^ TWord.apply
  }

  lazy val token: PackratParser[rToken] = positioned(delimiter | word)
  lazy val tokens: PackratParser[List[rToken]] = rep(token)

  def lex(input: String): ParseResult[List[rToken]] = {
    val reader = new PackratReader(new CharSequenceReader(input))
    parseAll(tokens, reader)
  }
}
