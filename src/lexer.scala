package parser
import scala.util.parsing.combinator._
import scala.util.matching.Regex

class LexerTable(val parent: Option[LexerTable] = None) {
  private val delimiters = collection.mutable.Set[Char]()
  def addDelimiter(c: Char): Unit = delimiters.add(c)
  def getDelimiters: Set[Char] = delimiters.toSet ++ parent.map(_.getDelimiters).getOrElse(Set.empty)
}

class rTokenStream(var tokens: List[rToken]) {
  def atEnd: Boolean = tokens.isEmpty
  def first: rToken = tokens.head
  def consume(): rToken = { val t = tokens.head; tokens = tokens.tail; t }
}

class rLexer(lexerTable: LexerTable) extends RegexParsers {
  override val whiteSpace: Regex = """([ \t\r\n]+|//.*)+""".r

  def tokens: Parser[List[rToken]] = rep(token)

  def token: Parser[rToken] = positioned {
    val ds = lexerTable.getDelimiters
    val dsParser = if (ds.isEmpty) failure("") else ds.map(c => literal(c.toString)).reduce(_ | _)
    dsParser ^^ { s => TWord(s) } |
    word
  }

  def word: Parser[TWord] = {
    val ds = lexerTable.getDelimiters
    val dsEscaped = ds.map {
      case '\\' => "\\\\"
      case ']'  => "\\]"
      case '-'  => "\\-"
      case '^'  => "\\^"
      case c    => c.toString
    }.mkString("")
    s"[^\\s$dsEscaped]+".r ^^ { s => TWord(s) }
  }

  def lex(input: String): ParseResult[List[rToken]] = parseAll(tokens, input)
}
