package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, Positional}
import scala.util.matching.Regex

// --- Tokens ---
sealed trait rToken extends Positional { def s: String }
case class TWord(s: String) extends rToken
case class TSpecial(s: String) extends rToken

// --- Token Table ---
class TokenTable(val parent: Option[TokenTable] = None) {
  private val specials = collection.mutable.Set[String]()
  def addSpecial(s: String): Unit = specials.add(s)
  def getSpecials: Set[String] = specials.toSet ++ parent.map(_.getSpecials).getOrElse(Set.empty)

  def getSpecialsRegex: String = {
    val ds = getSpecials
    if (ds.isEmpty) "" 
    else ds.toList.sortWith(_.length > _.length).map(java.util.regex.Pattern.quote).mkString("|")
  }
}

// --- Lexer ---
class rLexer(val table: TokenTable) extends RegexParsers with PackratParsers {
  override val whiteSpace: Regex = """(\s|//.*|(?s)/\*.*?\*/)+""".r

  lazy val tokens: PackratParser[List[rToken]] = rep(token)

  lazy val token: PackratParser[rToken] = positioned {
    val sRegex = table.getSpecialsRegex
    if (sRegex.nonEmpty) (sRegex.r ^^ { s => TSpecial(s) }) | word else word
  }
  // word は、空白および特殊記号に当たらない文字の連続
  lazy val word: PackratParser[TWord] = new PackratParser[TWord] {
    def apply(in: Input): ParseResult[TWord] = {
      lazy val source = in.source
      var offset = in.offset
      lazy val start = in.offset
      lazy val specials = table.getSpecials
      
      while (offset < source.length && 
             !source.charAt(offset).isWhitespace && 
             !specials.exists(s => source.subSequence(offset, source.length).toString.startsWith(s))) {
        offset += 1
      }
      
      if (offset == start) Failure("not a word", in)
      else Success(TWord(source.subSequence(start, offset).toString), in.drop(offset - start))
    }
  }

  def lex(input: String): ParseResult[List[rToken]] = parseAll(tokens, input)
}
