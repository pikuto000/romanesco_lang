package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import scala.util.matching.Regex

class rLexer extends RegexParsers with PackratParsers {
  override val skipWhitespace = true
  override protected val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\n|.)*?\*/)+""" .r

  lazy val word: PackratParser[TWord] = """[^\s()]+""" .r ^^ TWord.apply
  lazy val lparen: PackratParser[TLParen] = "(" ^^^ TLParen()
  lazy val rparen: PackratParser[TRParen] = ")" ^^^ TRParen()

  lazy val token: PackratParser[rToken] = positioned(lparen | rparen | word)
  lazy val tokens: PackratParser[List[rToken]] = rep(token)

  def lex(input: String): ParseResult[List[rToken]] = {
    val reader = new PackratReader(new CharSequenceReader(input))
    parseAll(tokens, reader)
  }
}