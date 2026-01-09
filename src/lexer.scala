package romanesco

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import java.util.regex.Pattern

object Lexer extends RegexParsers {
  import Token._

  override def skipWhitespace = false

  private def trim(code: String): String = {
    code.replaceAll("^(\\s|#.*)+", "")
  }
  
  private def matchPattern(p: String, in: Input): Option[String] = {
    val matcher = Pattern.compile(p).matcher(in.source.subSequence(in.offset, in.source.length))
    if (matcher.lookingAt()) Some(matcher.group()) else None
  }

  def whitespace: Parser[Token] = new Parser[Token] {
    def apply(in: Input) = matchPattern("\\s+", in) match {
      case Some(s) => Success(WS(s), in.drop(s.length))
      case _ => Failure("not whitespace", in)
    }
  }

  def comment: Parser[Token] = new Parser[Token] {
    def apply(in: Input) = matchPattern("#.*", in) match {
      case Some(s) => Success(Comment(s), in.drop(s.length))
      case _ => Failure("not a comment", in)
    }
  }

  def num: Parser[Token] = new Parser[Token] {
    def apply(in: Input) = matchPattern("\\d+(\\.\\d+)?", in) match {
      case Some(s) => Success(Num(BigDecimal(s)), in.drop(s.length))
      case _ => Failure("not a number", in)
    }
  }

  def kw: Parser[Token] = ("macro" | "syntax") ^^ { s => Keyword(s) }

  def id: Parser[Token] = new Parser[Token] {
    def apply(in: Input) = matchPattern("[a-zA-Z_$][a-zA-Z0-9_$]*", in) match {
      case Some(s) => Success(Var(s), in.drop(s.length))
      case _ => Failure("not an identifier", in)
    }
  }
  
  def sym: Parser[Token] = ("(" | ")" | "{" | "}" | "," | ";" | "`") ^^ { s => Sym(s) }
  def opLong: Parser[Token] = ("<<" | ">>") ^^ { s => Op(s) }
  def opShort: Parser[Token] = ("+" | "-" | "*" | "/" | "=" | "<" | ">" | ":") ^^ { s => Op(s) }

  private val allParsers = List(comment, whitespace, num, kw, id, opLong, opShort, sym)

  def lex(code: String): Either[String, List[List[Token]]] = {
    def solve(currentCode: String): List[List[Token]] = {
      if (currentCode.isEmpty) List(Nil)
      else {
        val reader = new CharSequenceReader(currentCode)
        val candidates = allParsers.flatMap { p =>
          p(reader) match {
            case Success(t, next) => Some((t, currentCode.substring(next.offset)))
            case _ => None
          }
        }
        if (candidates.isEmpty) Nil
        else candidates.flatMap { case (t, rest) => solve(rest).map(t :: _) }
      }
    }
    val results = solve(code)
    if (results.isEmpty) Left("Lexer Error: Could not tokenize any path")
    else Right(results.distinct)
  }
}