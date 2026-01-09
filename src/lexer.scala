package romanesco

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import java.util.regex.Pattern

object Lexer extends RegexParsers {
  import Token._

  override def skipWhitespace = false

  // ヘルパー: パターンマッチ
  private def matchRegex(p: String, in: Input): Option[String] = {
    val m = Pattern.compile(p).matcher(in.source.subSequence(in.offset, in.source.length))
    if (m.lookingAt()) Some(m.group()) else None
  }

  // 各トークンのパーサー定義
  def tNum: Parser[Token] = Parser(in => matchRegex("\\d+(\\.\\d+)?", in).map(s => Success(Num(BigDecimal(s)), in.drop(s.length))).getOrElse(Failure("", in)))
  def tId: Parser[Token] = Parser(in => matchRegex("[a-zA-Z_$][a-zA-Z0-9_$]*", in).map(s => Success(Var(s), in.drop(s.length))).getOrElse(Failure("", in)))
  def tWS: Parser[Token] = Parser(in => matchRegex("\\s+", in).map(s => Success(WS(s), in.drop(s.length))).getOrElse(Failure("", in)))
  def tComment: Parser[Token] = Parser(in => matchRegex("#.*", in).map(s => Success(Comment(s), in.drop(s.length))).getOrElse(Failure("", in)))
  def tKw: Parser[Token] = ("macro" | "syntax") ^^ Keyword.apply
  def tSym: Parser[Token] = ("(" | ")" | "{" | "}" | "," | ";" | "`" | "\\") ^^ Sym.apply
  def tOp: Parser[Token] = (":=" | "->" | "<<" | ">>" | "+" | "-" | "*" | "/" | "=" | "<" | ">" | ":") ^^ Op.apply

  private val parsers = List(tComment, tWS, tNum, tKw, tId, tOp, tSym)

  // すべての解釈を探索する
  def lex(code: String): Either[String, List[List[Token]]] = {
    def solve(s: String): List[List[Token]] = {
      if (s.isEmpty) List(Nil)
      else {
        val reader = new CharSequenceReader(s)
        val matches = parsers.flatMap(_(reader) match { case Success(t, n) => Some(t, s.substring(n.offset)); case _ => None })
        matches.flatMap { case (t, rest) => solve(rest).map(t :: _) }
      }
    }
    val res = solve(code).distinct
    if (res.isEmpty) Left("Lexer failed") else Right(res)
  }
}