package romanesco
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import java.util.regex.Pattern

object Lexer extends RegexParsers {
  import Token._
  override def skipWhitespace = false
  def tKw = ("syntax" | "if" | "then" | "else" | "macro") ^^ Kw.apply
  def tOp = ("->" | "<<" | ">>" | "==" | "&&" | "||" | "+" | "-" | "*" | "/" | "=" | "<" | ">") ^^ Op.apply
  def tSym = ("(" | ")" | "{" | "}" | "," | "`" | "\\") ^^ Sym.apply
  def tRegex = Parser(in => Pattern.compile("#.*|\\s+|\\d+(\\.\\d+)?|[a-zA-Z_$][a-zA-Z0-9_$]*").matcher(in.source.subSequence(in.offset, in.source.length)) match {
    case m if m.lookingAt() => val s = m.group(); val res = if s.startsWith("#") then Comment(s) else if s.trim.isEmpty then WS(s) else if s.head.isDigit then Num(BigDecimal(s)) else Var(s); Success(res, in.drop(s.length))
    case _ => Failure("", in)
  })
  def lex(code: String): Either[String, List[List[Token]]] = {
    def solve(s: String): List[List[Token]] = if s.isEmpty then List(Nil) else 
      (tKw | tOp | tSym | tRegex)(new CharSequenceReader(s)) match
        case Success(t, n) => solve(s.substring(n.offset)).map(t :: _)
        case _ => Nil
    val res = solve(code).distinct
    if (res.isEmpty) Left("Lexer failed") else Right(res)
  }
}
