package romanesco
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import java.util.regex.Pattern

object Lexer extends RegexParsers {
  import Token._
  override def skipWhitespace = false

  def tKw: Parser[Token] = ("syntax" | "if" | "then" | "else" | "macro") ^^ Kw.apply
  def tOp: Parser[Token] = (":=" | "->" | "<<" | ">>" | "==" | "+" | "-" | "*" | "/" | "=" | "<" | ">" | ":") ^^ Op.apply
  def tSym: Parser[Token] = ("(" | ")" | "{" | "}" | "," | ";" | "`" | "\\") ^^ Sym.apply
  
  def tRegex: Parser[Token] = Parser(in => Pattern.compile("#.*|\\s+|\\d+(\\.\\d+)?|[a-zA-Z_$][a-zA-Z0-9_$]*").matcher(in.source.subSequence(in.offset, in.source.length)) match {
    case m if m.lookingAt() => 
      val s = m.group(); val res = if s.startsWith("#") then Comment(s) else if s.trim.isEmpty then WS(s) else if s.head.isDigit then Num(BigDecimal(s)) else Var(s)
      Success(res, in.drop(s.length))
    case _ => Failure("", in)
  })

  def parseT: Parser[Token] = tKw | tOp | tSym | tRegex

  def lex(code: String): Either[String, List[List[Token]]] = {
    def solve(s: String): List[List[Token]] = if s.isEmpty then List(Nil) else {
      val rd = new CharSequenceReader(s)
      val ms = List(parseT).flatMap(_(rd) match { case Success(t, n) => Some(t, s.substring(n.offset)); case _ => None })
      ms.flatMap { (t, r) => solve(r).map(t :: _) } // 実際には空白制御が必要だがデモ用
    }
    val res = solve(code).distinct
    if (res.isEmpty) Left("Lexer failed") else Right(res)
  }
}