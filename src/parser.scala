package romanesco

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader, Position, NoPosition}

object SimpleParser extends Parsers {
  override type Elem = Token
  import Token._
  import AstExpr._
  import Stmt._

  class TokenReader(val allTokens: Seq[Token]) extends Reader[Token] {
    private val effectiveTokens = allTokens.dropWhile(t => t.isInstanceOf[WS] || t.isInstanceOf[Comment])
    override def first: Token = if (effectiveTokens.isEmpty) allTokens.headOption.getOrElse(null) else effectiveTokens.head
    override def atEnd: Boolean = effectiveTokens.isEmpty
    override def pos: Position = if (effectiveTokens.isEmpty) NoPosition else effectiveTokens.head.pos
    override def rest: Reader[Token] = {
      val head = first
      val index = allTokens.indexOf(head)
      new TokenReader(allTokens.drop(index + 1))
    }
    def remaining: Int = effectiveTokens.size
  }

  def anyOf[T](ps: Parser[AstExpr]*): Parser[AstExpr] = Parser { in =>
    val results = ps.map(_(in)).collect { case Success(res, next: TokenReader) => (res, next) }
    if (results.isEmpty) Failure("No alternatives matched", in)
    else {
      val maxNext = results.maxBy(_._2.remaining * -1)._2
      val options = results.map(_._1).toList
      if (options.size == 1) Success(options.head, maxNext)
      else Success(AstExpr.Ambiguous(options), maxNext)
    }
  }

  def anyToken: Parser[Token] = Parser { in =>
    val reader = in.asInstanceOf[TokenReader]
    if (reader.allTokens.isEmpty) Failure("EOF", in)
    else Success(reader.allTokens.head, new TokenReader(reader.allTokens.tail))
  }

  private def ident: Parser[String] = accept("identifier", { case Token.Var(n) => n })
  private def number: Parser[BigDecimal] = accept("number", { case Token.Num(v) => v })
  private def op(s: String): Parser[String] = accept(s"operator '$s'", { case Token.Op(`s`) => s })
  private def kw(s: String): Parser[String] = accept(s"keyword '$s'", { case Token.Keyword(`s`) => s })
  private def sym(s: String): Parser[String] = accept(s"symbol '$s'", { case Token.Sym(`s`) => s })

  def program: Parser[List[Stmt]] = rep(statement) <~ opt(sym(";"))

  def statement: Parser[Stmt] = (
      // 構文定義: syntax パターン = { テンプレート }
      // パターンの終わりを「= {」の出現で判断する
      kw("syntax") ~> rep1(not(op("=") ~ sym("{")) ~> anyToken) ~ (op("=") ~> sym("{") ~> rep(not(sym("}")) ~> anyToken) <~ sym("}")) ^^ {
        case pattern ~ template => Stmt.SyntaxDef(pattern, template)
      }
    | kw("macro") ~> ident ~ (sym("(") ~> repsep(ident, sym(",")) <~ sym(")")) ~ (op("=") ~> expr) ^^ {
        case name ~ params ~ body => Stmt.MacroDef(name, params, body)
      }
    | expr ~ op("=") ~ expr ^^ { case l ~ _ ~ r => Stmt.Constraint(l, r) }
    | sym("{") ~> rep(statement) <~ sym("}")  ^^ { stmts => Stmt.Block(stmts) }
  ) <~ opt(sym(";"))

  def expr: Parser[AstExpr] = term ~ rep((op("+") | op("-") | op("<<") | op(">>")) ~ term) ^^ {
    case t ~ list => list.foldLeft(t) {
      case (l, o ~ r) => AstExpr.BinOp(o, l, r)
    }
  }

  def term: Parser[AstExpr] = factor ~ rep((op("*") | op("/")) ~ factor) ^^ {
    case f ~ list => list.foldLeft(f) {
      case (l, o ~ r) => AstExpr.BinOp(o, l, r)
    }
  }

  def factor: Parser[AstExpr] = anyOf(
    (ident ~ op("<") ~ expr <~ op(">")) ^^ { case name ~ _ ~ inner => AstExpr.Templated(name, inner) },
    (ident ~ (sym("(") ~> expr <~ sym(")"))) ^^ { case name ~ arg => AstExpr.MacroCall(name, List(arg)) },
    (ident ~ (sym("(") ~> expr <~ sym(")"))) ^^ { case name ~ arg => AstExpr.BinOp("=", AstExpr.Var(name), arg) },
    (sym("`") ~> rep(not(sym("`")) ~> anyToken) <~ sym("`")) ^^ { tokens => AstExpr.Raw(tokens) },
    number ^^ { v => AstExpr.Num(v) },
    ident  ^^ { n => AstExpr.Var(n) },
    (sym("(") ~> expr <~ sym(")"))
  )
  
  def parse(tokens: List[Token]): ParseResult[List[Stmt]] = {
    program(new TokenReader(tokens))
  }
}
