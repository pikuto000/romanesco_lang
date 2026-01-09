package romanesco

import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader, Position, NoPosition}

object SimpleParser extends Parsers {
  override type Elem = Token
  import Token.{WS, Comment, Var => TVar, Num => TNum, Op => TOp, Keyword => TKw, Sym => TSym}
  import AstExpr._
  import Stmt._

  class TokenReader(val all: Seq[Token]) extends Reader[Token] {
    private val eff = all.dropWhile(t => t.isInstanceOf[WS] || t.isInstanceOf[Comment])
    override def first = if (eff.isEmpty) all.headOption.getOrElse(null) else eff.head
    override def atEnd = eff.isEmpty
    override def pos = if (eff.isEmpty) NoPosition else eff.head.pos
    override def rest = { val h = first; new TokenReader(all.drop(all.indexOf(h) + 1)) }
    def remaining = eff.size
  }

  def anyOf(ps: Parser[AstExpr]*): Parser[AstExpr] = Parser { in =>
    val rs = ps.map(_(in)).collect { case Success(r, n: TokenReader) => (r, n) }
    if (rs.isEmpty) Failure("", in)
    else { val m = rs.maxBy(_._2.remaining * -1); if (rs.size == 1) Success(m._1, m._2) else Success(Ambiguous(rs.map(_._1).toList), m._2) }
  }

  // --- 基本パーサー ---
  private def id: Parser[String] = accept("id", { case TVar(n) => n })
  private def num: Parser[BigDecimal] = accept("num", { case TNum(v) => v })
  private def op(s: String): Parser[String] = accept(s"op $s", { case TOp(`s`) => s })
  private def kw(s: String): Parser[String] = accept(s"kw $s", { case TKw(`s`) => s })
  private def sym(s: String): Parser[String] = accept(s"sym $s", { case TSym(`s`) => s })
  private def anyT: Parser[Token] = Parser(in => if (in.atEnd) Failure("", in) else Success(in.first, in.rest))

  // --- 文法 ---
  def program: Parser[List[Stmt]] = rep(stmt) <~ opt(sym(";"))

  def stmt: Parser[Stmt] = (
      kw("syntax") ~> rep1(not(op("=") ~ sym("{")) ~> anyT) ~ (op("=") ~> sym("{") ~> rep(not(sym("}")) ~> anyT) <~ sym("}")) ^^ { case p ~ t => SyntaxDef(p, t) }
    | kw("macro") ~> id ~ (sym("(") ~> repsep(id, sym(",")) <~ sym(")")) ~ (op("=") ~> expr) ^^ { case n ~ p ~ b => MacroDef(n, p, b) }
    | expr ~ op("=") ~ expr ^^ { case l ~ _ ~ r => Constraint(l, r) }
    | sym("{") ~> rep(stmt) <~ sym("}")  ^^ Block.apply
  ) <~ opt(sym(";"))

  def expr: Parser[AstExpr] = (
      sym("\u005c") ~> id ~ (op("->") ~> expr) ^^ { case p ~ b => Lambda(p, b) }
    | term ~ rep((op("+") | op("-") | op("<<") | op(">>")) ~ term) ^^ { case t ~ l => l.foldLeft(t) { case (x, o ~ y) => BinOp(o, x, y) } }
  )

  def term: Parser[AstExpr] = fact ~ rep((op("*") | op("/")) ~ fact) ^^ { case f ~ l => l.foldLeft(f) { case (x, o ~ y) => BinOp(o, x, y) } }

  def fact: Parser[AstExpr] = anyOf(
    (id ~ op("<") ~ expr <~ op(">")) ^^ { case n ~ _ ~ i => Templated(n, i) },
    (id ~ (sym("(") ~> repsep(expr, sym(",")) <~ sym(")"))) ^^ { case n ~ a => Apply(Var(n), a) },
    (id ~ (sym("(") ~> expr <~ sym(")"))) ^^ { case n ~ a => BinOp("=", Var(n), a) },
    (sym("`") ~> rep(not(sym("`")) ~> anyT) <~ sym("`")) ^^ Raw.apply,
    num ^^ Num.apply, id ^^ Var.apply, (sym("(") ~> expr <~ sym(")"))
  )
  
  def parse(ts: List[Token]) = program(new TokenReader(ts))
}