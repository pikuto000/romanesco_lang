package romanesco
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Reader, Position, NoPosition}

object SimpleParser extends Parsers {
  type Elem = Token
  import Token.{WS, Comment, Var => TV, Num => TN, Op => TO, Kw => TK, Sym => TS}
  import AstExpr._, Stmt._

  class TR(val all: Seq[Token]) extends Reader[Token] {
    val eff = all.dropWhile(t => t.isInstanceOf[WS] || t.isInstanceOf[Comment])
    def first = if (eff.isEmpty) all.headOption.getOrElse(null) else eff.head
    def atEnd = eff.isEmpty
    def pos = if (eff.isEmpty) NoPosition else eff.head.pos
    def rest = new TR(all.drop(all.indexOf(first) + 1))
  }

  def anyOf(ps: Parser[AstExpr]*): Parser[AstExpr] = Parser(in => {
    val rs = ps.map(_(in)).collect { case Success(r, n: TR) => (r, n) }
    if (rs.isEmpty) Failure("", in) else { val m = rs.maxBy(_._2.eff.size * -1); if (rs.size == 1) Success(m._1, m._2) else Success(Ambiguous(rs.map(_._1).toList), m._2) }
  })

  private def id = accept("id", { case TV(n) => n })
  private def nm = accept("num", { case TN(v) => v })
  private def o(s: String) = accept(s"o $s", { case TO(`s`) => s })
  private def k(s: String) = accept(s"k $s", { case TK(`s`) => s })
  private def s(s: String) = accept(s"s $s", { case TS(`s`) => s })
  private def anyT = Parser(in => if (in.atEnd) Failure("", in) else Success(in.first, in.rest))

  def program: Parser[List[Stmt]] = rep(stmt) <~ opt(s(";"))
  def stmt: Parser[Stmt] = (
      k("syntax") ~> rep1(not(o("=") ~ s("{")) ~> anyT) ~ (o("=") ~> s("{") ~> rep(not(s("}")) ~> anyT) <~ s("}")) ^^ { case p ~ t => SyntaxDef(p, t) }
    | expr ~ o("=") ~ expr ^^ { case l ~ _ ~ r => Constraint(l, r) }
    | expr ^^ Verify.apply
    | s("{") ~> rep(stmt) <~ s("}") ^^ Block.apply
  ) <~ opt(s(";"))

  def expr: Parser[AstExpr] = (
      k("if") ~> expr ~ (k("then") ~> expr) ~ (k("else") ~> expr) ^^ { case c ~ t ~ e => If(c, t, e) }
    | s("\u005c") ~> id ~ (o("->") ~> expr) ^^ { case p ~ b => Lambda(p, b) }
    | logicOr
  )
  def logicOr: Parser[AstExpr] = logicAnd ~ rep(o("||") ~ logicAnd) ^^ { case t ~ l => l.foldLeft(t) { case (x, o ~ y) => BinOp(o, x, y) } }
  def logicAnd: Parser[AstExpr] = compare ~ rep(o("&&") ~ compare) ^^ { case t ~ l => l.foldLeft(t) { case (x, o ~ y) => BinOp(o, x, y) } }
  def compare: Parser[AstExpr] = arith ~ rep((o("==") | o("<") | o(">")) ~ arith) ^^ { case t ~ l => l.foldLeft(t) { case (x, o ~ y) => BinOp(o, x, y) } }
  def arith: Parser[AstExpr] = term ~ rep((o("+") | o("-") | o("<<") | o(">>")) ~ term) ^^ { case t ~ l => l.foldLeft(t) { case (x, o ~ y) => BinOp(o, x, y) } }
  def term: Parser[AstExpr] = fact ~ rep((o("*") | o("/")) ~ fact) ^^ { case f ~ l => l.foldLeft(f) { case (x, o ~ y) => BinOp(o, x, y) } }
  def fact: Parser[AstExpr] = anyOf(
    o("!") ~> fact ^^ { e => UnOp("!", e) },
    (id ~ o("<") ~ expr <~ o(">")) ^^ { case n ~ _ ~ i => Templated(n, i) },
    (id ~ (s("(") ~> repsep(expr, s(",")) <~ s(")"))) ^^ { case n ~ a => Apply(Var(n), a) },
    (id ~ (s("(") ~> repsep(expr, s(",")) <~ s(")"))) ^^ { case n ~ a => BinOp("=", Var(n), a.head) },
    nm ^^ Num.apply, id ^^ Var.apply, s("(") ~> expr <~ s(")")
  )
  def parse(ts: List[Token]) = program(new TR(ts))
}