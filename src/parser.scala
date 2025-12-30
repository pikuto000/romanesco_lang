package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Positional,Reader}

// --- AST Nodes ---
trait Node {
  def eval(sym: SymbolTable): Any
  def rawName: String
}

case class Apply(
  fun: String, 
  args: Array[Node], 
  func: (Array[Node], SymbolTable) => Any, 
  customParser: Option[CustomParser] = None
) extends Node {
  override def eval(sym: SymbolTable): Any = func(args, sym)
  override def rawName: String = fun
}

case class Atom(s: String) extends Node {
  override def eval(sym: SymbolTable): Any = interpreter.eval(this, sym)
  override def rawName: String = s
}

case class ConstantNode(v: Any) extends Node {
  override def eval(sym: SymbolTable): Any = v
  override def rawName: String = v.toString
}

type CustomParser = (rParser, scala.util.parsing.input.Reader[rToken]) => scala.util.parsing.combinator.Parsers#ParseResult[Array[Node]]

// --- Tokens ---
sealed trait rToken extends Positional
case class TWord(s: String) extends rToken

class rTokenReader(tokens: Seq[rToken]) extends Reader[rToken] {
  override def first: rToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def rest: Reader[rToken] = new rTokenReader(tokens.tail)
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(scala.util.parsing.input.NoPosition)
}

// --- Parser ---
class rParser(sym: SymbolTable) extends Parsers with PackratParsers {
  override type Elem = rToken
  
  def anyWord: PackratParser[TWord] = accept("word", { case w: TWord if w.s != "(" && w.s != ")" => w })
  def token(s: String): PackratParser[TWord] = accept(s"token '$s'", { case w: TWord if w.s == s => w })

  lazy val program: PackratParser[List[Node]] = rep(expr)
  lazy val expr: PackratParser[Node] = paren_apply | bare_apply | atom
  
  lazy val atom: PackratParser[Node] = anyWord ^^ { case TWord(s) => Atom(s) }

  lazy val paren_apply: PackratParser[Node] = (token("(") ~> anyWord) >> { case TWord(name) =>
    sym.get(name).collect { case a: Apply => a } match {
      case Some(defApply) =>
        val p = if (defApply.customParser.isDefined) {
          new Parser[Array[Node]] {
            def apply(in: Input): ParseResult[Array[Node]] = {
              val res = defApply.customParser.get(rParser.this, in)
              res.asInstanceOf[ParseResult[Array[Node]]]
            }
          }
        } else {
          repN(defApply.args.length, expr).map(_.toArray)
        }
        (p <~ token(")")) ^^ { args => Apply(name, args, defApply.func, defApply.customParser) }
      case _ => failure(s"Undefined function: $name")
    }
  }

  lazy val bare_apply: PackratParser[Node] = anyWord >> { case TWord(name) =>
    sym.get(name).collect { case a: Apply => a } match {
      case Some(defApply) if defApply.args.length > 0 =>
        repN(defApply.args.length, expr).map(args => Apply(name, args.toArray, defApply.func, defApply.customParser))
      case _ => failure("not a bare apply")
    }
  }

  def parseProgram(tokens: List[rToken]): ParseResult[List[Node]] = program(new PackratReader(new rTokenReader(tokens)))
}

def prettyPrint(node:Node):Unit={
  def _p(n: Node, i: Int): Unit = {
    val s = "  " * i; n match { 
      case a: Apply => println(s"$s Apply(${a.fun})"); a.args.foreach(_p(_, i + 1)) 
      case a: Atom => println(s"$s Atom(${a.s})") 
      case c: ConstantNode => println(s"$s Constant(${c.v})") 
    }
  }
  _p(node, 0)
}
