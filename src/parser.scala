package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Positional,Reader}
import com.microsoft.z3._

// --- AST Nodes ---
trait Node { def rawName: String }
case class Apply(fun: String, args: Array[Node], func: (Array[Node], SymbolTable) => Any, customParser: Option[CustomParser] = None) extends Node {
  def rawName: String = fun
}
case class Atom(s: String) extends Node {
  def rawName: String = s
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
  def anyToken: PackratParser[TWord] = accept("any token", { case w: TWord => w })
  def word: PackratParser[TWord] = accept("word", { case w: TWord if w.s != "(" && w.s != ")" => w })
  def token(s: String): PackratParser[TWord] = accept(s"token '$s'", { case w: TWord if w.s == s => w })
  
  lazy val program: PackratParser[List[Node]] = rep(expr)
  lazy val expr: PackratParser[Node] = atom | paren_apply
  lazy val atom: PackratParser[Node] = word ^^ { case TWord(s) => Atom(s) }

  lazy val paren_apply: PackratParser[Node] = (token("(") ~> anyToken) >> { case TWord(name) =>
    sym.get(name) match {
      case Some(defApply: Apply) =>
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

  def parseProgram(tokens: List[rToken]): ParseResult[List[Node]] = program(new PackratReader(new rTokenReader(tokens)))
}

// --- Symbol Table ---
class SymbolTable(val parent: Option[SymbolTable] = None) {
  private val tab = collection.mutable.Map[String, (Any, Map[String, String])]()
  val lex: LexerTable = new LexerTable(parent.map(_.lex))
  val z3: Context = parent.map(_.z3).getOrElse(new Context())
  val logicalVars = collection.mutable.Map[String, IntExpr]()

  def get(key: String): Option[Any] = tab.get(key).map(_._1).orElse(parent.flatMap(_.get(key)))
  def getProp(key: String, p: String): Option[String] = tab.get(key).flatMap(_._2.get(p)).orElse(parent.flatMap(_.getProp(key, p)))
  def set(key: String, v: Any, props: Map[String, String] = Map.empty) = tab(key) = (v, props)
  def getTabKeys: Iterable[String] = tab.keys ++ parent.map(_.getTabKeys).getOrElse(Nil)
  def extend(): SymbolTable = new SymbolTable(Some(this))
  def close(): Unit = if (parent.isEmpty) z3.close()
  
  def getFunc(key:String): (Array[Node], SymbolTable) => Any = {
    get(key) match { case Some(Apply(_, _, f, _)) => f case _ => throw new RuntimeException(s"Symbol '$key' is not a function") }
  }
}

def prettyPrint(node:Node):Unit={
  def _p(n: Node, i: Int): Unit = {
    val s = "  " * i
    n match {
      case a: Apply => println(s"$s Apply(${a.fun})"); a.args.foreach(_p(_, i + 1))
      case a: Atom => println(s"$s Atom(${a.s})")
    }
  }
  _p(node, 0)
}
