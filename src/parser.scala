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

// Atom に scopeID を追加（マングリング用）
case class Atom(s: String, scopeID: Option[Int] = None) extends Node {
  override def eval(sym: SymbolTable): Any = interpreter.eval(this, sym)
  override def rawName: String = s
  
  // マングリングされた名前を返す
  def mangledName: String = scopeID match {
    case Some(id) => s"${s}__$id"
    case None => s
  }
}

// クォートされたノード（未評価の AST）
case class QuotedNode(node: Node) extends Node {
  override def eval(sym: SymbolTable): Any = node
  override def rawName: String = s"quoted(${node.rawName})"
}

type CustomParser = (rParser, scala.util.parsing.input.Reader[rToken]) => scala.util.parsing.combinator.PackratParsers#ParseResult[Node]

// --- Reader ---
class rTokenReader(tokens: Seq[rToken]) extends Reader[rToken] {
  override def first: rToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def rest: Reader[rToken] = new rTokenReader(tokens.tail)
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(scala.util.parsing.input.NoPosition)
}

// --- Parser ---
class rParser(sym: SymbolTable) extends Parsers with PackratParsers {
  override type Elem = rToken
  
  lazy val wordName: PackratParser[String] = accept("word", { case TWord(s) => s })
  lazy val  anyName: PackratParser[String] = accept("any name", { case t: rToken if t.s != "(" && t.s != ")" => t.s })
  def lit(s:String): PackratParser[String] ={
    accept(s"literal '$s'", { case t: rToken if t.s == s => t.s })
  }

  lazy val program: PackratParser[List[Node]] = rep(expr)
  lazy val expr: PackratParser[Node] = paren_apply | bare_apply | atom
  
  lazy val atom: PackratParser[Node] = wordName ^^ { s => Atom(s) }

  lazy val paren_apply: PackratParser[Node] = (lit("(") ~> anyName) >> { funName =>
    sym.get(funName).collect { case a: Apply => a } match {
      case Some(defApply) =>
        if (defApply.customParser.isDefined) {
          new PackratParser[Node] {
            def apply(in: Input): ParseResult[Node] = {
              defApply.customParser.get(rParser.this, in).asInstanceOf[ParseResult[Node]]
            }
          }
        } else {
          repN(defApply.args.length, expr).map(args => Apply(funName, args.toArray, defApply.func, defApply.customParser))
        } <~ lit(")")
      case _ => failure(s"Undefined function: $funName")
    }
  }

  lazy val bare_apply: PackratParser[Node] = anyName >> { funName =>
    sym.get(funName).collect { case a: Apply => a } match {
      case Some(defApply) =>
        if (defApply.customParser.isDefined) {
          new PackratParser[Node] {
            def apply(in: Input): ParseResult[Node] = {
              defApply.customParser.get(rParser.this, in).asInstanceOf[ParseResult[Node]]
            }
          }
        } else if (defApply.args.length > 0) {
          repN(defApply.args.length, expr).map(args => Apply(funName, args.toArray, defApply.func, defApply.customParser))
        } else {
          failure("not a bare apply")
        }
      case _ => failure("not a bare apply")
    }
  }

  def parseProgram(tokens: List[rToken]): ParseResult[List[Node]] = program(new PackratReader(new rTokenReader(tokens)))
}

def prettyPrint(node:Node):Unit={
  def _p(n: Node, i: Int): Unit = {
    val s = "  " * i; n match { 
      case a: Apply => println(s"$s Apply(${a.fun})"); a.args.foreach(_p(_, i + 1)) 
      case a: Atom => println(s"$s Atom(${a.s}, scope=${a.scopeID})") 
      case q: QuotedNode => println(s"$s Quoted"); _p(q.node, i + 1)
      case other => println(s"$s AnonymousNode($other)")
    }
  }
  _p(node, 0)
}