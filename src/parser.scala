package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Positional,Reader}
import scala.util.matching.Regex
import scala.util.boundary
import com.microsoft.z3._

case class ScopeID(id: Long)

case class SymbolEntry(node: Node, props: Map[String, String] = Map.empty)

object ScopeGraph {
  private var nextId = 0L
  def generate(): ScopeID = { val id = ScopeID(nextId); nextId += 1; id }
}

object NodeMetadata {
  private val metadata = new java.util.IdentityHashMap[Node, ScopeID]()
  def tag[T <: Node](node: T, scope: ScopeID): T = { metadata.put(node, scope); node }
  def getScope(node: Node): Option[ScopeID] = Option(metadata.get(node))
}

// --- AST Nodes with Methods ---
trait Node(val name:String) {
  def eval(sym: SymbolTable): Any
  def rawName: String
}

type CustomParser = (rParser, scala.util.parsing.input.Reader[rToken]) => scala.util.parsing.combinator.Parsers#ParseResult[Array[Node]]

case class Apply(
  fun:String, 
  args:Array[Node], 
  func: (Array[Node], SymbolTable) => Any, 
  customParser: Option[CustomParser] = None
) extends Node(if(fun=="") "apply" else fun) {
  
  override def eval(sym: SymbolTable): Any = func(args, sym)
  override def rawName: String = fun
}

case class Atom(s:String) extends Node(s) {
  override def eval(sym: SymbolTable): Any = {
    // Try to parse as number first
    try { BigDecimal(s) } 
    catch { case _: NumberFormatException => 
      // Look up in symbol table
      val resolvedNode = sym.lookHygienic(this)
      if (resolvedNode eq this) {
        s // Literal string fallback
      } else {
        interpreter.eval(resolvedNode, sym)
      }
    }
  }
  override def rawName: String = s
}

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
class rParser(sym:SymbolTable) extends Parsers with PackratParsers {
  override type Elem = rToken
  def tag[T <: Node](node: T): T = NodeMetadata.tag(node, sym.scopeID)
  def word: PackratParser[TWord] = accept("word", { case w: TWord if w.s != "(" && w.s != ")" => w })
  def token(s: String): PackratParser[TWord] = accept(s"token '$s'", { case w: TWord if w.s == s => w })
  def anyAtom: PackratParser[Node] = accept("any token", { case w: TWord if w.s != "(" && w.s != ")" => tag(Atom(w.s)) })

  lazy val program: PackratParser[List[Node]] = rep(expr)
  lazy val expr: PackratParser[Node] = paren_expr | application | atom
  lazy val paren_expr: PackratParser[Node] = (token("(") ~> opt(expr) <~ token(")")) ^^ { case None => tag(Atom("")) case Some(node) => node }
  lazy val atom: PackratParser[Node] = word ^^ { case TWord(s) => tag(Atom(s)) }
  
  lazy val application: PackratParser[Node] = word.flatMap { case TWord(name) =>
    sym.get(name) match {
      case Some(defNode) =>
        defNode match {
          case defApply: Apply =>
            if (defApply.customParser.isDefined) {
              new Parser[Node] {
                def apply(in: Input): ParseResult[Node] = {
                  val result = defApply.customParser.get(rParser.this, in)
                  result match {
                    case Success(args, next) => Success(tag(Apply(name, args, defApply.func, defApply.customParser)), next.asInstanceOf[Input])
                    case NoSuccess(msg, next) => Failure(msg, next.asInstanceOf[Input])
                  }
                }
              }
            } else {
              repN(defApply.args.length, expr).map { args => tag(Apply(name, args.toArray, defApply.func, None)) }
            }
          case _ => failure(s"$name is not an Apply node")
        }
      case None => failure(s"$name is not in symbol table")
    }
  }

  def parseProgram(tokens: List[rToken]): ParseResult[List[Node]] = program(new PackratReader(new rTokenReader(tokens)))
}

// --- Symbol Table ---
class SymbolTable(val parent: Option[SymbolTable] = None, val scopeID: ScopeID = ScopeGraph.generate()){
  private val tab: collection.mutable.Map[String, SymbolEntry] = collection.mutable.Map.empty
  val lex: LexerTable = new LexerTable(parent.map(_.lex))
  val z3: Context = parent.map(_.z3).getOrElse(new Context())
  val logicalVars: collection.mutable.Map[String, IntExpr] = parent.map(_.logicalVars).getOrElse(collection.mutable.Map.empty)

  def get(key: String): Option[Node] = tab.get(key).map(_.node).orElse(parent.flatMap(_.get(key)))
  def getProp(key: String, propName: String): Option[String] = tab.get(key).flatMap(_.props.get(propName)).orElse(parent.flatMap(_.getProp(key, propName)))
  def set(key:String, value:Node, props: Map[String, String] = Map.empty) = { tab(key) = SymbolEntry(value, props) }
  def getTabKeys: Iterable[String] = tab.keys ++ parent.map(_.getTabKeys).getOrElse(Nil)
  
  def gensym(prefix: String = "g"): String = {
    var id = 0
    def find(): String = { val n = s"${prefix}_$id"; id += 1; if (get(n).isDefined) find() else n }
    find()
  }

  def lookHygienic(atom: Atom): Node = get(atom.s).getOrElse(atom)
  def getFunc(key:String): (Array[Node], SymbolTable) => Any = {
    get(key) match { case Some(Apply(_, _, f, _)) => f case _ => throw new RuntimeException(s"Symbol '$key' is not a function") }
  }
  def extend(): SymbolTable = new SymbolTable(Some(this), ScopeGraph.generate())
  def close(): Unit = if (parent.isEmpty) z3.close()
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