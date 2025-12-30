package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Positional,Reader}
import scala.util.matching.Regex
import scala.util.boundary

// --- Scope and Hygiene ---
case class ScopeID(id: Long)

// Generic Symbol Entry with property map for metadata
case class SymbolEntry(node: Node, scope: ScopeID, props: Map[String, String] = Map.empty)

object ScopeGraph {
  private val parents = collection.mutable.Map[ScopeID, ScopeID]()
  private var nextId = 0L

  def generate(): ScopeID = {
    val id = ScopeID(nextId)
    nextId += 1
    id
  }

  def link(child: ScopeID, parent: ScopeID): Unit = {
    parents(child) = parent
  }

  def isAncestor(ancestor: ScopeID, child: ScopeID): Boolean = {
    if (ancestor == child) true
    else parents.get(child).map(p => isAncestor(ancestor, p)).getOrElse(false)
  }
}

object NodeMetadata {
  private val metadata = new java.util.IdentityHashMap[Node, ScopeID]()
  
  def tag[T <: Node](node: T, scope: ScopeID): T = {
    metadata.put(node, scope)
    node
  }
  
  def getScope(node: Node): Option[ScopeID] = Option(metadata.get(node))
}

// --- AST Nodes ---
trait Node(name:String)

type CustomParser = (rParser, scala.util.parsing.input.Reader[rToken]) => scala.util.parsing.combinator.Parsers#ParseResult[Array[Node]]

case class Apply(
  fun:String,
  args:Array[Node],
  func: (Array[Node], SymbolTable) => Any,
  customParser: Option[CustomParser] = None
) extends Node(if(fun=="") "apply" else fun)

case class Atom(s:String) extends Node(s)

// --- Tokens ---
sealed trait rToken extends Positional
case class TWord(s: String) extends rToken

// --- Token Reader ---
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

  def word: PackratParser[TWord] = accept("word", { case w: TWord => w })
  
  def token(s: String): PackratParser[TWord] = accept(s"token '$s'", { case w: TWord if w.s == s => w })
  
  def anyAtom: PackratParser[Node] = accept("any token", { case w: TWord => tag(Atom(w.s)) })

  lazy val program: PackratParser[List[Node]] = rep(expr)
  lazy val expr: PackratParser[Node] = paren_expr | application | atom
  
  lazy val paren_expr: PackratParser[Node] = (token("(") ~> opt(expr) <~ token(")")) ^^ {
    case None => tag(Atom(""))
    case Some(node) => node
  }

  lazy val atom: PackratParser[Node] = word ^^ { case TWord(s) => tag(Atom(s)) }
  
  lazy val application: PackratParser[Node] = word.flatMap { case TWord(name) =>
    sym.resolveEntry(name, sym.scopeID) match {
      case Some(entry) =>
        val defNode = entry.node
        defNode match {
          case defApply: Apply =>
            if (defApply.customParser.isDefined) {
              new Parser[Node] {
                def apply(in: Input): ParseResult[Node] = {
                  val result = defApply.customParser.get(rParser.this, in)
                  result match {
                    case Success(args, next) =>
                      val node = tag(Apply(name, args, defApply.func, defApply.customParser))
                      Success(node, next.asInstanceOf[Input])
                    case NoSuccess(msg, next) =>
                      Failure(msg, next.asInstanceOf[Input])
                  }
                }
              }
            } else {
              repN(defApply.args.length, expr).flatMap { args =>
                val node = tag(Apply(name, args.toArray, defApply.func))
                val (ok, msg) = parseEq(node, defNode)
                if(ok) success(node)
                else failure(msg)
              }
            }
          case _ => failure(s"$name is not an Apply node")
        }
      case None => failure(s"$name is not in symbol table")
    }
  }

  def parseEq(Node1:Node,Node2:Node):Tuple2[Boolean,String] = boundary {
    (Node1,Node2) match {
      case (Apply(fun1,args1,_,_),Apply(fun2,args2,_,_)) =>
        if(fun1 != fun2) boundary.break((false,s"name mismatch"))
        if(args1.length != args2.length) boundary.break((false, "arity mismatch"))
        for(i <- 0 until args1.length){
          val (b,s) = parseEq(args1(i),args2(i))
          if(!b) boundary.break((false,s))
        }
        (true,"")
      case (_, Atom(_)) => (true,"")
      case _ => (false,s"type mismatch")
    }
  }

  def parse(tokens: List[rToken]): ParseResult[Node] = {
    val reader = new PackratReader[rToken](new rTokenReader(tokens))
    expr(reader)
  }

  def parseProgram(tokens: List[rToken]): ParseResult[List[Node]] = {
    val reader = new PackratReader[rToken](new rTokenReader(tokens))
    program(reader)
  }
}

class SymbolTable(val parent: Option[SymbolTable] = None, val scopeID: ScopeID = ScopeGraph.generate()){
  private val tab: collection.mutable.Map[String, SymbolEntry] = collection.mutable.Map.empty
  val lex: LexerTable = new LexerTable(parent.map(_.lex))
  
  // Z3 Context for SMT solving (shared across scope hierarchy)
  val z3: com.microsoft.z3.Context = parent.map(_.z3).getOrElse(new com.microsoft.z3.Context())

  private var nextGensymId = 0
  def gensym(prefix: String = "g"): String = {
    val name = s"${prefix}_$nextGensymId"
    nextGensymId += 1
    if (get(name).isDefined) gensym(prefix) else name
  }

  private def mangle(key: String, id: ScopeID): String = s"${key}#${id.id}"

  def resolveEntry(key: String, requesterScope: ScopeID): Option[SymbolEntry] = {
    val mangledKey = mangle(key, requesterScope)
    
    def findMangled(current: SymbolTable): Option[SymbolEntry] = {
      current.tab.get(mangledKey).orElse(current.parent.flatMap(findMangled))
    }

    findMangled(this).orElse {
      def collectCandidates(current: SymbolTable): List[SymbolEntry] = {
        val local = current.tab.get(key).toList
        val inherited = current.parent.map(collectCandidates).getOrElse(Nil)
        local ++ inherited
      }

      val candidates = collectCandidates(this)
      val visible = candidates.filter { entry =>
        ScopeGraph.isAncestor(entry.scope, requesterScope)
      }

      visible match {
        case Nil => None
        case entry :: Nil => Some(entry)
        case multiple =>
          multiple.reduceLeft { (a, b) =>
            if (ScopeGraph.isAncestor(a.scope, b.scope)) b
            else if (ScopeGraph.isAncestor(b.scope, a.scope)) a
            else throw new RuntimeException(s"Ambiguous scope for symbol '$key'")
          } match { case entry => Some(entry) }
      }
    }
  }

  def look(key:String):Node = {
    resolveEntry(key, scopeID).map(_.node).getOrElse {
      throw new RuntimeException(s"Symbol '$key' not found")
    }
  }

  def lookHygienic(atom: Atom): Node = {
    val requesterScope = NodeMetadata.getScope(atom).getOrElse(scopeID)
    resolveEntry(atom.s, requesterScope).map(_.node).getOrElse(Atom(atom.s))
  }

  // Retrieve generic property from the environment
  def getProp(key: String, propName: String): Option[String] = {
    resolveEntry(key, scopeID).flatMap(_.props.get(propName))
  }
  
  def get(key: String): Option[Node] = resolveEntry(key, scopeID).map(_.node)
  
  def set(key:String, value:Node, props: Map[String, String] = Map.empty) = {
    tab(key) = SymbolEntry(value, scopeID, props)
  }

  def getFunc(key:String): (Array[Node], SymbolTable) => Any = {
    look(key) match {
      case Apply(_, _, f, _) => f
      case _ => throw new RuntimeException(s"Symbol '$key' is not a function")
    }
  }

  def cloneTable(): SymbolTable = {
    val newTable = new SymbolTable(parent, scopeID)
    newTable.tab ++= this.tab
    newTable
  }
  
  def extend(): SymbolTable = {
    val newID = ScopeGraph.generate()
    ScopeGraph.link(newID, this.scopeID)
    new SymbolTable(Some(this), newID)
  }
}

def prettyPrint(node:Node):Unit={
  def _prettyPrint(node: Node, indent: Int): Unit = {
    val indentStr = "  " * indent
    node match {
      case Apply(fun, args, _, _) =>
      println(s"${indentStr}Apply(fun='$fun', args=[")
      args.foreach(arg => _prettyPrint(arg, indent + 1))
      println(s"${indentStr}])")
      case Atom(s) =>
      val scope = NodeMetadata.getScope(node).map(_.id.toString).getOrElse("?")
      println(s"${indentStr}Atom(s='$s', scope=$scope)")
    }
  }
  _prettyPrint(node, 0)
}