package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Positional,Reader}
import scala.util.matching.Regex
import scala.util.boundary

// --- Scope and Hygiene ---
case class ScopeID(id: Long)

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

  def getParent(id: ScopeID): Option[ScopeID] = parents.get(id)

  // 祖先関係の確認
  def isAncestor(ancestor: ScopeID, child: ScopeID): Boolean = {
    if (ancestor == child) true
    else parents.get(child).map(p => isAncestor(ancestor, p)).getOrElse(false)
  }
}

object NodeMetadata {
  private val metadata = new java.util.IdentityHashMap[Node, ScopeID]()
  
  // ノードに出生地（ScopeID）を刻印する
  def tag[T <: Node](node: T, scope: ScopeID): T = {
    metadata.put(node, scope)
    node
  }
  
  def getScope(node: Node): Option[ScopeID] = Option(metadata.get(node))
}

// --- AST Nodes ---
trait Node(name:String)
case class Apply(fun:String,args:Array[Node],func: (Array[Node], SymbolTable) => Any) extends Node(if(fun=="") "apply" else fun)
case class Atom(s:String) extends Node(s)

// --- Tokens ---
sealed trait rToken extends Positional
case class TWord(s: String) extends rToken
case class TLParen() extends rToken
case class TRParen() extends rToken

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

  // ノード生成時に自動で刻印するヘルパー
  private def tag[T <: Node](node: T): T = NodeMetadata.tag(node, sym.scopeID)

  // Token patterns
  def word: PackratParser[TWord] = accept("word", { case w: TWord => w })
  def lparen: PackratParser[TLParen] = accept("lparen", { case l: TLParen => l })
  def rparen: PackratParser[TRParen] = accept("rparen", { case r: TRParen => r })

  lazy val program: PackratParser[List[Node]] = rep(expr)
  lazy val expr: PackratParser[Node] = paren_expr | application | atom
  
  lazy val paren_expr: PackratParser[Node] = (lparen ~> opt(expr) <~ rparen) ^^ {
    case None => tag(Atom(""))
    case Some(node) => node
  }

  lazy val atom: PackratParser[Node] = word ^^ { case TWord(s) => tag(Atom(s)) }
  
  lazy val application: PackratParser[Node] = word.flatMap { case TWord(name) =>
    val nodeOpt = sym.get(name)
    if(nodeOpt.isDefined){
      val defNode = nodeOpt.get
      defNode match {
        case defApply: Apply =>
          println(s"DEBUG Parser: application '$name' expecting ${defApply.args.length} args")
          repN(defApply.args.length, expr).flatMap { args =>
            val node = tag(Apply(name, args.toArray, defApply.func))
            val (ok, msg) = parseEq(node, defNode)
            if(ok) {
              println(s"DEBUG Parser: application '$name' succeeded")
              success(node)
            } else {
              println(s"DEBUG Parser: application '$name' failed parseEq: $msg")
              failure(msg)
            }
          }
        case _ => failure(s"$name is not an Apply node")
      }
    } else {
      failure(s"$name is not in symbol table")
    }
  }

  //ノードの構造を比較し、ブーリアンとメッセージを返す。
  def parseEq(Node1:Node,Node2:Node):Tuple2[Boolean,String] = boundary {
    (Node1,Node2) match {
      case (Apply(fun1,args1,_),Apply(fun2,args2,_)) =>
        if(fun1 != fun2) boundary.break((false,s"name is not equal. ${fun1} and ${fun2}"))
        if(args1.length != args2.length) boundary.break((false,s"args length is not equal. ${args1.length} and ${args2.length}"))
        for(i <- 0 until args1.length){
          val (b,s) = parseEq(args1(i),args2(i))
          if(!b) boundary.break((false,s))
        }
        (true,"")
      case (_, Atom(_)) => (true,"") // 定義側のAtomは任意のノードにマッチするプレースホルダとして扱う
      case _ => (false,s"Node type is not equal. ${Node1.getClass.getName} and ${Node2.getClass.getName}")
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
  // 名前 -> (ノード, 定義されたスコープID)
  private val tab: collection.mutable.Map[String, (Node, ScopeID)] = collection.mutable.Map.empty
  
  // マングリング用ヘルパー: 名前にスコープIDを付加する
  private def mangle(key: String, id: ScopeID): String = s"${key}#${id.id}"

  // gensym counter
  private var nextGensymId = 0
  def gensym(prefix: String = "g"): String = {
    val name = s"${prefix}_$nextGensymId"
    nextGensymId += 1
    if (get(name).isDefined) gensym(prefix) else name
  }
  
  /**
   * 衛生的な名前解決
   */
  def resolve(key: String, requesterScope: ScopeID): Option[(Node, ScopeID)] = {
    // 1. まず、要求元のスコープでマングリングされた名前を直接探す
    val mangledKey = mangle(key, requesterScope)
    
    def findMangled(current: SymbolTable): Option[(Node, ScopeID)] = {
      current.tab.get(mangledKey).orElse(current.parent.flatMap(findMangled))
    }

    findMangled(this).orElse {
      // 2. 見つからなければ、通常のレキシカルスコープ解決
      def collectCandidates(current: SymbolTable): List[(Node, ScopeID)] = {
        val local = current.tab.get(key).toList
        val inherited = current.parent.map(collectCandidates).getOrElse(Nil)
        local ++ inherited
      }

      val candidates = collectCandidates(this)
      val visible = candidates.filter { case (_, defScope) =>
        ScopeGraph.isAncestor(defScope, requesterScope)
      }

      visible match {
        case Nil => None
        case (node, defScope) :: Nil => Some((node, defScope))
        case multiple =>
          val best = multiple.reduceLeft { (a, b) =>
            if (ScopeGraph.isAncestor(a._2, b._2)) b
            else if (ScopeGraph.isAncestor(b._2, a._2)) a
            else throw new RuntimeException(s"Ambiguous scope for symbol '$key': defined in both scope ${a._2.id} and ${b._2.id}")
          }
          Some(best)
      }
    }
  }

  def look(key:String):Node = {
    resolve(key, scopeID).map(_._1).getOrElse {
      throw new RuntimeException(s"Symbol '$key' not found in scope ${scopeID.id}")
    }
  }

  def lookHygienic(atom: Atom): Node = {
    val requesterScope = NodeMetadata.getScope(atom).getOrElse(scopeID)
    resolve(atom.s, requesterScope).map(_._1).getOrElse {
      // どこにもなければ、フォールバックとしてAtomの文字列を返す
      Atom(atom.s)
    }
  }
  
  def get(key: String): Option[Node] = resolve(key, scopeID).map(_._1)

  def getWithScope(key: String): Option[(Node, ScopeID)] = resolve(key, scopeID)
  
  def getTabKeys: Iterable[String] = tab.keys
  
  def set(key:String,value:Node)={
    // 定義。現在はシンプルにローカルに保存
    tab(key) = (value, scopeID)
  }

  def getFunc(key:String): (Array[Node], SymbolTable) => Any = look(key).asInstanceOf[Apply].func

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

//インデントも含めて、読みやすく出力する。
def prettyPrint(node:Node):Unit={
  def _prettyPrint(node: Node, indent: Int): Unit = {
    val indentStr = "  " * indent
    node match {
      case Apply(fun, args, _) =>
      val name = if(fun == "") "apply" else fun
      println(s"${indentStr}Apply(fun=' $name ', args=[")
      args.foreach(arg => _prettyPrint(arg, indent + 1))
      println(s"${indentStr}])")
      case Atom(s) =>
      val scope = NodeMetadata.getScope(node).map(_.id.toString).getOrElse("?")
      println(s"${indentStr}Atom(s=' $s ', scope=$scope)")
    }
  }
  _prettyPrint(node, 0)
}