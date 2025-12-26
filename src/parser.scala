package parser
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position,Positional}
import scala.util.matching.Regex
import scala.util.boundary
import java.lang.invoke.SerializedLambda

trait Node(name:String)
case class Apply(fun:String,args:Array[Node],func: (Array[Node], SymbolTable) => Any) extends Node(if(fun=="") "apply" else fun)
case class Atom(s:String) extends Node(s)


class rParser(sym:SymbolTable) extends RegexParsers,Positional,PackratParsers{
  override val skipWhitespace=true
  //コメントアウトも含めた空白をスキップする
  override protected val whiteSpace: Regex = """(\s|//.*|(?m)/\*(\n|.)*?\*/)+""".r
  lazy val program:PackratParser[List[Node]] = rep(expr)
  lazy val expr:PackratParser[Node] = paren_expr|application|atom
  lazy val paren_expr:PackratParser[Node] = "(" ~> expr <~ ")"
  //括弧と空白以外のすべての文字をパースする。
  lazy val atom:PackratParser[Node] = """[^\s()]+""".r ^^ {
    case s:String => Atom(s)
  }
  
  lazy val application:PackratParser[Node] = ("""[^\s()]+""".r).flatMap { name =>
    if(sym.getTabKeys.exists(_ == name)){
      val defNode = sym.look(name)
      defNode match {
        case Apply(_, defArgs, funcExpr) =>
          repN(defArgs.length, expr).flatMap { args =>
            val node = Apply(name, args.toArray, funcExpr)
            val (ok, msg) = parseEq(node, defNode)
            if(ok) success(node)
            else failure(msg)
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
}

class SymbolTable{
  private var tab:Map[String,Node] = Map.empty
  def look(key:String):Node = tab(key)
  def getTabKeys: Iterable[String] = tab.keys // Changed from tab.keys to tab.keys
  def set(key:String,value:Node)={
    tab+=key->value
  }
  //Nodeのラムダ式を取得する。
  def getFunc(key:String): (Array[Node], SymbolTable) => Any = tab(key).asInstanceOf[Apply].func
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
      println(s"${indentStr}Atom(s=' $s ')")
    }
  }
  _prettyPrint(node, 0)
}
