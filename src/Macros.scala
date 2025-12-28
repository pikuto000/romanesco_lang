package parser
import scala.collection.mutable.Map
import scala.util.parsing.input.{Reader, CharSequenceReader}

// Romanescoの実行コンテキスト
class RomanescoContext(val name: String, val sym: SymbolTable) {
  val lexer = new rLexer
  
  def run(source: String): Unit = {
    lexer.lex(source) match {
      case lexer.Success(tokens, _) =>
      var reader: scala.util.parsing.input.Reader[rToken] = new rTokenReader(tokens)
      
      while (!reader.atEnd) {
        val parser = new rParser(sym)
        val packratReader = new parser.PackratReader(reader)
        
        parser.expr(packratReader) match {
          case parser.Success(node, next) =>
          // 1. マクロ展開
          val expanded = Expander.expand(node, this)
          
          // 2. 検証フェーズ
          val errors = Validator.validate(expanded, sym)
          if (errors.nonEmpty) {
            println(s"Validation errors in context '$name':")
            errors.foreach(err => println(s"  - $err"))
            return
          }
          
          // 3. 評価
          interpreter.eval(expanded, sym)
          
          reader = next
          
          case parser.NoSuccess(msg, next) =>
          println(s"Parse error at line ${next.pos.line}, column ${next.pos.column}: $msg")
          println(next.pos.longString)
          return
          
          case parser.Failure(msg, next) =>
          println(s"Parse error at line ${next.pos.line}, column ${next.pos.column}: $msg")
          return
        }
      }
      
      case lexer.NoSuccess(msg, next) =>
      throw new RuntimeException(s"[$name] Lexical error: $msg at ${next.pos}")
    }
  }
  
  def parse(source: String): List[Node] = {
    lexer.lex(source) match {
      case lexer.Success(tokens, _) =>
      val parser = new rParser(sym)
      parser.parseProgram(tokens) match {
        case parser.Success(nodes, _) => nodes
        case _ => throw new RuntimeException(s"[$name] Macro failed to parse source")
      }
      case _ => throw new RuntimeException(s"[$name] Macro failed to lex source")
    }
  }
  
  def eval(node: Node): Any = interpreter.eval(node, sym)
  def expand(node: Node): Node = Expander.expand(node, this)
}

// コンテキスト管理
object ContextManager {
  private val contexts = collection.mutable.Map[String, RomanescoContext]()
  def register(ctx: RomanescoContext): Unit = contexts(ctx.name) = ctx
  def get(name: String): RomanescoContext = contexts.getOrElse(name, throw new RuntimeException(s"Context '$name' not found"))
  def create(name: String, sym: SymbolTable): RomanescoContext = {
    val ctx = new RomanescoContext(name, sym)
    register(ctx)
    ctx
  }
}

// マクロ展開エンジン
object Expander {
  private val charMacros = collection.mutable.Set[String]()
  private val astMacros = collection.mutable.Set[String]()
  private val comptimeMacros = collection.mutable.Set[String]()
  
  def registerCharMacro(name: String): Unit = charMacros.add(name)
  def registerAstMacro(name: String): Unit = astMacros.add(name)
  def registerComptimeMacro(name: String): Unit = comptimeMacros.add(name)
  
  def isCharMacro(name: String): Boolean = charMacros.contains(name)
  def isAstMacro(name: String): Boolean = astMacros.contains(name)
  def isComptimeMacro(name: String): Boolean = comptimeMacros.contains(name)
  
  def expand(node: Node, ctx: RomanescoContext): Node = {
    val expanded = node match {
      case Apply(fun, args, _) if isAstMacro(fun) =>
      val macroFunc = ctx.sym.get(fun) match {
        case Some(Apply(_, _, f)) => f
        case _ => throw new RuntimeException(s"AST Macro implementation for '$fun' not found")
      }
      val secureSym = init.core()
      val result = macroFunc(args, secureSym)
      result match {
        case n: Node => expand(n, ctx) 
        case other => throw new RuntimeException(s"AST Macro '$fun' must return a Node, but got $other")
      }
      
      case Apply(fun, args, _) if isComptimeMacro(fun) =>
      val macroFunc = ctx.sym.get(fun) match {
        case Some(Apply(_, _, f)) => f
        case _ => throw new RuntimeException(s"Comptime Macro implementation for '$fun' not found")
      }
      val secureSym = init.core()
      val result = macroFunc(args, secureSym)
      NodeMetadata.tag(Atom(result.toString), ctx.sym.scopeID)
      
      case Apply(fun, args, f) =>
      Apply(fun, args.map(arg => expand(arg, ctx)), f)
      
      case other => other
    }
    if (NodeMetadata.getScope(expanded).isEmpty) {
      NodeMetadata.tag(expanded, ctx.sym.scopeID)
    }
    expanded
  }
}

// 検証エンジン
object Validator {
  def validate(node: Node, sym: SymbolTable): List[String] = {
    val errors = collection.mutable.ListBuffer[String]()
    
    def walk(n: Node): Unit = n match {
      case a @ Atom(s) =>
      val isNumber = try { BigDecimal(s); true } catch { case _: Exception => false }
      if (!isNumber && s.nonEmpty && s != "_") {
        NodeMetadata.getScope(a) match {
          case Some(birthScope) =>
          // 衛生的な解決を試みる
          try {
            sym.resolve(s, birthScope) match {
              case Some((_, defScope)) =>
              if (!ScopeGraph.isAncestor(defScope, birthScope)) {
                errors += s"Hygienic violation: symbol '$s' defined in scope ${defScope.id} is not reachable from birth scope ${birthScope.id}"
              }
              case None =>
            }
          } catch {
            case e: RuntimeException => errors += e.getMessage
          }
          case None =>
        }
      }
      
      case Apply(_, args, _) =>
      args.foreach(walk)
    }
    
    walk(node)
    errors.toList
  }
}
