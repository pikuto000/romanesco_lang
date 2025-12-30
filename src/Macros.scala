package parser
import scala.collection.mutable.Map
import scala.util.parsing.input.{Reader, CharSequenceReader}

// Romanescoの実行コンテキスト
class RomanescoContext(val name: String, val sym: SymbolTable) {
  private def createLexer() = new rLexer(sym.lex)
  
  def run(source: String): Unit = {
    var retry = true
    var skipCount = 0
    
    while (retry) {
      retry = false
      val lexer = createLexer()
      val currentLexerState = sym.lex.getDelimiters
      
      lexer.lex(source) match {
        case lexer.Success(tokens, _) =>
          val reader = new rTokenReader(tokens)
          val parser = new rParser(sym)
          val packratReader = new parser.PackratReader(reader)
          var currentReader: Reader[rToken] = packratReader
          
          var processedNodes = 0
          var hasError = false
          var errorMsg = ""
          var errorPos: scala.util.parsing.input.Position = scala.util.parsing.input.NoPosition
          
          try {
            while (!currentReader.atEnd && !hasError && !retry) {
              parser.expr(currentReader) match {
                case parser.Success(node, next) =>
                  processedNodes += 1
                  if (processedNodes > skipCount) {
                    val expanded = Expander.expand(node, this)
                    
                    val errors = Validator.validate(expanded, sym)
                    if (errors.nonEmpty) {
                      println(s"Validation errors:")
                      errors.foreach(e => println(s" - $e"))
                      return
                    }
                    
                    interpreter.eval(expanded, sym)
                    
                    val postLexerState = sym.lex.getDelimiters
                    if (currentLexerState != postLexerState) {
                      skipCount = processedNodes
                      retry = true
                    }
                  }
                  currentReader = next
                  
                case ns: parser.NoSuccess =>
                  hasError = true
                  errorMsg = ns.msg
                  errorPos = ns.next.pos
              }
            }
            
            if (hasError && !retry) {
              if (sym.lex.getDelimiters != currentLexerState) {
                retry = true
              } else {
                println(s"Parse error: $errorMsg at $errorPos")
              }
            }

          } catch {
            case e: Throwable =>
              println(s"Runtime error: ${e.getMessage}")
              return
          }

        case lexer.NoSuccess(msg, next) =>
          println(s"Lexical error: $msg at ${next.pos}")
          return
      }
    }
  }
  
  def parse(source: String): List[Node] = {
    val lexer = createLexer()
    lexer.lex(source) match {
      case lexer.Success(tokens, _) =>
      val parser = new rParser(sym)
      parser.parseProgram(tokens) match {
        case parser.Success(nodes, _) => nodes
        case ns: parser.NoSuccess => throw new RuntimeException(s"[$name] Parse error: ${ns.msg}")
      }
      case lexer.NoSuccess(msg, next) => throw new RuntimeException(s"[$name] Lexical error: $msg")
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

// マクロ展開エンジン (Stateless)
object Expander {
  def expand(node: Node, ctx: RomanescoContext): Node = {
    val expanded = node match {
      case Apply(fun, args, _, _) =>
        ctx.sym.getProp(fun, "phase") match {
          case Some("ast") =>
            val macroFunc = ctx.sym.getFunc(fun)
            val result = macroFunc(args, ctx.sym)
            result match {
              case n: Node => expand(n, ctx) 
              case other => throw new RuntimeException(s"AST Macro '$fun' must return a Node, but got $other")
            }
          case Some("comptime") =>
            val macroFunc = ctx.sym.getFunc(fun)
            val result = macroFunc(args, ctx.sym)
            NodeMetadata.tag(Atom(result.toString), ctx.sym.scopeID)
          case _ =>
            val f = ctx.sym.getFunc(fun)
            Apply(fun, args.map(arg => expand(arg, ctx)), f, node.asInstanceOf[Apply].customParser)
        }
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
              try {
                sym.resolveEntry(s, birthScope) match {
                  case Some(entry) =>
                    if (!ScopeGraph.isAncestor(entry.scope, birthScope)) {
                      errors += s"Hygienic violation: symbol '$s' defined in scope ${entry.scope.id} is not reachable from birth scope ${birthScope.id}"
                    }
                  case None =>
                }
              } catch {
                case e: RuntimeException => errors += e.getMessage
              }
            case None =>
          }
        }
      case Apply(_, args, _, _) =>
        args.foreach(walk)
    }
    walk(node)
    errors.toList
  }
}
