package parser
import scala.util.parsing.input.{Reader}

class RomanescoContext(val name: String, val sym: SymbolTable) {
  def run(source: String): Unit = {
    val lexer = new rLexer(sym.lex)
    lexer.lex(source) match {
      case lexer.Success(tokens, _) =>
        val rParserInst = new rParser(sym)
        var currentReader: Reader[rToken] = new rParserInst.PackratReader(new rTokenReader(tokens))
        
        while (!currentReader.atEnd) {
          rParserInst.expr(currentReader) match {
            case rParserInst.Success(node, next) =>
              val expanded = Expander.expand(node, this)
              interpreter.eval(expanded, sym)
              currentReader = next
            case ns: rParserInst.NoSuccess =>
              println(s"Parse error: ${ns.msg}")
              return
          }
        }
      case ns => println(s"Lexical error: $ns")
    }
  }
}

object ContextManager {
  private val contexts = collection.mutable.Map[String, RomanescoContext]()
  def create(name: String, sym: SymbolTable): RomanescoContext = {
    val ctx = new RomanescoContext(name, sym)
    contexts(name) = ctx
    ctx
  }
}

object Expander {
  def expand(node: Node, ctx: RomanescoContext): Any = node match {
    case Apply(fun, args, _, _) =>
      ctx.sym.getProp(fun, "phase") match {
        case Some("ast") =>
          val f = ctx.sym.get(fun).get.asInstanceOf[Apply].func
          val res = f(args, ctx.sym)
          res match {
            case n: Node => expand(n, ctx)
            case other => other
          }
        case _ => 
          Apply(fun, args.map(arg => expand(arg, ctx) match {
            case n: Node => n
            case other => Atom(other.toString)
          }), ctx.sym.getFunc(fun), None)
      }
    case other => other
  }
}