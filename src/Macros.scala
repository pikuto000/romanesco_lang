package parser
import scala.util.parsing.input.{Reader, Position}

class RomanescoContext(val name: String, val sym: SymbolTable) {
  private var lastMacroKeys = getMacroKeys()

  def run(source: String): Unit = {
    var currentSource = source
    
    while (currentSource.trim.nonEmpty) {
      val lexer = new rLexer(sym.lex)
      lexer.lex(currentSource) match {
        case lexer.Success(tokens, _) =>
          val rParserInst = new rParser(sym)
          val reader = new rParserInst.PackratReader(new rTokenReader(tokens))
          
          rParserInst.expr(reader) match {
            case rParserInst.Success(node, next) =>
              val expanded = Expander.expand(node, this)
              interpreter.eval(expanded, sym)
              
              val consumedOffset = if (next.atEnd) currentSource.length else getOffset(currentSource, next.pos)
              if (consumedOffset == 0) return
              currentSource = currentSource.substring(consumedOffset)
              
              lastMacroKeys = getMacroKeys()
              
            case ns: rParserInst.NoSuccess =>
              println(s"Parse error: ${ns.msg} at ${ns.next.pos}")
              return
          }
        case ns => 
          println(s"Lexical error: $ns")
          return
      }
    }
  }

  private def getMacroKeys(): Set[String] = {
    sym.getTabKeys.filter(k => sym.getProp(k, "phase").contains("ast")).toSet
  }

  private def getOffset(src: String, pos: Position): Int = {
    var line = 1; var col = 1; var offset = 0
    while (offset < src.length && (line < pos.line || (line == pos.line && col < pos.column))) {
      if (src.charAt(offset) == '\n') { line += 1; col = 1 } else { col += 1 }
      offset += 1
    }
    offset
  }
}

object Expander {
  def expand(node: Any, ctx: RomanescoContext, depth: Int = 0): Any = {
    if (depth > 100) throw new RuntimeException("Expansion too deep")
    node match {
      case app @ Apply(fun, args, _, _) =>
        ctx.sym.getProp(fun, "phase") match {
          case Some("ast") =>
            val f = ctx.sym.getFunc(fun)
            val res = f(args, ctx.sym)
            expand(res, ctx, depth + 1)
          case _ => 
            Apply(fun, args.map(arg => expand(arg, ctx, depth) match {
              case n: Node => n
              case other => ConstantNode(other)
            }), ctx.sym.getFunc(fun), app.customParser)
        }
      case other => other
    }
  }
}
