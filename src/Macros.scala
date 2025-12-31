package parser
import scala.util.parsing.input.{Reader, Position}

class RomanescoContext(val name: String, val sym: SymbolTable) {
  private var lastMacroKeys = getMacroKeys()
  private var lastDelimiters = sym.tokens.getSpecials

  def run(source: String): Unit = {
    var currentSource = source
    
    while (currentSource.trim.nonEmpty) {
      lazy val lexer = new rLexer(sym.tokens)
      lexer.lex(currentSource) match {
        case lexer.Success(tokens: List[rToken], _) =>
          lazy val rParserInst = new rParser(sym)
          lazy val reader = new rParserInst.PackratReader(new rTokenReader(tokens))
          
          rParserInst.expr(reader) match {
            case rParserInst.Success(node, next) =>
              lazy val expanded = Expander.expand(node, this)
              interpreter.eval(expanded, sym)
              
              if (hasSyntaxChanged()) {
                lazy val consumedOffset = if (next.atEnd) currentSource.length else getOffset(currentSource, next.pos)
                lazy val rest = currentSource.substring(consumedOffset)
                return run(rest)
              }

              lazy val consumedOffset = if (next.atEnd) currentSource.length else getOffset(currentSource, next.pos)
              if (consumedOffset == 0) return
              currentSource = currentSource.substring(consumedOffset)
              
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

  private def hasSyntaxChanged(): Boolean = {
    lazy val currentMacros = getMacroKeys()
    lazy val currentDelims = sym.tokens.getSpecials
    if (currentMacros != lastMacroKeys || currentDelims != lastDelimiters) {
      lastMacroKeys = currentMacros
      lastDelimiters = currentDelims
      true
    } else false
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
            lazy val f = ctx.sym.getFunc(fun)
            lazy val res = f(args, ctx.sym)
            expand(res, ctx, depth + 1)
          case _ => 
            Apply(fun, args.map(arg => expand(arg, ctx, depth) match {
              case n: Node => n
              case other => 
                // 無名ノードクラス。eval 時に中身 (other) を返す。
                new Node {
                  override def eval(s: SymbolTable): Any = other
                  override def rawName: String = other.toString
                  override def toString: String = s"Wrapped($other)"
                }
            }), ctx.sym.getFunc(fun), app.customParser)
        }
      case other => other
    }
  }
}