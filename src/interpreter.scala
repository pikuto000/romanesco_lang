package parser

object interpreter {
  private val MAX_EVAL_DEPTH = 1000
  
  def eval(node: Any, sym: SymbolTable, depth: Int = 0): Any = {
    if (depth > MAX_EVAL_DEPTH) throw new RuntimeException(s"Stack overflow: $node")
    
    node match {
      case app: Apply => app.func(app.args, sym)

      case a @ Atom(s, _) =>
        sym.get(a.mangledName).orElse(sym.get(s)) match {
          case Some(v: Node) => if (v == node) v else eval(v, sym, depth + 1)
          case Some(v) => v 
          case None => 
            try { BigDecimal(s.trim) }
            catch { case _: NumberFormatException => s }
        }

      case n: Node => n.eval(sym)
      case other => other
    }
  }
}