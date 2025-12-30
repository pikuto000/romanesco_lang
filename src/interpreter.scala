package parser

object interpreter {
  private val MAX_EVAL_DEPTH = 1000
  private var count=0
  def eval(node: Any, sym: SymbolTable, depth: Int = 0): Any = {
    if (depth > MAX_EVAL_DEPTH) throw new RuntimeException(s"Stack overflow: $node")
    if (count==100000){
      println("loop detected")
      sys.exit(1)
    }
    count+=1
    node match {
      case app: Apply => 
        app.func(app.args, sym)

      case a @ Atom(s) =>
        try { BigDecimal(s.trim) }
        catch { case _: NumberFormatException =>
          sym.get(s) match {
            case Some(v: Node) => if (v == node) v else eval(v, sym, depth + 1)
            case Some(v) => v 
            case None => s
          }
        }

      case other => other
    }
  }
}