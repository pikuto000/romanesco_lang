package parser

object interpreter {
  private val MAX_EVAL_DEPTH = 1000
  
  def eval(node: Any, sym: SymbolTable, depth: Int = 0): Any = {
    if (depth > MAX_EVAL_DEPTH) throw new RuntimeException(s"Stack overflow: $node")
    
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

      case n: Node =>
        // Node トレイトを実装した無名オブジェクトや、その他の Node
        val res = n.eval(sym)
        // もし eval した結果が自分自身（無名ノードなら other を返すように実装する）なら
        // その値を返す。
        res

      case other => other
    }
  }
}