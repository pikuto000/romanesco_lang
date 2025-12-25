package parser

//シンボルテーブルに登録されたラムダ式に基づいて評価する。
object interpreter{
  def eval(node:Node,sym:SymbolTable):Any = node match {
    case Apply(fun,args,func) => {
      // Direct invocation: The function in the Apply node handles evaluation of arguments and symbol table usage.
      func(args, sym)
    }
    case Atom(s) => {
      try {
        BigDecimal(s)
      } catch {
        case _:NumberFormatException => {
          if (sym.getTabKeys.exists(_ == s)) {
            val resolved = sym.look(s)
            // println(s"DEBUG: Symbol '$s' found. Resolved to: $resolved")
            eval(resolved, sym)
          } else {
            // DEBUG:
            // println(s"DEBUG: Symbol '$s' (codes: ${s.map(_.toInt).mkString(",")}) not found in symbol table. Keys: ${sym.getTabKeys.mkString(", ")}")
            s
          }
        }
      }
    }
  }
}