package parser

//シンボルテーブルに登録されたラムダ式に基づいて評価する。
object interpreter{
  def eval(node:Node,sym:SymbolTable):Any = node match {
    case Apply(fun,args,func) => {
      // Direct invocation: All logic (partial application, structural apply) is handled by the function itself.
      func(args, sym)
    }
    case Atom(s) => {
      try {
        BigDecimal(s)
      } catch {
        case _:NumberFormatException => {
          if (sym.getTabKeys.exists(_ == s)) {
            lazy val resolved = sym.look(s)
            eval(resolved, sym)
          } else {
            s
          }
        }
      }
    }
  }
}