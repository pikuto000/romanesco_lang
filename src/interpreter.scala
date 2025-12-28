package parser

//シンボルテーブルに登録されたラムダ式に基づいて評価する。
object interpreter{
  def eval(node:Node,sym:SymbolTable):Any = node match {
    case Apply(fun,args,func) => {
      // Direct invocation
      func(args, sym)
    }
    case a @ Atom(s) => {
      // Try to parse as number first
      val asNumber = try { Some(BigDecimal(s)) } catch { case _: NumberFormatException => None }
      
      asNumber.getOrElse {
        // Hygienic lookup based on the Atom's birth scope
        val resolvedNode = sym.lookHygienic(a)
        resolvedNode match {
          case `a` => s // If it resolved to itself, it's a literal string
          case Apply(_, defArgs, func) if defArgs.length > 0 => func // Operator value
          case other => eval(other, sym) // Constant or variable
        }
      }
    }
  }
}