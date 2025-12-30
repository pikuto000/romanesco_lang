package parser
import parser.Node
import parser.Apply
import parser.SymbolTable

//シンボルテーブルに登録されたラムダ式に基づいて評価する。
object interpreter {
  def eval(node:Any, sym:SymbolTable): Any = node match {
    case n: Node => n.eval(sym)
    case other => other // Already a concrete value (SymbolTable, BigDecimal, etc.)
  }
}