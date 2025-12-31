package parser

case class SymbolEntry(node: Any, props: Map[String, String] = Map.empty)

class SymbolTable(val parent: Option[SymbolTable] = None) {
  private val tab = collection.mutable.Map[String, SymbolEntry]()
  
  // 各環境にユニークなIDを付与
  val id: Int = SymbolTable.nextID()
  
  val tokens: TokenTable = new TokenTable(parent.map(_.tokens))
  val z3: com.microsoft.z3.Context = parent.map(_.z3).getOrElse(new com.microsoft.z3.Context())
  val logicalVars = collection.mutable.Map[String, com.microsoft.z3.IntExpr]()

  def get(key: String): Option[Any] = tab.get(key).map(_.node).orElse(parent.flatMap(_.get(key)))
  def getProp(key: String, p: String): Option[String] = tab.get(key).flatMap(_.props.get(p)).orElse(parent.flatMap(_.getProp(key, p)))
  def set(key: String, v: Any, props: Map[String, String] = Map.empty) = tab(key) = SymbolEntry(v, props)
  def getTabKeys: Iterable[String] = tab.keys ++ parent.map(_.getTabKeys).getOrElse(Nil)
  def extend(): SymbolTable = new SymbolTable(Some(this))
  
  def getFunc(key:String): (Array[Node], SymbolTable) => Any = {
    get(key) match { 
      case Some(app: Apply) => app.func 
      case Some(f: Function2[Array[Node], SymbolTable, Any] @unchecked) => f
      case _ => throw new RuntimeException(s"Symbol '$key' is not a function") 
    }
  }

  def gensym(prefix: String = "g"): String = {
    s"${prefix}_${SymbolTable.nextID()}"
  }

  def close(): Unit = if (parent.isEmpty) z3.close()
}

object SymbolTable {
  private var count = 0
  def nextID(): Int = { synchronized { count += 1; count } }
}