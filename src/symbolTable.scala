package parser

case class SymbolEntry(node: Any, props: Map[String, String] = Map.empty)

class SymbolTable(val parent: Option[SymbolTable] = None) {
  private val tab = collection.mutable.Map[String, SymbolEntry]()
  
  // 各環境にユニークなIDを付与
  lazy val id: Int = SymbolTable.nextID()
  
  // 構文制御用のテーブル
  lazy val tokens: TokenTable = new TokenTable(parent.map(_.tokens))
  
  // 拡張データ（ソルバーコンテキストなど、特定の機能が動的に利用する）
  lazy val metadata = collection.mutable.Map[String, Any]()

  def get(key: String): Option[Any] = tab.get(key).map(_.node).orElse(parent.flatMap(_.get(key)))
  def getProp(key: String, p: String): Option[String] = tab.get(key).flatMap(_.props.get(p)).orElse(parent.flatMap(_.getProp(key, p)))
  def set(key: String, v: Any, props: Map[String, String] = Map.empty) = tab(key) = SymbolEntry(v, props)
  lazy val getTabKeys: Iterable[String] = tab.keys ++ parent.map(_.getTabKeys).getOrElse(Nil)
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

  // クリーンアップ
  def close(): Unit = {
    metadata.get("solver").collect { case env: SolverEnv => env.close() }
    parent.foreach(_.close())
  }
}

object SymbolTable {
  private var count = 0
  def nextID(): Int = { synchronized { count += 1; count } }
}
