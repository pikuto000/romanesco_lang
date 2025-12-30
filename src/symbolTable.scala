package parser

// シンプルなシンボルエントリ
case class SymbolEntry(node: Any, props: Map[String, String] = Map.empty)

// シンボルテーブル (純粋なレキシカルスコープ)
class SymbolTable(val parent: Option[SymbolTable] = None) {
  private val tab = collection.mutable.Map[String, SymbolEntry]()
  val lex: LexerTable = new LexerTable(parent.map(_.lex))
  val z3: com.microsoft.z3.Context = parent.map(_.z3).getOrElse(new com.microsoft.z3.Context())
  val logicalVars = collection.mutable.Map[String, com.microsoft.z3.IntExpr]()

  // 親を辿って値を取得
  def get(key: String): Option[Any] = tab.get(key).map(_.node).orElse(parent.flatMap(_.get(key)))
  
  // プロパティの取得
  def getProp(key: String, p: String): Option[String] = tab.get(key).flatMap(_.props.get(p)).orElse(parent.flatMap(_.getProp(key, p)))
  
  // 値のセット
  def set(key: String, v: Any, props: Map[String, String] = Map.empty) = tab(key) = SymbolEntry(v, props)
  
  // キーの一覧 (デバッグ/リトライ用)
  def getTabKeys: Iterable[String] = tab.keys ++ parent.map(_.getTabKeys).getOrElse(Nil)
  
  // 新しいスコープの作成
  def extend(): SymbolTable = new SymbolTable(Some(this))
  
  // 関数オブジェクトの取得
  def getFunc(key:String): (Array[Node], SymbolTable) => Any = {
    get(key) match { 
      case Some(app: Apply) => app.func 
      case Some(f: Function2[Array[Node], SymbolTable, Any] @unchecked) => f
      case _ => throw new RuntimeException(s"Symbol '$key' is not a function") 
    }
  }

  def gensym(prefix: String = "g"): String = {
    var id = 0
    def f(): String = { 
      val n = s"${prefix}_$id"
      id += 1
      if (id > 10000) throw new RuntimeException("gensym failed to find a unique name")
      if (get(n).isDefined) f() else n 
    }
    f()
  }

  def close(): Unit = if (parent.isEmpty) z3.close()
}
