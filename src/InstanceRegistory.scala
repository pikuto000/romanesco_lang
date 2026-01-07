package romanesco

object InstanceRegistory {
  private val instanceMap = scala.collection.mutable.Map[String, Any]()

  def register(name: String, instance: Any): Unit = {
    instanceMap(name) = instance
  }

  def get(name: String): Option[Any] = instanceMap.get(name)

  // 便宜上、デフォルトのインスタンスを取得するためのヘルパー
  def default: (Lexer, Parser, Solver) = {
    val lexer = init.lex.setup("firstLexer")
    val solver = new Solver()
    val parser = init.parse.setup("firstParser", lexer, solver)
    (lexer, parser, solver)
  }
}