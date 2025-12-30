package parser

object interpreter {
  def eval(node: Any, sym: SymbolTable): Any = node match {
    case app: Apply => 
      // Apply ノードが eval に直接渡された場合は「実行」
      app.func(app.args, sym)

    case Atom(s) =>
      // Atom は数値リテラルか、シンボルのルックアップ
      try { BigDecimal(s.trim) }
      catch { case _: NumberFormatException =>
        sym.get(s) match {
          case Some(app: Apply) => app // 関数定義は実行せず、そのまま「値」として返す
          case Some(v) => interpreter.eval(v, sym) // 変数は再帰的に評価
          case None => s // 未登録なら文字列リテラル
        }
      }

    case other => other // すでに評価済みの値（SymbolTable, BigDecimal 等）
  }
}
