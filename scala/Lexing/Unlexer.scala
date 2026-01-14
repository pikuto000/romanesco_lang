package Lexing

/**
 * トークン列を文字列に戻すユーティリティ
 * デバッグやテスト、Z3制約生成で使用
 */
object UnLexer:
  
  /** 単一トークンを文字列表現に変換 */
  def unlexToken(tok: Token): String =
    tok.lexeme
  
  /**
   * トークン列を文字列に戻す
   * 全ての可能な連結方法を非決定的に列挙
   * 
   * 注：現在は単純連結のみ。将来的に空白挿入ルールを追加可能
   */
  def unlex(tokens: List[Token]): List[String] =
    tokens.foldLeft(List("")) { (acc, tok) =>
      for
        prefix <- acc
        s = unlexToken(tok)
      yield prefix + s
    }
  
  /**
   * デバッグ用：トークン列を読みやすい形式で表示
   */
  def show(tokens: List[Token]): String =
    tokens.map {
      case Token.WS(_) => " "
      case t => unlexToken(t)
    }.mkString
