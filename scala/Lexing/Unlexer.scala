package Lexing
import scala.collection.mutable.ListBuffer

object UnLexer:

  def unlexToken(
    tok: Token
  ): List[String] =
    tok match
      case Token.Op(op) =>
        List(op)
      case Token.Ident(name) =>
        List(name)
      case Token.Keyword(kw) =>
        List(kw)
      case Token.WS(ws) =>
        List(ws)
      case Token.Number(num) =>
        List(num)
      case Token.Delim(d) =>
        List(d)

  def unlex(
    tokens: List[Token]
  ): List[String] =
    tokens.foldLeft(List("")) { (acc, tok) =>
      for
        prefix <- acc
        s <- unlexToken(tok)
      yield prefix + s
    }