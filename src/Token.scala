package romanesco

import scala.util.parsing.input.Positional

// --- トークン定義 ---

enum Token extends Positional:
  case Num(value: BigDecimal)
  case Var(name: String)
  case Op(op: String)
  case Keyword(word: String)
  case Sym(symbol: String)
  case WS(content: String)      // 追加: 空白
  case Comment(content: String) // 追加: コメント