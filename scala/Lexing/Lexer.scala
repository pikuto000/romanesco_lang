package Lexing
import scala.util.matching.Regex

// ====================================== 
// Token 定義
// ====================================== 

enum Token:
  case Op(lexeme: String)
  case Ident(lexeme: String)
  case WS(lexeme: String)
  case Number(lexeme: String)
  case Delim(lexeme: String)


// ====================================== 
// Lexer 状態
// ====================================== 

final case class LexState(
  pos: Int,
  cost: Int,
  tokensRev: List[Token]
):
  inline def push(tok: Token, newPos: Int): LexState =
    LexState(newPos, cost + 1, tok +: tokensRev)

  inline def freeze: List[Token] =
    tokensRev.reverse


// ====================================== 
// Tokenizer & Regex
// ====================================== 

type Tokenizer = (String, Array[Char], Int) => List[(Token, Int)]

def lexRegex(
  pattern: Regex,
  converter: String => Token
): Tokenizer =
  (input, _, pos) =>
    val s = input.substring(pos)
    val matcher = pattern.pattern.matcher(s)
    val builder = List.newBuilder[(Token, Int)]
    var len = 1
    val maxLen = s.length
    while len <= maxLen do
      val sub = s.substring(0, len)
      if matcher.reset(sub).matches() then
        builder += ((converter(sub), pos + len))
      len += 1
    builder.result()

def lexRegexLongest(
  pattern: Regex,
  converter: String => Token
): Tokenizer =
  (input, _, pos) =>
    val matcher = pattern.pattern.matcher(input)
    matcher.region(pos, input.length)
    if matcher.lookingAt() then
      val sub = matcher.group()
      List((converter(sub), pos + sub.length))
    else
      Nil


// ====================================== 
// Lex モード
// ====================================== 

enum LexMode:
  case BestOnly
  case All
  case TopN(n: Int)


// ====================================== 
// Lexer 本体
// ====================================== 

def lexAll(
  input: String,
  tokenizers: List[Tokenizer],
  mode: LexMode
): List[List[Token]] =
  val chars = input.toCharArray
  val len = chars.length

  var frontier: List[LexState] =
    List(LexState(0, 0, Nil))

  var completed = List.empty[LexState]

  while frontier.nonEmpty do
    val nextStates =
      frontier.flatMap {
        state =>
          if state.pos >= len then Nil
          else
            tokenizers.flatMap {
              tokenizer =>
                tokenizer(input, chars, state.pos).map {
                  case (tok, newPos) => state.push(tok, newPos)
                }
            }
      }

    val (done, cont) = nextStates.partition(_.pos == len)

    mode match
      case LexMode.BestOnly if done.nonEmpty =>
        return done.sortBy(_.cost).map(_.freeze)
      case _ =>
        completed ++= done
        frontier = cont

  val sorted = completed.sortBy(_.cost)

  mode match
    case LexMode.TopN(n) => sorted.take(n).map(_.freeze)
    case _               => sorted.map(_.freeze)

// ====================================== 
// デリミタ
// ====================================== 
def lexDelim(delimiters: Set[String]): Tokenizer =
  (input, chars, pos) =>
    val builder = List.newBuilder[(Token, Int)]
    for d <- delimiters do
      if input.startsWith(d, pos) then
        builder += ((Token.Delim(d), pos + d.length))
    builder.result()