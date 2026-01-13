package Lexing
import scala.util.matching.Regex
import Undeterminable.*

// ====================================== 
// Token 定定義
// ====================================== 

enum Token:
  case Op(lexeme: String)
  case Ident(lexeme: String)
  case Keyword(lexeme: String)
  case WS(lexeme: String)
  case Number(lexeme: String)
  case Delim(lexeme: String)




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

def lexToTree(
  input: String,
  tokenizers: List[Tokenizer]
): tree[Token] =
  val chars = input.toCharArray
  val len = chars.length
  val memo = scala.collection.mutable.Map.empty[Int, tree[Token]]

  def build(pos: Int): tree[Token] =
    memo.getOrElseUpdate(pos, {
      if pos == len then
        tree.Node(Vector.empty, LazyList.empty)
      else
        val branches = tokenizers.to(LazyList).flatMap { tokenizer =>
          tokenizer(input, chars, pos).to(LazyList).flatMap { (tok, nextPos) =>
            build(nextPos) match
              case tree.DeadEnd => None
              case nextTree => Some(tree.Node(Vector(tok), LazyList(nextTree)))
          }
        }
        if branches.isEmpty then tree.DeadEnd
        else tree.Node(Vector.empty, branches)
    })

  build(0)

def lexAll(
  input: String,
  tokenizers: List[Tokenizer],
  mode: LexMode
): tree[Token] =
  lexToTree(input, tokenizers)

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