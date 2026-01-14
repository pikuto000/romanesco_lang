package Lexing

import scala.util.matching.Regex
import Undeterminable.*

// ======================================
// Token定義
// ======================================

/** romanesco言語のトークン */
enum Token:
  case Op(s: String)       // 演算子: +, -, ==, など
  case Ident(s: String)    // 識別子: x, factorial, など
  case Keyword(s: String)  // キーワード: syntax, if, など
  case WS(s: String)       // 空白
  case Number(s: String)   // 数値リテラル
  case Delim(s: String)    // デリミタ: (, ), {, }, など
  
  /** トークンの字句を取得 */
  def lexeme: String = this match
    case Op(s) => s
    case Ident(s) => s
    case Keyword(s) => s
    case WS(s) => s
    case Number(s) => s
    case Delim(s) => s

// ======================================
// Tokenizer型とコンストラクタ
// ======================================

/**
 * トークナイザー関数
 * 入力文字列の指定位置から、可能なトークンとその終了位置を返す
 * 
 * @param input 入力文字列全体
 * @param chars 入力のchar配列（高速アクセス用）
 * @param pos 現在位置
 * @return (トークン, 次の位置)のリスト
 */
type Tokenizer = (String, Array[Char], Int) => List[(Token, Int)]

/**
 * 正規表現マッチャー（全長マッチング）
 * 指定位置から、パターンにマッチする全ての長さの候補を返す
 * 
 * 例: "abc" に対して "[a-z]+" パターンを適用すると
 *     "a", "ab", "abc" 全てがマッチ候補
 */
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

/**
 * 正規表現マッチャー（最長一致のみ）
 * 貪欲マッチング用：一つの最長候補だけを返す
 */
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

/**
 * デリミタトークナイザー
 * 複数のデリミタを効率的にマッチング
 */
def lexDelim(delimiters: Set[String]): Tokenizer =
  (input, _, pos) =>
    val builder = List.newBuilder[(Token, Int)]
    
    for d <- delimiters do
      if input.startsWith(d, pos) then
        builder += ((Token.Delim(d), pos + d.length))
    
    builder.result()

// ======================================
// Lexモード
// ======================================

/** レキシング戦略を制御するモード */
enum LexMode:
  case BestOnly          // 最長トークン列のみ
  case All               // 全ての可能性
  case TopN(n: Int)      // 上位N個

// ======================================
// メインレキサー
// ======================================

/**
 * 入力文字列を非決定的にトークナイズし、木構造で返す
 * 
 * メモ化により、同じ位置からのトークナイズは一度だけ実行される
 * 
 * @param input 入力文字列
 * @param tokenizers 試行するトークナイザーのリスト
 * @return 全てのトークン列を表す木構造
 */
def lexToTree(
  input: String,
  tokenizers: List[Tokenizer],
  debug: Boolean = false
): tree[Token] =
  val chars = input.toCharArray
  val len = chars.length
  
  // メモ化テーブル：位置 → 木構造
  val memo = scala.collection.mutable.Map.empty[Int, tree[Token]]
  
  def build(pos: Int): tree[Token] =
    memo.getOrElseUpdate(pos, {
      // 入力終端
      if pos == len then
        if debug then println(s"[DEBUG] pos=$pos: Reached end of input")
        tree.Node(Vector.empty, List.empty)
      else
        if debug then println(s"[DEBUG] pos=$pos: char='${chars(pos)}' remaining='${input.substring(pos, Math.min(pos + 10, len))}...'".replace("\n", "\\n").replace("\r", "\\r"))
        
        // 全てのトークナイザーを試行
        val branches = tokenizers.to(List).flatMap { tokenizer =>
          val matches = tokenizer(input, chars, pos)
          if debug && matches.nonEmpty then 
            println(s"[DEBUG]   Tokenizer matched: ${matches.map { case (tok, nextPos) => s"${tok.lexeme}@$nextPos" }.mkString(", ")}".replace("\n", "\\n").replace("\r", "\\r"))
          
          // 現在位置でマッチする全てのトークン
          matches.to(List).flatMap { (tok, nextPos) =>
            // 再帰的に残りをトークナイズ
            build(nextPos) match
              case tree.DeadEnd => None
              case nextTree => Some(tree.single(tok, nextTree))
          }
        }
        
        if branches.isEmpty then
          if debug then println(s"[DEBUG] pos=$pos: DeadEnd (no tokenizer matched)")
          tree.DeadEnd
        else 
          tree.fork(branches)
    })
  
  build(0)

/**
 * レキシングのエントリーポイント
 * モードに応じて結果をフィルタ（将来実装）
 */
def lexAll(
  input: String,
  tokenizers: List[Tokenizer],
  mode: LexMode,
  debug: Boolean = false
): tree[Token] =
  // TODO: modeに応じたフィルタリング
  lexToTree(input, tokenizers, debug)
