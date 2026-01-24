package romanesco
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.boundary
import scala.collection.mutable
import Debug.logger
final class Tokenizer(rules:Map[String,Regex])extends RegexParsers {
  override val skipWhitespace = false
  type TokenType = Regex
  type Content = String
  type Row = Int
  type Col = Int
  type Token = Tuple4[Row,Col,TokenType,Content]
  type TokenTree = Tree[Token]

  // 木の構造自体をキャッシュする (offset, isEOF) -> 結果
  private val cache = mutable.Map[(Int, Boolean), Vector[TokenTree]]()
  
  // 正規表現のマッチング結果をキャッシュする (startOffset, endOffset) -> マッチしたルールのリスト
  private val matchCache = mutable.Map[(Int, Int), Vector[Regex]]()

  private def getMatches(s: String, start: Int, len: Int): Vector[Regex] = {
    matchCache.getOrElseUpdate((start, start + len), {
      val sub = s.substring(0, len)
      rules.values.filter(_.anchored.findFirstIn(sub).isDefined).toVector
    })
  }

  // 統合トークナイズメソッド
  def toknize(s: String): TokenTree = {
    clearCache()
    // 逆方向を先に走らせて matchCache を埋める（順序は任意）
    toknizeFromEOF(s)(offset = s.length)
    // 順方向の結果をメインのツリーとして採用
    lazy val branches = toknizeFromSOF(s)()
    Tree.V((0, 0, "SOF".r, "SOF"), branches)
  }

  // 前方向全探索トークナイズ
  def toknizeFromSOF(s:String)(row:Row=0,col:Col=0,offset:Int=0):Vector[TokenTree]={
    if (cache.contains((offset, false))) {
      logger.log(s"SOF cache hit at offset $offset")
      return cache((offset, false))
    }
    
    lazy val result = if (s.isEmpty) {
      Vector(Tree.V((row, col, "EOF".r, "EOF"), Vector.empty))
    } else {
      val rawBranches = (1 to s.length).flatMap { len =>
        lazy val matchedRules = getMatches(s, offset, len)
        lazy val matchedText = s.substring(0, len)
        lazy val new_row = row + matchedText.count(_ == '\n')
        lazy val new_col = if (matchedText.contains("\n")) matchedText.length - matchedText.lastIndexOf("\n") - 1 else col + matchedText.length
        
        lazy val nextString = s.substring(len)
        lazy val subTrees = toknizeFromSOF(nextString)(new_row, new_col, offset + len)
        matchedRules.map(r => Tree.V((row, col, r, matchedText), subTrees))
      }.toVector
      Tree.merge(rawBranches)
    }
    cache((offset, false)) = result
    result
  }
  
  // 逆方向全探索トークナイズ
  def toknizeFromEOF(s:String)(row:Row=0,col:Col=0,offset:Int=0):Vector[TokenTree]={
    if (cache.contains((offset, true))) {
      logger.log(s"EOF cache hit at offset $offset")
      return cache((offset, true))
    }
    
    lazy val result = if (s.isEmpty) {
      Vector(Tree.V((row, col, "SOF".r, "SOF"), Vector.empty))
    } else {
      val rawBranches = (0 until s.length).flatMap { start =>
        lazy val len = s.length - start
        lazy val sub = s.substring(start)
        lazy val matchedRules = getMatches(sub, start, len)
        lazy val matchedText = sub
        lazy val remainingString = s.substring(0, start)
        
        lazy val subTrees = toknizeFromEOF(remainingString)(row, col, start)
        matchedRules.map(r => Tree.V((row, col, r, matchedText), subTrees))
      }.toVector
      Tree.merge(rawBranches)
    }
    cache((offset, true)) = result
    result
  }
  def clearCache() = {
    cache.clear()
    matchCache.clear()
  }
}

@main def testTokenizer(): Unit = {
  Debug.logger.switch(true) // 大量に出る場合は一旦オフ
  val rules = Map(
    "excramation" -> "!".r,
    "semicolon" -> ";".r,
    "identifier" -> "[a-zA-Z_][a-zA-Z0-9_]*".r,
    "whitespace" -> "\\s+".r
  )

  def runTest(input: String): Unit = {
    println(s"\n" + "=" * 40)
    println(s"Testing input: '$input'")
    lazy val tokenizer = new Tokenizer(rules)
    val tree = tokenizer.toknize(input)
    
    println("\n--- TokenTree Structure ---")
    println(tree.prettyPrint())
    
    lazy val paths = tree.flatten.map(_.map(_._4).mkString(" -> ")).toSet
    println(s"--- Unique Interpretations (${paths.size} found) ---")
    paths.toSeq.sorted.foreach(p => println(s"  $p"))
  }

  // 複数のパターンを検証
  runTest("!test;")
  runTest("abc def")
}