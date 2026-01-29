package romanesco
import scala.util.matching.Regex
import scala.util.boundary
import scala.collection.mutable
import scala.collection.immutable
import Debug.logger
final class Tokenizer(rules:immutable.Map[String,Regex]){
  private type TokenType = Regex
  private type Content = String
  private type Row = UInt
  private type Col = UInt
  type Token = Tuple4[Row,Col,TokenType,Content]
  type TokenTree = Tree[Token]

  // 共通のパターンインスタンス（マージ時の等価性判定のため）
  private val SOFPattern = "SOF".r
  private val EOFPattern = "EOF".r

  // 木の構造自体をキャッシュする (offset, isEOF) -> 結果
  private val cache = mutable.Map[(UInt, Boolean), Vector[TokenTree]]()
  
  // 正規表現のマッチング結果をキャッシュする (startOffset, endOffset) -> マッチしたルールのリスト
  private val matchCache = mutable.Map[(UInt, UInt), Vector[Regex]]()

  private def getMatches(s: String, start: UInt, len: UInt): Vector[Regex] = {
    matchCache.getOrElseUpdate((start, start + len), {
      val sub = s.substring(0, len)
      rules.values.filter(_.anchored.findFirstIn(sub).isDefined).toVector
    })
  }

  // 統合トークナイズメソッド
  def toknize(s: String): TokenTree = {
    logger.log("starting tokenize. initializing cache...")
    clearCache()
    logger.log("cache initialized")
    // 逆方向を先に走らせて matchCache を埋める
    val fromEOFbranches = toknizeFromEOF(s)(offset = s.length)
    // 逆方向の結果（木構造は逆向き）をフラット化して反転させ、順方向の木として再構築する
    val reversedPaths = fromEOFbranches.flatMap(_.flatten).map(_.reverse)
    // パスの修正: 先頭のSOFを除去し、座標を再計算し、末尾にEOFを追加する
    val correctedPaths = reversedPaths.map { path =>
      // pathは [SOF, Token1, Token2, ..., TokenN] のはず
      // 先頭がSOFなら除去する（構造上、トークンのみの列にする）
      val tokens = if (path.headOption.exists(_._4 == "SOF")) path.tail else path
      var r:UInt = 0
      var c:UInt = 0
      val fixedTokens = tokens.map { case (_, _, tokenType, content) =>
        val currentToken = (r, c, tokenType, content)
        // 座標更新
        val newlines = content.count(_ == '\n')
        if (newlines > 0) {
          r += newlines
          c = content.length - content.lastIndexOf('\n') - 1
        } else {
          c += content.length
        }
        currentToken
      }
      val eofToken = (r, c, EOFPattern, "EOF")
      fixedTokens :+ eofToken
    }
    val fromEOFReversed = Tree.fromPaths(correctedPaths)
    // 順方向の結果
    val fromSOFbranches = toknizeFromSOF(s)()
    // 双方の結果を統合（マージ）する
    val unifiedBranches = Tree.merge(fromSOFbranches ++ fromEOFReversed)
    Tree.V((0, 0, SOFPattern, "SOF"), unifiedBranches)
  }

  // 前方向全探索トークナイズ
  def toknizeFromSOF(s:String)(row:Row=0,col:Col=0,offset:UInt=0):Vector[TokenTree]={
    logger.log(s"starting toknizeFromSOF. offset: $offset, s.length: ${s.length}")
    if (cache.contains((offset, false))) {
      logger.log(s"SOF cache hit at offset $offset")
      return cache((offset, false))
    }
    
    val result = if (s.isEmpty) {
      Vector(Tree.V((row, col, EOFPattern, "EOF"), Vector.empty))
    } else {
      val rawBranches = (1 to s.length).flatMap { len =>
        if (cache.get((offset + len, true)).exists(_.nonEmpty)) {
          val matchedRules = getMatches(s, offset, len)
          val matchedText = s.substring(0, len)
          val new_row:UInt = row + matchedText.count(_ == '\n')
          val new_col:UInt = if (matchedText.contains("\n")) matchedText.length - matchedText.lastIndexOf("\n") - 1 else col + matchedText.length
          
          val nextString = s.substring(len)
          val subTrees = toknizeFromSOF(nextString)(new_row, new_col, offset + len)
          matchedRules.map(r => Tree.V((row, col, r, matchedText), subTrees))
        } else {
          Vector.empty
        }
      }.toVector
      Tree.merge(rawBranches)
    }
    cache((offset, false)) = result
    result
  }
  
  // 逆方向全探索トークナイズ
  def toknizeFromEOF(s:String)(row:Row=0,col:Col=0,offset:UInt=0):Vector[TokenTree]={
    logger.log(s"starting toknizeFromEOF. offset: $offset, s.length: ${s.length}")
    if (cache.contains((offset, true))) {
      logger.log(s"EOF cache hit at offset $offset")
      return cache((offset, true))
    }
    
    val result = if (s.isEmpty) {
      Vector(Tree.V((row, col, SOFPattern, "SOF"), Vector.empty))
    } else {
      val rawBranches = (0 until s.length).flatMap { start =>
        val len:UInt = s.length - start
        val sub = s.substring(start)
        val matchedRules = getMatches(sub, start, len)
        val matchedText = sub
        val remainingString = s.substring(0, start)
        
        val subTrees = toknizeFromEOF(remainingString)(row, col, start)
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
/*
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
    val tokenizer = new Tokenizer(rules)
    val tree = tokenizer.toknize(input)
    
    println("\n--- TokenTree Structure ---")
    println(tree.prettyPrint())
    
    val paths = tree.flatten.map(_.map(_._4).mkString(" -> ")).toSet
    println(s"--- Unique Interpretations (${paths.size} found) ---")
    paths.toSeq.sorted.foreach(p => println(s"  $p"))
  }

  // 複数のパターンを検証
  runTest("!test;")
  runTest("abc def")
}
*/