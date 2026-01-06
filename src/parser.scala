package romanesco
import scala.collection.mutable.{Map => MutableMap}
import scala.util.parsing.combinator._
import scala.util.parsing.input._

// Lexerからのトークン格子を解析するパーサー
class Parser(val lexer: Lexer, tag: HygenicTag)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag) {

  // パーサーが扱う要素の型をLexerのTokenに設定
  type Elem = lexer.Token

  // トークン格子を読み取るためのReader実装
  class LatticeReader(override val offset: Int, val lattice: Map[Int, Array[lexer.Token]]) extends Reader[lexer.Token] {
    val availableTokens: Array[lexer.Token] = lattice.getOrElse(offset, Array.empty[lexer.Token])
    var currentTokenIdx = 0

    override def first: lexer.Token = {
      if (atEnd || availableTokens.isEmpty) null
      else availableTokens(currentTokenIdx)
    }

    override def atEnd: Boolean = {
      !lattice.keys.exists(_ >= offset)
    }

    override def rest: Reader[lexer.Token] = {
      if (atEnd || availableTokens.isEmpty) this
      else new LatticeReader(offset + availableTokens(currentTokenIdx).s.length, lattice)
    }

    override def pos: Position = {
      if (availableTokens.isEmpty) NoPosition
      else availableTokens(currentTokenIdx).pos
    }
    
    def select(idx: Int): LatticeReader = {
      val nr = new LatticeReader(offset, lattice)
      nr.currentTokenIdx = idx
      nr
    }
  }

  // 特定のトークンにマッチするユーティリティ
  def token(name: String): Parser[lexer.Token] = acceptIf(_.s == name)(t => s"Expected '$name' found '${t.s}'")

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag){
    private var rules: MutableMap[HygenicTag, PackratParser[Any]] = MutableMap.empty
    private var RuleOrder: MutableMap[HygenicTag, BigInt] = MutableMap.empty
    private var counter: BigInt = 0
    private var cachedRules: Array[(HygenicTag, PackratParser[Any])] = Array.empty
    private var update = true

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = {
      rules += (t -> p)
      RuleOrder += (t -> counter)
      counter += 1
      update = true
    }

    def getRules: Array[(HygenicTag, PackratParser[Any])] = {
      if (update) {
        cachedRules = rules.toArray.sortBy(x => RuleOrder(x._1))
        update = false
      }
      cachedRules
    }
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  // --- 最適化のためのキャッシュ ---
  private var lastLattice: Map[Int, Array[lexer.Token]] = Map.empty
  private var lastLatticeArray: Array[Array[lexer.Token]] = Array.empty
  private var nextContentOffset: Array[Int] = Array.empty

  def apply(lattice: Map[Int, Array[lexer.Token]]): ParseResult[Array[Any]] = {
    if (lattice.isEmpty) return Success(Array.empty, new LatticeReader(0, Map.empty))
    lastLattice = lattice

    // 1. 高速化のための配列化 (O(1) lookup)
    val maxOffset = lattice.keys.max
    lastLatticeArray = new Array[Array[lexer.Token]](maxOffset + 1)
    lattice.foreach { case (off, tokens) => lastLatticeArray(off) = tokens }

    // 2. 空白スキップ用のジャンプテーブル
    nextContentOffset = new Array[Int](maxOffset + 1)
    var lastContent = maxOffset + 1
    var i = maxOffset
    while (i >= 0) {
      val tokens = lastLatticeArray(i)
      if (tokens != null && tokens.exists(!_.tag.name.startsWith("whiteSpace"))) {
        lastContent = i
      }
      nextContentOffset(i) = lastContent
      i -= 1
    }

    var currentReader: Reader[lexer.Token] = new PackratReader(new LatticeReader(0, lattice))
    val results = scala.collection.mutable.ArrayBuffer[Any]()
    
    while (!currentReader.atEnd) {
      // 空白スキップ
      val startOffset = currentReader.offset
      if (startOffset < nextContentOffset.length) {
        val nextContent = nextContentOffset(startOffset)
        if (nextContent != startOffset) {
          currentReader = new PackratReader(new LatticeReader(nextContent, lattice))
        }
      }

      if (currentReader.atEnd) { /* Done */ } 
      else {
        dispatch(currentReader) match {
          case Success(res, next) =>
            if (res != ()) results += res
            currentReader = next
          case f: NoSuccess => return Failure(f.msg, currentReader)
        }
      }
    }
    val finalRes = results.toArray
    result = finalRes
    Success(finalRes, currentReader)
  }

  def dispatch: Parser[Any] = new Parser[Any] {
    def apply(in: Input): ParseResult[Any] = {
      val offset = in.offset
      if (offset >= lastLatticeArray.length) return Failure("EOF", in)
      
      val tokens = lastLatticeArray(offset)
      if (tokens == null) return Failure(s"No tokens at $offset", in)

      val currentRules = Parser.this.database.getRules
      val rulesLen = currentRules.length
      
      var tIdx = 0
      val tLen = tokens.length
      while (tIdx < tLen) {
        val t = tokens(tIdx)
        if (!t.tag.name.startsWith("whiteSpace")) {
          val readerWithToken = new LatticeReader(offset, lastLattice).select(tIdx)
          val packratIn = new PackratReader(readerWithToken)
          
          var rIdx = 0
          while (rIdx < rulesLen) {
            currentRules(rIdx)._2(packratIn) match {
              case s: Success[_] => return s
              case _ =>
            }
            rIdx += 1
          }
        }
        tIdx += 1
      }
      Failure("No match", in)
    }
  }

  var result: Array[Any] = Array.empty

  def printStream: Unit = {
    result.foreach { res =>
      println(s"printStream: Result: $res".replace("\n", "\\n").replace("\r", "\\r"))
    }
  }

  def eof: Parser[Unit] = new Parser[Unit] {
    def apply(in: Input) = if (in.atEnd) Success((), in) else Failure("Expected EOF", in)
  }
}