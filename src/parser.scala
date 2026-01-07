package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer, Queue => MutableQueue, Set => MutableSet}
import scala.util.parsing.combinator._
import scala.util.parsing.input._

// 1つのパース成功結果を表す
case class ParseEdge(result: Any, nextOffset: Int) {
  override def toString: String = s"Edge(-> $nextOffset, $result)"
}

// Lexerからのトークン格子を解析し、パース結果の格子を構築するパーサー
class Parser(val lexer: Lexer, tag: HygenicTag)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag) {

  type Elem = lexer.Token

  // LatticeReader: 空白スキップを行わない純粋なリーダー
  class LatticeReader(override val offset: Int, val lattice: Map[Int, Array[lexer.Token]], val tIdx: Int = 0) extends Reader[lexer.Token] {
    val availableTokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    override def first: lexer.Token = if (availableTokens.isDefinedAt(tIdx)) availableTokens(tIdx) else null
    override def atEnd: Boolean = availableTokens.isEmpty && !lattice.keys.exists(_ > offset)
    override def rest: Reader[lexer.Token] = {
      if (atEnd) this
      else {
        val nextRawOff = offset + first.s.length
        new LatticeReader(nextRawOff, lattice, 0)
      }
    }
    override def pos: Position = if (first != null) first.pos else NoPosition
    def select(newTIdx: Int): LatticeReader = new LatticeReader(offset, lattice, newTIdx)
    override def hashCode: Int = offset ^ tIdx
    override def equals(obj: Any): Boolean = obj match {
      case other: LatticeReader => offset == other.offset && tIdx == other.tIdx
      case _ => false
    }
    override def toString = s"Reader(@$offset, tIdx=$tIdx)"
  }

  // --- 明示的な空白スキップ ---
  def _S: Parser[List[lexer.Token]] = rep(acceptIf(t => {
    t != null && lexer.isWhitespace(t)
  })(_ => "whitespace expected"))

  def token(name: String): Parser[lexer.Token] = acceptIf(t => t != null && t.s == name)(t => s"Expected '$name' found '${if(t!=null) t.s else "EOF"}'")

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag){
    private var rules: MutableMap[HygenicTag, PackratParser[Any]] = MutableMap.empty
    private var RuleOrder: MutableMap[HygenicTag, BigInt] = MutableMap.empty
    private var counter: BigInt = 0
    private var update = true
    def set(t: HygenicTag, p: PackratParser[Any]): Unit = {
      rules += (t -> p); RuleOrder += (t -> counter); counter += 1; update = true
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = {
      if (update) { cachedRules = rules.toArray.sortBy(x => RuleOrder(x._1)); update = false }
      cachedRules
    }
    private var cachedRules: Array[(HygenicTag, PackratParser[Any])] = Array.empty
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  private var nextContentOffset: Array[Int] = Array.empty
  
  // パース結果の格子: Offset -> Array[ParseEdge]
  var parseLattice: Array[Array[ParseEdge]] = Array.empty

  // 指定された位置から始まるすべての可能性を探索する (最適化版: 配列ベース)
  def dispatchAll(in: Input, lattice: Map[Int, Array[lexer.Token]]): Array[ParseEdge] = {
    val offset = in.offset
    val tokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    val currentRules = database.getRules
    
    // 最大で (トークン数 * ルール数) のエッジが生まれる可能性があるが、
    // 実際はほとんどマッチしないので、初期サイズを小さくして拡張するか、
    // ArrayBuilder を使う。ArrayBuilder は内部で Array を拡張してくれる。
    val builder = Array.newBuilder[ParseEdge]
    // 頻繁なリサイズを避けるためのヒントがあれば良いが、とりあえずデフォルトで。

    var tIdx = 0
    val tokensLen = tokens.length
    val rulesLen = currentRules.length

    while (tIdx < tokensLen) {
      val t = tokens(tIdx)
      if (!lexer.isWhitespace(t)) {
        val packratIn = new PackratReader(new LatticeReader(offset, lattice, tIdx))
        var rIdx = 0
        while (rIdx < rulesLen) {
          val (tag, rule) = currentRules(rIdx)
          rule(packratIn) match {
            case Success(res, next) => 
              builder.addOne(ParseEdge(res, next.offset))
            case _ => 
          }
          rIdx += 1
        }
      }
      tIdx += 1
    }
    builder.result()
  }

  def apply(lattice: Map[Int, Array[lexer.Token]]): ParseResult[Array[Any]] = {
    if (lattice.isEmpty) return Success(Array.empty, new LatticeReader(0, Map.empty))
    val maxOffset = lattice.keys.max
    
    // ジャンプテーブルの再構築
    val latticeArr = new Array[Array[lexer.Token]](maxOffset + 1)
    lattice.foreach { case (off, tokens) => latticeArr(off) = tokens }
    nextContentOffset = new Array[Int](maxOffset + 1)
    var lastContent = maxOffset + 1
    var i = maxOffset
    while (i >= 0) {
      val tokens = latticeArr(i)
      if (tokens != null && tokens.exists(t => !lexer.isWhitespace(t))) {
        lastContent = i
      }
      nextContentOffset(i) = lastContent
      i -= 1
    }

    // パース格子の初期化
    parseLattice = new Array[Array[ParseEdge]](maxOffset + 2)
    val visited = MutableSet[Int]()
    val queue = MutableQueue[Int]()

    val startOff = if (0 < nextContentOffset.length) nextContentOffset(0) else 0
    queue.enqueue(startOff)
    visited.add(startOff)

    logger.log(s"[parse] Building Parse Lattice via BFS starting at $startOff (maxOffset: $maxOffset)")

    // BFS
    while (queue.nonEmpty) {
      val curr = queue.dequeue()
      
      // dispatchAll は Array[ParseEdge] を返すようになった
      val edges = dispatchAll(new LatticeReader(curr, lattice), lattice)
      if (edges.nonEmpty) { // 配列の空チェックは length > 0
        parseLattice(curr) = edges
        var eIdx = 0
        while (eIdx < edges.length) {
          val edge = edges(eIdx)
          val rawNext = edge.nextOffset
          val nextJump = if (rawNext < nextContentOffset.length) nextContentOffset(rawNext) else rawNext
          
          if (nextJump < parseLattice.length) {
            if (visited.add(nextJump)) {
              queue.enqueue(nextJump)
            }
          }
          eIdx += 1
        }
      }
    }

    logger.log("[parse] Path reconstruction...")

    // パス復元
    val results = ArrayBuffer[Any]()
    var p = startOff
    if (p < nextContentOffset.length) p = nextContentOffset(p)

    while (p < parseLattice.length) {
      val choices = parseLattice(p)
      if (choices == null || choices.isEmpty) {
        if (lattice.keys.exists(_ >= p)) {
           logger.log(s"[path] Unparsed tokens remain at $p")
        }
        p = Int.MaxValue 
      } else {
        if (choices.length > 1) {
          logger.log(s"[path] Ambiguity at $p: ${choices.length} possibilities.")
        }
        // 最長一致探索
        var best = choices(0)
        var maxOff = best.nextOffset
        var ci = 1
        while (ci < choices.length) {
          val c = choices(ci)
          if (c.nextOffset > maxOff) {
            maxOff = c.nextOffset
            best = c
          }
          ci += 1
        }
        
        results += best.result
        
        val rawNext = best.nextOffset
        if (rawNext < nextContentOffset.length) {
          p = nextContentOffset(rawNext)
        } else {
          p = rawNext
        }
      }
    }

    result = results.toArray
    logger.log(s"[parse] Finished. ${result.length} nodes produced.")
    Success(result, new LatticeReader(if (p == Int.MaxValue) maxOffset else p, lattice))
  }

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}