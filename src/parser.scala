package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer, Queue => MutableQueue, Set => MutableSet}
import scala.util.parsing.combinator._
import scala.util.parsing.input._

// 1つのパース成功結果を表す
case class ParseEdge(result: Any, nextOffset: Int, ruleTag: HygenicTag) {
  override def toString: String = s"Edge(-> $nextOffset, rule: ${ruleTag.mangledName}, res: $result)"
}

// Lexerからのトークン格子を解析し、パース結果の格子を構築するパーサー
class Parser(val lexer: Lexer, tag: HygenicTag)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag) {

  type Elem = lexer.Token

  // LatticeReader
  class LatticeReader(override val offset: Int, val lattice: Map[Int, Array[lexer.Token]], val tIdx: Int = 0) extends Reader[lexer.Token] {
    val availableTokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    override def first: lexer.Token = if (availableTokens.isDefinedAt(tIdx)) availableTokens(tIdx) else null
    override def atEnd: Boolean = availableTokens.isEmpty && !lattice.keys.exists(_ > offset)
    override def rest: Reader[lexer.Token] = {
      if (atEnd) this
      else {
        val nextRawOff = offset + first.s.length
        val nextJumpOff = if (nextRawOff < nextContentOffset.length) nextContentOffset(nextRawOff) else nextRawOff
        new LatticeReader(nextJumpOff, lattice, 0)
      }
    }
    override def pos: Position = if (first != null) first.pos else NoPosition
    def select(newTIdx: Int): LatticeReader = new LatticeReader(offset, lattice, newTIdx)
    override def hashCode: Int = offset ^ tIdx
    override def equals(obj: Any): Boolean = obj match {
      case other: LatticeReader => offset == other.offset && tIdx == other.tIdx
      case _ => false
    }
  }

  def _S: Parser[List[lexer.Token]] = rep(acceptIf(t => t != null && lexer.isWhitespace(t))(_ => "whitespace expected"))
  def token(name: String): Parser[lexer.Token] = acceptIf { case t: lexer.Token.Defined if t.s == name => true case _ => false }(t => s"Expected structural token '$name'")
  def dataToken(f: lexer.Token.otherwise => Boolean, msg: String): Parser[lexer.Token] = acceptIf { case t: lexer.Token.otherwise if f(t) => true case _ => false }(t => msg)

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag){
    val rules = ArrayBuffer[(HygenicTag, PackratParser[Any])]()
    var updated = false // ルールが追加されたらフラグを立てる

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      rules += ((t, p))
      updated = true
      logger.log(s"[database] Rule added: ${t.mangledName}")
    }
    def prepend(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      ((t, p)) +=: rules 
      updated = true
      logger.log(s"[database] Rule prepended: ${t.mangledName}")
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = rules.toArray
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  private var nextContentOffset: Array[Int] = Array.empty
  var parseLattice: Array[Array[ParseEdge]] = Array.empty

  // 特定の地点から格子を(再)構築する
  def buildLatticeFrom(offset: Int, lattice: Map[Int, Array[lexer.Token]]): Unit = {
    val maxOffset = if (lattice.isEmpty) 0 else lattice.keys.max
    logger.log(s"[parse] Building/Updating lattice from offset $offset to $maxOffset")
    
    // BFSを使って到達可能な範囲を探索・構築
    val queue = MutableQueue[Int]()
    val visited = MutableSet[Int]()
    
    queue.enqueue(offset)
    visited.add(offset)
    
    while (queue.nonEmpty) {
      val curr = queue.dequeue()
      val edges = dispatchAll(new LatticeReader(curr, lattice), lattice)
      if (edges.nonEmpty) {
        parseLattice(curr) = edges
        edges.foreach { edge =>
          val rawNext = edge.nextOffset
          val nextJump = if (rawNext < nextContentOffset.length) nextContentOffset(rawNext) else rawNext
          if (nextJump < parseLattice.length && !visited.contains(nextJump)) {
            visited.add(nextJump)
            queue.enqueue(nextJump)
          }
        }
      } else {
        parseLattice(curr) = null // マッチなしを明示
      }
    }
  }

  def dispatchAll(in: Input, lattice: Map[Int, Array[lexer.Token]]): Array[ParseEdge] = {
    val offset = in.offset
    val tokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    val currentRules = database.getRules
    val builder = Array.newBuilder[ParseEdge]

    var tIdx = 0
    while (tIdx < tokens.length) {
      val t = tokens(tIdx)
      if (!lexer.isWhitespace(t)) {
        val packratIn = new PackratReader(new LatticeReader(offset, lattice, tIdx))
        var rIdx = 0
        while (rIdx < currentRules.length) {
          val (ruleTag, rule) = currentRules(rIdx)
          rule(packratIn) match {
            case Success(res, next) => 
              builder.addOne(ParseEdge(res, next.offset, ruleTag))
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
    
    // 準備: ジャンプテーブルの構築
    val latticeArr = new Array[Array[lexer.Token]](maxOffset + 1)
    lattice.foreach { case (off, tokens) => latticeArr(off) = tokens }
    nextContentOffset = new Array[Int](maxOffset + 1)
    var lastContent = maxOffset + 1
    var i = maxOffset
    while (i >= 0) {
      val tokens = latticeArr(i)
      if (tokens != null && tokens.exists(t => !lexer.isWhitespace(t))) lastContent = i
      nextContentOffset(i) = lastContent
      i -= 1
    }

    parseLattice = new Array[Array[ParseEdge]](maxOffset + 2)
    
    // --- メインループ: パスを復元しながら、必要に応じて格子を再構築する ---
    val results = ArrayBuffer[Any]()
    val startOff = if (0 < nextContentOffset.length) nextContentOffset(0) else 0
    var p = startOff
    
    // 初回構築
    buildLatticeFrom(p, lattice)
    database.updated = false

    while (p < parseLattice.length) {
      // 1. 現在の位置にエッジがない場合、再構築を試みる
      if (parseLattice(p) == null) {
        buildLatticeFrom(p, lattice)
      }

      val choices = parseLattice(p)
      if (choices == null || choices.isEmpty) {
        // 本当にマッチするルールがない場合
        if (lattice.keys.exists(_ >= p)) logger.log(s"[parse] No rule matched at $p")
        p = Int.MaxValue // 終了
      } else {
        // 2. 最良のエッジを選択
        var best = choices(0)
        var maxOff = best.nextOffset
        var ci = 1
        while (ci < choices.length) {
          val c = choices(ci)
          if (c.nextOffset > maxOff) { maxOff = c.nextOffset; best = c }
          ci += 1
        }
        
        results += best.result
        val rawNext = best.nextOffset
        val nextP = if (rawNext < nextContentOffset.length) nextContentOffset(rawNext) else rawNext
        
        // 3. ルールが追加されていた場合、この地点以降の格子を無効化して再構築
        if (database.updated) {
          logger.log(s"[parse] Database updated by rule ${best.ruleTag.mangledName}. Re-scanning from $nextP")
          // 以降の格子をクリア
          for (j <- nextP until parseLattice.length) parseLattice(j) = null
          buildLatticeFrom(nextP, lattice)
          database.updated = false
        }
        
        p = nextP
      }
    }

    result = results.toArray
    Success(result, new LatticeReader(if (p == Int.MaxValue) maxOffset else p, lattice))
  }

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}
