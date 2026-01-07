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
    override def toString = s"Reader(@$offset, tIdx=$tIdx, token='${if(first!=null) first.s else "EOF"}')"
  }

  def _S: Parser[List[lexer.Token]] = rep(acceptIf(t => t != null && lexer.isWhitespace(t))(_ => "whitespace expected"))
  def token(name: String): Parser[lexer.Token] = acceptIf { case t: lexer.Token.Defined if t.s == name => true case _ => false }(t => s"Expected structural token '$name'")
  def dataToken(f: lexer.Token.otherwise => Boolean, msg: String): Parser[lexer.Token] = acceptIf { case t: lexer.Token.otherwise if f(t) => true case _ => false }(t => msg)

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag){
    val rules = ArrayBuffer[(HygenicTag, PackratParser[Any])]()
    var updated = false 

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      rules += ((t, p))
      updated = true
      logger.log(s"[database] Rule registered: ${t.mangledName}")
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
  
  // ソースコードを保持（再トークナイズ用）
  private var sourceCode: String = ""

  def buildLatticeFrom(offset: Int, lattice: Map[Int, Array[lexer.Token]]): Unit = {
    val maxOffset = if (lattice.isEmpty) 0 else lattice.keys.max
    logger.log(s"[parse] Building/Updating parse lattice from offset $offset")
    
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
          if (nextJump < parseLattice.length && visited.add(nextJump)) {
            queue.enqueue(nextJump)
          }
        }
      } else {
        parseLattice(curr) = null
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

  // 準備処理を共通化
  private def prepareLattice(lattice: Map[Int, Array[lexer.Token]]): Unit = {
    val maxOffset = if (lattice.isEmpty) 0 else lattice.keys.max
    val latticeArr = new Array[Array[lexer.Token]](maxOffset + 1)
    lattice.foreach { case (off, tokens) => if(off < latticeArr.length) latticeArr(off) = tokens }
    nextContentOffset = new Array[Int](maxOffset + 1)
    var lastContent = maxOffset + 1
    var i = maxOffset
    while (i >= 0) {
      val tokens = latticeArr(i)
      if (tokens != null && tokens.exists(t => !lexer.isWhitespace(t))) lastContent = i
      nextContentOffset(i) = lastContent
      i -= 1
    }
    if (parseLattice.length < maxOffset + 2) {
      val newLattice = new Array[Array[ParseEdge]](maxOffset + 2)
      Array.copy(parseLattice, 0, newLattice, 0, parseLattice.length)
      parseLattice = newLattice
    }
  }

  def apply(lattice: Map[Int, Array[lexer.Token]], source: String = ""): ParseResult[Array[Any]] = {
    if (lattice.isEmpty) return Success(Array.empty, new LatticeReader(0, Map.empty))
    sourceCode = source
    var currentLattice = lattice
    prepareLattice(currentLattice)

    val results = ArrayBuffer[Any]()
    val startOff = if (0 < nextContentOffset.length) nextContentOffset(0) else 0
    var p = startOff
    
    buildLatticeFrom(p, currentLattice)
    database.updated = false
    lexer.database.updated = false

    while (p < parseLattice.length) {
      if (parseLattice(p) == null) buildLatticeFrom(p, currentLattice)

      val choices = parseLattice(p)
      if (choices == null || choices.isEmpty) {
        p = Int.MaxValue 
      } else {
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
        
        // Lexer または Parser のルールが更新された場合、再構築
        if (database.updated || lexer.database.updated) {
          logger.log(s"[parse] Dynamic update detected. Re-scanning from offset $nextP")
          if (lexer.database.updated && sourceCode.nonEmpty) {
            // トークナイズのやり直し
            currentLattice = lexer.tokenize(sourceCode)
            prepareLattice(currentLattice)
            lexer.database.updated = false
          }
          // 以降のパース格子をクリアして再構築
          for (j <- nextP until parseLattice.length) parseLattice(j) = null
          buildLatticeFrom(nextP, currentLattice)
          database.updated = false
        }
        p = nextP
      }
    }

    result = results.toArray
    Success(result, new LatticeReader(if (p == Int.MaxValue) (if(currentLattice.isEmpty) 0 else currentLattice.keys.max) else p, currentLattice))
  }

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}