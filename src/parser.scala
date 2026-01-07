package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer, Queue => MutableQueue, Set => MutableSet}
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

case class ParseEdge(result: Any, nextOffset: Int, ruleTag: HygenicTag) {
  override def toString: String = s"Edge(-> $nextOffset, rule: ${ruleTag.mangledName}, res: $result)"
}

class Parser(val lexer: Lexer, tag: HygenicTag)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag) {

  type Elem = lexer.Token
  implicit val ec: ExecutionContext = ExecutionContext.global

  private var hasContentSuffix: java.util.BitSet = new java.util.BitSet()

  class LatticeReader(override val offset: Int, val lattice: scala.collection.Map[Int, Array[lexer.Token]], val tIdx: Int = 0) extends Reader[lexer.Token] {
    val availableTokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    override def first: lexer.Token = if (availableTokens.isDefinedAt(tIdx)) availableTokens(tIdx) else null
    override def atEnd: Boolean = !hasContentSuffix.get(offset)
    override def rest: Reader[lexer.Token] = {
      if (first == null) this
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
    private val registeredHashes = scala.collection.mutable.Set[Int]()
    var updated = false 

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[parser.db] Adding rule: ${t.name}")
        rules += ((t, p)); registeredHashes += t.hash; updated = true
      }
    }
    def prepend(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[parser.db] Prepending rule: ${t.name}")
        ((t, p)) +=: rules; registeredHashes += t.hash; updated = true
      }
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = rules.toArray
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  def anyRule: PackratParser[Node] = new PackratParser[Node] {
    def apply(in: Input): ParseResult[Node] = {
      val currentRules = database.getRules
      var i = 0
      while (i < currentRules.length) {
        val (tag, rule) = currentRules(i)
        if (tag.name != "parseProgram") {
          rule(in) match {
            case Success(res: Node, next) if next.offset > in.offset => 
              // logger.log(s"[anyRule] Match: ${tag.name} at ${in.offset}")
              return Success(res, next)
            case _ =>
          }
        }
        i += 1
      }
      Failure("No rule matched in database", in)
    }
  }

  var nextContentOffset: Array[Int] = Array.empty
  var parseLattice: java.util.concurrent.ConcurrentHashMap[Int, Array[ParseEdge]] = new java.util.concurrent.ConcurrentHashMap()
  private var sourceCode: String = ""

  def buildExhaustiveLattice(lattice: scala.collection.Map[Int, Array[lexer.Token]]): Unit = {
    val offsets = lattice.keys.toSeq.sorted
    val futures = offsets.map { offset =>
      Future {
        val edges = dispatchAll(new LatticeReader(offset, lattice), lattice)
        if (edges.nonEmpty) parseLattice.put(offset, edges)
      }
    }
    Await.result(Future.sequence(futures), Duration.Inf)
  }

  def dispatchAll(in: Input, lattice: scala.collection.Map[Int, Array[lexer.Token]]): Array[ParseEdge] = {
    val offset = in.offset
    val tokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    val currentRules = database.getRules
    val builder = Array.newBuilder[ParseEdge]
    
    // 【統合最適化】構造的トークンがある場合、同一文字列の一般的トークン(otherwise)を無視する
    val definedStrings = tokens.collect { case t: lexer.Token.Defined => t.s }.toSet
    
    var tIdx = 0
    while (tIdx < tokens.length) {
      val t = tokens(tIdx)
      val shouldSkip = t match {
        case _: lexer.Token.otherwise if definedStrings.contains(t.s) => true
        case _ => lexer.isWhitespace(t)
      }
      
      if (!shouldSkip) {
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

  private def prepareLattice(lattice: scala.collection.Map[Int, Array[lexer.Token]]): Unit = {
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

    hasContentSuffix.clear()
    var runningContent = false
    i = maxOffset
    while (i >= 0) {
      val tokens = latticeArr(i)
      if (tokens != null && tokens.exists(t => !lexer.isWhitespace(t))) runningContent = true
      if (runningContent) hasContentSuffix.set(i)
      i -= 1
    }
  }

  def apply(lattice: Map[Int, Array[lexer.Token]], source: String = ""): ParseResult[Array[Any]] = {
    if (lattice.isEmpty) return Success(Array.empty, new LatticeReader(0, Map.empty))
    sourceCode = source
    var currentLattice = lattice
    var finalNodes = Array.empty[Any]
    var lastReader: Reader[lexer.Token] = null

    var needsReevaluation = true
    var passCount = 0

    while (needsReevaluation && passCount < 10) {
      passCount += 1
      logger.log(s"[parser] === PASS #$passCount START ===")
      database.updated = false; lexer.database.updated = false
      prepareLattice(currentLattice)
      parseLattice.clear()
      
      buildExhaustiveLattice(currentLattice)

      val startOff = if (0 < nextContentOffset.length) nextContentOffset(0) else 0
      val choices = parseLattice.get(startOff)
      
      // PASS中、Programルールが成功しているなら、それを優先する
      if (choices != null && choices.exists(_.ruleTag.name == "parseProgram")) {
        val programEdge = choices.find(_.ruleTag.name == "parseProgram").get
        logger.log(s"[parser]   Program rule matched (length=${programEdge.nextOffset})")
        finalNodes = programEdge.result.asInstanceOf[Node].children.toArray
        lastReader = new LatticeReader(programEdge.nextOffset, currentLattice)
      } else {
        // 断片を繋ぎ合わせる（最長一致）
        val results = ArrayBuffer[Any]()
        var p = startOff
        while (p < maxOffsetSafe(currentLattice)) {
          val currChoices = parseLattice.get(p)
          if (currChoices == null || currChoices.isEmpty) { 
            val jump = if (p < nextContentOffset.length) nextContentOffset(p) else p + 1
            if (jump > p && jump < nextContentOffset.length + 1) p = jump else p = Int.MaxValue 
          } else {
            val best = currChoices.maxBy(_.nextOffset)
            results += best.result
            val nextRaw = best.nextOffset
            p = if (nextRaw < nextContentOffset.length) nextContentOffset(nextRaw) else nextRaw
          }
        }
        finalNodes = results.toArray
        lastReader = new LatticeReader(if (p == Int.MaxValue) maxOffsetSafe(currentLattice) else p, currentLattice)
      }

      if (database.updated || lexer.database.updated) {
        if (lexer.database.updated && sourceCode.nonEmpty) {
          logger.log("[parser] Re-tokenizing entire source...")
          currentLattice = lexer.tokenize(sourceCode)
        }
        needsReevaluation = true
      } else needsReevaluation = false
    }

    result = finalNodes
    Success(result, lastReader)
  }

  private def maxOffsetSafe(l: Map[Int, Array[lexer.Token]]): Int = if (l.isEmpty) 0 else l.keys.max + 1

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}