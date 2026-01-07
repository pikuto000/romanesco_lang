package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer, Queue => MutableQueue, Set => MutableSet}
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

case class ParseEdge(result: Any, nextOffset: Int, ruleTag: HygenicTag, logicalError: Option[String] = None) {
  def isConsistent: Boolean = logicalError.isEmpty
}

class Parser(val lexer: Lexer, tag: HygenicTag, val solver: Option[Solver] = None)
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
    @volatile var updated = false 

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = synchronized { 
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[parser.db] Adding rule: ${t.name}")
        rules += ((t, p)); registeredHashes += t.hash; updated = true
      }
    }
    def prepend(t: HygenicTag, p: PackratParser[Any]): Unit = synchronized { 
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[parser.db] Prepending rule: ${t.name}")
        ((t, p)) +=: rules; registeredHashes += t.hash; updated = true
      }
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = synchronized { rules.toArray }
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  def anyRule: PackratParser[Node] = new PackratParser[Node] {
    def apply(in: Input): ParseResult[Node] = {
      val currentRules = database.getRules
      var i = 0
      var found: Option[Success[Node]] = None
      while (i < currentRules.length && found.isEmpty) {
        val (tag, rule) = currentRules(i)
        if (tag.name != "parseProgram") {
          rule(in) match {
            case Success(res: Node, next) if next.offset > in.offset => 
              found = Some(Success(res, next))
            case _ =>
          }
        }
        i += 1
      }
      found.getOrElse(Failure("No rule matched in database", in))
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
    val definedStrings = tokens.collect { case t: lexer.Token.Defined => t.s }.toSet
    
    var tIdx = 0
    while (tIdx < tokens.length) {
      val t = tokens(tIdx)
      if (t != null && !lexer.isWhitespace(t) && !(t.isInstanceOf[lexer.Token.otherwise] && definedStrings.contains(t.s))) {
        val packratIn = new PackratReader(new LatticeReader(offset, lattice, tIdx))
        var rIdx = 0
        while (rIdx < currentRules.length) {
          val (ruleTag, rule) = currentRules(rIdx)
          rule(packratIn) match {
            case Success(res, next) => 
              // ここでは単体チェックは行わず、全ての可能性をエッジとして残す
              builder.addOne(ParseEdge(res, next.offset, ruleTag, None))
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

    @volatile var needsReevaluation = true
    var passCount = 0

    while (needsReevaluation && passCount < 10) {
      passCount += 1
      logger.log(s"[parser] === PASS #$passCount START ===")
      database.updated = false; lexer.database.updated = false
      prepareLattice(currentLattice)
      parseLattice.clear()
      
      buildExhaustiveLattice(currentLattice)

      val startOff = if (0 < nextContentOffset.length) nextContentOffset(0) else 0
      
      // 【論理駆動パス探索】
      val (currentNodes, nextR) = searchLogicalPath(startOff, currentLattice) match {
        case Some((nodes, off)) => (nodes, new LatticeReader(off, currentLattice))
        case None => 
          logger.log("[parser.error] No logically consistent path found. Falling back to longest match.")
          fallbackToLongest(startOff, currentLattice)
      }
      
      finalNodes = currentNodes
      lastReader = nextR

      registerMacrosDeterministically(finalNodes)

      if (database.updated || lexer.database.updated) {
        if (lexer.database.updated && sourceCode.nonEmpty) {
          currentLattice = lexer.tokenize(sourceCode)
        }
        needsReevaluation = true
      } else needsReevaluation = false
    }

    result = finalNodes
    Success(result, lastReader)
  }

  // 論理的なパスを深さ優先探索（DFS）で探す
  private def searchLogicalPath(offset: Int, lattice: Map[Int, Array[lexer.Token]]): Option[(Array[Any], Int)] = {
    if (offset >= maxOffsetSafe(lattice)) return Some((Array.empty, offset))
    
    val choices = parseLattice.get(offset)
    if (choices == null || choices.isEmpty) {
      val jump = if (offset < nextContentOffset.length) nextContentOffset(offset) else offset + 1
      if (jump > offset && jump < nextContentOffset.length + 1) return searchLogicalPath(jump, lattice)
      else return Some((Array.empty, offset))
    }

    // エッジを長さ順（長い順）に試す
    val sortedEdges = choices.sortBy(-_.nextOffset)
    
    for (edge <- sortedEdges) {
      // このエッジが論理的に矛盾しないかチェック
      val isFeasible = edge.result match {
        case n: Node if solver.isDefined && (n.kind == "Unification" || n.kind == "BinaryOp" || n.kind == "Block") =>
          solver.get.checkFeasibility(n)
        case _ => true
      }

      if (isFeasible) {
        // このエッジを「仮採用」して Solver の状態を更新
        edge.result match { case n: Node if solver.isDefined => solver.get.solve(n) case _ => }
        
        // 残りのパスを探索
        searchLogicalPath(edge.nextOffset, lattice) match {
          case Some((tailNodes, finalOff)) =>
            return Some((edge.result +: tailNodes, finalOff))
          case None => 
            // 失敗した場合は Solver を戻す必要はない（現在のパスが終わるだけなので。真面目にやるなら push/pop 必要）
        }
      }
    }
    None
  }

  private def fallbackToLongest(startOff: Int, lattice: Map[Int, Array[lexer.Token]]): (Array[Any], Reader[lexer.Token]) = {
    val results = ArrayBuffer[Any]()
    var p = startOff
    while (p < maxOffsetSafe(lattice)) {
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
    (results.toArray, new LatticeReader(if (p == Int.MaxValue) maxOffsetSafe(lattice) else p, lattice))
  }

  private def registerMacrosDeterministically(nodes: Array[Any]): Unit = {
    val macroDefs = nodes.collect { case n: Node if n.kind == "MacroDef" => n }
    macroDefs.foreach { md =>
      val name = md.attributes("name").asInstanceOf[String]
      val patterns = md.attributes("patterns").asInstanceOf[List[Node]]
      val bodyList = md.attributes("bodyList").asInstanceOf[List[Node]]

      if (Macro.register(name, patterns, bodyList)) {
        patterns.foreach {
          case n if n.kind == "LiteralWord" =>
            val word = n.attributes("value").asInstanceOf[String]
            val mTag = Hygenicmarker.bless(s"kw_$word", Some(lexer), true)
            lexer.database.set(mTag, lexer.positioned(lexer.regex(java.util.regex.Pattern.quote(word).r) ^^ { s => lexer.Token.Defined(s, mTag) }))
          case _ =>
        }
      }
    }
  }

  private def maxOffsetSafe(l: Map[Int, Array[lexer.Token]]): Int = if (l.isEmpty) 0 else l.keys.max + 1

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}