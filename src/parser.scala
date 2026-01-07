package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer, Queue => MutableQueue, Set => MutableSet}
import scala.util.parsing.combinator._
import scala.util.parsing.input._

case class ParseEdge(result: Any, nextOffset: Int, ruleTag: HygenicTag) {
  override def toString: String = s"Edge(-> $nextOffset, rule: ${ruleTag.mangledName}, res: $result)"
}

class Parser(val lexer: Lexer, tag: HygenicTag)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag) {

  type Elem = lexer.Token

  class LatticeReader(override val offset: Int, val lattice: scala.collection.Map[Int, Array[lexer.Token]], val tIdx: Int = 0) extends Reader[lexer.Token] {
    val availableTokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    override def first: lexer.Token = if (availableTokens.isDefinedAt(tIdx)) availableTokens(tIdx) else null
    override def atEnd: Boolean = {
      val res = !lattice.keys.exists(off => off >= offset && lattice(off).exists(t => !lexer.isWhitespace(t)))
      // logger.log(s"[reader] atEnd at $offset: $res")
      res
    }
    override def rest: Reader[lexer.Token] = {
      if (first == null) {
        logger.log(s"[reader] rest at $offset: current is NULL, returning self")
        this
      } else {
        val nextRawOff = offset + first.s.length
        val nextJumpOff = if (nextRawOff < nextContentOffset.length) nextContentOffset(nextRawOff) else nextRawOff
        // logger.log(s"[reader] rest at $offset: nextRaw=$nextRawOff, nextJump=$nextJumpOff")
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

  def _S: Parser[List[lexer.Token]] = {
    // logger.log("[parser] _S skip start")
    rep(acceptIf(t => t != null && lexer.isWhitespace(t))(_ => "whitespace expected"))
  }
  
  def token(name: String): Parser[lexer.Token] = {
    // logger.log(s"[parser] Expecting Defined token: '$name'")
    acceptIf { case t: lexer.Token.Defined if t.s == name => true case _ => false }(t => s"Expected structural token '$name'")
  }
  
  def dataToken(f: lexer.Token.otherwise => Boolean, msg: String): Parser[lexer.Token] = {
    // logger.log(s"[parser] Expecting data token: $msg")
    acceptIf { case t: lexer.Token.otherwise if f(t) => true case _ => false }(t => msg)
  }

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag){
    val rules = ArrayBuffer[(HygenicTag, PackratParser[Any])]()
    private val registeredHashes = scala.collection.mutable.Set[Int]()
    var updated = false 

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      logger.log(s"[parser.db] Attempting to set rule: ${t.name}")
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[parser.db]   Rule ${t.name} is new (hash=${t.hash}). Adding...")
        rules += ((t, p))
        registeredHashes += t.hash
        updated = true
        logger.log(s"[parser.db]   Updated flag set to true")
      } else {
        logger.log(s"[parser.db]   Rule ${t.name} (hash=${t.hash}) already exists. Skipping")
      }
    }
    def prepend(t: HygenicTag, p: PackratParser[Any]): Unit = { 
      logger.log(s"[parser.db] Attempting to prepend rule: ${t.name}")
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[parser.db]   Rule ${t.name} is new. Prepending...")
        ((t, p)) +=: rules 
        registeredHashes += t.hash
        updated = true
        logger.log(s"[parser.db]   Updated flag set to true")
      } else {
        logger.log(s"[parser.db]   Rule ${t.name} already exists. Skipping")
      }
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = rules.toArray
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  def anyRule: PackratParser[Node] = new PackratParser[Node] {
    def apply(in: Input): ParseResult[Node] = {
      // logger.log(s"[parser] anyRule triggered at ${in.offset}")
      val currentRules = database.getRules
      var i = 0
      while (i < currentRules.length) {
        val (tag, rule) = currentRules(i)
        if (tag.name != "parseProgram") {
          rule(in) match {
            case Success(res: Node, next) if next.offset > in.offset => 
              logger.log(s"[parser]   anyRule MATCH: rule=${tag.name}, next=${next.offset}")
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
  var parseLattice: Array[Array[ParseEdge]] = Array.empty
  private var sourceCode: String = ""

  def buildExhaustiveLattice(lattice: scala.collection.Map[Int, Array[lexer.Token]]): Unit = {
    val maxOffset = if (lattice.isEmpty) 0 else lattice.keys.max
    logger.log(s"[parser] buildExhaustiveLattice start (max: $maxOffset)")
    for (offset <- 0 to maxOffset) {
      if (lattice.contains(offset)) {
        val edges = dispatchAll(new LatticeReader(offset, lattice), lattice)
        if (edges.nonEmpty) {
          logger.log(s"[parser]   Populated offset $offset with ${edges.length} edges")
          parseLattice(offset) = edges
        }
      }
    }
    logger.log("[parser] buildExhaustiveLattice finished")
  }

  def dispatchAll(in: Input, lattice: scala.collection.Map[Int, Array[lexer.Token]]): Array[ParseEdge] = {
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
              logger.log(s"[dispatch] MATCH at $offset: rule=${ruleTag.mangledName}, next=${next.offset}")
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
    logger.log("[parser] prepareLattice start")
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
    if (parseLattice.length < maxOffset + 2) parseLattice = new Array[Array[ParseEdge]](maxOffset + 2)
    logger.log(s"[parser] prepareLattice end. Jump table size: ${nextContentOffset.length}")
  }

  def apply(lattice: Map[Int, Array[lexer.Token]], source: String = ""): ParseResult[Array[Any]] = {
    logger.log(s"[parser] apply called with source length ${source.length}")
    if (lattice.isEmpty) return Success(Array.empty, new LatticeReader(0, Map.empty))
    sourceCode = source
    var currentLattice = lattice
    var finalNodes = Array.empty[Any]
    var lastReader: Reader[lexer.Token] = null

    var needsReevaluation = true
    var passCount = 0

    while (needsReevaluation && passCount < 5) {
      passCount += 1
      logger.log(s"[parser] === PASS #$passCount START ===")
      database.updated = false
      lexer.database.updated = false
      prepareLattice(currentLattice)
      for (j <- 0 until parseLattice.length) parseLattice(j) = null
      
      buildExhaustiveLattice(currentLattice)

      val startOff = if (0 < nextContentOffset.length) nextContentOffset(0) else 0
      val choices = parseLattice(startOff)
      
      if (choices != null && choices.exists(_.ruleTag.name == "parseProgram")) {
        val programEdge = choices.find(_.ruleTag.name == "parseProgram").get
        logger.log(s"[parser]   Program rule MATCHED! Length=${programEdge.nextOffset}")
        val programNode = programEdge.result.asInstanceOf[Node]
        finalNodes = programNode.children.toArray
        lastReader = new LatticeReader(programEdge.nextOffset, currentLattice)
      } else {
        logger.log("[parser]   Program rule NOT matched. Falling back to fragment stitching...")
        val results = ArrayBuffer[Any]()
        var p = startOff
        while (p < parseLattice.length) {
          val currChoices = parseLattice(p)
          if (currChoices == null || currChoices.isEmpty) { 
            val jump = if (p < nextContentOffset.length) nextContentOffset(p) else p + 1
            logger.log(s"[parser]     Hole at $p. Jumping to $jump")
            if (jump > p && jump < parseLattice.length) p = jump else p = Int.MaxValue 
          } else {
            val best = currChoices.maxBy(_.nextOffset)
            logger.log(s"[parser]     Path at $p: selected ${best.ruleTag.name} (next=${best.nextOffset})")
            results += best.result
            val nextRaw = best.nextOffset
            p = if (nextRaw < nextContentOffset.length) nextContentOffset(nextRaw) else nextRaw
          }
        }
        finalNodes = results.toArray
        lastReader = new LatticeReader(if (p == Int.MaxValue) (if(currentLattice.isEmpty) 0 else currentLattice.keys.max) else p, currentLattice)
      }

      val pUpdated = database.updated
      val lUpdated = lexer.database.updated
      logger.log(s"[parser] PASS #$passCount finished. ParserUpdated=$pUpdated, LexerUpdated=$lUpdated")
      if (pUpdated || lUpdated) {
        if (lUpdated && sourceCode.nonEmpty) {
          logger.log("[parser] Re-lexing entire source due to Lexer update...")
          currentLattice = lexer.tokenize(sourceCode)
        }
        needsReevaluation = true
      } else {
        logger.log("[parser] Fixed point reached. Pass iteration stops.")
        needsReevaluation = false
      }
    }

    result = finalNodes
    logger.log(s"[parser] apply complete. Nodes: ${result.length}")
    Success(result, lastReader)
  }

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}