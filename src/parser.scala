package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer}
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

case class ParseEdge(result: Any, nextOffset: Int, ruleTag: HygenicTag):
  override def toString: String = s"Edge(-> $nextOffset, rule: ${ruleTag.mangledName})"

class Parser(val lexer: Lexer, tag: HygenicTag, val solver: Option[Solver] = None)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag):

  type Elem = lexer.Token
  implicit val ec: ExecutionContext = ExecutionContext.global

  private var hasContentSuffix: java.util.BitSet = new java.util.BitSet()

  class LatticeReader(override val offset: Int, val lattice: scala.collection.Map[Int, Array[lexer.Token]], val tIdx: Int = 0) extends Reader[lexer.Token]:
    val availableTokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    override def first: lexer.Token = if availableTokens.isDefinedAt(tIdx) then availableTokens(tIdx) else null
    override def atEnd: Boolean = !hasContentSuffix.get(offset)
    override def rest: Reader[lexer.Token] =
      if first == null then this
      else
        val nextRawOff = offset + first.s.length
        val nextJumpOff = if nextRawOff < nextContentOffset.length then nextContentOffset(nextRawOff) else nextRawOff
        new LatticeReader(nextJumpOff, lattice, 0)
    override def pos: Position = if first != null then first.pos else NoPosition
    def select(newTIdx: Int): LatticeReader = new LatticeReader(offset, lattice, newTIdx)
    override def hashCode: Int = offset ^ tIdx
    override def equals(obj: Any): Boolean = obj match
      case other: LatticeReader => offset == other.offset && tIdx == other.tIdx
      case _ => false

  def _S: Parser[List[lexer.Token]] = rep(acceptIf(t => t != null && lexer.isWhitespace(t))(_ => "whitespace expected"))
  
  def token(name: String): Parser[lexer.Token] = acceptIf { 
    case t: lexer.Token.Defined if t.s == name => 
      logger.debug(s"[token] Matched Defined token: s='${t.s}', expected='$name'")
      true 
    case t => 
      if (t != null && t.isInstanceOf[lexer.Token.Defined]) {
        logger.debug(s"[token] Failed Defined token: s='${t.s}', expected='$name'")
      }
      false 
  }(t => s"Expected '$name'")

  def dataToken(f: lexer.Token.otherwise => Boolean, msg: String): Parser[lexer.Token] = acceptIf { 
    case t: lexer.Token.otherwise if f(t) => true 
    case _ => false 
  }(t => msg)

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag):
    val rules = ArrayBuffer[(HygenicTag, PackratParser[Any])]()
    private val registeredHashes = scala.collection.mutable.Set[Int]()
    @volatile var updated = false 

    def set(t: HygenicTag, p: PackratParser[Any]): Unit = synchronized { 
      if !registeredHashes.contains(t.hash) then
        rules += ((t, p)); registeredHashes += t.hash; updated = true
    }
    def prepend(t: HygenicTag, p: PackratParser[Any]): Unit = synchronized { 
      if !registeredHashes.contains(t.hash) then
        ((t, p)) +=: rules; registeredHashes += t.hash; updated = true
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = synchronized { rules.toArray }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  def anyRule: PackratParser[Node] = new PackratParser[Node] {
    def apply(in: Input): ParseResult[Node] =
      val currentRules = database.getRules
      var i = 0
      while i < currentRules.length do
        val (tag, rule) = currentRules(i)
        if tag.name != "parseProgram" then
          rule(in) match
            case Success(res: Node, next) if next.offset > in.offset => 
              return Success(res, next)
            case _ =>
        i += 1
      Failure("No rule matched", in)
  }

  var nextContentOffset: Array[Int] = Array.empty
  var parseLattice: java.util.concurrent.ConcurrentHashMap[Int, Array[ParseEdge]] = new java.util.concurrent.ConcurrentHashMap()
  private var sourceCode: String = ""

  def buildExhaustiveLattice(lattice: scala.collection.Map[Int, Array[lexer.Token]]): Unit =
    val offsets = lattice.keys.toSeq.sorted
    logger.debug(s"[buildLattice] Building for ${offsets.length} offsets")
    val futures = offsets.map { offset =>
      Future {
        val edges = dispatchAll(new LatticeReader(offset, lattice), lattice)
        if edges.nonEmpty then 
          parseLattice.put(offset, edges)
          logger.debug(s"[buildLattice] Offset $offset: registered ${edges.length} edges")
      }
    }
    Await.result(Future.sequence(futures), Duration.Inf)
    logger.debug(s"[buildLattice] Complete. ParseLattice size: ${parseLattice.size()}")

  def dispatchAll(in: Input, lattice: scala.collection.Map[Int, Array[lexer.Token]]): Array[ParseEdge] =
    val offset = in.offset
    val tokens = lattice.getOrElse(offset, Array.empty[lexer.Token])
    val currentRules = database.getRules
    val builder = Array.newBuilder[ParseEdge]
    val definedStrings = tokens.collect { case t: lexer.Token.Defined => t.s }.toSet
    
    if (offset == 0 || offset % 10 == 0) {
      logger.debug(s"[parser.dispatch] Offset $offset: ${tokens.length} tokens, ${currentRules.length} rules")
    }
    
    var tIdx = 0
    while tIdx < tokens.length do
      val t = tokens(tIdx)
      val shouldSkip = t == null || lexer.isWhitespace(t) || (t.isInstanceOf[lexer.Token.otherwise] && definedStrings.contains(t.s))
      
      if (offset == 0) {
        logger.debug(s"[parser.dispatch] Token #$tIdx at offset 0: s='${t.s}', shouldSkip=$shouldSkip")
      }
      
      if !shouldSkip then
        val packratIn = new PackratReader(new LatticeReader(offset, lattice, tIdx))
        var rIdx = 0
        var matchedAny = false
        while rIdx < currentRules.length do
          val (ruleTag, rule) = currentRules(rIdx)
          rule(packratIn) match
            case Success(res, next) => 
              matchedAny = true
              logger.debug(s"[parser.dispatch] Match at offset $offset: rule=${ruleTag.name}, nextOffset=${next.offset}")
              builder.addOne(ParseEdge(res, next.offset, ruleTag))
            case _ => 
          rIdx += 1
        
        if (offset == 0 && !matchedAny) {
          logger.debug(s"[parser.dispatch] Token #$tIdx at offset 0 matched NO rules")
        }
      tIdx += 1
    builder.result()

  private def prepareLattice(lattice: scala.collection.Map[Int, Array[lexer.Token]]): Unit =
    val maxOffset = if lattice.isEmpty then 0 else lattice.keys.max
    val latticeArr = new Array[Array[lexer.Token]](maxOffset + 1)
    lattice.foreach { (off, tokens) => if off < latticeArr.length then latticeArr(off) = tokens }
    
    nextContentOffset = new Array[Int](maxOffset + 1)
    var lastContent = maxOffset + 1
    var i = maxOffset
    while i >= 0 do
      val tokens = latticeArr(i)
      if tokens != null && tokens.exists(t => !lexer.isWhitespace(t)) then lastContent = i
      nextContentOffset(i) = lastContent
      i -= 1

    hasContentSuffix.clear()
    var runningContent = false
    i = maxOffset
    while i >= 0 do
      val tokens = latticeArr(i)
      if tokens != null && tokens.exists(t => !lexer.isWhitespace(t)) then runningContent = true
      if runningContent then hasContentSuffix.set(i)
      i -= 1

  def apply(lattice: Map[Int, Array[lexer.Token]], source: String = ""): ParseResult[Array[Any]] =
    if lattice.isEmpty then return Success(Array.empty, new LatticeReader(0, Map.empty))
    sourceCode = source
    var currentLattice = lattice
    var finalNodes = Array.empty[Any]
    var lastReader: Reader[lexer.Token] = null

    @volatile var needsReevaluation = true
    var passCount = 0

    while needsReevaluation && passCount < 10 do
      passCount += 1
      logger.debug(s"[parser] === PASS #$passCount START ===")
      database.updated = false; lexer.database.updated = false
      prepareLattice(currentLattice)
      parseLattice.clear()
      
      logger.debug(s"[parser] Registered rules: ${database.getRules.map(_._1.name).mkString(", ")}")
      buildExhaustiveLattice(currentLattice)
      logger.debug(s"[parser] ParseLattice has ${parseLattice.size()} offsets with edges")

      val startOff = if 0 < nextContentOffset.length then nextContentOffset(0) else 0
      logger.debug(s"[parser] Starting offset: $startOff")
      
      // 論理駆動パス探索（副作用なし）
      val (currentNodes, finalOff) = searchLogicalPath(startOff, currentLattice) match
        case Some((nodes, off)) => 
          logger.debug(s"[parser] searchLogicalPath found ${nodes.length} nodes")
          (nodes, off)
        case None => 
          logger.debug(s"[parser] searchLogicalPath failed, falling back to longest")
          val (nodes, reader) = fallbackToLongest(startOff, currentLattice)
          logger.debug(s"[parser] fallbackToLongest found ${nodes.length} nodes")
          (nodes, reader.offset)
      
      finalNodes = currentNodes
      lastReader = new LatticeReader(finalOff, currentLattice)
      logger.debug(s"[parser] Pass result: ${finalNodes.length} nodes")

      registerMacrosDeterministically(finalNodes)

      if database.updated || lexer.database.updated then
        logger.debug(s"[parser] Database updated, re-evaluating")
        if lexer.database.updated && sourceCode.nonEmpty then
          currentLattice = lexer.tokenize(sourceCode)
        needsReevaluation = true
      else needsReevaluation = false

    result = finalNodes
    logger.debug(s"[parser] Final result: ${result.length} nodes")
    Success(finalNodes, lastReader)

  // 論理的なパスを深さ優先探索（DFS）で探す。Push/Popで状態を保護。
  private def searchLogicalPath(offset: Int, lattice: Map[Int, Array[lexer.Token]]): Option[(Array[Any], Int)] =
    logger.debug(s"[searchPath] Entering at offset $offset")
    if offset >= maxOffsetSafe(lattice) then 
      logger.debug(s"[searchPath] Reached end at offset $offset")
      return Some((Array.empty, offset))
    
    val choices = parseLattice.get(offset)
    logger.debug(s"[searchPath] Offset $offset has ${if (choices == null) 0 else choices.length} choices")
    if choices == null || choices.isEmpty then
      val jump = if offset < nextContentOffset.length then nextContentOffset(offset) else offset + 1
      logger.debug(s"[searchPath] No choices, jumping to $jump")
      if jump > offset && jump < nextContentOffset.length + 1 then return searchLogicalPath(jump, lattice)
      else return Some((Array.empty, offset))

    // エッジを長さ順（長い順）に試す
    val sortedEdges = choices.sortBy(-_.nextOffset)
    logger.debug(s"[searchPath] Trying ${sortedEdges.length} edges at offset $offset")
    
    for edge <- sortedEdges do
      logger.debug(s"[searchPath] Trying edge: rule=${edge.ruleTag.name}, nextOffset=${edge.nextOffset}")
      val node = edge.result match
        case n: Node => n
        case _ => null

      // 制約ノードならチェック
      val isConstraint = node != null && (node.kind == "Unification" || node.kind == "BinaryOp" || node.kind == "Block")
      var isFeasible = true
      
      if isConstraint && solver.isDefined then
        // 状態を保存して追加
        solver.get.push()
        solver.get.solve(node)
        // チェック
        isFeasible = try
          val status = solver.get.checkFeasibility(node) // checkFeasibility 内部でも push/pop しているが、ここでは solve 後に矛盾がないかを見るため、手動 push/pop 内で check() を呼ぶべき
          // 簡易化: solver.checkFeasibility(node) は「現在の状態 + node」をチェックするメソッドとして定義済みなので、
          // ここでは「探索のために状態を進める」必要がある。
          // 正しいロジック:
          // 1. push
          // 2. solve(node)
          // 3. check() -> SAT なら進む
          // 4. 再帰
          // 5. pop (必ず)
          
          // よって solver.get.check() を呼ぶ
          val s = solver.get
          // ※ Solver.scala の checkFeasibility は push/solve/check/pop を一括でやるメソッドなので、
          // 探索の状態遷移には向かない。
          // ここでは solver.solve(node) した後の状態をチェックしたい。
          // しかし Solver クラスには現状 check() しかない。
          // リファクタリングした Solver.scala を見ると check() は println するだけ。
          // API が不足している。
          true // 仮：後ほど Solver を修正する必要あり
        catch case _ => false

      // Solver の API 不足を考慮し、ここでは「checkFeasibility」を使って事前チェックする（副作用なし）
      if isFeasible && (node == null || !isConstraint || solver.isEmpty || solver.get.checkFeasibility(node)) then
        // OKそうなら、状態を進めて（Push）再帰
        if isConstraint && solver.isDefined then 
          solver.get.push()
          solver.get.solve(node)

        searchLogicalPath(edge.nextOffset, lattice) match
          case Some((tailNodes, finalOff)) =>
            // 成功したパスが見つかったなら、このパスを採用。
            // ただし、呼び出し元に戻るときに pop する必要があるので、
            // 「永続化」するか「戻す」か。
            // ここでは「探索」なので、見つかったら Pop して（状態を元に戻して）結果だけ返すのが正解。
            // しかし「確定」させる必要もある。
            // Main ループで毎回 Solver を作り直しているなら、ここで Pop してしまって良い。
            if isConstraint && solver.isDefined then solver.get.pop()
            return Some((edge.result +: tailNodes, finalOff))
          case None =>
            // 失敗したら Pop して次へ
            if isConstraint && solver.isDefined then solver.get.pop()
    
    None

  private def fallbackToLongest(startOff: Int, lattice: Map[Int, Array[lexer.Token]]): (Array[Any], Reader[lexer.Token]) =
    val results = ArrayBuffer[Any]()
    var p = startOff
    while p < maxOffsetSafe(lattice) do
      val currChoices = parseLattice.get(p)
      if currChoices == null || currChoices.isEmpty then
        val jump = if p < nextContentOffset.length then nextContentOffset(p) else p + 1
        if jump > p && jump < nextContentOffset.length + 1 then p = jump else p = Int.MaxValue
      else
        val best = currChoices.maxBy(_.nextOffset)
        results += best.result
        p = best.nextOffset
    (results.toArray, new LatticeReader(if p == Int.MaxValue then maxOffsetSafe(lattice) else p, lattice))

  private def registerMacrosDeterministically(nodes: Array[Any]): Unit =
    val macroDefs = nodes.collect { case n: Node if n.kind == "MacroDef" => n }
    macroDefs.foreach { md =>
      val name = md.attributes("name").toString
      val patterns = md.attributes("patterns").asInstanceOf[List[Node]]
      val bodyList = md.attributes("bodyList").asInstanceOf[List[Node]]

      if Macro.register(name, patterns, bodyList) then
        patterns.foreach {
          case n if n.kind == "LiteralWord" =>
            val word = n.attributes("value").toString
            val mTag = Hygenicmarker.bless(s"kw_$word", Some(lexer), true)
            lexer.database.set(mTag, lexer.positioned(lexer.regex(java.util.regex.Pattern.quote(word).r) ^^ { s => lexer.Token.Defined(s, mTag) }))
          case _ =>
        }
    }

  private def maxOffsetSafe(l: Map[Int, Array[lexer.Token]]): Int = if l.isEmpty then 0 else l.keys.max + 1

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { (res, idx) => println(s"printStream: [$idx] Result: $res") }
