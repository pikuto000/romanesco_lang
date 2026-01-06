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
      // offsetが格子の最大キーを超えている場合は終了
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

  // 格子の分岐を考慮したディスパッチャー
  // 入力位置にある全トークン候補に対し、登録された全ルールをバックトラッキングで試す
  def dispatch: Parser[Any] = new Parser[Any] {
    def apply(in: Input): ParseResult[Any] = {
      val offset = in.pos match {
        case op: OffsetPosition => op.offset
        case _ => in.offset
      }

      // 空白スキップ
      var currentOffset = offset
      var tokensAtThisPos = lastLattice.getOrElse(currentOffset, Array.empty[lexer.Token])
      while (tokensAtThisPos.nonEmpty && tokensAtThisPos.forall(_.tag.name.startsWith("whiteSpace"))) {
        val wsLen = tokensAtThisPos(0).s.length
        currentOffset += wsLen
        tokensAtThisPos = lastLattice.getOrElse(currentOffset, Array.empty[lexer.Token])
      }

      val currentRules = database.getRules

      if (tokensAtThisPos.isEmpty) {
        if (in.atEnd) Success((), in) else Failure(s"No tokens at $currentOffset", in)
      } else {
        // 実質的なトークン候補（空白以外）
        val contentTokens = tokensAtThisPos.filterNot(_.tag.name.startsWith("whiteSpace"))
        
        def tryTokens(tIdx: Int): ParseResult[Any] = {
          if (tIdx >= contentTokens.length) Failure("No match in lattice", in)
          else {
            val t = contentTokens(tIdx)
            val readerWithToken = new LatticeReader(currentOffset, lastLattice).select(tokensAtThisPos.indexOf(t))
            
            def tryRules(rIdx: Int): ParseResult[Any] = {
              if (rIdx >= currentRules.length) tryTokens(tIdx + 1)
              else {
                val (tag, ruleParser) = currentRules(rIdx)
                ruleParser(new PackratReader(readerWithToken)) match {
                  case s: Success[_] => 
                    logger.log(s"[parse] matched: ${tag.mangledName} with '${t.s}'")
                    s
                  case _ => tryRules(rIdx + 1)
                }
              }
            }
            tryRules(0)
          }
        }
        tryTokens(0)
      }
    }
  }

  private var lastLattice: Map[Int, Array[lexer.Token]] = Map.empty

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

  def apply(lattice: Map[Int, Array[lexer.Token]]): ParseResult[Array[Any]] = {
    lastLattice = lattice
    var currentReader: Reader[lexer.Token] = new PackratReader(new LatticeReader(0, lattice))
    val results = scala.collection.mutable.ArrayBuffer[Any]()
    
    while (!currentReader.atEnd) {
      dispatch(currentReader) match {
        case Success(res, next) =>
          if (res != ()) results += res
          currentReader = next
        case f: NoSuccess => return Failure(f.msg, currentReader)
      }
    }
    val finalRes = results.toArray
    result = finalRes
    Success(finalRes, currentReader)
  }

  var result: Array[Any] = Array.empty

  def printStream: Unit = {
    result.foreach {
      res =>
        println(s"printStream: Result: $res".replace("\n", "\\n").replace("\r", "\\r"))
    }
  }

  def eof: Parser[Unit] = new Parser[Unit] {
    def apply(in: Input) = if (in.atEnd) Success((), in) else Failure("Expected EOF", in)
  }
}
