package romanesco
import scala.collection.mutable.{Map => MutableMap}
import scala.util.parsing.combinator._
import scala.util.parsing.input._

// Lexerからのトークン格子を解析するパーサー
class Parser(val lexer: Lexer, tag: HygenicTag)
  extends Parsers
  with PackratParsers
  with HygenicObj(tag) {

  type Elem = lexer.Token

  // LatticeReader: 空白スキップを一切行わない「純粋な」リーダー
  class LatticeReader(override val offset: Int, val lattice: Map[Int, Array[lexer.Token]], val tIdx: Int = 0) extends Reader[lexer.Token] {
    val availableTokens = lattice.getOrElse(offset, Array.empty[lexer.Token])

    override def first: lexer.Token = if (availableTokens.isDefinedAt(tIdx)) availableTokens(tIdx) else null

    override def atEnd: Boolean = {
      // 現在の位置にトークンがなく、かつこれ以降にもトークンがない場合に終了
      availableTokens.isEmpty && !lattice.keys.exists(_ > offset)
    }

    // 単純に次の位置へ進むだけ
    override def rest: Reader[lexer.Token] = {
      if (atEnd) this
      else {
        val nextRawOff = offset + first.s.length
        new LatticeReader(nextRawOff, lattice, 0)
      }
    }

    override def pos: Position = if (first != null) first.pos else NoPosition
    def select(newTIdx: Int): LatticeReader = new LatticeReader(offset, lattice, newTIdx)
  }

  // --- 明示的な空白スキップ ---
  // 0個以上の空白トークンにマッチする
  def _S: Parser[List[lexer.Token]] = rep(acceptIf(_.isWhiteSpace)(_ => "whitespace expected"))

  // --- ユーティリティ ---
  def token(name: String): Parser[lexer.Token] = acceptIf(_.s == name)(t => s"Expected '$name' found '${t.s}'")

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag){
    private var rules: MutableMap[HygenicTag, PackratParser[Any]] = MutableMap.empty
    private var RuleOrder: MutableMap[HygenicTag, BigInt] = MutableMap.empty
    private var counter: BigInt = 0
    private var update = true
    def set(t: HygenicTag, p: PackratParser[Any]): Unit = {
      rules += (t -> p)
      RuleOrder += (t -> counter)
      counter += 1
      update = true
    }
    def getRules: Array[(HygenicTag, PackratParser[Any])] = {
      if (update) { cachedRules = rules.toArray.sortBy(x => RuleOrder(x._1)); update = false }
      cachedRules
    }
    private var cachedRules: Array[(HygenicTag, PackratParser[Any])] = Array.empty
  }

  def addSyntax(t: HygenicTag)(p: PackratParser[Any]): Unit = database.set(t, p)

  private var lastLattice: Map[Int, Array[lexer.Token]] = Map.empty

  def combinedDispatch: Parser[Any] = new Parser[Any] {
    def apply(in: Input): ParseResult[Any] = {
      val currentRules = database.getRules
      var bestRes: Option[ParseResult[Any]] = None
      var maxOff = -1
      
      var i = 0
      while (i < currentRules.length) {
        val (tag, rule) = currentRules(i)
        rule(in) match {
          case s: Success[Any] =>
            if (s.next.offset > maxOff) {
              maxOff = s.next.offset
              bestRes = Some(s)
            }
          case _ => 
        }
        i += 1
      }
      bestRes.getOrElse(Failure("No rule matched", in))
    }
  }

  def apply(lattice: Map[Int, Array[lexer.Token]]): ParseResult[Array[Any]] = {
    lastLattice = lattice
    // 最初が空白なら飛ばして開始
    val initialReader = new PackratReader(new LatticeReader(0, lattice))
    
    // rep( (空白を飛ばす) ~> dispatch )
    val program = rep(_S ~> combinedDispatch)
    
    logger.log(s"[parse] Starting explicit whitespace parse.")
    program(initialReader) match {
      case Success(res, next) =>
        result = res.toArray
        Success(result, next)
      case f: NoSuccess => f.asInstanceOf[ParseResult[Array[Any]]]
    }
  }

  var result: Array[Any] = Array.empty
  def printStream: Unit = result.zipWithIndex.foreach { case (res, idx) => println(s"printStream: [$idx] Result: $res") }
}