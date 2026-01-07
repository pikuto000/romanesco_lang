package romanesco
import scala.collection.mutable.{Map => MutableMap, ArrayBuffer}
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration

class Lexer(tag: HygenicTag) extends RegexParsers with HygenicObj(tag) {
  override def skipWhitespace = false
  
  implicit val ec: ExecutionContext = ExecutionContext.global

  trait Token extends Positional {
    def s: String
    def tag: HygenicTag
  }
  object Token {
    case class Defined(s: String, tag: HygenicTag) extends Token
    case class otherwise(s: String, tag: HygenicTag) extends Token
  }

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag: HygenicTag) extends HygenicObj(tag) {
    val rules = ArrayBuffer[(HygenicTag, Parser[Token])]()
    private val registeredHashes = scala.collection.mutable.Set[Int]()
    @volatile var updated = false // メモリ可視性を確保

    def set(t: HygenicTag, p: Parser[Token]): Unit = synchronized {
      if (!registeredHashes.contains(t.hash)) {
        logger.log(s"[lexer.db] Attempting to set rule: ${t.name}")
        rules += ((t, p))
        registeredHashes += t.hash
        updated = true
        logger.log(s"[lexer.db]   Rule ${t.name} added. Updated flag set to true")
      }
    }
    
    def getRules: Array[(HygenicTag, Parser[Token])] = synchronized { rules.toArray }
  }

  private var whitespaceTag: Option[HygenicTag] = None
  def registerWhitespace(t: HygenicTag): Unit = synchronized { whitespaceTag = Some(t) }
  def isWhitespace(t: Token): Boolean = synchronized { whitespaceTag.exists(_.hash == t.tag.hash) }

  def tokenize(source: String): Map[Int, Array[Token]] = {
    logger.log(s"[lexer] tokenize called")
    val lattice = buildFullLattice(source)
    lattice
  }

  private def buildFullLattice(source: String): Map[Int, Array[Token]] = {
    logger.log(s"[lexer] buildFullLattice start. Source length: ${source.length}")
    val tokensAtOffset = new java.util.concurrent.ConcurrentHashMap[Int, ArrayBuffer[Token]]()
    val offsets = source.indices
    
    val futures = offsets.map { offset =>
      Future {
        val input = source.substring(offset)
        val currentRules = database.getRules
        var i = 0
        while (i < currentRules.length) {
          val (tag, parser) = currentRules(i)
          parse(parser, input) match {
            case Success(token, _) =>
              val buffer = tokensAtOffset.computeIfAbsent(offset, _ => ArrayBuffer[Token]())
              buffer.synchronized { buffer += token }
            case _ =>
          }
          i += 1
        }
      }
    }
    
    Await.result(Future.sequence(futures), Duration.Inf)
    
    import scala.jdk.CollectionConverters._
    tokensAtOffset.asScala.map { case (off, buf) => (off, buf.toArray) }.toMap
  }
}