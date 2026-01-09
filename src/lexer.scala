package romanesco
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import java.util.concurrent.ConcurrentHashMap

class Lexer(tag: HygenicTag) extends RegexParsers with HygenicObj(tag) {
  override def skipWhitespace = false
  implicit val ec: ExecutionContext = ExecutionContext.global

  sealed trait Token extends Positional { def s: String; def tag: HygenicTag }
  object Token {
    case class Defined(s: String, tag: HygenicTag) extends Token
    case class otherwise(s: String, tag: HygenicTag) extends Token
  }

  val database = new d(Hygenicmarker.bless("database", Some(this), true))
  class d(tag:HygenicTag) extends HygenicObj(tag) {
    val rules = new ConcurrentHashMap[Int, Parser[Token]]()
    @volatile var updated = false 
    def set(t: HygenicTag, p: Parser[Token]): Unit = {
      if (!rules.containsKey(t.hash)) {
        logger.debug(s"[lexer.db] New rule: ${t.name}")
        rules.put(t.hash, p)
        updated = true
      }
    }
  }

  private var whitespaceTag: Option[HygenicTag] = None
  def registerWhitespace(t: HygenicTag): Unit = { whitespaceTag = Some(t) }
  def isWhitespace(t: Token): Boolean = whitespaceTag.exists(_.hash == t.tag.hash)

  private var latticeResult: Map[Int, Array[Token]] = Map.empty

  def tokenize(source: String): Map[Int, Array[Token]] = {
    val lattice = new ConcurrentHashMap[Int, Array[Token]]()
    val offsets = 0 until source.length
    
    val futures = offsets.map { offset =>
      Future {
        val tokens = whoAreYou(new CharSequenceReader(source.drop(offset)))
        if (tokens.nonEmpty) {
          lattice.put(offset, tokens.toArray)
        }
      }
    }
    Await.result(Future.sequence(futures), Duration.Inf)
    
    import scala.jdk.CollectionConverters._
    val result = lattice.asScala.toMap
    latticeResult = result
    result
  }
  
  def printStream: Unit = {
    logger.debug("[lexer] printStream begin")
    latticeResult.keys.toSeq.sorted.foreach { offset =>
      val tokens = latticeResult(offset)
      println(s"Position $offset:")
      tokens.foreach { t =>
        val kind = t match {
          case Token.Defined(_, _) => "Defined"
          case Token.otherwise(_, _) => "otherwise"
        }
        println(s"  - [$kind] Token: '${t.s}', Tag: ${t.tag.mangledName}".replace("\n", "\\n").replace("\r", "\\r"))
      }
    }
    logger.debug("[lexer] printStream end")
  }

  private def whoAreYou(in: Input): List[Token] = {
    val results = scala.collection.mutable.ListBuffer[Token]()
    
    val commentRegex = "#[^\n]*".r
    commentRegex.findPrefixOf(in.source.subSequence(in.offset, in.source.length())) match {
      case Some(c) if whitespaceTag.isDefined => {
        results += Token.otherwise(c, whitespaceTag.get)
      }
      case _ =>
    }

    import scala.jdk.CollectionConverters._
    val rules = database.rules.values().asScala
    rules.foreach { rule =>
      rule(in) match {
        case Success(t, _) => results += t
        case _ =>
      }
    }
    results.toList
  }
}
