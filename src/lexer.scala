package romanesco
import scala.collection.mutable.Map
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Positional
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

class Lexer(tag:HygenicTag)
extends RegexParsers
with HygenicObj(tag)
{
  override def skipWhitespace: Boolean = false
  implicit val ec: ExecutionContext = ExecutionContext.global

  private def buildFullLattice(source: String): scala.collection.immutable.Map[Int, Array[Token]] = {
    logger.log(s"[lexer] buildFullLattice start. Source length: ${source.length}")
    val lattice = new ConcurrentHashMap[Int, Array[Token]]()
    val reader = new CharSequenceReader(source)
    val len = source.length

    val futures = (0 until len).map {
      offset =>
        Future {
          logger.log(s"[lexer]   Scanning offset $offset...")
          val currentIn = reader.drop(offset)
          val matches = database.whoAreYou(currentIn)
          if (matches.nonEmpty) {
            logger.log(s"[lexer]   Found ${matches.length} matches at offset $offset")
            lattice.put(offset, matches.map(_._1))
          } else {
            logger.log(s"[lexer]   No match at offset $offset")
          }
        }
    }
    logger.log(s"[lexer] Awaiting ${futures.length} scan tasks...")
    Await.result(Future.sequence(futures), Duration.Inf)
    logger.log("[lexer] All scan tasks completed.")
    lattice.asScala.toMap
  }
  
  var latticeResult: Map[Int, Array[Token]] = scala.collection.mutable.Map.empty

  private val whitespaceTags = scala.collection.mutable.Set[HygenicTag]()
  def registerWhitespace(t: HygenicTag): Unit = {
    logger.log(s"[lexer] Registering whitespace tag: ${t.mangledName}")
    whitespaceTags += t
  }
  def isWhitespace(t: Token): Boolean = whitespaceTags.contains(t.tag)

  def apply(reader: java.io.Reader): scala.collection.immutable.Map[Int, Array[Token]] = {
    logger.log("[lexer] apply(Reader) called")
    val sb = new StringBuilder()
    val buf = new Array[Char](4096)
    var n = reader.read(buf)
    while (n != -1) {
      sb.appendAll(buf, 0, n)
      n = reader.read(buf)
    }
    logger.log(s"[lexer] Source read complete. Length: ${sb.length}")
    tokenize(sb.toString)
  }

  def tokenize(source: String): scala.collection.immutable.Map[Int, Array[Token]] = {
    logger.log("[lexer] tokenize called")
    val res = buildFullLattice(source)
    latticeResult = scala.collection.mutable.Map(res.toSeq*)
    logger.log(s"[lexer] Final lattice has ${latticeResult.size} populated offsets")
    res
  }

  def updateLattice(source: String, fromOffset: Int): Unit = {
    logger.log(s"[lexer] updateLattice called from offset $fromOffset")
    val newEdges = scan(source, fromOffset)
    logger.log(s"[lexer] Merging ${newEdges.size} new edge sets into lattice")
    newEdges.foreach { case (off, tokens) => 
      logger.log(s"[lexer]   Updating offset $off with ${tokens.length} tokens")
      latticeResult.put(off, tokens) 
    }
  }

  private def scan(source: String, fromOffset: Int): scala.collection.immutable.Map[Int, Array[Token]] = {
    logger.log(s"[lexer] scan(from=$fromOffset) start")
    val lattice = new ConcurrentHashMap[Int, Array[Token]]()
    val reader = new CharSequenceReader(source)
    val len = source.length
    val futures = (fromOffset until len).map {
      offset =>
        Future {
          val currentIn = reader.drop(offset)
          val matches = database.whoAreYou(currentIn)
          if (matches.nonEmpty) lattice.put(offset, matches.map(_._1))
        }
    }
    Await.result(Future.sequence(futures), Duration.Inf)
    logger.log(s"[lexer] scan(from=$fromOffset) finished")
    lattice.asScala.toMap
  }

  def printStream: Unit = {
    logger.log("[lexer] printStream begin")
    latticeResult.keys.toSeq.sorted.foreach {
      offset =>
        val tokens = latticeResult(offset)
        println(s"Position $offset:")
        tokens.foreach {
          t =>
            val kind = t match { case Token.Defined(_, _) => "Defined" case Token.otherwise(_, _) => "otherwise" }
            println(s"  - [$kind] Token: '${t.s}', Tag: ${t.tag.mangledName}".replace("\n", "\\n").replace("\r", "\\r"))
        }
    }
    logger.log("[lexer] printStream end")
  }

  val database = new d(Hygenicmarker.bless("database",Some(this),true))
  class d(tag:HygenicTag) extends HygenicObj(tag) {
    private val tokenizeRules = scala.collection.mutable.ArrayBuffer[(HygenicTag, Parser[Token])]()
    private val registeredNames = scala.collection.mutable.Set[String]() 
    var updated = false 

    def set(t: HygenicTag, p: Parser[Token]): Unit = {
      logger.log(s"[lexer.db] Attempting to set rule: ${t.name}")
      if (!registeredNames.contains(t.name)) {
        logger.log(s"[lexer.db]   Rule ${t.name} is new. Adding...")
        tokenizeRules += ((t, positioned(p)))
        registeredNames += t.name
        updated = true
        logger.log(s"[lexer.db]   Rule ${t.name} added. Updated flag set to true")
      } else {
        logger.log(s"[lexer.db]   Rule ${t.name} already exists. Skipping")
      }
    }
    
    def whoAreYou(w:Input): Array[(Token, Input)] = {
      logger.log(s"[lexer.db] whoAreYou at offset ${w.offset}")
      val matches = scala.collection.mutable.ArrayBuffer[(Token, Input)]()
      var i = 0
      val len = tokenizeRules.length
      while (i < len) {
        val (tag, parser) = tokenizeRules(i)
        parser(w) match {
          case Success(res, next) => 
            logger.log(s"[lexer.db]   Rule ${tag.name} MATCHED: '${res.s}'")
            matches += ((res, next)) 
          case _ =>
        }
        i += 1
      }
      matches.toArray
    }
  }

  enum Token extends Positional {
    def s: String
    def tag: HygenicTag
    case Defined(override val s: String, override val tag: HygenicTag)
    case otherwise(override val s: String, override val tag: HygenicTag)
  }
}
