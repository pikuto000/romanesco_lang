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

  // 全ての文字位置において全ルールを試行し、格子を構築する
  private def buildFullLattice(source: String): scala.collection.immutable.Map[Int, Array[Token]] = {
    val lattice = new ConcurrentHashMap[Int, Array[Token]]()
    val reader = new CharSequenceReader(source)
    val len = source.length

    // 各位置での試行を Future で並行実行
    val futures = (0 until len).map {
      offset =>
        Future {
          val currentIn = reader.drop(offset)
          val matches = database.whoAreYou(currentIn)
          if (matches.nonEmpty) {
            lattice.put(offset, matches.map(_._1))
          }
        }
    }

    Await.result(Future.sequence(futures), Duration.Inf)
    lattice.asScala.toMap
  }
  
  var latticeResult: scala.collection.immutable.Map[Int, Array[Token]] = scala.collection.immutable.Map.empty

  private val whitespaceTags = scala.collection.mutable.Set[HygenicTag]()
  def registerWhitespace(t: HygenicTag): Unit = whitespaceTags += t
  def isWhitespace(t: Token): Boolean = whitespaceTags.contains(t.tag)

  def apply(reader: java.io.Reader): scala.collection.immutable.Map[Int, Array[Token]] = {
    val sb = new StringBuilder()
    val buf = new Array[Char](4096)
    var n = reader.read(buf)
    while (n != -1) {
      sb.appendAll(buf, 0, n)
      n = reader.read(buf)
    }
    tokenize(sb.toString)
  }

  def tokenize(source: String): scala.collection.immutable.Map[Int, Array[Token]] = {
    logger.log(s"[token] build exhaustive lattice begin (len: ${source.length})")
    val res = buildFullLattice(source)
    latticeResult = res
    logger.log("[token] build exhaustive lattice end")
    res
  }

  def printStream: Unit = {
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
  }

  val database = new d(Hygenicmarker.bless("database",Some(this),true))
  class d(tag:HygenicTag) extends HygenicObj(tag) {
    private val tokenizeRules = scala.collection.mutable.ArrayBuffer[(HygenicTag, Parser[Token])]()
    var updated = false // ルール追加フラグ

    def set(t:HygenicTag, p:Parser[Token]): Unit = {
      tokenizeRules += ((t, positioned(p)))
      updated = true
      logger.log(s"[token] database added rule: ${t.mangledName}")
    }
    
    def whoAreYou(w:Input): Array[(Token, Input)] = {
      val matches = scala.collection.mutable.ArrayBuffer[(Token, Input)]()
      var i = 0
      val len = tokenizeRules.length
      while (i < len) {
        val (_, parser) = tokenizeRules(i)
        parser(w) match {
          case Success(res, next) => matches += ((res, next)) 
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

