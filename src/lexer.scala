package romanesco
import scala.collection.mutable.Map
import scala.util.parsing.combinator._
//ソースの位置情報を把握できるようにする。
import scala.util.parsing.input.OffsetPosition
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
  
  // 並行処理用のコンテキスト
  implicit val ec: ExecutionContext = ExecutionContext.global

  // 全ての可能性を含んだトークン格子を並行して構築するメソッド
  private def Lattice(input:Input): scala.collection.immutable.Map[Int, Array[Token]] = {
    val lattice = new ConcurrentHashMap[Int, Array[Token]]()
    val visited = ConcurrentHashMap.newKeySet[Int]()

    def explore(current: Input): Future[Unit] = {
      val offset = current.offset
      val isNew = visited.add(offset)
      // logger.log(s"[token] explore at offset $offset, atEnd: ${current.atEnd}, isNew: $isNew")
      
      if (current.atEnd || !isNew) {
        Future.successful(())
      } else {
        val matches = database.whoAreYou(current)
        val tasks = scala.collection.mutable.ArrayBuffer[Future[Unit]]()

        if (matches.nonEmpty) {
          lattice.put(offset, matches.map(_._1))
          matches.foreach { case (token, next) => 
            logger.log(s"[token] matched '${token.s.replace("\n", "\\n")}', jumping from $offset to ${next.offset}")
            tasks += explore(next) 
          }
        }
        
        if (matches.isEmpty && !current.atEnd) {
          tasks += explore(current.rest)
        }

        Future.sequence(tasks.toSeq).map(_ => ())
      }
    }

    // 探索の開始と待機
    Await.result(explore(input), Duration.Inf)
    lattice.asScala.toMap
  }
  
  // パース結果を保持する型をMap[Int, Array[Token]]に変更（位置 -> その位置から始まる全トークン）
  var latticeResult: scala.collection.immutable.Map[Int, Array[Token]] = scala.collection.immutable.Map.empty

  def apply(reader: java.io.Reader): scala.collection.immutable.Map[Int, Array[Token]] = {
    // ReaderからStringへ変換して正規化（確実なオフセット追跡のため）
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
    logger.log("[token] build lattice (parallel) begin")
    val in = new scala.util.parsing.input.CharSequenceReader(source)
    val res = Lattice(in)
    latticeResult = res
    logger.log("[token] build lattice (parallel) end")
    res
  }

  // デバッグ用のトークンストリームを出力するメソッドを格子対応に変更
  def printStream: Unit = {
    latticeResult.keys.toSeq.sorted.foreach { offset =>
      val tokens = latticeResult(offset)
      println(s"Position $offset:")
      tokens.foreach { t =>
        println(s"  - Token: '${t.s}', Tag: ${t.tag.mangledName}".replace("\n", "\\n").replace("\r", "\\r"))
      }
    }
  }
  //データベース。特定のワードやデリミタ、ルールを定義する。ハッシュ値の親はこのLexerクラスにする。
  val database=new d(Hygenicmarker.bless("database",Some(this),true))
  class d(tag:HygenicTag) extends HygenicObj(tag) {
    //基本的なトークナイズルールのマップ
    private var definedTokenizeRules:Map[HygenicTag,Parser[Token]]=Map.empty
    private var otherwisetokenizeRules:Map[HygenicTag,Parser[Token]]=Map.empty
    
    // パフォーマンスのためのキャッシュ
    def cachedDefinedRules: Array[(HygenicTag, Parser[Token])] = definedTokenizeRules.toArray
    def cachedOtherwiseRules: Array[(HygenicTag, Parser[Token])] = otherwisetokenizeRules.toArray
    
    private var counter:BigInt=0
    private var update=false
    def updateswitchable(b:Boolean)={
      update=b
      logger.log(s"[token] database ${tag.mangledName} update flag is now ${update}")
    }
    //トークナイズルールの順番
    private var RuleOrder:Map[HygenicTag,BigInt]=Map.empty
    
    //
    def set(t:HygenicTag,p:Parser[Token]): Unit = {
      //pが返すTokenの状態に応じてマップの保存先を変える。
      val pp = positioned(p)
      p match{
        case p if p.isInstanceOf[Parser[Token.Defined]] =>
        definedTokenizeRules += (t -> pp)
        RuleOrder += (t -> counter)
        case p if p.isInstanceOf[Parser[Token.otherwise]] =>
        otherwisetokenizeRules += (t -> pp)
        RuleOrder += (t -> counter)
        case _ => throw new AssertionError("Token enumuation has 2 cases, neither of which")
        
      }
      counter += 1
      updateswitchable(true)
    }
    
    //どのルールでパースするか判断し、マッチした全てを返す
    def whoAreYou(w:Input): Array[(Token, Input)] = {
      logger.log("[token] whoAreYou begin (all matches mode)")
      
      val currentDefinedRules = cachedDefinedRules
      val currentOtherwiseRules = cachedOtherwiseRules
      val matches = scala.collection.mutable.ArrayBuffer[(Token, Input)]()

      // 定義済みルールを全て探索
      var i = 0
      val defLen = currentDefinedRules.length
      while (i < defLen) {
        val (_, parser) = currentDefinedRules(i)
        parser(w) match {
          case Success(res, next) =>
            logger.log(s"[token] match found: '${res.s}'")
            matches += ((res, next))
          case _ =>
        }
        i += 1
      }

      // その他ルールを全て探索
      i = 0
      val otherLen = currentOtherwiseRules.length
      while (i < otherLen) {
        val (_, parser) = currentOtherwiseRules(i)
        parser(w) match {
          case Success(res, next) =>
            logger.log(s"[token] match found: '${res.s}'")
            matches += ((res, next))
          case _ =>
        }
        i += 1
      }
      
      logger.log(s"[token] whoAreYou end, found ${matches.size} matches")
      matches.toArray
    }
    def dumpDataBase: (Array[(HygenicTag, Parser[Token])],Array[(HygenicTag, Parser[Token])]) = (definedTokenizeRules.toArray,otherwisetokenizeRules.toArray)
  }
  //定義されたトークンとそうでないトークンを区別するenum
  //主な使い道はデリミタとただのワードの区別
  //特別に定義されたデリミタを中心にDefinedを用いる。
  //それ以外の場合はotherwiseを用いる。
  enum Token extends Positional{
    def s: String
    def tag: HygenicTag
    def isWhiteSpace: Boolean = tag.name.startsWith("whiteSpace") || tag.name == "ws"
    case Defined(val s:String,val tag:HygenicTag)
    case otherwise(val s:String,val tag:HygenicTag)
  }
}
