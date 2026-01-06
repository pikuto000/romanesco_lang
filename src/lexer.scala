package romanesco
import scala.collection.mutable.Map
import scala.util.parsing.combinator._
//ソースの位置情報を把握できるようにする。
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Positional


class Lexer(tag:HygenicTag)
extends RegexParsers
with HygenicObj(tag)
{
  override def skipWhitespace: Boolean = false
  
  
  //トークナイズをするメソッド
  private def Stream(input:Input): ParseResult[Array[Token]] = {
    logger.log("[token] tokenize begin")
    lazy val tokenizer: Parser[Token] ={
      input => {
        database.whoAreYou(input) match {
          case Some(p) => p(input)
          case None => Failure("No matching token rule found", input)
        }
      }
    }
    lazy val parseRes = phrase(rep(tokenizer)) (input) match {
      case Success(res, next) =>
      result = res.toArray
      Success(res.toArray, next)
      case NoSuccess(msg, next) =>
      Error(msg, next)
    }
    logger.log("[token] tokenize end")
    parseRes
  }
  def apply(reader: java.io.Reader): Array[Token] = {
    lazy val in=scala.util.parsing.input.StreamReader(reader)
    Stream(in) match {
      case Success(res, next) => {result=res;res}
      case NoSuccess(msg, next) => throw new Exception(msg)
    }
  }
  //一旦パース結果をおいておく場所。
  //主にデバッグやマクロによる編集のために使用される。
  var result:Array[Token]=Array.empty
  
  //デバッグ用のトークンを文字列としてダンプするメソッド
  def dumpResult:Array[(String,Boolean,HygenicTag)]=
  result.map(x=>x match {
    case Token.Defined(s,t)=>(s,true,t)
    case Token.otherwise(s,t)=>(s,false,t)
  })
  
  //デバッグ用のトークンストリームを出力するメソッド
  def printStream: Unit = {
    dumpResult.foreach(s=>
      s match {
        case (content, isDefined, tag) =>
          println(s"printStream: Token: '${content}', isDefined: $isDefined, Name: ${tag.mangledName}, ancestorHash: ${tag.ancestorHash}".replace("\n", "\\n").replace("\r", "\\r"))
      }
    )
  }
  //データベース。特定のワードやデリミタ、ルールを定義する。ハッシュ値の親はこのLexerクラスにする。
  lazy val database=new d(Hygenicmarker.bless("database",Some(this),true))
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
      lazy val pp = positioned(p)
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
    
        //どのルールでパースするか判断する
    
        //最長一致原則と登録順(RuleOrder)を用いて解決する。
    
        //definedToknizeRulesを用いて解決できないときは、otherwisetokenizeRulesを用いて解決する。
    
        def whoAreYou(w:Input):Option[Parser[Token]]={
          logger.log("[token] whoAreYou begin")
          val currentDefinedRules = cachedDefinedRules
          val currentOtherwiseRules = cachedOtherwiseRules
          var maxMatchRes: Token = null
          var maxMatchNext: Input = null
          var maxLen = -1
          var maxOrder: BigInt = -1
          // 定義済みルールを探索 (高速なwhileループ)
          var i = 0
          lazy val defLen = currentDefinedRules.length
          while (i < defLen) {
            lazy val (tag, parser) = currentDefinedRules(i)
            logger.log(s"[token] trying toknizer for tag: ${tag.mangledName}")
            parser(w) match {
              case Success(res, next) =>
                logger.log(s"[token] matched with parser for tag: ${tag.mangledName}")
                lazy val len = res.s.length
                if (len > maxLen) {
                  logger.log(s"[token] new longest match for tag: ${tag.mangledName}, length: ${len}")
                  maxLen = len
                  maxMatchRes = res
                  maxMatchNext = next
                  maxOrder = RuleOrder(tag)
                } else if (len == maxLen) {
                  lazy val order = RuleOrder(tag)
                  if (order > maxOrder) {
                    logger.log(s"[token] same length match but newer rule for tag: ${tag.mangledName}, length: ${len}")
                    maxMatchRes = res
                    maxMatchNext = next
                    maxOrder = order
                  } else {
                    logger.log(s"[token] match discarded (shorter or older rule) for tag: ${tag.mangledName}")
                  }
                } else {
                   logger.log(s"[token] match discarded (shorter) for tag: ${tag.mangledName}")
                }
              case _ =>
                logger.log(s"[token] no success for tag: ${tag.mangledName}")
            }
            i += 1
          }
    
          // その他ルールを探索
          i = 0
          lazy val otherLen = currentOtherwiseRules.length
          while (i < otherLen) {
            lazy val (tag, parser) = currentOtherwiseRules(i)
            logger.log(s"[token] trying parser for tag: ${tag.mangledName}")
            parser(w) match {
              case Success(res, next) =>
                logger.log(s"[token] matched with parser for tag: ${tag.mangledName}")
                lazy val len = res.s.length
                if (len > maxLen) {
                  logger.log(s"[token] new longest match for tag: ${tag.mangledName}, length: ${len}")
                  maxLen = len
                  maxMatchRes = res
                  maxMatchNext = next
                  maxOrder = RuleOrder(tag)
                } else if (len == maxLen) {
                  lazy val order = RuleOrder(tag)
                  if (order > maxOrder) {
                    logger.log(s"[token] same length match but newer rule for tag: ${tag.mangledName}, length: ${len}")
                    maxMatchRes = res
                    maxMatchNext = next
                    maxOrder = order
                  } else {
                     logger.log(s"[token] match discarded (shorter or older rule) for tag: ${tag.mangledName}")
                  }
                } else {
                  logger.log(s"[token] match discarded (shorter) for tag: ${tag.mangledName}")
                }
              case _ =>
                logger.log(s"[token] no success for tag: ${tag.mangledName}")
            }
            i += 1
          }
      logger.log("[token] whoAreYou end")
      if (maxMatchRes != null) {
        // 結果を直接返すパーサーを生成して返す (二重パース回避)
        Some(new Parser[Token] {
          def apply(in: Input) = Success(maxMatchRes, maxMatchNext)
        })
      } else {
        None
      }
    }
    def dumpDataBase: (Array[(HygenicTag, Parser[Token])],Array[(HygenicTag, Parser[Token])]) = (definedTokenizeRules.toArray,otherwisetokenizeRules.toArray)
  }
  //定義されたトークンとそうでないトークンを区別するenum
  //主な使い道はデリミタとただのワードの区別
  //特別に定義されたデリミタを中心にDefinedを用いる。
  //それ以外の場合はotherwiseを用いる。
  enum Token extends Positional{
    def s: String
    case Defined(val s:String,val tag:HygenicTag)
    case otherwise(val s:String,val tag:HygenicTag)
  }
}
