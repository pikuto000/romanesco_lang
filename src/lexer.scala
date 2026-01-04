package romanesco
import scala.collection.mutable.Map
import scala.util.parsing.combinator._
//ソースの位置情報を把握できるようにする。
import scala.util.parsing.input._


class Lexer(tag:HygenicTag)
  extends 
    RegexParsers,
    HygenicObj(tag),
    PackratParsers
{
  override def skipWhitespace: Boolean = false


  //トークナイズをするメソッド
  private def Stream(input:Input): ParseResult[Array[Token]] = {
    logger.log("tokenize begin")
    val tokenizer:Parser[Token]={
      input => {
        database.whoAreYou(input) match {
          case Some(p) => p(input).asInstanceOf[ParseResult[Token]]
          case None => Failure("No matching token rule found",input)
        }
      }
    }
    phrase(rep(tokenizer)) (input) match {
      case Success(res, next) =>
        result = res.toArray
        Success(res.toArray, next)
      case NoSuccess(msg, next) =>
        Error(msg, next)
    }
    logger.log("tokenize end")
    Success(result, input)
  }
  def apply(reader: java.io.Reader): Array[Token] = {
    Stream(scala.util.parsing.input.StreamReader(reader)) match {
      case Success(res, next) => {result=res;res}
      case NoSuccess(msg, next) => throw new Exception(msg)
    }
  }
  //一旦パース結果をおいておく場所。
  var result:Array[Token]=Array.empty

  //デバッグ用のトークンを文字列としてダンプするメソッド
  def dumpResult:Array[(String,Boolean,HygenicTag)]=
    result.map(x=>x match {
      case Token.Defined(s,t)=>(s,true,t)
      case Token.otherwise(s,t)=>(s,false,t)
    })

  //デバッグ用のトークンストリームを出力するメソッド
  def printStream:Unit={
    dumpResult.foreach((ds,db,dt)=>println(s"printStream: TokenStream($ds), Defined:$db, TagName is ${dt.name}, Hash is ${dt.hash}, ancestorHash is ${dt.ancestorHash}, isOpaque:${dt.isOpaque}"))
  }

  //データベース。特定のワードやデリミタ、ルールを定義する。
  object database {
    //基本的なトークナイズルールのマップ
    private var definedTokenizeRules:Map[String,Parser[Token]]=Map.empty
    private var otherwisetokenizeRules:Map[String,Parser[Token]]=Map.empty
    private var counter:BigInt=0
    //トークナイズルールの順番
    private var RuleOrder:Map[String,BigInt]=Map.empty

    //
    def set(s:String,p:Parser[Token]): Unit = {
      p match{
        //Parser[Token]が、Definedかotherwiseかで登録先を変える
        case p_defined 
          if p_defined.isInstanceOf[Parser[Token.Defined]] =>{
          definedTokenizeRules += (s -> p)
          RuleOrder += (s -> counter)
          counter += 1
        }
        case p_otherwise 
          if p_otherwise.isInstanceOf[Parser[Token.otherwise]] =>{
          otherwisetokenizeRules += (s -> p)
          RuleOrder += (s -> counter)
          counter += 1
        }
        case _ => throw new AssertionError("Token enumuation has 2 case, but not Defined or otherwise.")
      }
    }

    //どのルールでパースするか判断する
    //最長一致原則と登録順(RuleOrder)を用いて解決する。
    //definedToknizeRulesを用いて解決できないときは、otherwisetokenizeRulesを用いて解決する。
    def whoAreYou(w:Input):Option[Parser[Token]]={
      logger.log("starting ask to database.")
      
      var bestParser: Option[Parser[Token]] = None
      var maxLen: Int = -1
      var bestRuleName: String = ""

      def updateBest(s: String, p: Parser[Token], res: Token): Unit = {
        val strContent = res match {
          case Token.Defined(c,t) => c
          case Token.otherwise(c,t) => c
        }
        val len = strContent.length
        
        logger.log(s"rule ${s} succeeded. Length: $len")

        if (bestParser.isEmpty) {
             maxLen = len
             bestRuleName = s
             bestParser = Some(p)
             logger.log(s"New maxchamp: rule ${s}, length ${len}")
        } else {
             if (len > maxLen) {
                 logger.log(s"rule ${s} length ($len) > maxchamp rule ${bestRuleName} length ($maxLen). Update.")
                 maxLen = len
                 bestRuleName = s
                 bestParser = Some(p)
             } else if (len == maxLen) {
                 logger.log(s"rule ${s} length ($len) == maxchamp rule ${bestRuleName} length ($maxLen). Compare RuleOrder.")
                 // Earlier rule (smaller index) wins.
                 if (RuleOrder(s) < RuleOrder(bestRuleName)) {
                     logger.log(s"rule ${s} order (${RuleOrder(s)}) < maxchamp rule ${bestRuleName} order (${RuleOrder(bestRuleName)}). Update.")
                     bestRuleName = s
                     bestParser = Some(p)
                 } else {
                     logger.log(s"rule ${s} order (${RuleOrder(s)}) >= maxchamp rule ${bestRuleName} order (${RuleOrder(bestRuleName)}). Keep maxchamp.")
                 }
             } else {
                 logger.log(s"rule ${s} length ($len) < maxchamp rule ${bestRuleName} length ($maxLen). Ignored.")
             }
        }
      }

      logger.log("ask to definedTokenizeRules.")
      for ((s, p) <- definedTokenizeRules) {
        logger.log(s"current maxchamp is ${if(bestRuleName.isEmpty) "__EMPTY__" else bestRuleName}")
        p(w) match {
          case Success(result, _) => 
            updateBest(s, p, result)
          case NoSuccess(_, _) => 
            logger.log(s"rule ${s} failed.")
        }
      }

      if (bestParser.isEmpty) {
        logger.log("ask to otherwisetokenizeRules.")
        for ((s, p) <- otherwisetokenizeRules) {
            logger.log(s"current maxchamp is ${if(bestRuleName.isEmpty) "__EMPTY__" else bestRuleName}")
            p(w) match {
              case Success(result, _) => 
                updateBest(s, p, result)
              case NoSuccess(_, _) => 
                logger.log(s"rule ${s} failed.")
            }
        }
      }

      logger.log(s"finished ask to database. maxchamp is ${if(bestRuleName.isEmpty) "__EMPTY__" else bestRuleName}")
      bestParser
    }
    def dumpDataBase:Array[(String, Parser[Token])]=definedTokenizeRules.toArray
  }
  //定義されたトークンとそうでないトークンを区別するenum
  //主な使い道はデリミタとただのワードの区別
  enum Token{
		case Defined(s:String,tag:HygenicTag)
		case otherwise(s:String,tag:HygenicTag)
	}
}
