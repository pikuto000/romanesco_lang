package romanesco
import scala.collection.mutable.Map
import scala.util.parsing.combinator._
//ソースの位置情報を把握できるようにする。
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Positional
//トークンルールの投機的並行処理を行うためのライブラリ
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration



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
          case Some(p) => p(input) match {
            case Success(res, next) => Success(res, next)
            case NoSuccess(msg, next) => Failure(msg, next)
          }
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
    private var cachedDefinedRules: Seq[(HygenicTag, Parser[Token])] = Seq.empty
    private var cachedOtherwiseRules: Seq[(HygenicTag, Parser[Token])] = Seq.empty
    
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
    //イテレーターでの処理ではなく、並行処理によって競わせる。
    //並行処理はdefinedTokenRulesとotherwisetokenRulesを投機的に実行する。
    def whoAreYou(w:Input):Option[Parser[Token]]={
      logger.log("[token] whoAreYou begin")
      if (update){
        logger.log("[token] update database")
        // ここでシーケンスに変換してキャッシュしておく
        cachedDefinedRules = definedTokenizeRules.toSeq
        cachedOtherwiseRules = otherwisetokenizeRules.toSeq
        updateswitchable(false)
      }
      
      import scala.concurrent.ExecutionContext.Implicits.global

      // シーケンシャルに最長一致を探す
      def findBestMatch(rules: Seq[(HygenicTag, Parser[Token])], input: Input): Option[(Token, Input, HygenicTag)] = {
        var maxMatch: Option[(Token, Input, HygenicTag)] = None
        lazy val it = rules.iterator
        while(it.hasNext) {
          lazy val (tag, parser) = it.next()
          logger.log(s"[token] trying parser for tag: ${tag.mangledName}")
          parser(input) match {
            case Success(res, next) =>
              logger.log(s"[token] matched with parser for tag: ${tag.mangledName}")
              lazy val currentLen = res.s.length
              lazy val isBetter = maxMatch match {
                case None => true
                case Some((prevRes, _, prevTag)) =>
                  lazy val prevLen = prevRes.s.length
                  // より長いマッチ、または同じ長さなら登録順が後のもの (RuleOrderが大きいもの) を優先
                  if (currentLen > prevLen) {
                    logger.log(s"[token] new longest match for tag: ${tag.mangledName}, length: ${currentLen}")
                    true
                  } else if (currentLen == prevLen && RuleOrder(tag) > RuleOrder(prevTag)) {
                    logger.log(s"[token] same length match but newer rule for tag: ${tag.mangledName}, length: ${currentLen}")
                    true
                  } else {
                    logger.log(s"[token] match discarded (shorter or older rule) for tag: ${tag.mangledName}")
                    false
                  }
              }
              if (isBetter) {
                maxMatch = Some((res, next, tag))
              }
            case NoSuccess(_, _) =>
              logger.log(s"[token] no success for tag: ${tag.mangledName}")
          }
        }
        maxMatch
      }

      // 並列に最長一致を探す
      def parallelFindBestMatch(rules: Seq[(HygenicTag, Parser[Token])], input: Input, minRulesPerThread: Int): Option[(Token, Input, HygenicTag)] = {
        lazy val numProcessors = Runtime.getRuntime.availableProcessors()
        // バッチサイズを計算。ルール数/プロセッサ数 と minRulesPerThread の大きい方を採用
        lazy val optimalBatchSize = Math.ceil(rules.size.toDouble / numProcessors).toInt
        lazy val batchSize = Math.max(optimalBatchSize, minRulesPerThread)

        lazy val futures = rules.grouped(batchSize).map { batch =>
          Future {
            findBestMatch(batch, input)
          }
        }
        
        lazy val results = Await.result(Future.sequence(futures), Duration.Inf)

        // 結果のマージ
        results.foldLeft[Option[(Token, Input, HygenicTag)]](None) { (acc, resOpt) =>
          resOpt match {
            case Some((res, next, tag)) =>
              acc match {
                case None => resOpt
                case Some((prevRes, _, prevTag)) =>
                  lazy val currentLen = res.s.length
                  lazy val prevLen = prevRes.s.length
                  if (currentLen > prevLen) resOpt
                  else if (currentLen == prevLen && RuleOrder(tag) > RuleOrder(prevTag)) resOpt
                  else acc
              }
            case None => acc
          }
        }
      }

      // ルール数と入力サイズに基づいて検索戦略を選択する
      def search(rules: Seq[(HygenicTag, Parser[Token])]): Option[(Token, Input, HygenicTag)] = {
        if (rules.isEmpty) return None

        // 入力サイズを取得（CharSequenceReaderの場合のみ有効、それ以外はInt.Maxlazy valueとする）
        lazy val inputLength = w match {
          case csr: scala.util.parsing.input.CharSequenceReader => csr.source.length()
          case _ => Int.MaxValue
        }

        // 入力サイズに応じて、1スレッドあたりに割り当てる最小ルール数を調整
        // 入力が短い場合は、オーバーヘッドを避けるために1スレッドあたりの担当ルール数を増やす（並列度を下げる、あるいはシーケンシャルにする）
        // 例: 入力が1000文字未満なら、1スレッドあたり最低100ルールは処理させる
        lazy val minRulesPerThread = if (inputLength < 1000) 100 else 10
        lazy val numProcessors = Runtime.getRuntime.availableProcessors()

        // 並列化すべきか判定
        // プロセッサが複数あり、かつルール数が(プロセッサ数 * minRulesPerThread)を超える場合など
        // ここでは単純に、ルール数が minRulesPerThread 未満ならシーケンシャルにする
        if (numProcessors <= 1 || rules.size < minRulesPerThread) {
          logger.log("[token] running sequentially")
          findBestMatch(rules, w)
        } else {
          logger.log("[token] running in parallel")
          parallelFindBestMatch(rules, w, minRulesPerThread)
        }
      }

      // definedTokenizeRulesから最長一致を試みる
      logger.log("[token] attempting to match with definedTokenizeRules")
      lazy val definedMatch = search(cachedDefinedRules)
      
      //otherwiseTokenizeRulesの最長一致を試みる
      logger.log("[token] attempting to match with otherwisetokenizeRules")
      lazy val otherwiseMatch = search(cachedOtherwiseRules)

      // 最終的な結果を決定
      lazy val finalMatch = (definedMatch, otherwiseMatch) match {
        case (Some((dRes, dNext, dTag)), Some((oRes, oNext, oTag))) =>
          // 両方マッチした場合、より長い方を優先。同じ長さならDefinedを優先
          if (dRes.s.length > oRes.s.length) {
            logger.log(s"[token] definedMatch is longer. Tag: ${dTag.mangledName}")
            definedMatch
          } else if (oRes.s.length > dRes.s.length) {
            logger.log(s"[token] otherwiseMatch is longer. Tag: ${oTag.mangledName}")
            otherwiseMatch
          } else {
            logger.log(s"[token] definedMatch and otherwiseMatch have same length. Prioritizing definedMatch. Tag: ${dTag.mangledName}")
            definedMatch // 長さが同じならDefinedを優先
          }
        case (Some(_), None) => 
          logger.log(s"[token] only definedMatch found. Tag: ${definedMatch.get._3.mangledName}")
          definedMatch
        case (None, Some(_)) => 
          logger.log(s"[token] only otherwiseMatch found. Tag: ${otherwiseMatch.get._3.mangledName}")
          otherwiseMatch
        case (None, None) => 
          logger.log("[token] no match found in either definedTokenizeRules or otherwiseTokenizeRules")
          None
      }
      logger.log("[token] whoAreYou end")
      finalMatch match {
        case Some((res, _, tag)) =>
          // 最終的に選ばれたパーサーを返す
          // res.isInstanceOf[Token.Defined] のチェックは型消去の影響で信頼できない場合があるため、
          // 両方のマップから検索して見つかった方を返す。
          definedTokenizeRules.get(tag).orElse(otherwisetokenizeRules.get(tag))
        case None => None
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
