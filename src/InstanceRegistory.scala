package romanesco
import scala.collection.mutable.Map

object InstanceRegistory {
  private var instances:Map[String,Any]=Map.empty
  
  // Lexerを起動して登録する
  def launch(replaceNewLine:Option[String]=None):Lexer={
    val lex=init.lex.setup("firstLexer")
    instances += (lex.tag.name -> lex)
    lex
  }

  // Parserを起動して登録する
  def launchParser(lexer: Lexer): Parser = {
    val parser = init.parse.setup("firstParser", lexer)
    instances += (parser.tag.name -> parser)
    parser
  }

  // 名前でインスタンスを取得する
  def get(name: String): Option[Any] = instances.get(name)
}
