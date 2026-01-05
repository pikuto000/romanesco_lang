package romanesco
import scala.collection.mutable.Map

object InstanceRegistory {
  private var instances:Map[String,Any]=Map.empty
  //パースを開始するメソッド
  def launch(replaceNewLine:Option[String]=None):Lexer={
    val lex=init.lex.setup("firstLexer")
    instances += (lex.tag.name -> lex)
    lex
  }
}
