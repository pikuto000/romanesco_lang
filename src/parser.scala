package romanesco
import scala.collection.mutable.Map
import scala.util.parsing.combinator._

class Parser(tag:HygenicTag)
  extends
    RegexParsers,
    PackratParsers,
    HygenicObj(tag)
{
  override def skipWhitespace: Boolean = false
  def importLexer(lex:Lexer):Lexer=lex

  object database{
    private var definedParseRules:Map[String,Parser[Lexer#Token]]=Map.empty
    private var counter:BigInt=0
    private var RuleOrder:Map[String,BigInt]=Map.empty
  }
}
