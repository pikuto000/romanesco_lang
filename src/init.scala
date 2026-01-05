package romanesco
import scala.util.parsing.combinator._

object init {

  object lex{
    def setup(lexName:String)={
      val lexer=new Lexer(Hygenicmarker.bless(s"lexer:${lexName}",None,true))
      core(lexer)
      lexer
    }

    def core(lexer:Lexer):Unit={
      lexer.database.set(
        Hygenicmarker.bless("whiteSpace",Some(lexer),true),
        lexer.regex("""\s+""".r) ^^ {
          ws => lexer.Token.otherwise(ws,Hygenicmarker.bless(s"whiteSpace:'${ws}'",Some(lexer),true))
        }
      )
      lexer.database.set(
        Hygenicmarker.bless("word",Some(lexer),true),
        lexer.regex("""\S+""".r) ^^ {
          word => lexer.Token.otherwise(word,Hygenicmarker.bless(s"word:${word}",Some(lexer),true))
        }
      )
    }

  }

  object parse{
    ???
  }

}
