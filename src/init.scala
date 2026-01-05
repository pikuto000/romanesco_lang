package romanesco
import scala.util.parsing.combinator._
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Positional

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
      new lexer.Parser[lexer.Token] {
        def apply(in: lexer.Input) = {
          val start = in.pos
          lexer.regex("""\s+""".r)(in) match {
            case lexer.Success(ws, next) =>
              val row = start.line
              val col = start.column
              lexer.Success(
                lexer.Token.otherwise(ws,Hygenicmarker.bless(s"whiteSpace:'${ws}', row:${row}, column:${col}",Some(lexer),true)),
                next
              )
            case ns: lexer.NoSuccess => ns
          }
        }
      }
      )
      lexer.database.set(
      Hygenicmarker.bless("word",Some(lexer),true),
      new lexer.Parser[lexer.Token] {
        def apply(in: lexer.Input) = {
          val start = in.pos
          lexer.regex("""\S+""".r)(in) match {
            case lexer.Success(word, next) =>
              val row = start.line
              val col = start.column
              lexer.Success(
                lexer.Token.otherwise(word,Hygenicmarker.bless(s"word:'${word}', row:${row}, column:${col}",Some(lexer),true)),
                next
              )
            case ns: lexer.NoSuccess => ns
          }
        }
      }
      )
    }
    
  }
  
  object parse{
    ???
  }
}
