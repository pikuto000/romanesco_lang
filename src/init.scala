package romanesco
import scala.util.parsing.combinator._
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Positional

object init {
  
  object lex{
    def setup(lexName:String)={
      lazy val lexer=new Lexer(Hygenicmarker.bless(s"lexer:${lexName}",None,true))
      core(lexer)
      lexer
    }
    
    def core(lexer:Lexer):Unit={
      lexer.database.set(
      Hygenicmarker.bless("whiteSpace",Some(lexer),true),
      new lexer.Parser[lexer.Token] {
        def apply(in: lexer.Input) = {
          lazy val start = in.pos
          lexer.regex("""\s+""".r)(in) match {
            case lexer.Success(ws, next) =>
              lazy val row = start.line
              lazy val col = start.column
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
          lazy val start = in.pos
          lexer.regex("""\S+""".r)(in) match {
            case lexer.Success(word, next) =>
              lazy val row = start.line
              lazy val col = start.column
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
    def setup(parseName: String, lexer: Lexer) = {
      val parser = new Parser(lexer, Hygenicmarker.bless(s"parser:${parseName}", None, true))
      core(parser)
      parser
    }

    def core(parser: Parser): Unit = {
      val testTag = Hygenicmarker.bless("anyWord", Some(parser), true)
      // どんなトークンにも1つマッチするルール
      parser.addSyntax(testTag)(parser.acceptIf(_ => true)(_ => "Expected any token"))
      
      logger.log(s"[parse] core setup for ${parser.tag.mangledName}")
    }
  }
}
