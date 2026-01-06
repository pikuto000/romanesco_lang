package romanesco

object init {
  
  object lex {
    def setup(lexName: String): Lexer = {
      val lexer = new Lexer(Hygenicmarker.bless(s"lexer:${lexName}", None, true))
      common(lexer)
      lexer
    }

    // どの言語構成でも共通で使いそうな基本ルール
    def common(lexer: Lexer): Unit = {
      import lexer._
      // 空白
      lexer.database.set(
        Hygenicmarker.bless("whiteSpace", Some(lexer), true),
        lexer.positioned(lexer.regex("""\s+""".r) ^^ { ws => 
          lexer.Token.otherwise(ws, Hygenicmarker.bless(s"ws", Some(lexer), true))
        })
      )
    }

    // 識別子や数値などのリテラル
    def literals(lexer: Lexer): Unit = {
      import lexer._
      // 数字
      lexer.database.set(
        Hygenicmarker.bless("number", Some(lexer), true),
        lexer.positioned(lexer.regex("""\d+""".r) ^^ { n => 
          lexer.Token.otherwise(n, Hygenicmarker.bless(s"num:$n", Some(lexer), true))
        })
      )
      // 識別子
      lexer.database.set(
        Hygenicmarker.bless("ident", Some(lexer), true),
        lexer.positioned(lexer.regex("""[a-zA-Z_][a-zA-Z0-9_]*""".r) ^^ { id => 
          lexer.Token.otherwise(id, Hygenicmarker.bless(s"id:$id", Some(lexer), true))
        })
      )
    }

    // 演算子などの記号
    def operators(lexer: Lexer): Unit = {
      import lexer._
      lexer.database.set(
        Hygenicmarker.bless("equal", Some(lexer), true),
        lexer.positioned(lexer.regex("""=""".r) ^^ { op => 
          lexer.Token.Defined(op, Hygenicmarker.bless(s"op:$op", Some(lexer), true))
        })
      )
    }
  }
  
  object parse {
    def setup(parseName: String, lexer: Lexer): Parser = {
      val parser = new Parser(lexer, Hygenicmarker.bless(s"parser:${parseName}", None, true))
      // デフォルトでは何も入れず、Main側で必要なモジュールを注入する形にする
      parser
    }

    // 基本的なリテラルをASTに変換するルール
    def literals(parser: Parser): Unit = {
      import parser._
      lazy val identP = acceptIf(t => t.tag.name.startsWith("id:"))(_ => "id expected")
      lazy val numP = acceptIf(t => t.tag.name.startsWith("num:"))(_ => "num expected")

      parser.addSyntax(Hygenicmarker.bless("parseVariable", Some(parser), true))(
        identP ^^ { t => AST.Variable(t.tag) }
      )
      parser.addSyntax(Hygenicmarker.bless("parseLiteral", Some(parser), true))(
        numP ^^ { t => AST.IntLiteral(BigInt(t.s)) }
      )
    }

    // Oz風の論理制約ルール
    def logic(parser: Parser): Unit = {
      import parser._
      // literalsが先に呼ばれていることを前提とするか、内部で必要なパーサーを定義する
      lazy val varP = acceptIf(t => t.tag.name.startsWith("id:"))(_ => "id expected") ^^ { t => AST.Variable(t.tag) }
      lazy val valP = acceptIf(t => t.tag.name.startsWith("num:"))(_ => "num expected") ^^ { t => AST.IntLiteral(BigInt(t.s)) }

      val unificationParser = 
        ((varP <~ _S) ~ (token("=") <~ _S) ~ (valP | varP)) ^^ {
          case v ~ _ ~ rhs => AST.Unification(v, rhs)
        }

      parser.addSyntax(Hygenicmarker.bless("parseUnify", Some(parser), true))(unificationParser)
    }
  }

  object semantics {
    def execute(results: Array[Any], solver: Solver): Unit = {
      logger.log(s"[semantics] Executing ${results.length} AST nodes...")
      results.foreach {
        case ast: AST => solver.solve(ast)
        case other => logger.log(s"[semantics] Skipping non-AST node: $other")
      }
      solver.check()
    }
  }
}
