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
      // 数字 (小数対応)
      lexer.database.set(
        Hygenicmarker.bless("number", Some(lexer), true),
        lexer.positioned(lexer.regex("""\d+(\.\d+)?""".r) ^^ { n => 
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
      lexer.database.set(
        Hygenicmarker.bless("plus", Some(lexer), true),
        lexer.positioned(lexer.regex("""\+""".r) ^^ { op => 
          lexer.Token.Defined(op, Hygenicmarker.bless(s"op:$op", Some(lexer), true))
        })
      )
      lexer.database.set(
        Hygenicmarker.bless("minus", Some(lexer), true),
        lexer.positioned(lexer.regex("""\-""".r) ^^ { op => 
          lexer.Token.Defined(op, Hygenicmarker.bless(s"op:$op", Some(lexer), true))
        })
      )
      lexer.database.set(
        Hygenicmarker.bless("mul", Some(lexer), true),
        lexer.positioned(lexer.regex("""\*""".r) ^^ { op => 
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
      val identP = acceptIf(t => t.tag.name.startsWith("id:"))(_ => "id expected")
      val numP = acceptIf(t => t.tag.name.startsWith("num:"))(_ => "num expected")

      parser.addSyntax(Hygenicmarker.bless("parseVariable", Some(parser), true))(
        identP ^^ { t => AST.Variable(t.tag) }
      )
      parser.addSyntax(Hygenicmarker.bless("parseLiteral", Some(parser), true))(
        numP ^^ { t => AST.DecimalLiteral(BigDecimal(t.s)) }
      )
    }

    // Oz風の論理制約ルール (算術演算を含む)
    def logic(parser: Parser): Unit = {
      import parser._
      
      // 基本要素
      lazy val varP = acceptIf(t => t.tag.name.startsWith("id:"))(_ => "id expected") ^^ { t => AST.Variable(t.tag) }
      lazy val valP = acceptIf(t => t.tag.name.startsWith("num:"))(_ => "num expected") ^^ { t => AST.DecimalLiteral(BigDecimal(t.s)) }
      
      // 演算子
      lazy val plus = token("+")
      lazy val minus = token("-")
      lazy val mul = token("*")

      // 数式パーサー (再帰的定義)
      lazy val factor: PackratParser[AST] = 
        valP | varP 

      // term: factor * factor ...
      lazy val term: PackratParser[AST] = 
        (factor ~ rep(_S ~> mul ~ (_S ~> factor))) ^^ {
          case f ~ list => list.foldLeft(f) {
            case (x, _ ~ y) => AST.BinaryOp("*", x, y)
          }
        }

      // expression: term + term ...
      lazy val expression: PackratParser[AST] = 
        (term ~ rep(_S ~> (plus | minus) ~ (_S ~> term))) ^^ {
          case t ~ list => list.foldLeft(t) {
            case (x, op ~ y) => AST.BinaryOp(op.s, x, y)
          }
        }

      // 単一化: variable = expression (または expression = expression もあり得るが今回は単純化)
      // 左辺は expression も許容するように変更
      val unificationParser: PackratParser[AST.Unification] = 
        ((expression <~ _S) ~ (token("=") <~ _S) ~ expression) ^^ {
          case lhs ~ _ ~ rhs => AST.Unification(lhs, rhs)
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
