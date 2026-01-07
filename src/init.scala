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
      // 空白タグを作成
      val wsTag = Hygenicmarker.bless("whiteSpace", Some(lexer), true)
      
      // 空白として登録
      lexer.registerWhitespace(wsTag)
      
      // ルール設定
      lexer.database.set(
        wsTag,
        lexer.positioned(lexer.regex("""\s+""".r) ^^ { ws => 
          lexer.Token.otherwise(ws, wsTag)
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
      val ops = List("=", "+", "-", "*", ">=", "<=", ">", "<")
      ops.foreach { opStr =>
        val name = opStr match {
          case "=" => "equal"
          case "+" => "plus"
          case "-" => "minus"
          case "*" => "mul"
          case ">" => "gt"
          case "<" => "lt"
          case ">=" => "ge"
          case "<=" => "le"
        }
        lexer.database.set(
          Hygenicmarker.bless(name, Some(lexer), true),
          lexer.positioned(lexer.regex(java.util.regex.Pattern.quote(opStr).r) ^^ { op => 
            lexer.Token.Defined(op, Hygenicmarker.bless(s"op:$op", Some(lexer), true))
          })
        )
      }
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
        identP ^^ { t => Node("Variable", t.tag) }
      )
      parser.addSyntax(Hygenicmarker.bless("parseLiteral", Some(parser), true))(
        numP ^^ { t => 
          Node("DecimalLiteral", Hygenicmarker.bless("literal", Some(parser), true), Map("value" -> BigDecimal(t.s)))
        }
      )
    }

    // Oz風の論理制約ルール (算術演算を含む)
    def logic(parser: Parser): Unit = {
      import parser._
      
      // 基本要素
      lazy val varP = acceptIf(t => t.tag.name.startsWith("id:"))(_ => "id expected") ^^ { t => Node("Variable", t.tag) }
      lazy val valP = acceptIf(t => t.tag.name.startsWith("num:"))(_ => "num expected") ^^ { t => 
        Node("DecimalLiteral", Hygenicmarker.bless("literal", Some(parser), true), Map("value" -> BigDecimal(t.s)))
      }
      
      // 演算子
      lazy val plus = token("+")
      lazy val minus = token("-")
      lazy val mul = token("*")
      lazy val gt = token(">")
      lazy val lt = token("<")
      lazy val ge = token(">=")
      lazy val le = token("<=")

      // 数式パーサー (再帰的定義)
      lazy val factor: PackratParser[Node] = 
        valP | varP 

      // term: factor * factor ...
      lazy val term: PackratParser[Node] = 
        (factor ~ rep(_S ~> mul ~ (_S ~> factor))) ^^ {
          case f ~ list => list.foldLeft(f) {
            case (x, _ ~ y) => 
              Node("BinaryOp", Hygenicmarker.bless("mul_op", Some(parser), true), Map("op" -> "*"), List(x, y))
          }
        }

      // arithmetic: term + term ...
      lazy val arithmetic: PackratParser[Node] = 
        (term ~ rep(_S ~> (plus | minus) ~ (_S ~> term))) ^^ {
          case t ~ list => list.foldLeft(t) {
            case (x, op ~ y) => 
              Node("BinaryOp", Hygenicmarker.bless(s"bin_op_${op.s}", Some(parser), true), Map("op" -> op.s), List(x, y))
          }
        }

      // comparison: arithmetic > arithmetic ...
      lazy val comparison: PackratParser[Node] =
        (arithmetic ~ rep(_S ~> (gt | lt | ge | le) ~ (_S ~> arithmetic))) ^^ {
          case t ~ list => list.foldLeft(t) {
            case (x, op ~ y) =>
              Node("BinaryOp", Hygenicmarker.bless(s"cmp_op_${op.s}", Some(parser), true), Map("op" -> op.s), List(x, y))
          }
        }

      // 単一化: variable = expression
      // 左辺は comparison も許容するように変更
      val unificationParser: PackratParser[Node] = 
        ((comparison <~ _S) ~ (token("=") <~ _S) ~ comparison) ^^ {
          case lhs ~ _ ~ rhs => 
            Node("Unification", Hygenicmarker.bless("unify", Some(parser), true), Map.empty, List(lhs, rhs))
        }

      parser.addSyntax(Hygenicmarker.bless("parseUnify", Some(parser), true))(unificationParser)
      
      // 制約としての比較式単体も許容する
      parser.addSyntax(Hygenicmarker.bless("parseComparison", Some(parser), true))(comparison)
    }
  }

  object semantics {
    def execute(results: Array[Any], solver: Solver): Unit = {
      logger.log(s"[semantics] Executing ${results.length} AST nodes...")
      results.foreach {
        case node: Node => solver.solve(node)
        case other => logger.log(s"[semantics] Skipping non-Node object: $other")
      }
      solver.check()
    }
  }

  object debug {
    def testEvaluator(results: Array[Any]): Unit = {
      val evaluator = new Evaluator()
      logger.log("[debug] Testing Evaluator with parsed nodes...")
      results.foreach {
        case node: Node if node.kind == "Unification" =>
          try {
            val rhsValue = evaluator.eval(node.children(1))
            logger.log(s"[debug] Right-hand side of unification evaluates to: $rhsValue")
          } catch {
            case e: Exception => logger.log(s"[debug] Evaluation skipped: ${e.getMessage}")
          }
        case _ => 
      }
    }
  }
}
