package romanesco

object init {
  
  object lex {
    def setup(lexName: String): Lexer = {
      val lexer = new Lexer(Hygenicmarker.bless(s"lexer:${lexName}", None, true))
      common(lexer); keywords(lexer); literals(lexer); operators(lexer)
      lexer
    }
    def common(lexer: Lexer): Unit = {
      import lexer._
      val wsTag = Hygenicmarker.bless("whiteSpace", Some(lexer), true)
      lexer.registerWhitespace(wsTag)
      lexer.database.set(wsTag, lexer.positioned(lexer.regex("\\s+".r) ^^ { ws => lexer.Token.otherwise(ws, wsTag) }))
    }
    def keywords(lexer: Lexer): Unit = {
      import lexer._
      List("syntax").foreach {
        kw =>
          val tag = Hygenicmarker.bless(s"kw_$kw", Some(lexer), true)
          lexer.database.set(tag, lexer.positioned(lexer.regex(s"$kw\\b".r) ^^ { s => lexer.Token.Defined(s, tag) }))
      }
    }
    def literals(lexer: Lexer): Unit = {
      import lexer._
      lexer.database.set(Hygenicmarker.bless("number", Some(lexer), true), lexer.positioned(lexer.regex("\\d+(\\.\\d+)?".r) ^^ { n => lexer.Token.otherwise(n, Hygenicmarker.bless(s"num:$n", Some(lexer), true)) }))
      lexer.database.set(Hygenicmarker.bless("ident", Some(lexer), true), lexer.positioned(lexer.regex("[a-zA-Z_][a-zA-Z0-9_]*".r) ^^ { id => lexer.Token.otherwise(id, Hygenicmarker.bless(s"id:$id", Some(lexer), true)) }))
    }
    def operators(lexer: Lexer): Unit = {
      import lexer._
      List("==", ">=", "<=", "=", "+", "-", "*", ">", "<", "(", ")", "{", "}", ";").foreach {
        opStr =>
          val tag = Hygenicmarker.bless(s"op_$opStr", Some(lexer), true)
          lexer.database.set(tag, lexer.positioned(lexer.regex(java.util.regex.Pattern.quote(opStr).r) ^^ { op => lexer.Token.Defined(op, tag) }))
      }
    }
  }
  
  object parse {
    def setup(parseName: String, lexer: Lexer): Parser = new Parser(lexer, Hygenicmarker.bless(s"parser:${parseName}", None, true))

    def getProgramParser(parser: Parser): parser.PackratParser[Node] = {
      import parser._
      val stmt = getStatementParser(parser)
      (rep1(stmt <~ _S)) ^^ { stmts => Node("Program", Hygenicmarker.bless("program", Some(parser), true), Map.empty, stmts) }
    }

    def getStatementParser(parser: Parser): parser.PackratParser[Node] = {
      import parser._
      val syntax = getSyntaxDefParser(parser)
      val block = getBlockParser(parser)
      val expr = getExpressionParser(parser, true)
      val unification = ((expr <~ _S) ~ (token("=") <~ _S) ~ expr) ^^ { case lhs ~ _ ~ rhs => Node("Unification", Hygenicmarker.bless("unify", Some(parser), true), Map.empty, List(lhs, rhs)) }
      (syntax | block | unification | expr) <~ _S <~ opt(token(";"))
    }

    def getBlockParser(parser: Parser): parser.PackratParser[Node] = {
      import parser._
      lazy val stmt = getStatementParser(parser)
      (token("{") ~> _S ~> rep(stmt <~ _S) <~ token("}")) ^^ { stmts => 
        Node("Block", Hygenicmarker.bless("block", Some(parser), true), Map.empty, stmts) 
      }
    }

    def getSyntaxDefParser(parser: Parser): parser.PackratParser[Node] = {
      import parser._
      val exprWithMacro = getExpressionParser(parser, true)
      val block = getBlockParser(parser)
      lazy val syntaxKeyword = token("syntax")
      lazy val macroNameP = dataToken(_.tag.name.startsWith("id:"), "macro name") ^^ { t => t.s }
      
      //ボディ側で「複数のノード」を返せるようにブロックまたは式を受理
      (syntaxKeyword ~ _S ~ macroNameP ~ _S ~ rep(exprWithMacro <~ _S) ~ token("=") ~ _S ~ (block | exprWithMacro)) ^^ {
        case (_ ~ _ ~ (name: String) ~ _ ~ (patterns: List[Node]) ~ _ ~ _ ~ (body: Node)) =>
          // ボディがBlockならその子供たち（List[Node]）を、単一の式なら1要素のリストを登録
          val bodyList = if (body.kind == "Block") body.children else List(body)
          
          if (Macro.register(name, patterns, bodyList)) {
            val mTag = Hygenicmarker.bless(s"macro_token_$name", Some(lexer), true)
            lexer.database.set(mTag, lexer.positioned(lexer.regex(java.util.regex.Pattern.quote(name).r) ^^ { s => lexer.Token.Defined(s, mTag) }))
            val macroRule = (token(name) ~ repN(patterns.length, _S ~> exprWithMacro)) ^^ { 
              case t ~ parsedArgs => Node("MacroCall", Hygenicmarker.bless(s"call_$name", Some(parser), true), Map("name" -> name), parsedArgs)
            }
            parser.database.prepend(Hygenicmarker.bless(s"macro_$name", Some(parser), true), macroRule)
          }
          Node("MacroDef", Hygenicmarker.bless("macro", Some(parser), true), Map("name" -> name))
      }
    }

    def getExpressionParser(parser: Parser, allowMacros: Boolean): parser.PackratParser[Node] = {
      import parser._
      lazy val varP = dataToken(_.tag.name.startsWith("id:"), "id") ^^ { t => Node("Variable", t.tag) }
      lazy val valP = dataToken(_.tag.name.startsWith("num:"), "num") ^^ { t => Node("DecimalLiteral", Hygenicmarker.bless("literal", Some(parser), true), Map("value" -> BigDecimal(t.s))) }
      lazy val factor: PackratParser[Node] = (token("(") ~> _S ~> getExpressionParser(parser, allowMacros) <~ _S <~ token(")")) | (if (allowMacros) (anyRule | valP | varP) else (valP | varP))
      lazy val term: PackratParser[Node] = (factor ~ rep(_S ~> token("*") ~> factor)) ^^ { case f ~ list => list.foldLeft(f)((x, y) => Node("BinaryOp", Hygenicmarker.bless("mul_op", Some(parser), true), Map("op" -> "*"), List(x, y))) }
      lazy val arithmetic: PackratParser[Node] = (term ~ rep(_S ~> (token("+") | token("-")) ~ (_S ~> term))) ^^ { case t ~ list => list.foldLeft(t) { case (x, op ~ y) => Node("BinaryOp", Hygenicmarker.bless(s"bin_op_${op.s}", Some(parser), true), Map("op" -> op.s), List(x, y)) } } 
      lazy val comparison: PackratParser[Node] = (arithmetic ~ rep(_S ~> (token("==") | token(">=") | token("<=") | token(">") | token("<")) ~ (_S ~> arithmetic))) ^^ { case t ~ list => list.foldLeft(t) { case (x, op ~ y) => Node("BinaryOp", Hygenicmarker.bless(s"cmp_op_${op.s}", Some(parser), true), Map("op" -> op.s), List(x, y)) } } 
      comparison
    }

    def literals(parser: Parser): Unit = {
      import parser._
      parser.addSyntax(Hygenicmarker.bless("parseLiteral", Some(parser), true))(dataToken(_.tag.name.startsWith("num:"), "number") ^^ { t => Node("DecimalLiteral", Hygenicmarker.bless("literal", Some(parser), true), Map("value" -> BigDecimal(t.s))) })
      parser.addSyntax(Hygenicmarker.bless("parseVariable", Some(parser), true))(dataToken(_.tag.name.startsWith("id:"), "identifier") ^^ { t => Node("Variable", t.tag) })
    }

    def logic(parser: Parser): Unit = {
      import parser._
      parser.addSyntax(Hygenicmarker.bless("parseProgram", Some(parser), true))(getProgramParser(parser))
    }
  }

  object semantics {
    def execute(results: Array[Any], solver: Solver): Unit = {
      results.foreach { case node: Node => solver.solve(node); case _ => }
      solver.check()
    }
  }

  object optimization {
    def apply(results: Array[Any]): Array[Any] = Macro.ConstantFolding(Macro.expand(results))
  }
}