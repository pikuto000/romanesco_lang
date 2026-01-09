package romanesco

object MacroExpander {
  import AstExpr._
  import Stmt._

  private var registry = Map[String, (List[String], AstExpr)]()
  private var syntaxRules = List[(List[Token], List[Token])]()

  def reset(): Unit = {
    registry = Map.empty
    syntaxRules = Nil
  }

  def register(df: MacroDef): Unit = {
    registry += (df.name -> (df.params, df.body))
  }

  def registerSyntax(sd: SyntaxDef): Unit = {
    println(s"DEBUG: Registering Syntax: ${sd.pattern.mkString(", ")} -> ${sd.template.mkString(", ")}")
    syntaxRules = (sd.pattern, sd.template) :: syntaxRules
    syntaxRules = syntaxRules.sortBy(-_._1.size)
  }

  def transformTokens(tokens: List[Token]): List[Token] = {
    if (tokens.isEmpty) Nil
    else {
      val matchOpt = syntaxRules.collectFirst { 
        case (pattern, template) if matchTokensDetailed(pattern, tokens).isDefined => 
          val (env, consumed) = matchTokensDetailed(pattern, tokens).get
          (template, env, consumed)
      }

      matchOpt match {
        case Some((template, env, consumed)) =>
          println(s"DEBUG: Match Found! Consumed: $consumed")
          val substituted = substituteTokens(template, env)
          substituted ++ transformTokens(tokens.drop(consumed))
        case None =>
          tokens.head :: transformTokens(tokens.tail)
      }
    }
  }

  private def matchTokensDetailed(pattern: List[Token], targets: List[Token]): Option[(Map[String, List[Token]], Int)] = {
    var env = Map[String, List[Token]]()
    var p = pattern
    var t = targets
    var consumed = 0

    while (p.nonEmpty) {
      // パターン側の空白・コメントをスキップ
      if (p.head.isInstanceOf[Token.WS] || p.head.isInstanceOf[Token.Comment]) {
        p = p.tail
      } else if (t.isEmpty) {
        return None
      } else if (t.head.isInstanceOf[Token.WS] || t.head.isInstanceOf[Token.Comment]) {
        // ターゲット側の空白・コメントをスキップ
        t = t.tail
        consumed += 1
      } else {
        // 実質的なトークンの比較
        (p.head, t.head) match {
          case (Token.Var(name), target) if name.startsWith("$") =>
            env += (name -> List(target))
            p = p.tail; t = t.tail; consumed += 1
          case (ph, th) if ph.getClass == th.getClass && ph == th =>
            p = p.tail; t = t.tail; consumed += 1
          case _ => 
            return None
        }
      }
    }
    Some((env, consumed))
  }

  private def substituteTokens(template: List[Token], env: Map[String, List[Token]]): List[Token] = {
    template.flatMap {
      case Token.Var(name) if env.contains(name) => env(name)
      case t => List(t)
    }
  }

  def expandAll(stmts: List[Stmt]): List[Stmt] = {
    stmts.flatMap {
      case m: MacroDef => register(m); None
      case s: SyntaxDef => registerSyntax(s); None
      case Constraint(l, r) => Some(Constraint(expand(l), expand(r)))
      case Block(s) => Some(Block(expandAll(s)))
      case Branch(options) => Some(Branch(expandAll(options)))
      case s: SyntaxDef => None // 二重登録防止
    }
  }

  private def expand(expr: AstExpr): AstExpr = expr match {
    case MacroCall(name, args) =>
      if (registry.contains(name)) {
        val (params, body) = registry(name)
        val expandedArgs = args.map(expand)
        val env = params.zip(expandedArgs).toMap
        expand(substitute(body, env))
      } else expr
    case BinOp(op, l, r) => BinOp(op, expand(l), expand(r))
    case Ambiguous(options) =>
      val expandedOptions = options.map(expand)
      val specialized = expandedOptions.filter { case _: MacroCall => false; case _ => true }
      if (specialized.nonEmpty) specialized.head else Ambiguous(expandedOptions)
    case _ => expr
  }

  private def substitute(expr: AstExpr, env: Map[String, AstExpr]): AstExpr = expr match {
    case Var(n) if env.contains(n) => env(n)
    case BinOp(op, l, r) => BinOp(op, substitute(l, env), substitute(r, env))
    case MacroCall(n, args) => MacroCall(n, args.map(a => substitute(a, env)))
    case Ambiguous(options) => Ambiguous(options.map(o => substitute(o, env)))
    case _ => expr
  }
}
