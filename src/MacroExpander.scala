package romanesco

object MacroExpander {
  import Token._, AstExpr._, Stmt._
  private var syntax = List[(List[Token], List[Token])]()

  def reset(): Unit = syntax = Nil
  def registerAll(ss: List[Stmt]): Unit = ss.foreach {
    case SyntaxDef(p, t) => syntax = ((p, t) :: syntax).sortBy(-_._1.size)
    case Block(s) => registerAll(s); case Branch(o) => o.foreach(st => registerAll(List(st))); case _ =>
  }

  def transform(ts: List[Token]): List[Token] = if ts.isEmpty then Nil else 
    if ts.head.raw == "syntax" then transform(ts.dropWhile(_.raw != "}").drop(1).dropWhile(_.raw == ";"))
    else syntax.collectFirst { case (p, t) if matchT(p, ts).isDefined => (t, matchT(p, ts).get) } match
      case Some((tpl, (env, cons))) => substituteT(tpl, env) ++ transform(ts.drop(cons))
      case _ => ts.head :: transform(ts.tail)

  private def matchT(p: List[Token], t: List[Token]): Option[(Map[String, List[Token]], Int)] = (p, t) match
    case (Nil, _) => Some(Map.empty, 0)
    case (WS(_) :: pr, _) => matchT(pr, t)
    case (Comment(_) :: pr, _) => matchT(pr, t)
    case (_, WS(_) :: tr) => matchT(p, tr).map((e, c) => (e, c + 1))
    case (_, Comment(_) :: tr) => matchT(p, tr).map((e, c) => (e, c + 1))
    case (Token.Var(n) :: pr, th :: tr) if n.startsWith("$") => matchT(pr, tr).map((e, c) => (e + (n -> List(th)), c + 1))
    case (ph :: pr, th :: tr) if ph.raw == th.raw => matchT(pr, tr).map((e, c) => (e, c + 1))
    case _ => None

  private def substituteT(tpl: List[Token], env: Map[String, List[Token]]) = tpl.flatMap {
    case Token.Var(n) if env.contains(n) => env(n); case t => List(t)
  }
}
