package romanesco

object MacroExpander {
  import Token._, AstExpr._, Stmt._
  private var syntax = List[(List[Token], List[Token])]()

  def reset(): Unit = { syntax = Nil }

  def registerAll(ss: List[Stmt]): Unit = ss.foreach {
    case SyntaxDef(p, t) => syntax = ((p, t) :: syntax).sortBy(-_._1.size)
    case Block(s) => registerAll(s)
    case Branch(o) => o.foreach(st => registerAll(List(st)))
    case _ =>
  }

  def transform(ts: List[Token]): List[Token] = if ts.isEmpty then Nil else {
    val headWord = ts.head match { case Token.Kw(w) => w; case Token.Var(n) => n; case _ => "" }
    if (headWord == "syntax") {
      val rest = ts.dropWhile(t => !t.isInstanceOf[Token.Sym] || t.asInstanceOf[Token.Sym].s != "}").drop(1)
      val finalRest = if (rest.nonEmpty && rest.head.isInstanceOf[Token.Sym] && rest.head.asInstanceOf[Token.Sym].s == ";") rest.tail else rest
      transform(finalRest)
    } else {
      syntax.collectFirst { case (p, t) if matchT(p, ts).isDefined => (t, matchT(p, ts).get) } match {
        case Some((tpl, (env, cons))) => substituteT(tpl, env) ++ transform(ts.drop(cons))
        case None => ts.head :: transform(ts.tail)
      }
    }
  }

  private def matchT(p: List[Token], t: List[Token]): Option[(Map[String, List[Token]], Int)] = {
    var (env, pi, ti, cons) = (Map[String, List[Token]](), p, t, 0)
    while (pi.nonEmpty) {
      while (ti.nonEmpty && (ti.head.isInstanceOf[WS] || ti.head.isInstanceOf[Comment])) { ti = ti.tail; cons += 1 }
      if (pi.head.isInstanceOf[WS] || pi.head.isInstanceOf[Comment]) pi = pi.tail
      else if (ti.isEmpty) return None
      else pi.head match {
        case Token.Var(n) if n.startsWith("$") => env += (n -> List(ti.head)); pi = pi.tail; ti = ti.tail; cons += 1
        case h if h == ti.head => pi = pi.tail; ti = ti.tail; cons += 1
        case _ => return None
      }
    }
    Some((env, cons))
  }

  private def substituteT(tpl: List[Token], env: Map[String, List[Token]]) = tpl.flatMap {
    case Token.Var(n) if env.contains(n) => env(n)
    case t => List(t)
  }
}