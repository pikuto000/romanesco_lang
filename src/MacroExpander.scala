package romanesco

object MacroExpander {
  import AstExpr._
  import Stmt._

  private var macros = Map[String, (List[String], AstExpr)]()
  private var syntax = List[(List[Token], List[Token])]()

  def reset(): Unit = { macros = Map.empty; syntax = Nil }

  def expandAll(stmts: List[Stmt]): List[Stmt] = stmts.flatMap {
    case m: MacroDef => macros += (m.name -> (m.params, m.body)); None
    case s: SyntaxDef => syntax = ((s.pattern, s.template) :: syntax).sortBy(-_._1.size); None
    case Constraint(l, r) => Some(Constraint(expand(l), expand(r)))
    case Block(s) => Some(Block(expandAll(s)))
    case Branch(o) => Some(Branch(expandAll(o)))
  }

  def transformTokens(ts: List[Token]): List[Token] = {
    if (ts.isEmpty) Nil
    else {
      val found = syntax.collectFirst { case (p, t) if matchT(p, ts).isDefined => (t, matchT(p, ts).get) }
      found match {
        case Some((tpl, (env, cons))) => substituteT(tpl, env) ++ transformTokens(ts.drop(cons))
        case None => ts.head :: transformTokens(ts.tail)
      }
    }
  }

  private def matchT(p: List[Token], t: List[Token]): Option[(Map[String, List[Token]], Int)] = {
    var (env, pi, ti, cons) = (Map[String, List[Token]](), p, t, 0)
    while (pi.nonEmpty) {
      if (pi.head.isInstanceOf[Token.WS]) pi = pi.tail
      else if (ti.nonEmpty && ti.head.isInstanceOf[Token.WS]) { ti = ti.tail; cons += 1 }
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

  private def expand(e: AstExpr): AstExpr = e match {
    case MacroCall(n, a) if macros.contains(n) =>
      val (p, b) = macros(n); expand(substitute(b, p.zip(a.map(expand)).toMap))
    case BinOp(o, l, r) => BinOp(o, expand(l), expand(r))
    case Ambiguous(o) =>
      val ex = o.map(expand); val sp = ex.filterNot(_.isInstanceOf[MacroCall])
      if (sp.nonEmpty) sp.head else Ambiguous(ex)
    case _ => e
  }

  private def substitute(e: AstExpr, env: Map[String, AstExpr]): AstExpr = e match {
    case Var(n) if env.contains(n) => env(n)
    case BinOp(o, l, r) => BinOp(o, substitute(l, env), substitute(r, env))
    case MacroCall(n, a) => MacroCall(n, a.map(substitute(_, env)))
    case Ambiguous(o) => Ambiguous(o.map(substitute(_, env)))
    case _ => e
  }
}