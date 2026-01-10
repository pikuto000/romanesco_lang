package romanesco

class Interpreter{
  import AstExpr._, Stmt._
  def eval(e: AstExpr, env: Map[String, BigDecimal]): BigDecimal = e match {
    case Num(v) => v
    case Var(n) => env(n)
    case BinOp("+", l, r) => eval(l, env) + eval(r, env)
    case BinOp("-", l, r) => eval(l, env) - eval(r, env)
    case BinOp("*", l, r) => eval(l, env) * eval(r, env)
    case BinOp("/", l, r) => eval(l, env) / eval(r, env)
    case BinOp("==", l, r) => if (eval(l, env) == eval(r, env)) 1 else 0
    case BinOp(">", l, r) => if (eval(l, env) > eval(r, env)) 1 else 0
    case BinOp("<", l, r) => if (eval(l, env) < eval(r, env)) 1 else 0
    case If(c, t, el) => if (eval(c, env) != 0) eval(t, env) else eval(el, env)
    case _ => throw new NotImplementedError(s"Evaluation of $e not supported yet in interpreter")
  }
  def exec(s: Stmt, env: Map[String, BigDecimal]): Map[String, BigDecimal] = s match {
    case Constraint(Var(n), v) => env + (n -> eval(v, env))
    case Block(ss) => ss.foldLeft(env)((e, s) => exec(s, e))
    case _ => throw new NotImplementedError(s"Execution of $s not supported yet in interpreter")
  }
  def run(ss: List[Stmt]): Map[String, BigDecimal] = {
    ss.foldLeft(Map[String, BigDecimal]())((e, s) => exec(s, e))
  }
}