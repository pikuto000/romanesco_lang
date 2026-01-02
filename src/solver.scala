package parser
import com.microsoft.z3._

class SolverEnv {
  lazy val context = new Context()
  lazy val logicalVars = collection.mutable.Map[String, IntExpr]()
  
  def close(): Unit = context.close()
}

object RomanescoSolver {
  // SymbolTable の metadata から SolverEnv を取得する拡張メソッド的なヘルパー
  def getEnv(sym: SymbolTable): SolverEnv = {
    sym.metadata.get("solver") match {
      case Some(env: SolverEnv) => env
      case _ => 
        // 親を辿る
        sym.parent.map(getEnv).getOrElse {
          // 最上位でも無ければ新規作成して登録（遅延初期化）
          lazy val newEnv = new SolverEnv()
          sym.metadata("solver") = newEnv
          newEnv
        }
    }
  }

  def getOrCreateVar(name: String, sym: SymbolTable): IntExpr = {
    lazy val env = getEnv(sym)
    env.logicalVars.getOrElseUpdate(name, env.context.mkIntConst(name))
  }

  def solve(node: Node, sym: SymbolTable): Any = {
    lazy val env = getEnv(sym)
    lazy val solver = env.context.mkSolver()
    lazy val constraint = buildConstraint(node, sym, env)
    solver.add(constraint.asInstanceOf[BoolExpr])
    if (solver.check() == Status.SATISFIABLE) {
      lazy val model = solver.getModel()
      env.logicalVars.map { case (name, v) => name -> model.evaluate(v, false).toString }
    } else "unsat"
  }

  private def buildConstraint(node: Any, sym: SymbolTable, env: SolverEnv): Expr[?] = node match {
    case Atom(s, _) =>
      try { env.context.mkInt(s.toInt) }
      catch { case _: Exception => getOrCreateVar(s, sym) } // getOrCreateVar 内部で env を再取得するが許容範囲
    case Apply(fun, args, _, _) =>
      lazy val symProp = sym.getProp(fun, "smt-op")
      symProp match {
        case Some("add") => env.context.mkAdd(buildConstraint(args(0), sym, env).asInstanceOf[ArithExpr[IntSort]], buildConstraint(args(1), sym, env).asInstanceOf[ArithExpr[IntSort]])
        case Some("sub") => env.context.mkSub(buildConstraint(args(0), sym, env).asInstanceOf[ArithExpr[IntSort]], buildConstraint(args(1), sym, env).asInstanceOf[ArithExpr[IntSort]])
        case Some("mul") => env.context.mkMul(buildConstraint(args(0), sym, env).asInstanceOf[ArithExpr[IntSort]], buildConstraint(args(1), sym, env).asInstanceOf[ArithExpr[IntSort]])
        case Some("eq")  => env.context.mkEq(buildConstraint(args(0), sym, env), buildConstraint(args(1), sym, env))
        case Some("gt")  => env.context.mkGt(buildConstraint(args(0), sym, env).asInstanceOf[ArithExpr[IntSort]], buildConstraint(args(1), sym, env).asInstanceOf[ArithExpr[IntSort]])
        case Some("lt")  => env.context.mkLt(buildConstraint(args(0), sym, env).asInstanceOf[ArithExpr[IntSort]], buildConstraint(args(1), sym, env).asInstanceOf[ArithExpr[IntSort]])
        case Some("and") => env.context.mkAnd(buildConstraint(args(0), sym, env).asInstanceOf[BoolExpr], buildConstraint(args(1), sym, env).asInstanceOf[BoolExpr])
        case Some("or")  => env.context.mkOr(buildConstraint(args(0), sym, env).asInstanceOf[BoolExpr], buildConstraint(args(1), sym, env).asInstanceOf[BoolExpr])
        case Some("not") => env.context.mkNot(buildConstraint(args(0), sym, env).asInstanceOf[BoolExpr])
        case _ => throw new RuntimeException(s"Unsupported SMT op: $fun")
      }
    case b: Boolean => if (b) env.context.mkTrue() else env.context.mkFalse()
    case n: BigDecimal => env.context.mkInt(n.toBigInt.toString)
    case n: Int => env.context.mkInt(n)
    case other => throw new RuntimeException(s"Unsupported value in SMT: $other")
  }
}
