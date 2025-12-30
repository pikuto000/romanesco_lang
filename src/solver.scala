package parser

object RomanescoSolver {
  def getOrCreateVar(name: String, sym: SymbolTable): com.microsoft.z3.IntExpr = {
    sym.logicalVars.getOrElseUpdate(name, sym.z3.mkIntConst(name))
  }

  def solve(node: Node, sym: SymbolTable): Any = {
    val solver = sym.z3.mkSolver()
    val constraint = buildConstraint(node, sym)
    solver.add(constraint.asInstanceOf[com.microsoft.z3.BoolExpr])
    if (solver.check() == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel()
      sym.logicalVars.map { case (name, v) => name -> model.evaluate(v, false).toString }
    } else "unsat"
  }

  private def buildConstraint(node: Node, sym: SymbolTable): com.microsoft.z3.Expr[?] = node match {
    case Atom(s) =>
      try { sym.z3.mkInt(s.toInt) }
      catch { case _: Exception => getOrCreateVar(s, sym) }
    case Apply(fun, args, _, _) =>
      val symProp = sym.getProp(fun, "smt-op")
      symProp match {
        case Some("add") => sym.z3.mkAdd(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]])
        case Some("sub") => sym.z3.mkSub(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]])
        case Some("mul") => sym.z3.mkMul(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]])
        case Some("eq")  => sym.z3.mkEq(buildConstraint(args(0), sym), buildConstraint(args(1), sym))
        case Some("gt")  => sym.z3.mkGt(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]])
        case Some("lt")  => sym.z3.mkLt(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.ArithExpr[com.microsoft.z3.IntSort]])
        case Some("and") => sym.z3.mkAnd(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.BoolExpr], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.BoolExpr])
        case Some("or")  => sym.z3.mkOr(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.BoolExpr], buildConstraint(args(1), sym).asInstanceOf[com.microsoft.z3.BoolExpr])
        case Some("not") => sym.z3.mkNot(buildConstraint(args(0), sym).asInstanceOf[com.microsoft.z3.BoolExpr])
        case _ => throw new RuntimeException(s"Unsupported SMT op: $fun")
      }
    case ConstantNode(v) => 
      v match {
        case b: Boolean => if (b) sym.z3.mkTrue() else sym.z3.mkFalse()
        case n: BigDecimal => sym.z3.mkInt(n.toBigInt.toString)
        case _ => throw new RuntimeException("Unsupported constant in SMT")
      }
  }
}
