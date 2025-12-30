package parser
import com.microsoft.z3._
import scala.collection.mutable.Map

object RomanescoSolver {

  /**
   * Translates a Romanesco AST into a Z3 Expression based on environment properties.
   */
  def toZ3(node: Node, sym: SymbolTable): Expr[?] = {
    val z3 = sym.z3
    
    node match {
      case a @ Atom(v) =>
        // Try parsing as integer literal
        try { z3.mkInt(v.toInt) }
        catch { case _: Exception => 
          // Otherwise, treat as a logical variable
          getOrCreateVar(v, sym)
        }

      case Apply(fun, args, _, _) =>
        // Dispatch based on "smt-op" property in the SymbolTable
        sym.getProp(fun, "smt-op") match {
          case Some("add") => z3.mkAdd(toZ3(args(0), sym).asInstanceOf[ArithExpr[?]], toZ3(args(1), sym).asInstanceOf[ArithExpr[?]])
          case Some("sub") => z3.mkSub(toZ3(args(0), sym).asInstanceOf[ArithExpr[?]], toZ3(args(1), sym).asInstanceOf[ArithExpr[?]])
          case Some("mul") => z3.mkMul(toZ3(args(0), sym).asInstanceOf[ArithExpr[?]], toZ3(args(1), sym).asInstanceOf[ArithExpr[?]])
          case Some("eq")  => z3.mkEq(toZ3(args(0), sym), toZ3(args(1), sym))
          case Some("and") => z3.mkAnd(toZ3(args(0), sym).asInstanceOf[BoolExpr], toZ3(args(1), sym).asInstanceOf[BoolExpr])
          case Some("or")  => z3.mkOr(toZ3(args(0), sym).asInstanceOf[BoolExpr], toZ3(args(1), sym).asInstanceOf[BoolExpr])
          case Some("not") => z3.mkNot(toZ3(args(0), sym).asInstanceOf[BoolExpr])
          case Some("gt")  => z3.mkGt(toZ3(args(0), sym).asInstanceOf[ArithExpr[?]], toZ3(args(1), sym).asInstanceOf[ArithExpr[?]])
          case Some("lt")  => z3.mkLt(toZ3(args(0), sym).asInstanceOf[ArithExpr[?]], toZ3(args(1), sym).asInstanceOf[ArithExpr[?]])
          
          // Special case for '?' (logical variable declaration)
          case _ if fun == "?" =>
            val name = args(0) match {
              case Atom(n) => n
              case _ => interpreter.eval(args(0), sym).toString
            }
            getOrCreateVar(name, sym)
            
          case other => throw new RuntimeException(s"Symbol '$fun' has no SMT mapping (property 'smt-op' is missing or unknown: $other)")
        }
      
      case other => throw new RuntimeException(s"Unsupported node type in SMT: ${other.getClass.getName}")
    }
  }

  /**
   * Retrieves or creates a Z3 IntExpr associated with the given name in the current SymbolTable.
   */
  def getOrCreateVar(name: String, sym: SymbolTable): IntExpr = {
    sym.logicalVars.getOrElseUpdate(name, sym.z3.mkIntConst(name))
  }

  /**
   * Solves the given constraint and returns the result as a Romanesco data structure.
   */
  def solve(constraintNode: Node, sym: SymbolTable): Any = {
    val solver = sym.z3.mkSolver()
    val constraint = toZ3(constraintNode, sym).asInstanceOf[BoolExpr]
    solver.add(constraint)
    
    if (solver.check() == Status.SATISFIABLE) {
      val model = solver.getModel()
      // For now, return the model string. Future: convert to Romanesco AST.
      model.toString
    } else {
      "UNSAT"
    }
  }
}
