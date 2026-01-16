package Core

import Parsing.{Expr as ASTExpr, Stmt, Pattern}

/**
 * AST → Core変換器
 */
object Translator:
  import Core.Expr.*
  
  def translateStmt(stmt: Stmt): (Option[String], Expr) = stmt match
    case Stmt.ExprStmt(expr) =>
      (None, translateExpr(expr))
    
    case Stmt.Assignment(name, value) =>
      (Some(name), translateExpr(value))
    
    case Stmt.MacroDef(name, patterns, body) =>
      val coreBody = translateExpr(body)
      (Some(name), coreBody)
  
  def translateExpr(expr: ASTExpr): Expr = expr match
    case ASTExpr.Num(value) =>
      Atom(value)
    
    case ASTExpr.Var(name) =>
      Atom(name)
    
    case ASTExpr.BinOp(op, left, right) =>
      // op left right → apply(apply(op, left), right)
      val opExpr = Atom(op)
      val leftExpr = translateExpr(left)
      val rightExpr = translateExpr(right)
      Apply(Apply(opExpr, leftExpr), rightExpr)
    
    case ASTExpr.Call(f, args) =>
      // f(a, b, c) → apply(apply(apply(f, a), b), c)
      val fExpr = translateExpr(f)
      args.foldLeft(fExpr) { (acc, arg) =>
        Apply(acc, translateExpr(arg))
      }
    
    case ASTExpr.Lambda(param, body) =>
      // \param -> body → apply(apply(lambda, param), body)
      val bodyExpr = translateExpr(body)
      Apply(Apply(Atom("lambda"), Atom(param)), bodyExpr)
    
    case ASTExpr.Block(stmts) =>
      translateBlock(stmts)
  
  private def translateBlock(stmts: List[Stmt]): Expr =
    stmts match
      case Nil =>
        Atom("true")
      
      case single :: Nil =>
        translateStmt(single) match
          case (None, expr) => expr
          case (Some(name), value) => 
            Apply(Apply(Atom("="), Atom(name)), value)
      
      case first :: rest =>
        val firstExpr = translateStmt(first) match
          case (None, expr) => expr
          case (Some(name), value) =>
            Apply(Apply(Atom("="), Atom(name)), value)
        
        val restExpr = translateBlock(rest)
        Apply(Apply(Atom("seq"), firstExpr), restExpr)
  
  def translateProgram(stmts: List[Stmt]): (Map[String, Expr], Option[Expr]) =
    val bindings = scala.collection.mutable.Map.empty[String, Expr]
    var lastExpr: Option[Expr] = None
    
    stmts.foreach { stmt =>
      translateStmt(stmt) match
        case (Some(name), value) =>
          bindings(name) = value
        case (None, expr) =>
          lastExpr = Some(expr)
    }
    
    (bindings.toMap, lastExpr)
  
  def showExpr(expr: Expr): String = expr match
    case Atom(name) => name
    case Apply(Apply(Atom(op), left), right) if isBinOp(op) =>
      s"(${showExpr(left)} $op ${showExpr(right)})"
    case Apply(f, arg) =>
      s"(${showExpr(f)} ${showExpr(arg)})"
  
  private def isBinOp(name: String): Boolean =
    Set("+", "-", "*", "/", ">", "<", ">=", "<=", "==", "!=", "and", "or", "=", "seq").contains(name)
