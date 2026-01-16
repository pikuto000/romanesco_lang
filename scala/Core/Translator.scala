package Core

import Parsing.{Expr as ASTExpr, Stmt}

/**
 * AST → Core変換器
 */
object Translator:
  import Core.Expr.*
  
  def translateExpr(expr: ASTExpr): Expr = expr match
    case ASTExpr.Num(value) =>
      Atom(value)
    
    case ASTExpr.Var(name) =>
      Atom(name)
    
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
    
    case ASTExpr.Block(exprs) =>
      translateBlock(exprs)
  
  private def translateBlock(exprs: List[ASTExpr]): Expr =
    exprs match
      case Nil =>
        Atom("true")
      
      case first :: Nil =>
        translateExpr(first)
      
      case first :: rest =>
        val firstExpr = translateExpr(first)
        val restExpr = translateBlock(rest)
        Apply(Apply(Atom("seq"), firstExpr), restExpr)
  
  def translateProgram(stmts: List[Stmt]): (Map[String, Expr], Option[Expr]) =
    val bindings = scala.collection.mutable.Map.empty[String, Expr]
    var lastExpr: Option[Expr] = None
    
    stmts.foreach { case Stmt.ExprStmt(expr) =>
      expr match
        // 代入の検出: = name value
        case ASTExpr.Call(ASTExpr.Var("="), ASTExpr.Var(name) :: value :: Nil) =>
          bindings(name) = translateExpr(value)
        case _ =>
          lastExpr = Some(translateExpr(expr))
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