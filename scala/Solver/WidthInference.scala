package Solver

import com.microsoft.z3.*
import Parsing.Expr as ASTExpr
import scala.collection.mutable

/**
 * Z3を使用したビット幅推論
 */
class WidthInference(debug: Boolean = false):
  private val cfg = new java.util.HashMap[String, String]()
  cfg.put("model", "true")
  private val ctx = new Context(cfg)
  private val optimizer = ctx.mkOptimize()
  private val wVars = mutable.Map[String, IntExpr]()

  def log(msg: String): Unit =
    if debug then println(s"[INFER] $msg")

  def getW(name: String): IntExpr =
    wVars.getOrElseUpdate(name, {
      val w = ctx.mkIntConst(s"w_$name")
      optimizer.Assert(ctx.mkGe(w, ctx.mkInt(0)))
      optimizer.MkMinimize(w)
      w
    })

  def infer(exprs: List[ASTExpr]): Map[String, Long] =
    log("Starting Bit-Width Inference with Optimization")
    try
      exprs.foreach(unify)
      exprs.foreach(analyze)
      
      if optimizer.Check() == Status.SATISFIABLE then
        val model = optimizer.getModel
        val res = wVars.flatMap { case (name, w) =>
          Option(model.evaluate(w, false)).map { evalRes =>
            name -> evalRes.asInstanceOf[IntNum].getInt64
          }
        }.toMap
        log(s"Inference Successful: $res")
        res
      else
        log("Inference Failed (Unsat)")
        Map.empty
    catch
      case e: Exception =>
        log(s"Inference Error: ${e.getMessage}")
        if debug then e.printStackTrace()
        Map.empty
    finally
      // Context should ideally be managed better, but for now we close it
      // Actually, if we close it here, we might have issues if we return Z3 objects
      // but we are returning Map[String, Long].
      ctx.close()

  private def unify(expr: ASTExpr): Unit = expr match
    case ASTExpr.Call(ASTExpr.Var(op), args) if isArithmetic(op) =>
      val vList = args.collect {
        case ASTExpr.Var(name) if !isNum(name) => getW(name)
      }
      if vList.length >= 2 then
        vList.sliding(2).foreach {
          case Seq(v1, v2) => optimizer.Assert(ctx.mkEq(v1, v2))
          case _ => 
        }
      args.foreach(unify)
    
    case ASTExpr.Call(f, args) =>
      unify(f)
      args.foreach(unify)
    
    case ASTExpr.Lambda(_, body) =>
      unify(body)
    
    case ASTExpr.Block(exprs) =>
      exprs.foreach(unify)
    
    case _ =>

  private def analyze(expr: ASTExpr): ArithExpr[IntSort] = expr match
    case ASTExpr.Num(value) =>
      val bits = Math.max(1, java.math.BigInteger.valueOf(value.toLong).bitLength())
      ctx.mkInt(bits)
    
    case ASTExpr.Var(name) =>
      if isNum(name) then
        val bits = Math.max(1, java.math.BigInteger.valueOf(name.toLong).bitLength())
        ctx.mkInt(bits)
      else
        getW(name)
    
    case ASTExpr.Call(ASTExpr.Var("="), List(ASTExpr.Var(name), rhs)) =>
      val wVal = analyze(rhs)
      val wTgt = getW(name)
      optimizer.Assert(ctx.mkGe(wTgt, wVal))
      wTgt

    case ASTExpr.Call(ASTExpr.Var("+"), args) =>
      val bits = args.map(analyze)
      // Optimization: x + 0 -> no growth
      val isZero = args.exists {
        case ASTExpr.Var(name) if name == "0" => true
        case ASTExpr.Num("0") => true
        case _ => false
      }
      if isZero then
        log("  Optimization: +0 detected, maintaining width")
        maxZ3(bits)
      else
        ctx.mkAdd(maxZ3(bits), ctx.mkInt(1)).asInstanceOf[IntExpr]

    case ASTExpr.Call(ASTExpr.Var("*"), args) =>
      val bits = args.map(analyze)
      // Optimization: x * 0 -> 1 bit
      val isZero = args.exists {
        case ASTExpr.Var(name) if name == "0" => true
        case ASTExpr.Num("0") => true
        case _ => false
      }
      if isZero then
        log("  Optimization: *0 detected, 1 bit")
        ctx.mkInt(1)
      else
        // Optimization: x * 1 -> no growth
        val isOne = args.exists {
          case ASTExpr.Var(name) if name == "1" => true
          case ASTExpr.Num("1") => true
          case _ => false
        }
        if isOne then
          log("  Optimization: *1 detected, maintaining width")
          maxZ3(bits)
        else
          ctx.mkAdd(bits*).asInstanceOf[IntExpr]

    case ASTExpr.Call(f, args) =>
      val fBit = analyze(f) // Probably not useful but for completeness
      val bits = args.map(analyze)
      if bits.isEmpty then fBit else maxZ3(bits)

    case ASTExpr.Lambda(_, body) =>
      analyze(body)

    case ASTExpr.Block(exprs) =>
      if exprs.isEmpty then ctx.mkInt(0)
      else analyze(exprs.last)
    
    case _ => ctx.mkInt(0)

  private def isArithmetic(op: String): Boolean =
    Set("+", "-", "*", "/", "==", ">", "<", ">=", "<=").contains(op)

  private def isNum(s: String): Boolean =
    s.forall(_.isDigit)

  private def maxZ3(vars: List[ArithExpr[IntSort]]): ArithExpr[IntSort] =
    if vars.isEmpty then ctx.mkInt(0)
    else vars.tail.foldLeft(vars.head) { (acc, v) =>
      ctx.mkITE(ctx.mkGt(v, acc), v, acc).asInstanceOf[ArithExpr[IntSort]]
    }
