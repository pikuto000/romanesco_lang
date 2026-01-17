package Core

import Parsing.Expr as ASTExpr
import scala.collection.mutable

/**
 * romanesco評価器
 * 
 * 非決定的な評価をサポート (List[(Value, Env)])
 */
object Evaluator:
  import Expr.*
  import Value.*
  
  def eval(expr: Expr, env: Env): List[(Value, Env)] =
    try
      expr match
        case Atom(name) => evalAtom(name, env)
        case Apply(f, arg) => evalApply(f, arg, env)
    catch
      case _: Contradiction => Nil
      case _: MatchFailure => Nil

  private def evalAtom(name: String, env: Env): List[(Value, Env)] =
    if Prelude.bindings.contains(name) then List((PrimOp(name), env))
    else
      env.lookup(name) match
        case Some(Atom(n)) if n == name => List((AtomVal(name), env))
        case Some(expr) => eval(expr, env)
        case None =>
          if name.toIntOption.isDefined then 
            val iv = name.toInt
            val w = env.lookupWidth(name).getOrElse(Math.max(1L, java.math.BigInteger.valueOf(iv.toLong).bitLength().toLong))
            List((NumVal(iv, Some(w)), env))
          else if name == "true" then List((BoolVal(true), env))
          else if name == "false" then List((BoolVal(false), env))
          else List((AtomVal(name), env))

  private def evalApply(f: Expr, arg: Expr, env: Env): List[(Value, Env)] =
    for
      case (fVal, env1) <- eval(f, env)
      res <- fVal match
        case PrimOp(op) if Set("and", "or", "seq", "=").contains(op) =>
          for case (argVal, env2) <- eval(arg, env1)
          yield (PartialBinOp(op, argVal, None), env2)
        
        case PrimOp("lambda") =>
          arg match
            case Atom(param) => List((PartialBinOp("lambda", AtomVal(param), None), env1))
            case _ => Nil
            
        case PrimOp(op) =>
          for case (argVal, env2) <- eval(arg, env1)
          yield (PartialBinOp(op, argVal, None), env2)
          
        case PartialBinOp("and", leftVal, _) =>
          evalAnd(leftVal, arg, env1)
          
        case PartialBinOp("or", leftVal, _) =>
          List((leftVal, env1)) ++ eval(arg, env1)
          
        case PartialBinOp("seq", _, _) =>
          eval(arg, env1)
          
        case PartialBinOp("=", leftVal, _) =>
          unify(leftVal, arg, env1)
          
        case PartialBinOp("lambda", AtomVal(param), _) =>
          List((Closure(param, arg, env1), env1))
          
        case PartialBinOp(op, leftVal, _) =>
          for case (rightVal, env2) <- eval(arg, env1)
          yield (Prelude.evalPrimitive(op, List(leftVal, rightVal)), env2)
          
        case Closure(param, body, closureEnv) =>
          for 
            case (argVal, env2) <- eval(arg, env1)
            case (resVal, _) <- eval(body, closureEnv.extend(param, valueToExpr(argVal)))
          yield (resVal, env2)
          
        case _ => Nil
    yield res

  private def evalAnd(left: Value, right: Expr, env: Env): List[(Value, Env)] =
    left match
      case BoolVal(false) => List((BoolVal(false), env))
      case BoolVal(true) => eval(right, env)
      case _ =>
        for case (rightVal, env2) <- eval(right, env)
        yield (PairVal(left, rightVal), env2)

  private def unify(v1: Value, expr2: Expr, env: Env): List[(Value, Env)] =
    for
      case (v2, env2) <- eval(expr2, env)
      newEnv <- _unify(v1, v2, env2).toList
    yield (v2, newEnv)

  private def _unify(v1: Value, v2: Value, env: Env): Option[Env] =
    val val1 = resolve(v1, env)
    val val2 = resolve(v2, env)
    
    if (val1 == val2) Some(env)
    else (val1, val2) match
      case (AtomVal(n1), _) if !Prelude.bindings.contains(n1) =>
        Some(env.extend(n1, valueToExpr(val2)))
      case (_, AtomVal(n2)) if !Prelude.bindings.contains(n2) =>
        Some(env.extend(n2, valueToExpr(val1)))
      case (PairVal(l1, r1), PairVal(l2, r2)) =>
        _unify(l1, l2, env).flatMap(env2 => _unify(r1, r2, env2))
      case _ => if (val1 == val2) Some(env) else None

  private def resolve(v: Value, env: Env): Value = v match
    case AtomVal(name) =>
      env.lookup(name) match
        case Some(Atom(n)) if n == name => v
        case Some(expr) => 
          val results = eval(expr, env)
          if (results.nonEmpty) resolve(results.head._1, results.head._2) else v
        case _ => v
    case PairVal(l, r) => PairVal(resolve(l, env), resolve(r, env))
    case _ => v

  private def valueToExpr(value: Value): Expr = value match
    case NumVal(n, _) => Atom(n.toString)
    case BoolVal(b) => Atom(b.toString)
    case AtomVal(name) => Atom(name)
    case PrimOp(name) => Atom(name)
    case PairVal(left, right) => Apply(Apply(Atom("and"), valueToExpr(left)), valueToExpr(right))
    case Closure(param, body, _) => Apply(Apply(Atom("lambda"), Atom(param)), body)
    case _ => Atom("null")

    def evalProgram(stmts: List[Parsing.Stmt]): (Env, Option[Value]) =

      val astExprs = stmts.collect { case Parsing.Stmt.ExprStmt(e) => e }

      val coreExprs = astExprs.map(Translator.translateExpr)

      

      val inference = new Solver.WidthInference()

      val widths = inference.infer(astExprs)

      

      val initialEnv = Env.initial.extendAllWidths(widths)

      var paths = List[(Option[Value], Env)]((None, initialEnv))

      

      for (coreExpr <- coreExprs) do

        paths = paths.flatMap { case (_, currentEnv) =>

          coreExpr match

            case Apply(Apply(Atom("="), Atom(name)), valueExpr) =>

              eval(valueExpr, currentEnv).map { case (v, e) =>

                (Some(v), e.extend(name, valueToExpr(v)))

              }

            case _ =>

              eval(coreExpr, currentEnv).map { case (v, e) => (Some(v), e) }

        }

      

      paths.headOption match

        case Some((v, e)) => (e, v.map(res => resolve(res, e)))

        case None => (initialEnv, None)

  