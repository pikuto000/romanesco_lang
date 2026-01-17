package Core

/**
 * romanescoのコア言語
 */
enum Expr:
  case Atom(name: String)
  case Apply(f: Expr, arg: Expr)

object Expr:
  def show(expr: Expr): String = expr match
    case Atom(name) => name
    case Apply(f, arg) => s"(${show(f)} ${show(arg)})"

case class Env(bindings: Map[String, Expr], widths: Map[String, Long] = Map.empty):
  def lookup(name: String): Option[Expr] = bindings.get(name)
  def lookupWidth(name: String): Option[Long] = widths.get(name)
  def extend(name: String, value: Expr): Env = Env(bindings + (name -> value), widths)
  def extendWidth(name: String, width: Long): Env = Env(bindings, widths + (name -> width))
  def extendAll(newBindings: Map[String, Expr]): Env = Env(bindings ++ newBindings, widths)
  def extendAllWidths(newWidths: Map[String, Long]): Env = Env(bindings, widths ++ newWidths)
  def contains(name: String): Boolean = bindings.contains(name)

object Env:
  def empty: Env = Env(Map.empty)
  def initial: Env = Env(Prelude.bindings)

enum Value:
  case NumVal(n: Int, width: Option[Long] = None)
  case BoolVal(b: Boolean)
  case PairVal(left: Value, right: Value)
  case Closure(param: String, body: Expr, env: Env)
  case PrimOp(name: String)
  case AtomVal(name: String)
  case PartialBinOp(op: String, leftVal: Value, rightExpr: Option[Expr])

object Value:
  def show(v: Value): String = v match
    case NumVal(n, Some(w)) => s"$n:$w"
    case NumVal(n, None) => n.toString
    case BoolVal(b) => b.toString
    case PairVal(left, right) => s"(${show(left)}, ${show(right)})"
    case Closure(param, _, _) => s"<lambda $param>"
    case PrimOp(name) => s"<$name>"
    case AtomVal(name) => name
    case PartialBinOp(op, left, _) => s"<$op ${show(left)} _>"

case class Contradiction(message: String) extends Exception(message)
case class MatchFailure(message: String) extends Exception(message)

object Prelude:
  import Expr.*
  import Value.*
  
  val bindings: Map[String, Expr] = Map(
    "and" -> Atom("and"), "or" -> Atom("or"), "seq" -> Atom("seq"), "=" -> Atom("="),
    "+" -> Atom("+"), "-" -> Atom("-"), "*" -> Atom("*"), "/" -> Atom("/"),
    ">" -> Atom(">"), "<" -> Atom("<"), ">=" -> Atom(">="), "<=" -> Atom("<="),
    "==" -> Atom("=="), "!=" -> Atom("!="), "lambda" -> Atom("lambda"),
    "true" -> Atom("true"), "false" -> Atom("false")
  )
  
  def evalPrimitive(op: String, args: List[Value]): Value = (op, args) match
    case ("+", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("+", a, b, wa, wb)
      else NumVal(a + b, None)
    
    case ("-", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("-", a, b, wa, wb)
      else NumVal(a - b, None)
    
    case ("*", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("*", a, b, wa, wb)
      else NumVal(a * b, None)
    
    case ("/", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("/", a, b, wa, wb)
      else NumVal(a / b, None)

    case ("and", List(NumVal(a, wa), NumVal(b, wb))) if wa.isDefined || wb.isDefined =>
      evalBitVecOp("and", a, b, wa, wb)

    case ("or", List(NumVal(a, wa), NumVal(b, wb))) if wa.isDefined || wb.isDefined =>
      evalBitVecOp("or", a, b, wa, wb)

    case (">", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp(">", a, b, wa, wb)
      else BoolVal(a > b)
      
    case ("<", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("<", a, b, wa, wb)
      else BoolVal(a < b)

    case (">=", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp(">=", a, b, wa, wb)
      else BoolVal(a >= b)

    case ("<=", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("<=", a, b, wa, wb)
      else BoolVal(a <= b)

    case ("==", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("==", a, b, wa, wb)
      else BoolVal(a == b)

    case ("!=", List(NumVal(a, wa), NumVal(b, wb))) =>
      if wa.isDefined || wb.isDefined then evalBitVecOp("!=", a, b, wa, wb)
      else BoolVal(a != b)

    case ("and", List(BoolVal(a), BoolVal(b))) => BoolVal(a && b)
    case ("or", List(BoolVal(a), BoolVal(b))) => BoolVal(a || b)
    case ("==", List(a, b)) => BoolVal(a == b)
    case ("!=", List(a, b)) => BoolVal(a != b)
    case _ => throw new RuntimeException(s"Cannot apply $op to $args")

  private def evalBitVecOp(op: String, va: Int, vb: Int, wa: Option[Long], wb: Option[Long]): Value =
    val ctx = new com.microsoft.z3.Context()
    try
      val w1 = wa.getOrElse(Math.max(1L, java.math.BigInteger.valueOf(va.toLong).bitLength().toLong))
      val w2 = wb.getOrElse(Math.max(1L, java.math.BigInteger.valueOf(vb.toLong).bitLength().toLong))
      
      var resW = Math.max(w1, w2)
      if (op == "+") resW = Math.max(w1, w2) + 1
      else if (op == "*") resW = w1 + w2
      
      val za = ctx.mkBV(va, resW.toInt)
      val zb = ctx.mkBV(vb, resW.toInt)
      
      op match
        case "+" => fromZ3(ctx.mkBVAdd(za, zb), resW)
        case "-" => fromZ3(ctx.mkBVSub(za, zb), resW)
        case "and" => fromZ3(ctx.mkBVAND(za, zb), resW)
        case "or" => fromZ3(ctx.mkBVOR(za, zb), resW)
        case "*" => fromZ3(ctx.mkBVMul(za, zb), resW)
        case "/" => fromZ3(ctx.mkBVUDiv(za, zb), resW)
        case "==" => BoolVal(ctx.mkEq(za, zb).simplify().isTrue)
        case "!=" => BoolVal(ctx.mkEq(za, zb).simplify().isFalse)
        case ">" => BoolVal(ctx.mkBVUGT(za, zb).simplify().isTrue)
        case "<" => BoolVal(ctx.mkBVULT(za, zb).simplify().isTrue)
        case ">=" => BoolVal(ctx.mkBVUGE(za, zb).simplify().isTrue)
        case "<=" => BoolVal(ctx.mkBVULE(za, zb).simplify().isTrue)
        case _ => throw new RuntimeException(s"Unknown bitvec op: $op")
    finally
      ctx.close()

  private def fromZ3(expr: com.microsoft.z3.Expr[?], width: Long): Value =
    val simp = expr.simplify()
    if (simp.isInstanceOf[com.microsoft.z3.BitVecNum]) then
      NumVal(simp.asInstanceOf[com.microsoft.z3.BitVecNum].getInt, Some(width))
    else
      throw new RuntimeException(s"Z3 result not concrete: $simp")
  
  
  
    
  
  