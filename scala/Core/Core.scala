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

case class Env(bindings: Map[String, Expr]):
  def lookup(name: String): Option[Expr] = bindings.get(name)
  def extend(name: String, value: Expr): Env = Env(bindings + (name -> value))
  def extendAll(newBindings: Map[String, Expr]): Env = Env(bindings ++ newBindings)
  def contains(name: String): Boolean = bindings.contains(name)

object Env:
  def empty: Env = Env(Map.empty)
  def initial: Env = Env(Prelude.bindings)

enum Value:
  case NumVal(n: Int)
  case BoolVal(b: Boolean)
  case PairVal(left: Value, right: Value)
  case Closure(param: String, body: Expr, env: Env)
  case PrimOp(name: String)
  case AtomVal(name: String)
  case PartialBinOp(op: String, leftVal: Value, rightExpr: Option[Expr])

object Value:
  def show(v: Value): String = v match
    case NumVal(n) => n.toString
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
    case ("+", List(NumVal(a), NumVal(b))) => NumVal(a + b)
    case ("-", List(NumVal(a), NumVal(b))) => NumVal(a - b)
    case ("*", List(NumVal(a), NumVal(b))) => NumVal(a * b)
    case ("/", List(NumVal(a), NumVal(b))) => NumVal(a / b)
    case (">", List(NumVal(a), NumVal(b))) => BoolVal(a > b)
    case ("<", List(NumVal(a), NumVal(b))) => BoolVal(a < b)
    case (">=", List(NumVal(a), NumVal(b))) => BoolVal(a >= b)
    case ("<=", List(NumVal(a), NumVal(b))) => BoolVal(a <= b)
    case ("==", List(NumVal(a), NumVal(b))) => BoolVal(a == b)
    case ("!=", List(NumVal(a), NumVal(b))) => BoolVal(a != b)
    case ("==", List(a, b)) => BoolVal(a == b)
    case ("!=", List(a, b)) => BoolVal(a != b)
    case _ => throw new RuntimeException(s"Cannot apply $op to $args")