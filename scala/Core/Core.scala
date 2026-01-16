package Core

/**
 * romanescoのコア言語
 * 
 * 「全てが適用」の原則に完全に従う
 * - Atom: 恒等射として機能する基本要素
 * - Apply: 関数適用（唯一の操作）
 * 
 * and, or, = などの演算子も全てApplyで表現される
 */
enum Expr:
  case Atom(name: String)
  case Apply(f: Expr, arg: Expr)

object Expr:
  /** デバッグ用: 式を文字列に変換 */
  def show(expr: Expr): String = expr match
    case Atom(name) => name
    case Apply(f, arg) => s"(${show(f)} ${show(arg)})"

/**
 * 評価環境
 * 名前からExprへのバインディングを保持
 */
case class Env(bindings: Map[String, Expr]):
  /** 変数を検索 */
  def lookup(name: String): Option[Expr] =
    bindings.get(name)
  
  /** 新しいバインディングを追加 */
  def extend(name: String, value: Expr): Env =
    Env(bindings + (name -> value))
  
  /** 複数のバインディングを追加 */
  def extendAll(newBindings: Map[String, Expr]): Env =
    Env(bindings ++ newBindings)
  
  /** 変数が存在するか */
  def contains(name: String): Boolean =
    bindings.contains(name)

object Env:
  /** 空の環境 */
  def empty: Env = Env(Map.empty)
  
  /** 初期環境（Preludeを含む） */
  def initial: Env = Env(Prelude.bindings)

/**
 * 値（評価結果）
 */
enum Value:
  case NumVal(n: Int)                              // 数値
  case BoolVal(b: Boolean)                         // 真偽値
  case PairVal(left: Value, right: Value)          // ペア（積型）
  case Closure(param: String, body: Expr, env: Env) // クロージャ
  case PrimOp(name: String)                        // プリミティブ演算子
  case AtomVal(name: String)                       // 評価できないアトム
  
  // 部分適用された演算子（内部表現）
  case PartialBinOp(op: String, left: Value)       // 二項演算子の部分適用

object Value:
  /** 値を文字列表現に変換 */
  def show(v: Value): String = v match
    case NumVal(n) => n.toString
    case BoolVal(b) => b.toString
    case PairVal(left, right) => s"(${show(left)}, ${show(right)})"
    case Closure(param, _, _) => s"<lambda$param>"
    case PrimOp(name) => s"<$name>"
    case AtomVal(name) => name
    case PartialBinOp(op, left) => s"<$op ${show(left)} _>"

/**
 * 例外：制約の矛盾
 */
case class Contradiction(message: String) extends Exception(message)

/**
 * 例外：パターンマッチ失敗
 */
case class MatchFailure(message: String) extends Exception(message)

/**
 * Prelude: 組み込み関数と演算子
 * 
 * romanescoの最小限の標準ライブラリ
 */
object Prelude:
  import Expr.*
  import Value.*
  
  /**
   * Preludeで定義された全ての組み込み演算子
   * これらは初期環境に含まれる
   */
  val bindings: Map[String, Expr] = Map(
    // 論理演算子
    "and" -> Atom("and"),
    "or" -> Atom("or"),
    "seq" -> Atom("seq"),
    
    // 等号（パターンマッチング）
    "=" -> Atom("="),
    
    // 算術演算子
    "+" -> Atom("+"),
    "-" -> Atom("-"),
    "*" -> Atom("*"),
    "/" -> Atom("/"),
    
    // 比較演算子
    ">" -> Atom(">"),
    "<" -> Atom("<"),
    ">=" -> Atom(">="),
    "<=" -> Atom("<="),
    "==" -> Atom("=="),
    "!=" -> Atom("!="),
    
    // 特殊形式
    "lambda" -> Atom("lambda"),
    "true" -> Atom("true"),
    "false" -> Atom("false")
  )
  
  /**
   * プリミティブ演算の実装
   */
  def evalPrimitive(op: String, args: List[Value]): Value = (op, args) match
    // 算術演算
    case ("+", List(NumVal(a), NumVal(b))) => NumVal(a + b)
    case ("-", List(NumVal(a), NumVal(b))) => NumVal(a - b)
    case ("*", List(NumVal(a), NumVal(b))) => NumVal(a * b)
    case ("/", List(NumVal(a), NumVal(b))) => NumVal(a / b)
    
    // 比較演算
    case (">", List(NumVal(a), NumVal(b))) => BoolVal(a > b)
    case ("<", List(NumVal(a), NumVal(b))) => BoolVal(a < b)
    case (">=", List(NumVal(a), NumVal(b))) => BoolVal(a >= b)
    case ("<=", List(NumVal(a), NumVal(b))) => BoolVal(a <= b)
    case ("==", List(NumVal(a), NumVal(b))) => BoolVal(a == b)
    case ("!=", List(NumVal(a), NumVal(b))) => BoolVal(a != b)
    
    case _ =>
      throw new RuntimeException(s"Cannot apply $op to $args")
