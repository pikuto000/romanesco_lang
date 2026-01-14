package Core

/**
 * romanescoのコア言語
 * 
 * 「全てが適用」の原則に従い、最小限の構造のみを持つ
 * - Atom: 恒等射として機能する基本要素
 * - Apply: 関数適用（唯一の操作）
 */
enum Expr:
  case Atom(name: String)
  case Apply(f: Expr, arg: Expr)

/**
 * 評価環境
 * 名前からCoreへのバインディングを保持
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

object Env:
  /** 空の環境 */
  def empty: Env = Env(Map.empty)
  
  /** 初期環境（Preludeを含む） */
  def initial: Env = Env(Prelude.bindings)

/**
 * 値（評価結果）
 * 
 * 将来的にクロージャや特殊な値を追加可能
 */
enum Value:
  case NumVal(n: Int)                           // 数値
  case BoolVal(b: Boolean)                      // 真偽値
  case Closure(param: String, body: Expr, env: Env)  // クロージャ
  case PrimOp(name: String, f: List[Value] => Value) // プリミティブ演算
  case AtomVal(name: String)                    // 評価できないアトム
  case PartiallyApplied(name: String, fn: List[Value] => Value, firstArg: Value) // 部分適用
  case LambdaBuilder(param: String)             // ラムダ構築中

object Value:
  /** 値を文字列表現に変換 */
  def show(v: Value): String = v match
    case NumVal(n) => n.toString
    case BoolVal(b) => b.toString
    case Closure(param, _, _) => s"<closure λ$param>"
    case PrimOp(name, _) => s"<prim $name>"
    case AtomVal(name) => name
    case PartiallyApplied(name, _, firstArg) => s"<partially applied $name($firstArg, ...)>"
    case LambdaBuilder(param) => s"<lambda builder $param>"

/**
 * Prelude: 組み込み関数と演算子
 * 
 * romanescoの最小限の標準ライブラリ
 * Scalaで実装されているが、将来的にユーザーが再定義可能
 */
object Prelude:
  import Expr.*
  import Value.*
  
  // ========================================
  // プリミティブ演算子の実装
  // ========================================
  
  /** 二項演算の汎用ヘルパー */
  private def binaryNumOp(name: String, op: (Int, Int) => Int): Value =
    PrimOp(name, {
      case List(NumVal(a), NumVal(b)) => NumVal(op(a, b))
      case args => throw new RuntimeException(s"$name: expected 2 numbers, got $args")
    })
  
  /** 比較演算の汎用ヘルパー */
  private def comparisonOp(name: String, op: (Int, Int) => Boolean): Value =
    PrimOp(name, {
      case List(NumVal(a), NumVal(b)) => BoolVal(op(a, b))
      case args => throw new RuntimeException(s"$name: expected 2 numbers, got $args")
    })
  
  /** 論理演算の汎用ヘルパー */
  private def logicalOp(name: String, op: (Boolean, Boolean) => Boolean): Value =
    PrimOp(name, {
      case List(BoolVal(a), BoolVal(b)) => BoolVal(op(a, b))
      case args => throw new RuntimeException(s"$name: expected 2 booleans, got $args")
    })
  
  // ========================================
  // 組み込み演算子
  // ========================================
  
  /** 加算 */
  val add: Value = binaryNumOp("+", _ + _)
  
  /** 減算 */
  val sub: Value = binaryNumOp("-", _ - _)
  
  /** 乗算 */
  val mul: Value = binaryNumOp("*", _ * _)
  
  /** 除算 */
  val div: Value = binaryNumOp("/", _ / _)
  
  /** 大なり */
  val gt: Value = comparisonOp(">", _ > _)
  
  /** 小なり */
  val lt: Value = comparisonOp("<", _ < _)
  
  /** 以上 */
  val gte: Value = comparisonOp(">=", _ >= _)
  
  /** 以下 */
  val lte: Value = comparisonOp("<=", _ <= _)
  
  /** 等価 */
  val eq: Value = comparisonOp("==", _ == _)
  
  /** 非等価 */
  val neq: Value = comparisonOp("!=", _ != _)
  
  /** 論理積 */
  val and: Value = logicalOp("and", _ && _)
  
  /** 論理和 */
  val or: Value = logicalOp("or", _ || _)
  
  /** 論理否定 */
  val not: Value = PrimOp("not", {
    case List(BoolVal(b)) => BoolVal(!b)
    case args => throw new RuntimeException(s"not: expected 1 boolean, got $args")
  })
  
  // ========================================
  // 特殊形式
  // ========================================
  
  /** ラムダ（特殊なアトム） */
  val LAMBDA = Atom("λ")
  
  /** 制約（特殊なアトム） */
  val CONSTRAINT = Atom("constraint")
  
  // ========================================
  // Preludeの環境バインディング
  // ========================================
  
  /** 
   * Preludeで定義された全ての組み込み関数
   * これらは初期環境に含まれる
   */
  val bindings: Map[String, Expr] = Map.empty
  
  /**
   * アトム名からプリミティブ演算を取得
   */
  def getPrimitive(name: String): Option[Value] = name match
    case "+" => Some(add)
    case "-" => Some(sub)
    case "*" => Some(mul)
    case "/" => Some(div)
    case ">" => Some(gt)
    case "<" => Some(lt)
    case ">=" => Some(gte)
    case "<=" => Some(lte)
    case "==" => Some(eq)
    case "!=" => Some(neq)
    case "and" => Some(and)
    case "or" => Some(or)
    case "not" => Some(not)
    case _ => None
