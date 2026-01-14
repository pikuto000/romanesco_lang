package Core
import Core.Value
/**
 * romanesco評価器
 * 
 * Core式を評価して値を生成
 * 「全てが適用」の原則に従い、全ての計算は関数適用として実行される
 */
object Evaluator:
  import Expr.*
  import Value.*
  
  /**
   * Core式を評価
   * 
   * @param expr 評価する式
   * @param env 評価環境
   * @return 評価結果の値
   */
  def eval(expr: Expr, env: Env): Value = expr match
    case Atom(name) =>
      // アトムの評価
      evalAtom(name, env)
    
    case Apply(f, arg) =>
      // 関数適用の評価
      evalApply(f, arg, env)
  
  /**
   * アトムを評価
   * 
   * 1. 環境から検索
   * 2. プリミティブ演算子として解決
   * 3. 数値リテラルとして解釈
   * 4. 評価不可能なアトムとして返す
   */
  private def evalAtom(name: String, env: Env): Value =
    // 環境から検索
    env.lookup(name) match
      case Some(expr) => eval(expr, env)
      case None =>
        // プリミティブ演算子?
        Prelude.getPrimitive(name) match
          case Some(primOp) => primOp
          case None =>
            // 数値リテラル?
            name.toIntOption match
              case Some(n) => NumVal(n)
              case None =>
                // 真偽値?
                name match
                  case "true" => BoolVal(true)
                  case "false" => BoolVal(false)
                  case _ => AtomVal(name)
  
  /**
   * 関数適用を評価
   * 
   * apply(f, arg) を評価:
   * 1. f を評価 → 関数値
   * 2. arg を評価 → 引数値
   * 3. 関数を引数に適用
   */
  private def evalApply(f: Expr, arg: Expr, env: Env): Value =
    val fVal = eval(f, env)
    
    fVal match
      case PrimOp(name, fn) =>
        // プリミティブ演算子の適用
        // カリー化されているので、部分適用を処理
        val argVal = eval(arg, env)
        
        // 二項演算子は2つの引数を取る
        // 最初の引数で部分適用されたクロージャを返す
        fn match
          case _ => 
            // 簡易実装: 2引数を一度に取る
            // TODO: 正しいカリー化の実装
            argVal match
              case NumVal(_) | BoolVal(_) =>
                // 部分適用: 最初の引数を記憶
                PartiallyApplied(name, fn, argVal)
              case _ =>
                throw new RuntimeException(s"Cannot apply $name to non-value: $argVal")
      
      case PartiallyApplied(name, fn, firstArg) =>
        // 部分適用済みの演算子に第2引数を適用
        val secondArg = eval(arg, env)
        fn(List(firstArg, secondArg))
      
      case Closure(param, body, closureEnv) =>
        // クロージャの適用
        val argVal = eval(arg, env)
        val newEnv = closureEnv.extend(param, valueToExpr(argVal))
        eval(body, newEnv)
      
      case AtomVal("λ") =>
        // ラムダ式の構築
        // apply(apply(λ, param), body) → closure
        // 簡易実装: 引数がアトムであることを仮定
        arg match
          case Atom(param) =>
            LambdaBuilder(param)
          case Apply(Atom(param), body) =>
            Closure(param, body, env)
          case _ =>
            throw new RuntimeException(s"Invalid lambda form: λ $arg")
      
      case LambdaBuilder(param) =>
        // ラムダ本体の適用
        Closure(param, arg, env)
      
      case v =>
        throw new RuntimeException(s"Cannot apply non-function: $v")
  
  /**
   * 値をCore式に変換（環境に格納するため）
   */
  private def valueToExpr(value: Value): Expr = value match
    case NumVal(n) => Atom(n.toString)
    case BoolVal(b) => Atom(b.toString)
    case AtomVal(name) => Atom(name)
    case Closure(param, body, env) =>
      // クロージャはそのまま保持（環境に含める）
      Apply(Apply(Prelude.LAMBDA, Atom(param)), body)
    case _ =>
      throw new RuntimeException(s"Cannot convert value to expr: $value")
  
  /**
   * プログラムを評価
   * 
   * @param stmts ステートメントのリスト
   * @return 最終的な環境と評価結果
   */
  def evalProgram(stmts: List[Parsing.Stmt]): (Env, Option[Value]) =
    val (bindings, lastExpr) = Translator.translateProgram(stmts)
    
    // 初期環境にバインディングを追加
    var currentEnv = Env.initial.extendAll(bindings)
    
    // 各バインディングを評価（相互参照のため）
    val evaluatedBindings = bindings.map { case (name, expr) =>
      val value = eval(expr, currentEnv)
      (name, valueToExpr(value))
    }
    
    currentEnv = Env.initial.extendAll(evaluatedBindings)
    
    // 最終式を評価
    val result = lastExpr.map(eval(_, currentEnv))
    
    (currentEnv, result)

