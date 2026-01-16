package Core

/**
 * romanesco評価器
 * 
 * 「全てが適用」の原則に従い、Atom + Applyのみを評価
 * 論理演算子(and, or, =)も適用として処理
 */
object Evaluator:
  import Expr.*
  import Value.*
  
  /**
   * Core式を評価
   */
  def eval(expr: Expr, env: Env): Value = expr match
    case Atom(name) =>
      evalAtom(name, env)
    
    case Apply(f, arg) =>
      evalApply(f, arg, env)
  
  /**
   * アトムを評価
   */
  private def evalAtom(name: String, env: Env): Value =
    // 環境から検索
    env.lookup(name) match
      case Some(Atom(n)) if n == name =>
        // Self-referential atom (e.g. from Prelude or x = x)
        if Prelude.bindings.contains(name) then
          PrimOp(name)
        else
          AtomVal(name)

      case Some(expr) => eval(expr, env)
      case None =>
        // プリミティブ演算子?
        if Prelude.bindings.contains(name) then
          PrimOp(name)
        else
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
   */
  private def evalApply(f: Expr, arg: Expr, env: Env): Value =
    val fVal = eval(f, env)
    
    fVal match
      // 特殊演算子: and
      case PrimOp("and") =>
        val leftVal = eval(arg, env)
        PartialBinOp("and", leftVal)
      
      case PartialBinOp("and", leftVal) =>
        evalAnd(leftVal, arg, env)
      
      // 特殊演算子: or
      case PrimOp("or") =>
        val leftVal = eval(arg, env)
        PartialBinOp("or", leftVal)
      
      case PartialBinOp("or", leftVal) =>
        evalOr(leftVal, arg, env)
      
      // 特殊演算子: seq (ブロック用)
      case PrimOp("seq") =>
        val leftVal = eval(arg, env)
        PartialBinOp("seq", leftVal)
      
      case PartialBinOp("seq", leftVal) =>
        evalSeq(leftVal, arg, env)
      
      // 特殊演算子: =
      case PrimOp("=") =>
        val leftVal = eval(arg, env)
        PartialBinOp("=", leftVal)
      
      case PartialBinOp("=", leftVal) =>
        evalEq(leftVal, arg, env)
      
      // ラムダ（lambdaに2つの引数を適用）
      case PrimOp("lambda") =>
        // λ param → PartialLambda
        arg match
          case Atom(param) =>
            PartialBinOp("lambda", AtomVal(param))
          case _ =>
            throw new RuntimeException(s"Lambda parameter must be atom, got $arg")
      
      case PartialBinOp("lambda", AtomVal(param)) =>
        // (λ param) body → Closure
        Closure(param, arg, env)
      
      // プリミティブ演算子（算術・比較）
      case PrimOp(op) =>
        val argVal = eval(arg, env)
        PartialBinOp(op, argVal)
      
      case PartialBinOp(op, leftVal) =>
        val rightVal = eval(arg, env)
        Prelude.evalPrimitive(op, List(leftVal, rightVal))
      
      // クロージャの適用
      case Closure(param, body, closureEnv) =>
        val argVal = eval(arg, env)
        val newEnv = closureEnv.extend(param, valueToExpr(argVal))
        eval(body, newEnv)
      
      case v =>
        throw new RuntimeException(s"Cannot apply non-function: ${Value.show(v)}")
  
  /**
   * and演算子の評価
   * 
   * 1. 制約の結合: 左が制約なら、その制約下で右を評価
   * 2. 積型の構成: 左が値なら、ペアを構成
   */
  private def evalAnd(left: Value, right: Expr, env: Env): Value =
    left match
      // 真偽値: 短絡評価
      case BoolVal(false) => BoolVal(false)
      case BoolVal(true) => eval(right, env)
      
      // 数値など: ペアを構成
      case _ =>
        val rightVal = eval(right, env)
        PairVal(left, rightVal)

  /**
   * seq演算子の評価（ブロックのセマンティクス）
   * 
   * 左側を評価した後（既に評価済み）、右側を評価して返す
   */
  private def evalSeq(left: Value, right: Expr, env: Env): Value =
    left match
      case BoolVal(false) => BoolVal(false) // 失敗は伝播
      case _ => eval(right, env)
  
  /**
   * or演算子の評価
   * 
   * 左側を試し、失敗したら右側を試す
   */
  private def evalOr(left: Value, right: Expr, env: Env): Value =
    try
      // 左側が制約として成功するか確認
      left match
        case BoolVal(false) => eval(right, env)
        case BoolVal(true) => left
        case _ => left  // 値があれば成功
    catch
      case _: Contradiction =>
        // 左が失敗したら右を試す
        eval(right, env)
      case _: MatchFailure =>
        eval(right, env)
  
  /**
   * =演算子の評価（パターンマッチング + 束縛）
   * 
   * left = right の形で、以下のいずれかを実行：
   * 1. rightがパターン → leftをパターンマッチング
   * 2. rightが未束縛変数 → 変数に値を束縛
   * 3. rightが値 → 等価性チェック
   */
  private def evalEq(left: Value, right: Expr, env: Env): Value =
    right match
      // 未束縛変数への束縛
      case Atom(name) if !env.contains(name) =>
        // グローバルな束縛（関数名への束縛など）
        // 注: 環境を更新するが、ここでは値を返すだけ
        // 実際の環境更新は呼び出し側で処理
        left
      
      // コンストラクタパターン: Apply(Apply(Cons, p1), p2)
      case Apply(Apply(cons, p1), p2) =>
        matchPattern(left, cons, p1, p2, env)
      
      // 単純なコンストラクタ: Atom("Nil") など
      case Atom(consName) =>
        left match
          case AtomVal(name) if name == consName => left
          case BoolVal(true) if consName == "true" => left
          case _ => throw MatchFailure(s"Pattern $consName does not match ${Value.show(left)}")
      
      // 既存値との等価性チェック
      case expr =>
        val rightVal = eval(expr, env)
        if valuesEqual(left, rightVal) then
          left
        else
          throw Contradiction(s"${Value.show(left)} != ${Value.show(rightVal)}")
  
  /**
   * パターンマッチング
   * 
   * left = Cons p1 p2 の形で、leftをペアとして分解
   */
  private def matchPattern(
    scrutinee: Value,
    cons: Expr,
    p1: Expr,
    p2: Expr,
    env: Env
  ): Value =
    scrutinee match
      case PairVal(left, right) =>
        // p1とleftをマッチング
        evalEq(left, p1, env)
        // p2とrightをマッチング
        evalEq(right, p2, env)
        // マッチング成功、元の値を返す
        scrutinee
      
      case _ =>
        throw MatchFailure(s"Expected pair for pattern, got ${Value.show(scrutinee)}")
  
  /**
   * 値の等価性チェック
   */
  private def valuesEqual(a: Value, b: Value): Boolean = (a, b) match
    case (NumVal(n1), NumVal(n2)) => n1 == n2
    case (BoolVal(b1), BoolVal(b2)) => b1 == b2
    case (AtomVal(a1), AtomVal(a2)) => a1 == a2
    case (PairVal(l1, r1), PairVal(l2, r2)) =>
      valuesEqual(l1, l2) && valuesEqual(r1, r2)
    case _ => false
  
  /**
   * 値をCore式に変換（環境に格納するため）
   */
  private def valueToExpr(value: Value): Expr = value match
    case NumVal(n) => Atom(n.toString)
    case BoolVal(b) => Atom(b.toString)
    case AtomVal(name) => Atom(name)
    case PrimOp(name) => Atom(name)
    case PairVal(left, right) =>
      Apply(Apply(Atom("and"), valueToExpr(left)), valueToExpr(right))
    case Closure(param, body, _) =>
      Apply(Apply(Atom("lambda"), Atom(param)), body)
    case _ =>
      throw new RuntimeException(s"Cannot convert value to expr: ${Value.show(value)}")
  
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
      try
        val value = eval(expr, currentEnv)
        (name, valueToExpr(value))
      catch
        case e: Exception =>
          println(s"Warning: Failed to evaluate $name: ${e.getMessage}")
          (name, expr)
    }
    
    currentEnv = Env.initial.extendAll(evaluatedBindings)
    
    // 最終式を評価
    val result = lastExpr.map { expr =>
      try
        eval(expr, currentEnv)
      catch
        case e: Contradiction =>
          println(s"Constraint failed: ${e.getMessage}")
          BoolVal(false)
        case e: MatchFailure =>
          println(s"Pattern match failed: ${e.getMessage}")
          BoolVal(false)
    }
    
    (currentEnv, result)
