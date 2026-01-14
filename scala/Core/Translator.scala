package Core

import Parsing.{Expr as ASTExpr, Stmt, Pattern}

/**
 * AST → Core変換器
 * 
 * パーサーが生成したASTを、romanescoのコア言語（Expr）に変換
 */
object Translator:
  import Core.Expr.*
  
  /**
   * ステートメントをCoreに変換
   * 
   * @param stmt パースされたステートメント
   * @return Core式とバインディング（変数定義の場合）
   */
  def translateStmt(stmt: Stmt): (Option[String], Expr) = stmt match
    case Stmt.ExprStmt(expr) =>
      (None, translateExpr(expr))
    
    case Stmt.Assignment(name, value) =>
      (Some(name), translateExpr(value))
    
    case Stmt.MacroDef(name, patterns, body) =>
      // TODO: マクロ定義の完全な実装
      // 現在は簡易的にラムダ式として扱う
      val coreBody = translateExpr(body)
      (Some(name), coreBody)
  
  /**
   * 式をCoreに変換
   * 
   * 二項演算は Apply(Apply(op, left), right) に展開
   */
  def translateExpr(expr: ASTExpr): Expr = expr match
    case ASTExpr.Num(value) =>
      // 数値リテラルはアトムとして表現
      Atom(value)
    
    case ASTExpr.Var(name) =>
      // 変数参照はアトム
      Atom(name)
    
    case ASTExpr.BinOp(op, left, right) =>
      // 二項演算: op left right → apply(apply(op, left), right)
      val opExpr = Atom(op)
      val leftExpr = translateExpr(left)
      val rightExpr = translateExpr(right)
      Apply(Apply(opExpr, leftExpr), rightExpr)
    
    case ASTExpr.Call(name, args) =>
      // 関数呼び出し: f(a, b, c) → apply(apply(apply(f, a), b), c)
      val fExpr = Atom(name)
      args.foldLeft(fExpr) { (acc, arg) =>
        Apply(acc, translateExpr(arg))
      }
    
    case ASTExpr.Block(stmts) =>
      // ブロック: 最後の式を値として、それ以外は順次評価
      translateBlock(stmts)
  
  /**
   * ブロックをCoreに変換
   * 
   * ブロック内の文を順次評価し、最後の式が値となる
   * 中間の代入は let 式として表現（将来実装）
   */
  private def translateBlock(stmts: List[Stmt]): Expr =
    stmts match
      case Nil =>
        // 空のブロック → unit的な値
        Atom("unit")
      
      case single :: Nil =>
        // 単一の文
        translateStmt(single) match
          case (None, expr) => expr
          case (Some(name), value) => 
            // 代入だけのブロック → 値を返す
            value
      
      case first :: rest =>
        // 複数の文: let式として展開
        translateStmt(first) match
          case (None, expr) =>
            // 式文: 評価して捨てる（副作用のため）
            // TODO: sequencing operator
            translateBlock(rest)
          
          case (Some(name), value) =>
            // 代入: let name = value in rest
            val restExpr = translateBlock(rest)
            // let は lambda + apply で表現
            // let x = v in body → (λx. body) v
            Apply(
              Apply(Prelude.LAMBDA, Atom(name)),
              Apply(value, restExpr)
            )
  
  /**
   * プログラム全体を変換
   * 
   * @param stmts ステートメントのリスト
   * @return 環境へのバインディングと最終式
   */
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
  
  /**
   * デバッグ用: Core式を文字列に変換
   */
  def showExpr(expr: Expr): String = expr match
    case Atom(name) => name
    case Apply(f, arg) =>
      val fStr = showExpr(f)
      val argStr = showExpr(arg)
      s"($fStr $argStr)"
