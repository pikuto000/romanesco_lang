package Parsing

import Lexing.Token
import Undeterminable.tree

/**
 * パース結果
 * @param value パースされた値
 * @param remaining 残りのトークン列
 */
case class ParseResult[+A](value: A, remaining: List[Token]):
  /** 残りトークンに対して次のパーサーを適用 */
  def flatMap[B](f: A => Option[ParseResult[B]]): Option[ParseResult[B]] =
    f(value)
  
  def map[B](f: A => B): ParseResult[B] =
    ParseResult(f(value), remaining)

/**
 * romanesco パーサー
 * 
 * 非決定的トークン列を受け取り、AST木を構築
 */
object Parser:
  
  // ======================================
  // エントリーポイント
  // ======================================
  
  /**
   * トークンツリーから全てのパスを抽出してパース
   * 各トークン列候補に対してパースを試行
   */
  def parseAll(tokenTree: tree[Token]): tree[List[Stmt]] =
    val paths = tokenTree.flattenPaths.map(_.toList)
    
    if paths.isEmpty then
      tree.DeadEnd
    else
      val results = paths.flatMap { tokens =>
        // 空白トークンをフィルタ（意味解析に不要）
        val filtered = tokens.filterNot(_.isInstanceOf[Token.WS])
        
        parseProgram(filtered) match
          case Some(ParseResult(stmts, Nil)) => 
            Some(stmts)
          
          case Some(ParseResult(stmts, remaining)) => 
            // TODO: より詳細なエラーレポート
            println(s"Warning: Unparsed tokens: $remaining")
            Some(stmts)
          
          case None => 
            // TODO: パース失敗の原因を特定
            println(s"Failed to parse: ${filtered.take(10).mkString(" ")}...")
            None
      }
      
      if results.isEmpty then
        tree.DeadEnd
      else
        tree.fork(results.map(r => tree.leaf(r)))
  
  // ======================================
  // プログラム・ステートメントレベル
  // ======================================
  
  /** プログラム全体をパース */
  def parseProgram(tokens: List[Token]): Option[ParseResult[List[Stmt]]] =
    parseStatements(tokens, Nil)
  
  /** ステートメント列をパース */
  private def parseStatements(
    tokens: List[Token], 
    acc: List[Stmt]
  ): Option[ParseResult[List[Stmt]]] =
    tokens match
      case Nil => 
        Some(ParseResult(acc.reverse, Nil))
      
      case _ => 
        parseStatement(tokens) match
          case Some(ParseResult(stmt, rest)) => 
            parseStatements(rest, stmt :: acc)
          case None => 
            Some(ParseResult(acc.reverse, tokens))
  
  /** 単一ステートメントをパース */
  private def parseStatement(tokens: List[Token]): Option[ParseResult[Stmt]] =
    parseMacroDef(tokens)
      .orElse(parseAssignment(tokens))
      .orElse(parseExprStmt(tokens))
  
  // ======================================
  // マクロ定義
  // ======================================
  
  /**
   * マクロ定義をパース
   * 構文: syntax <name> <pattern>* = <body>
   */
  private def parseMacroDef(tokens: List[Token]): Option[ParseResult[Stmt]] =
    tokens match
      case Token.Keyword("syntax") :: rest =>
        for
          ParseResult(name, rest1) <- parseIdent(rest)
          ParseResult(patterns, rest2) <- parsePatterns(rest1)
          case Token.Op("=") :: rest3 <- Some(rest2)
          ParseResult(body, rest4) <- parseExpr(rest3)
        yield 
          ParseResult(Stmt.MacroDef(name, patterns, body), rest4)
      
      case _ => None
  
  /** パターン列をパース（= まで） */
  private def parsePatterns(tokens: List[Token]): Option[ParseResult[List[Pattern]]] =
    def loop(toks: List[Token], acc: List[Pattern]): Option[ParseResult[List[Pattern]]] =
      toks match
        case Token.Op("=") :: _ => 
          Some(ParseResult(acc.reverse, toks))
        
        case _ =>
          parsePattern(toks) match
            case Some(ParseResult(pat, rest)) => loop(rest, pat :: acc)
            case None => Some(ParseResult(acc.reverse, toks))
    
    loop(tokens, Nil)
  
  /** 単一パターンをパース */
  private def parsePattern(tokens: List[Token]): Option[ParseResult[Pattern]] =
    tokens match
      case Token.Delim("[") :: Token.Ident(name) :: Token.Delim("]") :: rest =>
        Some(ParseResult(Pattern.VarPattern(name), rest))
      
      case Token.Ident(name) :: rest =>
        Some(ParseResult(Pattern.WordPattern(name), rest))
      
      case Token.Number(num) :: rest =>
        Some(ParseResult(Pattern.NumPattern(num), rest))
      
      case _ => None
  
  // ======================================
  // 代入・式文
  // ======================================
  
  /** 変数代入をパース: x = <expr> */
  private def parseAssignment(tokens: List[Token]): Option[ParseResult[Stmt]] =
    tokens match
      case Token.Ident(name) :: Token.Op("=") :: rest =>
        parseExpr(rest).map { case ParseResult(expr, rest2) =>
          ParseResult(Stmt.Assignment(name, expr), rest2)
        }
      
      case _ => None
  
  /** 式文をパース */
  private def parseExprStmt(tokens: List[Token]): Option[ParseResult[Stmt]] =
    parseExpr(tokens).map { case ParseResult(expr, rest) =>
      ParseResult(Stmt.ExprStmt(expr), rest)
    }
  
  // ======================================
  // 式パーサー（優先順位降順）
  // ======================================
  
  /** 式をパース（トップレベル） */
  def parseExpr(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseComparison(tokens)
  
  /** 比較演算: ==, !=, <, >, <=, >= */
  private def parseComparison(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinaryOp(
      tokens,
      parseAdditive,
      Set("=", "<", ">", "<=", ">=","&", "|", "not")
    )
  
  /** 加減算: +, - */
  private def parseAdditive(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinaryOp(
      tokens,
      parseMultiplicative,
      Set("+", "-")
    )
  
  /** 乗除算: *, / */
  private def parseMultiplicative(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinaryOp(
      tokens,
      parsePrimary,
      Set("*", "/")
    )
  
  /**
   * 二項演算子の汎用パーサー
   * TODO: 結合性を考慮した実装に改善
   */
  private def parseBinaryOp(
    tokens: List[Token],
    parseOperand: List[Token] => Option[ParseResult[Expr]],
    operators: Set[String]
  ): Option[ParseResult[Expr]] =
    parseOperand(tokens).flatMap { case ParseResult(left, rest) =>
      rest match
        case Token.Op(op) :: rest2 if operators.contains(op) =>
          parseOperand(rest2).map { case ParseResult(right, rest3) =>
            ParseResult(Expr.BinOp(op, left, right), rest3)
          }
        case _ => 
          Some(ParseResult(left, rest))
    }
  
  // ======================================
  // 基本式
  // ======================================
  
  /** 基本式をパース */
  private def parsePrimary(tokens: List[Token]): Option[ParseResult[Expr]] =
    tokens match
      // 数値リテラル
      case Token.Number(n) :: rest => 
        Some(ParseResult(Expr.Num(n), rest))
      
      // 関数呼び出し: f(args)
      case Token.Ident(name) :: Token.Delim("(") :: rest =>
        parseArgs(rest).map { case ParseResult(args, rest2) =>
          ParseResult(Expr.Call(name, args), rest2)
        }
      
      // 変数参照
      case Token.Ident(name) :: rest => 
        Some(ParseResult(Expr.Var(name), rest))
      
      // ブロック: { ... }
      case Token.Delim("{") :: rest => 
        parseBlock(rest)
      
      // 括弧でグループ化: (expr)
      case Token.Delim("(") :: rest =>
        parseExpr(rest).flatMap { case ParseResult(expr, rest2) =>
          rest2 match
            case Token.Delim(")") :: rest3 => 
              Some(ParseResult(expr, rest3))
            case _ => None
        }
      
      case _ => None
  
  /** ブロックをパース: { stmt; stmt; ... } */
  private def parseBlock(tokens: List[Token]): Option[ParseResult[Expr]] =
    def loop(toks: List[Token], acc: List[Stmt]): Option[ParseResult[List[Stmt]]] =
      toks match
        case Token.Delim("}") :: _ => 
          Some(ParseResult(acc.reverse, toks))
        
        case _ =>
          parseStatement(toks) match
            case Some(ParseResult(stmt, rest)) =>
              rest match
                case Token.Delim(";") :: rest2 => loop(rest2, stmt :: acc)
                case _ => loop(rest, stmt :: acc)
            case None => 
              Some(ParseResult(acc.reverse, toks))
    
    loop(tokens, Nil).flatMap { case ParseResult(stmts, rest) =>
      rest match
        case Token.Delim("}") :: rest2 => 
          Some(ParseResult(Expr.Block(stmts), rest2))
        case _ => None
    }
  
  /** 関数引数をパース: (arg1, arg2, ...) */
  private def parseArgs(tokens: List[Token]): Option[ParseResult[List[Expr]]] =
    def loop(toks: List[Token], acc: List[Expr]): Option[ParseResult[List[Expr]]] =
      toks match
        case Token.Delim(")") :: rest => 
          Some(ParseResult(acc.reverse, rest))
        
        case _ =>
          parseExpr(toks).flatMap { case ParseResult(expr, rest) =>
            rest match
              case Token.Delim(",") :: rest2 => 
                loop(rest2, expr :: acc)
              case Token.Delim(")") :: rest2 => 
                Some(ParseResult((expr :: acc).reverse, rest2))
              case _ => None
          }
    
    loop(tokens, Nil)
  
  // ======================================
  // ヘルパー
  // ======================================
  
  /** 識別子トークンをパース */
  private def parseIdent(tokens: List[Token]): Option[ParseResult[String]] =
    tokens match
      case Token.Ident(name) :: rest => 
        Some(ParseResult(name, rest))
      case _ => None
