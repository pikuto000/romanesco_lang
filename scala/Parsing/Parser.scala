package Parsing

import Lexing.Token
import Undeterminable.tree
import Parsing.{Expr, Stmt, Pattern}

// パーサー結果
case class ParseResult[+A](value: A, remaining: List[Token])

object Parser:
  
  // トークンツリーから全てのパスを抽出してパース
  def parseAll(tokenTree: tree[Token]): tree[List[Stmt]] =
    val paths = tokenTree.flattenPaths.map(_.toList)
    
    if paths.isEmpty then
      tree.DeadEnd
    else
      val results = paths.flatMap { tokens =>
        // 空白トークンをフィルタ
        val filtered = tokens.filterNot(_.isInstanceOf[Token.WS])
        parseProgram(filtered) match
          case Some(ParseResult(stmts, Nil)) => Some(stmts)
          case Some(ParseResult(stmts, remaining)) => 
            println(s"Warning: Unparsed tokens: $remaining")
            Some(stmts)
          case None => 
            println(s"Failed to parse: $filtered")
            None
      }
      
      if results.isEmpty then
        tree.DeadEnd
      else
        tree.Node(Vector.empty, results.map(r => tree.Node(Vector(r), LazyList.empty)))
  
  // プログラム全体をパース
  def parseProgram(tokens: List[Token]): Option[ParseResult[List[Stmt]]] =
    parseStatements(tokens, Nil)
  
  def parseStatements(tokens: List[Token], acc: List[Stmt]): Option[ParseResult[List[Stmt]]] =
    tokens match
      case Nil => Some(ParseResult(acc.reverse, Nil))
      case _ => 
        parseStatement(tokens) match
          case Some(ParseResult(stmt, rest)) => parseStatements(rest, stmt :: acc)
          case None => Some(ParseResult(acc.reverse, tokens))
  
  // 単一のステートメントをパース
  def parseStatement(tokens: List[Token]): Option[ParseResult[Stmt]] =
    parseMacroDef(tokens)
      .orElse(parseAssignment(tokens))
      .orElse(parseExprStmt(tokens))
  
  // マクロ定義: syntax test [X] = { X }
  def parseMacroDef(tokens: List[Token]): Option[ParseResult[Stmt]] =
    tokens match
      case Token.Keyword("syntax") :: rest =>
        for
          ParseResult(name, rest1) <- parseIdent(rest)
          ParseResult(patterns, rest2) <- parsePatterns(rest1, Nil)
          case Token.Op("=") :: rest3 <- Some(rest2)
          ParseResult(body, rest4) <- parseExpr(rest3)
        yield ParseResult(Stmt.MacroDef(name, patterns, body), rest4)
      case _ => None
  
  // パターンリストをパース
  def parsePatterns(tokens: List[Token], acc: List[Pattern]): Option[ParseResult[List[Pattern]]] =
    tokens match
      case Token.Op("=") :: _ => Some(ParseResult(acc.reverse, tokens))
      case _ =>
        parsePattern(tokens) match
          case Some(ParseResult(pat, rest)) => parsePatterns(rest, pat :: acc)
          case None => Some(ParseResult(acc.reverse, tokens))
  
  // 単一のパターンをパース
  def parsePattern(tokens: List[Token]): Option[ParseResult[Pattern]] =
    tokens match
      case Token.Delim("[") :: Token.Ident(name) :: Token.Delim("]") :: rest =>
        Some(ParseResult(Pattern.VarPattern(name), rest))
      case Token.Ident(name) :: rest =>
        Some(ParseResult(Pattern.WordPattern(name), rest))
      case Token.Number(num) :: rest =>
        Some(ParseResult(Pattern.NumPattern(num), rest))
      case _ => None
  
  // 代入: x = 42
  def parseAssignment(tokens: List[Token]): Option[ParseResult[Stmt]] =
    tokens match
      case Token.Ident(name) :: Token.Op("=") :: rest =>
        parseExpr(rest).map { case ParseResult(expr, rest2) =>
          ParseResult(Stmt.Assignment(name, expr), rest2)
        }
      case _ => None
  
  // 式ステートメント
  def parseExprStmt(tokens: List[Token]): Option[ParseResult[Stmt]] =
    parseExpr(tokens).map { case ParseResult(expr, rest) =>
      ParseResult(Stmt.ExprStmt(expr), rest)
    }
  
  // 式をパース（優先順位考慮）
  def parseExpr(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseComparison(tokens)
  
  // 比較演算: ==, !=, <, >, <=, >=
  def parseComparison(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseAdditive(tokens).flatMap { case ParseResult(left, rest) =>
      rest match
        case Token.Op(op @ ("==" | "!=" | "<" | ">" | "<=" | ">=")) :: rest2 =>
          parseAdditive(rest2).map { case ParseResult(right, rest3) =>
            ParseResult(Expr.BinOp(op, left, right), rest3)
          }
        case _ => Some(ParseResult(left, rest))
    }
  
  // 加減算: +, -
  def parseAdditive(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseMultiplicative(tokens).flatMap { case ParseResult(left, rest) =>
      rest match
        case Token.Op(op @ ("+" | "-")) :: rest2 =>
          parseMultiplicative(rest2).map { case ParseResult(right, rest3) =>
            ParseResult(Expr.BinOp(op, left, right), rest3)
          }
        case _ => Some(ParseResult(left, rest))
    }
  
  // 乗除算: *, /
  def parseMultiplicative(tokens: List[Token]): Option[ParseResult[Expr]] =
    parsePrimary(tokens).flatMap { case ParseResult(left, rest) =>
      rest match
        case Token.Op(op @ ("*" | "/")) :: rest2 =>
          parsePrimary(rest2).map { case ParseResult(right, rest3) =>
            ParseResult(Expr.BinOp(op, left, right), rest3)
          }
        case _ => Some(ParseResult(left, rest))
    }
  
  // 基本式: 数値、変数、関数呼び出し、ブロック、括弧
  def parsePrimary(tokens: List[Token]): Option[ParseResult[Expr]] =
    tokens match
      case Token.Number(n) :: rest => Some(ParseResult(Expr.Num(n), rest))
      case Token.Ident(name) :: Token.Delim("(") :: rest =>
        parseArgs(rest, Nil).map { case ParseResult(args, rest2) =>
          ParseResult(Expr.Call(name, args), rest2)
        }
      case Token.Ident(name) :: rest => Some(ParseResult(Expr.Var(name), rest))
      case Token.Delim("{") :: rest => parseBlock(rest)
      case Token.Delim("(") :: rest =>
        parseExpr(rest).flatMap { case ParseResult(expr, rest2) =>
          rest2 match
            case Token.Delim(")") :: rest3 => Some(ParseResult(expr, rest3))
            case _ => None
        }
      case _ => None
  
  // ブロック: { stmt1; stmt2; ... }
  def parseBlock(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBlockStmts(tokens, Nil).flatMap { case ParseResult(stmts, rest) =>
      rest match
        case Token.Delim("}") :: rest2 => Some(ParseResult(Expr.Block(stmts), rest2))
        case _ => None
    }
  
  def parseBlockStmts(tokens: List[Token], acc: List[Stmt]): Option[ParseResult[List[Stmt]]] =
    tokens match
      case Token.Delim("}") :: _ => Some(ParseResult(acc.reverse, tokens))
      case _ =>
        parseStatement(tokens) match
          case Some(ParseResult(stmt, rest)) =>
            rest match
              case Token.Delim(";") :: rest2 => parseBlockStmts(rest2, stmt :: acc)
              case _ => parseBlockStmts(rest, stmt :: acc)
          case None => Some(ParseResult(acc.reverse, tokens))
  
  // 関数引数: (arg1, arg2, ...)
  def parseArgs(tokens: List[Token], acc: List[Expr]): Option[ParseResult[List[Expr]]] =
    tokens match
      case Token.Delim(")") :: rest => Some(ParseResult(acc.reverse, rest))
      case _ =>
        parseExpr(tokens).flatMap { case ParseResult(expr, rest) =>
          rest match
            case Token.Delim(",") :: rest2 => parseArgs(rest2, expr :: acc)
            case Token.Delim(")") :: rest2 => Some(ParseResult((expr :: acc).reverse, rest2))
            case _ => None
        }
  
  // ヘルパー
  def parseIdent(tokens: List[Token]): Option[ParseResult[String]] =
    tokens match
      case Token.Ident(name) :: rest => Some(ParseResult(name, rest))
      case _ => None
