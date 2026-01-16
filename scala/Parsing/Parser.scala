package Parsing

import Lexing.Token
import Undeterminable.tree

case class ParseResult[+A](value: A, remaining: List[Token]):
  def flatMap[B](f: A => Option[ParseResult[B]]): Option[ParseResult[B]] =
    f(value)
  
  def map[B](f: A => B): ParseResult[B] =
    ParseResult(f(value), remaining)

object Parser:
  
  def parseAll(tokenTree: tree[Token]): tree[List[Stmt]] =
    val paths = tokenTree.flattenPaths.map(_.toList)
    
    if paths.isEmpty then
      tree.DeadEnd
    else
      val results = paths.flatMap { tokens =>
        val filtered = tokens.filterNot(_.isInstanceOf[Token.WS])
        
        parseProgram(filtered) match
          case Some(ParseResult(stmts, Nil)) => Some(stmts)
          case Some(ParseResult(stmts, remaining)) => 
            println(s"Warning: Unparsed tokens: $remaining")
            Some(stmts)
          case None => 
            println(s"Failed to parse: ${filtered.take(10).mkString(" ")}...")
            None
      }
      
      if results.isEmpty then tree.DeadEnd
      else tree.fork(results.map(r => tree.leaf(r)))
  
  def parseProgram(tokens: List[Token]): Option[ParseResult[List[Stmt]]] =
    parseStatements(tokens, Nil)
  
  private def parseStatements(
    tokens: List[Token], 
    acc: List[Stmt]
  ): Option[ParseResult[List[Stmt]]] =
    tokens match
      case Nil => Some(ParseResult(acc.reverse, Nil))
      case Token.Delim("\n") :: rest => parseStatements(rest, acc)
      case Token.Delim(";") :: rest => parseStatements(rest, acc)
      case _ => 
        parseStatement(tokens) match
          case Some(ParseResult(stmt, rest)) => parseStatements(rest, stmt :: acc)
          case None => Some(ParseResult(acc.reverse, tokens))
  
  private def parseStatement(tokens: List[Token]): Option[ParseResult[Stmt]] =
    parseMacroDef(tokens)
      .orElse(parseAssignment(tokens))
      .orElse(parseExprStmt(tokens))
  
  private def parseMacroDef(tokens: List[Token]): Option[ParseResult[Stmt]] =
    tokens match
      case Token.Keyword("syntax") :: rest =>
        for
          ParseResult(name, rest1) <- parseIdent(rest)
          ParseResult(patterns, rest2) <- parsePatterns(rest1)
          case Token.Op("=") :: rest3 <- Some(rest2)
          ParseResult(body, parseResult4) <- parseExpr(rest3) match
            case Some(res) => Some(res)
            case None => None
        yield ParseResult(Stmt.MacroDef(name, patterns, body), parseResult4)
      case _ => None
  
  private def parsePatterns(tokens: List[Token]): Option[ParseResult[List[Pattern]]] =
    def loop(toks: List[Token], acc: List[Pattern]): Option[ParseResult[List[Pattern]]] =
      toks match
        case Token.Op("=") :: _ => Some(ParseResult(acc.reverse, toks))
        case _ =>
          parsePattern(toks) match
            case Some(ParseResult(pat, rest)) => loop(rest, pat :: acc)
            case None => Some(ParseResult(acc.reverse, toks))
    loop(tokens, Nil)
  
  private def parsePattern(tokens: List[Token]): Option[ParseResult[Pattern]] =
    tokens match
      case Token.Delim("[") :: Token.Ident(name) :: Token.Delim("]") :: rest =>
        Some(ParseResult(Pattern.VarPattern(name), rest))
      case Token.Ident(name) :: rest =>
        Some(ParseResult(Pattern.WordPattern(name), rest))
      case Token.Number(num) :: rest =>
        Some(ParseResult(Pattern.NumPattern(num), rest))
      case _ => None
  
  private def parseAssignment(tokens: List[Token]): Option[ParseResult[Stmt]] =
    tokens match
      case Token.Ident(name) :: Token.Op("=") :: rest =>
        parseExpr(rest).map { case ParseResult(expr, rest2) =>
          ParseResult(Stmt.Assignment(name, expr), rest2)
        }
      case _ => None
  
  private def parseExprStmt(tokens: List[Token]): Option[ParseResult[Stmt]] =
    parseExpr(tokens).map { case ParseResult(expr, rest) =>
      ParseResult(Stmt.ExprStmt(expr), rest)
    }
  
  // 式パーサー（優先順位降順）
  def parseExpr(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseOr(tokens)
  
  private def parseBinOpIterative(
    tokens: List[Token],
    parseOperand: List[Token] => Option[ParseResult[Expr]],
    operators: Set[String]
  ): Option[ParseResult[Expr]] =
    parseOperand(tokens).map { case ParseResult(initialLeft, rest) =>
      def loop(currentLeft: Expr, toks: List[Token]): ParseResult[Expr] =
        toks match
          case Token.Op(op) :: rest2 if operators.contains(op) =>
            parseOperand(rest2) match
              case Some(ParseResult(right, rest3)) =>
                loop(Expr.BinOp(op, currentLeft, right), rest3)
              case None => ParseResult(currentLeft, toks)
          case _ => ParseResult(currentLeft, toks)
      
      loop(initialLeft, rest)
    }

  private def parseBinOpIterativeRight(
    tokens: List[Token],
    parseOperand: List[Token] => Option[ParseResult[Expr]],
    operators: Set[String]
  ): Option[ParseResult[Expr]] =
    parseOperand(tokens).flatMap { case ParseResult(left, rest) =>
      rest match
        case Token.Op(op) :: rest2 if operators.contains(op) =>
          parseBinOpIterativeRight(rest2, parseOperand, operators).map { case ParseResult(right, rest3) =>
            ParseResult(Expr.BinOp(op, left, right), rest3)
          }
        case _ => Some(ParseResult(left, rest))
    }

  private def parseOr(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinOpIterative(tokens, parseAnd, Set("or"))
  
  private def parseAnd(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinOpIterative(tokens, parseEquality, Set("and"))
  
  private def parseEquality(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinOpIterative(tokens, parseMultiplicative, Set("="))
  
  private def parseMultiplicative(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinOpIterative(tokens, parseAdditive, Set("*", "/"))
  
  private def parseAdditive(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinOpIterative(tokens, parseComparison, Set("+", "-"))
  
  private def parseComparison(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseBinOpIterative(tokens, parseApply, Set("==", "!=", "<", ">", "<=", ">="))
  
  private def parseApply(tokens: List[Token]): Option[ParseResult[Expr]] =
    parseLambda(tokens).map { case ParseResult(f, rest) =>
      def isArg(t: Token): Boolean = t match
        case Token.Number(_) | Token.Ident(_) | Token.Delim("(" | "{") => 
          t match
            case Token.Ident(name) => !Set("and", "or", "=", "syntax", "λ").contains(name)
            case _ => true
        case Token.Op(_) => false
        case _ => false

      def loop(currentF: Expr, toks: List[Token]): ParseResult[Expr] =
        toks match
          case t :: _ if isArg(t) =>
            parseLambda(toks) match
              case Some(ParseResult(arg, rest2)) =>
                loop(Expr.Call(currentF, List(arg)), rest2)
              case None =>
                ParseResult(currentF, toks)
          case _ =>
            ParseResult(currentF, toks)
      
      loop(f, rest)
    }

  /** ラムダ式または通常の式 */
  private def parseLambda(tokens: List[Token]): Option[ParseResult[Expr]] =
    tokens match
      case Token.Delim("\\") :: Token.Ident(param) :: Token.Op("->") :: rest =>
        parseExpr(rest).map { case ParseResult(body, rest2) =>
          ParseResult(Expr.Lambda(param, body), rest2)
        }
      case _ =>
        parseCall(tokens)
  
  private def parseCall(tokens: List[Token]): Option[ParseResult[Expr]] =
    parsePrimary(tokens)

  private def parsePrimary(tokens: List[Token]): Option[ParseResult[Expr]] =
    tokens match
      case Token.Number(n) :: rest => 
        Some(ParseResult(Expr.Num(n), rest))
      
      case Token.Ident(name) :: rest => 
        Some(ParseResult(Expr.Var(name), rest))
      
      case Token.Delim("{") :: rest => 
        parseBlock(rest)
      
      case Token.Delim("(") :: rest =>
        parseExpr(rest).flatMap { case ParseResult(expr, rest2) =>
          rest2 match
            case Token.Delim(")") :: rest3 => 
              Some(ParseResult(expr, rest3))
            case _ => None
        }
      
      case _ => None
  
  private def parseBlock(tokens: List[Token]): Option[ParseResult[Expr]] =
    def loop(toks: List[Token], acc: List[Stmt]): Option[ParseResult[List[Stmt]]] =
      toks match
        case Token.Delim("}") :: _ => Some(ParseResult(acc.reverse, toks))
        case _ =>
          parseStatement(toks) match
            case Some(ParseResult(stmt, rest)) =>
              rest match
                case Token.Delim(";") :: rest2 => loop(rest2, stmt :: acc)
                case _ => loop(rest, stmt :: acc)
            case None => Some(ParseResult(acc.reverse, toks))
    
    loop(tokens, Nil).flatMap { case ParseResult(stmts, rest) =>
      rest match
        case Token.Delim("}") :: rest2 => 
          Some(ParseResult(Expr.Block(stmts), rest2))
        case _ => None
    }
  
  private def parseArgs(tokens: List[Token]): Option[ParseResult[List[Expr]]] =
    tokens match
      case Token.Delim(")") :: rest => Some(ParseResult(Nil, rest))
      case _ =>
        def loop(toks: List[Token], acc: List[Expr]): Option[ParseResult[List[Expr]]] =
          parseExpr(toks).flatMap { case ParseResult(expr, rest) =>
            rest match
              case Token.Delim(",") :: rest2 => loop(rest2, expr :: acc)
              case Token.Delim(")") :: rest2 => 
                Some(ParseResult((expr :: acc).reverse, rest2))
              case _ => None
          }
        loop(tokens, Nil)
  
  private def parseIdent(tokens: List[Token]): Option[ParseResult[String]] =
    tokens match
      case Token.Ident(name) :: rest => Some(ParseResult(name, rest))
      case _ => None
