// ==========================================
// TestParser.scala
// テスト用の簡易パーサー
// ==========================================

package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._

object TestParser:
  import ExprBuilder._

  def parse(input: String): Expr =
    val tokens = tokenize(input)
    parseExpr(tokens)._1

  private def tokenize(s: String): List[String] =
    s.replaceAll("([→∧∨∀∃=().⇒⊃×⊥⊤01])", " $1 ")
      .split("\\s+")
      .filter(_.nonEmpty)
      .toList

  private def parseExpr(tokens: List[String]): (Expr, List[String]) =
    parseImplication(tokens)

  private def parseImplication(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseOr(tokens)
    rest1 match
      case ("→" | "⇒" | "⊃") :: rest2 =>
        val (right, rest3) = parseImplication(rest2)
        (left → right, rest3)
      case _ => (left, rest1)

  private def parseOr(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseAnd(tokens)
    rest1 match
      case "∨" :: rest2 =>
        val (right, rest3) = parseOr(rest2)
        (left ∨ right, rest3)
      case _ => (left, rest1)

  private def parseAnd(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseQuantifier(tokens)
    rest1 match
      case ("∧" | "×") :: rest2 =>
        val (right, rest3) = parseAnd(rest2)
        (left ∧ right, rest3)
      case _ => (left, rest1)

  private def parseQuantifier(tokens: List[String]): (Expr, List[String]) =
    tokens match
      case "∀" :: varName :: "." :: rest =>
        val (body, rest2) = parseExpr(rest)
        (sym("∀")(v(varName), body), rest2)
      case "∃" :: varName :: "." :: rest =>
        val (body, rest2) = parseExpr(rest)
        (sym("∃")(v(varName), body), rest2)
      case _ => parseEquality(tokens)

  private def parseEquality(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseAtom(tokens)
    rest1 match
      case "=" :: rest2 =>
        val (right, rest3) = parseAtom(rest2)
        (left === right, rest3)
      case _ => (left, rest1)

  private def parseAtom(tokens: List[String]): (Expr, List[String]) =
    tokens match
      case "(" :: rest =>
        val (expr, rest2) = parseExpr(rest)
        rest2 match
          case ")" :: rest3 => (expr, rest3)
          case _            => throw new Exception("Unmatched parenthesis")
      case ("⊤" | "1") :: rest => (⊤, rest)
      case ("⊥" | "0") :: rest => (⊥, rest)
      case name :: "(" :: rest =>
        val (args, rest2) = parseArgs(rest)
        (sym(name)(args*), rest2)
      case name :: rest => (sym(name), rest)
      case Nil          => throw new Exception("Unexpected end of input")

  private def parseArgs(tokens: List[String]): (List[Expr], List[String]) =
    tokens match
      case ")" :: rest => (Nil, rest)
      case _           =>
        val (arg, rest1) = parseAtom(tokens)
        rest1 match
          case "," :: rest2 =>
            val (args, rest3) = parseArgs(rest2)
            (arg :: args, rest3)
          case ")" :: rest2 => (List(arg), rest2)
          case _            => throw new Exception("Expected ',' or ')'")
