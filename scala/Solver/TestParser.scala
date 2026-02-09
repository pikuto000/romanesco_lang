// ==========================================
// TestParser.scala
// テスト用の簡易パーサー（数字の自動変換を抑制）
// ==========================================

package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Solver.core.LogicSymbols._
import scala.collection.mutable

object TestParser:
  import ExprBuilder._

  def parse(input: String): Expr =
    val tokens = tokenize(input)
    parseExpr(tokens)._1

  private def tokenize(s: String): List[String] =
    val symbols = Set('→', '∧', '∨', '∀', '∃', '=', '(', ')', '.', ',', '⇒', '⊃', '×', '⊥', '⊤', '∘')
    
    val sb = new StringBuilder
    val tokens = mutable.ListBuffer.empty[String]
    
    def flush(): Unit = if (sb.nonEmpty) { tokens += sb.toString; sb.clear() }

    for (c <- s) {
      if (symbols.contains(c)) {
        flush(); tokens += c.toString
      } else if (c.isWhitespace) {
        flush()
      } else {
        sb.append(c)
      }
    }
    flush()
    tokens.toList

  private def parseExpr(tokens: List[String]): (Expr, List[String]) =
    parseImplication(tokens)

  private def parseImplication(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseOr(tokens)
    rest1 match
      case (Implies | ImpliesAlt1 | ImpliesAlt2) :: rest2 =>
        val (right, rest3) = parseImplication(rest2)
        (left → right, rest3)
      case _ => (left, rest1)

  private def parseOr(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseAnd(tokens)
    rest1 match
      case Or :: rest2 =>
        val (right, rest3) = parseOr(rest2)
        (left ∨ right, rest3)
      case _ => (left, rest1)

  private def parseAnd(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseEquality(tokens)
    rest1 match
      case (And | Product | "×") :: rest2 =>
        val (right, rest3) = parseAnd(rest2)
        (left ∧ right, rest3)
      case _ => (left, rest1)

  private def parseEquality(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseComposition(tokens)
    rest1 match
      case Eq :: rest2 =>
        val (right, rest3) = parseComposition(rest2)
        (left === right, rest3)
      case _ => (left, rest1)

  private def parseComposition(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseQuantifier(tokens)
    rest1 match
      case Compose :: rest2 =>
        val (right, rest3) = parseComposition(rest2)
        (sym(Compose)(left, right), rest3)
      case _ => (left, rest1)

  private def parseQuantifier(tokens: List[String]): (Expr, List[String]) =
    tokens match
      case Forall :: varName :: "." :: rest =>
        val (body, rest2) = parseExpr(rest)
        (sym(Forall)(v(varName), body), rest2)
      case Exists :: varName :: "." :: rest =>
        val (body, rest2) = parseExpr(rest)
        (sym(Exists)(v(varName), body), rest2)
      case _ => parseAtom(tokens)

  private def parseAtom(tokens: List[String]): (Expr, List[String]) =
    tokens match
      case "(" :: rest =>
        val (expr, rest2) = parseExpr(rest)
        rest2 match
          case ")" :: rest3 => (expr, rest3)
          case _            => throw new Exception(s"Unmatched parenthesis at ${rest2.take(5).mkString(" ")}")
      case (True | "⊤") :: rest => (sym(True), rest)
      case (False | "⊥") :: rest => (sym(False), rest)
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
          case _            => throw new Exception(s"Expected ',' or ')' but found '${rest1.headOption.getOrElse("EOF")}'")
