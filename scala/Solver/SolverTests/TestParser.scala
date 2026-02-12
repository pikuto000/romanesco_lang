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

  def parse(input: String, variables: Set[String] = Set.empty): Expr =
    val tokens = tokenize(input)
    parseExpr(tokens, variables)._1

  private def tokenize(s: String): List[String] =
    val symbols = Set('→', '∧', '∨', '∀', '∃', '=', '(', ')', '.', ',', ':', '⇒', '⊃', '×', '⊥', '⊤', '∘', '□', '◇', 'K', 'O', '⊸', '!', '?', '⊗', '⊕', '&', 'G', 'F', 'X', 'U', '*', '↦', 'λ', '¬')
    
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

  private def parseExpr(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    parseImplication(tokens, vars)

  private def parseImplication(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseLinearImplication(tokens, vars)
    rest1 match
      case (Implies | ImpliesAlt1 | ImpliesAlt2) :: rest2 =>
        val (right, rest3) = parseImplication(rest2, vars)
        (left → right, rest3)
      case _ => (left, rest1)

  private def parseLinearImplication(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseUntil(tokens, vars)
    rest1 match
      case LImplies :: rest2 =>
        val (right, rest3) = parseLinearImplication(rest2, vars)
        (sym(LImplies)(left, right), rest3)
      case _ => (left, rest1)

  private def parseUntil(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseOr(tokens, vars)
    rest1 match
      case Until :: rest2 =>
        val (right, rest3) = parseUntil(rest2, vars)
        (sym(Until)(left, right), rest3)
      case _ => (left, rest1)

  private def parseOr(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseLPlus(tokens, vars)
    rest1 match
      case Or :: rest2 =>
        val (right, rest3) = parseOr(rest2, vars)
        (left ∨ right, rest3)
      case _ => (left, rest1)

  private def parseLPlus(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseAnd(tokens, vars)
    rest1 match
      case LPlus :: rest2 =>
        val (right, rest3) = parseLPlus(rest2, vars)
        (sym(LPlus)(left, right), rest3)
      case _ => (left, rest1)

  private def parseAnd(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseSepAnd(tokens, vars)
    rest1 match
      case (And | Product | "×" | LWith) :: rest2 =>
        val op = rest1.head
        val (right, rest3) = parseAnd(rest2, vars)
        (sym(op)(left, right), rest3)
      case _ => (left, rest1)

  private def parseSepAnd(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseTensor(tokens, vars)
    rest1 match
      case SepAnd :: rest2 =>
        val (right, rest3) = parseSepAnd(rest2, vars)
        (sym(SepAnd)(left, right), rest3)
      case _ => (left, rest1)

  private def parseTensor(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseEquality(tokens, vars)
    rest1 match
      case Tensor :: rest2 =>
        val (right, rest3) = parseTensor(rest2, vars)
        (sym(Tensor)(left, right), rest3)
      case _ => (left, rest1)

  private def parseEquality(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parsePointsTo(tokens, vars)
    rest1 match
      case Eq :: rest2 =>
        val (right, rest3) = parseEquality(rest2, vars)
        (left === right, rest3)
      case _ => (left, rest1)

  private def parsePointsTo(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseComposition(tokens, vars)
    rest1 match
      case PointsTo :: rest2 =>
        val (right, rest3) = parsePointsTo(rest2, vars)
        (sym(PointsTo)(left, right), rest3)
      case _ => (left, rest1)

  private def parseComposition(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    val (left, rest1) = parseUnary(tokens, vars)
    rest1 match
      case Compose :: rest2 =>
        val (right, rest3) = parseComposition(rest2, vars)
        (sym(Compose)(left, right), rest3)
      case _ => (left, rest1)

  private def parseUnary(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    tokens match
      case (Box | Diamond | Knowledge | Obligation | Bang | Question | Globally | Finally | Next | Not) :: rest =>
        val op = tokens.head
        val (body, rest2) = parseUnary(rest, vars)
        (sym(op)(body), rest2)
      case _ => parseQuantifier(tokens, vars)

  private def parseQuantifier(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    tokens match
      case Forall :: varName :: ":" :: rest =>
        val (typeExpr, rest2) = parseExpr(rest, vars)
        rest2 match
          case "." :: rest3 =>
            val (body, rest4) = parseExpr(rest3, vars + varName)
            (sym(Forall)(v(varName), typeExpr, body), rest4)
          case _ => throw new Exception(s"Expected '.' after type in Forall at ${rest2.take(5).mkString(" ")}")
      case (Forall | Exists | Lambda) :: varName :: "." :: rest =>
        val op = tokens.head
        val (body, rest2) = parseExpr(rest, vars + varName)
        (sym(op)(v(varName), body), rest2)
      case _ => parseAtom(tokens, vars)

  private def parseAtom(tokens: List[String], vars: Set[String]): (Expr, List[String]) =
    tokens match
      case "(" :: rest =>
        val (expr, rest2) = parseExpr(rest, vars)
        rest2 match
          case ")" :: rest3 => (expr, rest3)
          case _            => throw new Exception(s"Unmatched parenthesis at ${rest2.take(5).mkString(" ")}")
      case (True | "⊤") :: rest => (sym(True), rest)
      case (False | "⊥") :: rest => (sym(False), rest)
      case name :: "(" :: rest =>
        val (args, rest2) = parseArgs(rest, vars)
        val head = if (vars.contains(name)) v(name) else sym(name)
        (head(args*), rest2)
      case name :: rest => 
        val atom = if (vars.contains(name)) v(name) else sym(name)
        (atom, rest)
      case Nil          => throw new Exception("Unexpected end of input")

  private def parseArgs(tokens: List[String], vars: Set[String]): (List[Expr], List[String]) =
    tokens match
      case ")" :: rest => (Nil, rest)
      case _           =>
        val (arg, rest1) = parseExpr(tokens, vars)
        rest1 match
          case "," :: rest2 =>
            val (args, rest3) = parseArgs(rest2, vars)
            (arg :: args, rest3)
          case ")" :: rest2 => (List(arg), rest2)
          case _            => throw new Exception(s"Expected ',' or ')' but found '${rest1.headOption.getOrElse("EOF")}'")
