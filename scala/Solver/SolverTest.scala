// ==========================================
// Main.scala
// CLI・REPL
// ==========================================

package romanesco

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import scala.io.StdIn
import romanesco.Utils.Debug.logger

@main def runProver(args: String*): Unit =
  println("=== Romanesco Prover v3.0 (Category Theory Edition) ===")
  println("Core: 5 types (Expr: 4 + ProofStep: 1)")
  println("Type 'help' for commands, 'exit' to quit")
  println()

  if args.nonEmpty then
    // ファイルから読み込み
    val source = scala.io.Source.fromFile(args.head)
    try
      for
        line <- source.getLines if line.trim.nonEmpty && !line.startsWith("//")
      do processInput(line)
    finally
      source.close
  else
    // REPL
    while true do
      print("> ")
      val input = StdIn.readLine
      if input == null || input == "exit" then return
      else if input == "help" then showHelp
      else if input == "enableDebug" then logger.switch(true)
      else if input == "disableDebug" then logger.switch(false)
      else if input.trim.nonEmpty then processInput(input)

def showHelp: Unit =
  println("""
Commands:
  <expr>          - Prove the expression
  help            - Show this help
  exit            - Exit the prover

Syntax:
  A ∧ B          - Conjunction (and)
  A ∨ B          - Disjunction (or)
  A → B          - Implication
  ∀x. P          - Universal quantification
  ∃x. P          - Existential quantification
  a = b          - Equality
  ⊤              - True
  ⊥              - False

Examples:
  A → A
  A ∧ B → B ∧ A
  ∀x. P(x) → ∃x. P(x)
  """)

def processInput(input: String): Unit =
  try
    // 簡易パーサー（TODO: romanescoのパーサーで置き換え）
    val expr = SimpleParser.parse(input)
    println(s"Goal: $expr")

    Prover.prove(expr) match
      case Some(proof) =>
        println(s"✓ Proof found (${proof.length} steps):")
        proof.zipWithIndex.foreach { case (step, i) =>
          println(s"  ${i + 1}. $step")
        }
      case None =>
        println("✗ No proof found")
  catch case e: Exception => println(s"Error: ${e.getMessage}")

// 簡易パーサー（TODO: 後でromanescoパーサーに置き換え）
object SimpleParser:
  import ExprBuilder._

  def parse(input: String): Expr =
    val tokens = tokenize(input)
    parseExpr(tokens)._1

  private def tokenize(s: String): List[String] =
    s.replaceAll("([→∧∨∀∃=().])", " $1 ")
      .split("\\s+")
      .filter(_.nonEmpty)
      .toList

  private def parseExpr(tokens: List[String]): (Expr, List[String]) =
    parseImplication(tokens)

  private def parseImplication(tokens: List[String]): (Expr, List[String]) =
    val (left, rest1) = parseOr(tokens)
    rest1 match
      case "→" :: rest2 =>
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
      case "∧" :: rest2 =>
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
      case "⊤" :: rest         => (⊤, rest)
      case "⊥" :: rest         => (⊥, rest)
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

@main def testSomeCases = {
  logger.switch(false) // 大量のログで見づらくなるのを防ぐためデフォルトはオフ

  val cases = List(
    "A → A",
    "A ∧ B → B ∧ A",
    "A ∨ B → B ∨ A",
    "(A ∧ B → C) → (A → (B → C))",
    "(A → (B → C)) → (A ∧ B → C)",
    "(A → B) ∧ (B → C) → (A → C)",
    "(A → B) ∧ A → B",
    "∀x. P(x) → ∃x. P(x)",
    "∀x. (P(x) ∧ Q(x)) → (∀x. P(x)) ∧ (∀x. Q(x))",
    "((∀x. P(x)) ∨ (∀x. Q(x))) → ∀x. (P(x) ∨ Q(x))",
    "a = b → b = a",
    "a = b ∧ b = c → a = c",
    "⊤",
    "A ∧ ⊥ → B",
    "A → (A → B) → B"
  )

  cases.foreach { input =>
    println(s"\n[Test Case] $input")
    try {
      val expr = SimpleParser.parse(input)
      val result = romanesco.Utils.times.watch {
        Prover.prove(expr)
      }
      result match {
        case Some(proof) =>
          println(s"✓ Solved in ${proof.length} steps")
          proof.zipWithIndex.foreach { case (step, i) =>
          // println(s"  ${i + 1}. $step")
          }
        case None =>
          println("✗ Failed to prove")
      }
    } catch {
      case e: Exception =>
        println(s"Error parsing/proving '$input': ${e.getMessage}")
    }
  }
}
