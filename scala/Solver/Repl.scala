// ==========================================
// Repl.scala
// インタラクティブ・タクティクス・シェル (修正版)
// ==========================================

package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger
import scala.io.StdIn

object Repl:
  private var history: List[ProofState] = Nil
  private var loadedLemmas: List[CatRule] = Nil
  private var sessionLemmas: List[CatRule] = Nil

  def main(args: Array[String]): Unit =
    println("=== Romanesco Interactive Tactic Shell ===")
    println("Type 'help' for commands, 'exit' to quit.")
    println()

    loop()

  def loop(): Unit =
    var state: Option[ProofState] = None
    
    while true do
      state match
        case None =>
          print("Start Goal > ")
          val input = StdIn.readLine()
          if input == null || input == "exit" then return
          else if input.trim.isEmpty then ()
          else if input == "help" then showGeneralHelp()
          else
            try
              val expr = TestParser.parse(input)
              val s = ProofState(List(Goal(Nil, Nil, expr)), Nil, expr)
              state = Some(s)
              history = List(s)
              println(s"\nInitial Goal: $expr")
              showCurrentState(s)
            catch case e: Exception => println(s"Error: ${e.getMessage}")

        case Some(s) if s.isSolved =>
          println("\n🎉 Goal solved successfully!")
          if (s.completedProofs.nonEmpty) {
            println("\nFinal Proof Tree:")
            // サブゴールが複数ある場合、正しい順序で表示するのは複雑なので最新のものを表示
            println(s.completedProofs.head.format(0))
          }
          println("\nExiting to main loop.")
          state = None

        case Some(s) =>
          print(s"Tactic [${s.goals.length} subgoals] > ")
          val input = StdIn.readLine()
          if input == null || input == "exit" then return
          else if input == "abort" then state = None
          else if input == "undo" then
            if history.size > 1 then
              history = history.tail
              val prevState = history.head
              state = Some(prevState)
              showCurrentState(prevState)
            else
              println("Already at the initial state.")
          else if input == "help" then showTacticHelp()
          else
            handleTactic(input, s) match
              case Left(err) => println(s"Error: $err")
              case Right(newState) =>
                state = Some(newState)
                history = newState :: history
                showCurrentState(newState)

  def showCurrentState(s: ProofState): Unit =
    s.currentGoal match
      case Some(g) =>
        println("\n" + g.toString)
      case None =>
        println("\nNo active goals.")

  def handleTactic(input: String, s: ProofState): Either[String, ProofState] =
    val parts = input.trim.split("\\s+").toList
    parts match
      case "intro" :: Nil => Tactics.intro(s, None)
      case "intro" :: name :: Nil => Tactics.intro(s, Some(name))
      case "induction" :: Nil => Tactics.induction(s, None)
      case "induction" :: name :: Nil => Tactics.induction(s, Some(name))
      case "destruct" :: name :: Nil => Tactics.destruct(s, name)
      case "rewrite" :: name :: Nil => Tactics.rewrite(s, name)
      case "reflexivity" :: Nil => Tactics.reflexivity(s)
      case "split" :: Nil => Tactics.split(s)
      case "apply" :: name :: Nil => Tactics.applyHyp(s, name)
      case "exact" :: name :: Nil => Tactics.exact(s, name)
      case "assumption" :: Nil => Tactics.assumption(s)
      case "auto" :: Nil => 
        val prover = new Prover(ProverConfig(rules = StandardRules.all ++ loadedLemmas ++ sessionLemmas))
        s.currentGoal match {
          case Some(g) => 
            prover.prove(g.target) match {
              case Right(result) => 
                println("✓ Auto solved the goal!")
                result.generatedLemma.foreach { lemma =>
                   println(s"  Generated Lemma: $lemma")
                   sessionLemmas = sessionLemmas :+ lemma
                }
                Right(s.copy(goals = s.goals.tail, completedProofs = result.tree :: s.completedProofs))
              case Left(fail) => 
                println(s"✗ Auto failed: ${fail.reason}")
                Left("Auto could not solve the goal.")
            }
          case None => Left("No active goal.")
        }
      case "save" :: filename :: Nil =>
        LemmaManager.saveLemmas(filename, sessionLemmas)
        println(s"Saved ${sessionLemmas.size} session lemmas to $filename")
        Right(s)
      case "load" :: filename :: Nil =>
        val newLemmas = LemmaManager.loadLemmas(filename)
        loadedLemmas = loadedLemmas ++ newLemmas
        println(s"Loaded ${newLemmas.size} lemmas from $filename")
        Right(s)
      case "lemmas" :: Nil =>
        println("\n--- Loaded Lemmas ---")
        loadedLemmas.foreach(println)
        println("\n--- Session Lemmas ---")
        sessionLemmas.foreach(println)
        Right(s)
      case _ => Left(s"Unknown tactic or invalid arguments: $input")

  def showGeneralHelp(): Unit =
    println("""
General Commands:
  <expression>    - Start a new proof with the given goal
  help            - Show this help
  exit            - Exit the shell
    """)

  def showTacticHelp(): Unit =
    println("""
Tactics:
  intro [name]    - Introduction rule (forall / implies)
  induction [var] - Apply induction on a variable
  destruct <hyp>  - Decompose a hypothesis (exists / and / or)
  rewrite <hyp>   - Rewrite using an equality hypothesis (bidirectional)
  reflexivity     - Solve a goal of the form a = a
  split           - Split a product/conjunction goal into subgoals
  apply <hyp>     - Apply an implication or universal hypothesis
  exact <hyp>     - Solve goal by exact match with hypothesis
  assumption      - Try to solve goal using any hypothesis
  auto            - Automatically prove the current subgoal
  lemmas          - List loaded and session lemmas
  save <file>     - Save session lemmas to file
  load <file>     - Load lemmas from file
  undo            - Undo the last tactic
  abort           - Abort the current proof
    """)

@main def runRepl = Repl.main(Array())