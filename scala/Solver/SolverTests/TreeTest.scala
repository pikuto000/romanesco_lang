package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger

@main def testTreeInduction = {
  logger.switch(false)
  logger.setMaxDepth(20)
  println("=== Tree Induction Test ===")

  // Rules for Tree reverse
  // reverse(leaf) = leaf
  // reverse(node(l, v, r)) = node(reverse(r), v, reverse(l))

  // Use TestParser with known variables
  val vars = Set("l", "v", "r")

  val treeRules = List(
    CatRule(
      "reverse_leaf",
      TestParser.parse("reverse(leaf)", vars),
      TestParser.parse("leaf", vars)
    ),
    CatRule(
      "reverse_node",
      TestParser.parse("reverse(node(l, v, r))", vars),
      TestParser.parse("node(reverse(r), v, reverse(l))", vars)
    )
  )

  val allRules = StandardRules.all ++ treeRules

  // Goal: ∀t. reverse(reverse(t)) = t
  val goalStr = "∀t. reverse(reverse(t)) = t"
  println(s"Goal: $goalStr")
  val goal = TestParser.parse(goalStr)

  val config = ProverConfig(classical = false, rules = allRules)
  val prover = new Prover(config)

  prover.prove(goal, rules = allRules) match {
    case Right(res) =>
      println("✓ Proof found:")
      println(res.tree.format(1))
    case Left(trace) =>
      println("✗ No proof found. Failure trace:")
      println(trace.format())
  }
}
