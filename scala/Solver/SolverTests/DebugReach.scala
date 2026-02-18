package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

@main def debugReach(): Unit = {
  logger.switch(true)
  logger.setMaxDepth(12)

  val graphRules = List(
    CatRule(
      "reach_refl",
      App(Sym("reachable"), List(Var("x"), Var("x"))),
      Sym(True),
      List(Var("x"))
    ),
    CatRule(
      "reach_edge",
      App(Sym("reachable"), List(Var("x"), Var("y"))),
      App(Sym("edge"), List(Var("x"), Var("y"))),
      List(Var("x"), Var("y"))
    ),
    CatRule(
      "reach_trans",
      App(Sym("reachable"), List(Var("x"), Var("z"))),
      App(
        Sym(And),
        List(
          App(Sym("reachable"), List(Var("x"), Var("y"))),
          App(Sym("reachable"), List(Var("y"), Var("z")))
        )
      ),
      List(Var("x"), Var("y"), Var("z"))
    )
  )

  val allRules = graphRules ++ StandardRules.all
  val config = ProverConfig(rules = allRules, algebras = Nil, maxComplexity = 200)
  val prover = new Prover(config)

  // Test normalization
  val e = App(Sym("reachable"), List(Var("x_0"), Var("y_1")))
  val norm = Rewriter.normalize(e, allRules)
  println(s"normalize(reachable(x_0, y_1)) = $norm")

  val goalStr = "∀x. ∀y. edge(x, y) → reachable(x, y)"
  val goal = TestParser.parse(goalStr)

  prover.prove(goal, maxDepth = 10, timeoutMs = 30000) match {
    case Right(t) =>
      println("PROOF FOUND:")
      println(t.tree.format(1))
    case Left(trace) =>
      println("NO PROOF FOUND")
      println(trace.format())
  }
}
