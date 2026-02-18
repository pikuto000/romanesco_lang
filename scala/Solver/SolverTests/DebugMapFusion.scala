package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

@main def debugMapFusion2(): Unit = {
  logger.switch(true)
  logger.setMaxDepth(12)

  println("=== Debug: Map Fusion - What plugins fire at depth 5/5? ===")

  val map  = Sym("map")
  val nil  = Sym("nil")
  val cons = Sym("cons")
  val comp = Sym(Compose)

  val f  = Var("f");  val g  = Var("g")
  val x  = Var("x");  val xs = Var("xs")

  val map_nil = CatRule("map_nil", App(map, List(f, nil)), nil)
  val map_cons = CatRule(
    "map_cons",
    App(map, List(f, App(cons, List(x, xs)))),
    App(cons, List(App(f, List(x)), App(map, List(f, xs))))
  )
  val compose_app = CatRule(
    "compose_app",
    App(App(comp, List(f, g)), List(x)),
    App(f, List(App(g, List(x))))
  )

  val mapRules = List(map_nil, map_cons, compose_app)
  val allRules = StandardRules.all ++ mapRules

  val config = ProverConfig(classical = false, rules = allRules, algebras = StandardRules.defaultAlgebras)
  val prover = new Prover(config)

  val goalStr = "∀f. ∀g. ∀xs. map(f, map(g, xs)) = map(f ∘ g, xs)"
  val goal = TestParser.parse(goalStr)

  // Only run limit=4 to see the specific issue
  prover.prove(goal, rules = allRules, maxDepth = 4, timeoutMs = 30000) match {
    case Right(t) =>
      println("PROOF FOUND:")
      println(t.tree.format(1))
    case Left(trace) =>
      println("NO PROOF FOUND")
  }
}
