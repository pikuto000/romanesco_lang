package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger
import romanesco.Solver.core.LogicSymbols._

@main def testMapFusion = {
  logger.switch(false)
  println("=== Map Fusion Test ===")

  // Set up variables for parsing
  val vars = Set("f", "g", "x", "xs")

  val map = Expr.Sym("map")
  val nil = Expr.Sym("nil")
  val cons = Expr.Sym("cons")
  val comp = Expr.Sym(Compose)

  val f = Expr.Var("f")
  val g = Expr.Var("g")
  val x = Expr.Var("x")
  val xs = Expr.Var("xs")

  // map f nil = nil
  val map_nil = CatRule("map_nil", Expr.App(map, List(f, nil)), nil)

  // map f (cons(x, xs)) = cons(f x, map f xs)
  // Note: f x is Expr.App(f, List(x))
  val map_cons = CatRule(
    "map_cons",
    Expr.App(map, List(f, Expr.App(cons, List(x, xs)))),
    Expr.App(cons, List(Expr.App(f, List(x)), Expr.App(map, List(f, xs))))
  )

  // (f ∘ g) x = f (g x)
  val compose_app = CatRule(
    "compose_app",
    Expr.App(Expr.App(comp, List(f, g)), List(x)),
    Expr.App(f, List(Expr.App(g, List(x))))
  )

  val mapRules = List(map_nil, map_cons, compose_app)
  val allRules = StandardRules.all ++ mapRules

  // Goal: ∀f. ∀g. ∀xs. map f (map g xs) = map (f ∘ g) xs
  // TestParser should handle this with variables
  val goalStr = "∀f. ∀g. ∀xs. map(f, map(g, xs)) = map(f ∘ g, xs)"
  println(s"Goal: $goalStr")
  val goal = TestParser.parse(goalStr)
  val config = ProverConfig(classical = false, rules = allRules)
  val prover = new Prover(config)

  prover.prove(goal, rules = allRules) match {
    case Right(t) =>
      println("✓ Proof found:")
      println(t.tree.format(1))
    case Left(trace) =>
      println("✗ No proof found. Failure trace:")
      println(trace.format())
  }
}
