package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

@main def debugVecMap3(): Unit = {
  logger.switch(true)
  logger.setMaxDepth(8)

  val vecAlgebra = InitialAlgebra(
    name = "Vec",
    varPrefix = "v",
    constructors = List(
      ConstructorDef("vnil", Nil, ConstructorType.Point),
      ConstructorDef("vcons", List(ArgType.Constant, ArgType.Recursive, ArgType.Constant), ConstructorType.Point)
    )
  )

  val vecRules = List(
    CatRule("vmap_nil", App(Sym("vmap"), List(Var("f"), Sym("vnil"))), Sym("vnil"), List(Var("f"))),
    CatRule("vmap_cons",
      App(Sym("vmap"), List(Var("f"), App(Sym("vcons"), List(Var("x"), Var("xs"), Var("n"))))),
      App(Sym("vcons"), List(App(Var("f"), List(Var("x"))), App(Sym("vmap"), List(Var("f"), Var("xs"))), Var("n"))),
      List(Var("f"), Var("x"), Var("xs"), Var("n"))),
    CatRule("id_rule", App(Sym("id"), List(Var("x"))), Var("x"), List(Var("x")))
  )

  val allRules = vecRules ++ StandardRules.natPlusRules ++ StandardRules.all
  val config = ProverConfig(rules = allRules, algebras = List(vecAlgebra, StandardRules.natAlgebra), maxComplexity = 300)
  val prover = new Prover(config)

  println("=== Test: ∀v:Vec. ∀n. vmap(id, v) = v ===")
  val goal = TestParser.parse("∀v:Vec. ∀n. vmap(id, v) = v")
  prover.prove(goal, maxDepth = 20, timeoutMs = 60000) match {
    case Right(t) => println("PROOF FOUND"); println(t.tree.format(1))
    case Left(trace) => println("NO PROOF: " + trace.reason)
  }
}
