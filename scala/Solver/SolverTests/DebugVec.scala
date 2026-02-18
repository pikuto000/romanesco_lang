package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

@main def debugVecMap(): Unit = {
  logger.switch(true)
  logger.setMaxDepth(15)

  val vecAlgebra = InitialAlgebra(
    name = "Vec",
    varPrefix = "v",
    constructors = List(
      ConstructorDef("vnil", Nil, ConstructorType.Point),
      ConstructorDef(
        "vcons",
        List(ArgType.Constant, ArgType.Recursive, ArgType.Constant),
        ConstructorType.Point
      )
    )
  )

  val vecRules = List(
    CatRule(
      "vmap_nil",
      App(Sym("vmap"), List(Var("f"), Sym("vnil"))),
      Sym("vnil"),
      List(Var("f"))
    ),
    CatRule(
      "vmap_cons",
      App(
        Sym("vmap"),
        List(Var("f"), App(Sym("vcons"), List(Var("x"), Var("xs"), Var("n"))))
      ),
      App(
        Sym("vcons"),
        List(
          App(Var("f"), List(Var("x"))),
          App(Sym("vmap"), List(Var("f"), Var("xs"))),
          Var("n")
        )
      ),
      List(Var("f"), Var("x"), Var("xs"), Var("n"))
    ),
    CatRule(
      "id_rule",
      App(Sym("id"), List(Var("x"))),
      Var("x"),
      List(Var("x"))
    )
  )

  val allRules = vecRules ++ StandardRules.natPlusRules ++ StandardRules.all
  val config = ProverConfig(
    rules = allRules,
    algebras = List(vecAlgebra, StandardRules.natAlgebra),
    maxComplexity = 300
  )
  val prover = new Prover(config)

  // Test normalization
  val e1 = App(Sym("vmap"), List(Sym("id"), Sym("vnil")))
  println(s"normalize(vmap(id, vnil)) = ${Rewriter.normalize(e1, allRules)}")

  val e2 = App(Sym("vmap"), List(Sym("id"), App(Sym("vcons"), List(Var("a_0"), Var("v_1"), Var("a_2")))))
  println(s"normalize(vmap(id, vcons(a_0, v_1, a_2))) = ${Rewriter.normalize(e2, allRules)}")

  val e3 = App(Sym("id"), List(Var("a_0")))
  println(s"normalize(id(a_0)) = ${Rewriter.normalize(e3, allRules)}")

  // Simple test: without the extra ∀n
  println("\n=== Simple: ∀v:Vec. vmap(id, v) = v ===")
  val goal1 = TestParser.parse("∀v:Vec. vmap(id, v) = v")
  prover.prove(goal1, maxDepth = 15, timeoutMs = 30000) match {
    case Right(t) =>
      println("PROOF FOUND")
      println(t.tree.format(1))
    case Left(trace) =>
      println("NO PROOF: " + trace.reason)
  }
}

@main def debugVecMap2(): Unit = {
  import romanesco.Solver.core._
  import romanesco.Solver.core.Expr._
  import romanesco.Solver.core.LogicSymbols._
  import romanesco.Solver.TestParser
  import romanesco.Utils.Debug.logger

  logger.switch(false)

  val vecAlgebra = InitialAlgebra(
    name = "Vec",
    varPrefix = "v",
    constructors = List(
      ConstructorDef("vnil", Nil, ConstructorType.Point),
      ConstructorDef(
        "vcons",
        List(ArgType.Constant, ArgType.Recursive, ArgType.Constant),
        ConstructorType.Point
      )
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

  println("=== Test with extra forall n: ∀v:Vec. ∀n. vmap(id, v) = v ===")
  val goal2 = TestParser.parse("∀v:Vec. ∀n. vmap(id, v) = v")
  prover.prove(goal2, maxDepth = 25, timeoutMs = 30000) match {
    case Right(t) => println("PROOF FOUND"); println(t.tree.format(1))
    case Left(trace) => println("NO PROOF: " + trace.reason)
  }

  println("\n=== Test vlength: ∀v. ∀w. vlength(vappend(v, w)) = plus(vlength(v), vlength(w)) ===")
  val goal3 = TestParser.parse("∀v. ∀w. vlength(vappend(v, w)) = plus(vlength(v), vlength(w))")
  prover.prove(goal3, maxDepth = 25, timeoutMs = 30000) match {
    case Right(t) => println("PROOF FOUND"); println(t.tree.format(1))
    case Left(trace) => println("NO PROOF: " + trace.reason)
  }
}
