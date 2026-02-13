package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.Unifier._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object UnificationAdvancedTest {
  def main(args: Array[String]): Unit = {
    logger.switch(true)

    println("=== Advanced Unification Test ===")

    // Test 1: Duplicate arguments (Miller pattern violation)
    // ?P(x, x) = f(x)
    // Should ideally find λv0.λv1. f(v0) or λv0.λv1. f(v1)
    test(
      "Duplicate Args",
      App(Meta(MetaId(1)), List(Var("x"), Var("x"))),
      App(Sym("f"), List(Var("x")))
    )

    // Test 2: Partial dependency
    // ?P(x, y) = f(x)
    // Should find λv0.λv1. f(v0)
    test(
      "Partial Dependency",
      App(Meta(MetaId(2)), List(Var("x"), Var("y"))),
      App(Sym("f"), List(Var("x")))
    )

    // Test 3: Structured arguments
    // ?P(S(x)) = S(S(x))
    // Should find λv0. S(v0)
    test(
      "Structured Args",
      App(Meta(MetaId(3)), List(App(Sym("S"), List(Var("x"))))),
      App(Sym("S"), List(App(Sym("S"), List(Var("x")))))
    )

        // Test 4: Nested Meta-applications

        // ?P(x) = f(?Q(x))

        // This is a flex-rigid case, should solve ?P -> λv0. f(?Q(v0))

        test("Nested Meta",

          App(Meta(MetaId(4)), List(Var("x"))),

          App(Sym("f"), List(App(Meta(MetaId(5)), List(Var("x"))))))

    

            // Test 5: Non-deterministic abstraction

    

            // ?P(x, x) = f(x, x)

    

            // Multiple valid abstractions exist.

    

            test("Non-deterministic Abstraction",

    

              App(Meta(MetaId(6)), List(Var("x"), Var("x"))),

    

              App(Sym("f"), List(Var("x"), Var("x"))))

    

        

    

            // Test 6: Pruning (Meta-variable dependency reduction)

    

            // ?P(x) = f(?Q(x, y))

    

            // Should succeed by pruning ?Q to not depend on y.

    

            test("Pruning",

    

              App(Meta(MetaId(7)), List(Var("x"))),

    

              App(Sym("f"), List(App(Meta(MetaId(8)), List(Var("x"), Var("y"))))))

    

          }

    

        

    

  def test(name: String, e1: Expr, e2: Expr): Unit = {
    println(s"[Case: $name]")
    println(s"Unifying: $e1 = $e2")
    val results = unify(e1, e2, emptySubst)
    if (results.isEmpty) {
      println("✗ FAIL: No solutions found")
    } else {
      results.zipWithIndex.foreach { case (s, i) =>
        println(s"✓ Solution $i: $s")
        val applied = applySubst(e1, s)
        println(s"  Check: $applied == ${applySubst(e2, s)}")
      }
    }
  }
}
