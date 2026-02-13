package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object HoTTAdvancedTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.hott ++ StandardRules.all
    val config = ProverConfig(rules = rules, maxComplexity = 300)
    val prover = new Prover(config)

    println("=== HoTT Phase 1: Transport Computation Test ===")

    val testCases = List(
      // 1. Product type transport
      ("transport(λz. ×(A(z), B(z)), p, pair(u, v)) = pair(transport(λz. A(z), p, u), transport(λz. B(z), p, v))", true),
      
      // 2. Coproduct type transport
      ("transport(λz. +(A(z), B(z)), p, inl(u)) = inl(transport(λz. A(z), p, u))", true),
      
      // 3. Function type transport
      ("transport(λz. →(A(z), B(z)), p, f) = λx. transport(λz. B(z), p, f(transport(λz. A(z), inv(p), x)))", true),

      // --- Phase 2: h-levels (isProp, isSet) ---
      
      // 4. Definition of isProp: If A is a Prop, any two elements are equal.
      ("isProp(A) → ∀x:A. ∀y:A. path(A, x, y)", true),
      
      // 5. Prop hierarchy: A Prop is also a Set.
      ("isProp(A) → isSet(A)", true),
      
      // 6. Composition: Product of Props is a Prop.
      ("(isProp(A) ∧ isProp(B)) → isProp(×(A, B))", true),
      
      // 7. K-Axiom (Definition of isSet): If A is a Set, any two paths are equal.
      ("isSet(A) → ∀x:A. ∀y:A. ∀p:path(A, x, y). ∀q:path(A, x, y). path(path(A, x, y), p, q)", true),

      // --- Phase 3: Generalized HITs ---
      
      // 8. Interval Induction: P(0) and P(1) and P(0)=P(1) implies ∀x:Interval. P(x)
      ("(P(zero) ∧ P(one) ∧ path(Type, P(zero), P(one))) → ∀i:Interval. P(i)", true),

      // 9. Suspension Induction: P(north) and P(south) and ∀a. P(north)=P(south) implies ∀s:Susp. P(s)
      ("(P(north) ∧ P(south) ∧ ∀a. path(Type, P(north), P(south))) → ∀s:Susp. P(s)", true),

      // --- Phase 4: Cubical Operations ---
      
      // 10. Comp reduction: comp(A, refl(x), u) = u
      ("comp(A, refl(x), u) = u", true)
    )

    testCases.foreach { case (input, expected) =>
      print(s"Case: $input ... ")
      try {
        val goal = TestParser.parse(input)
        // We use prover.prove to see if the rewriter can reduce both sides to the same thing
        val result = prover.prove(goal, maxDepth = 5)
        result match {
          case Right(_)    => println("✓ OK (Solved)")
          case Left(trace) =>
            if (expected) {
              println(s"✗ FAIL (Should have been solved) - Reason: ${trace.reason}")
            } else println("✓ OK (Failed as expected)")
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }

    println("\n=== Phase 5: Proof Mining Test ===")
    val miningGoal = TestParser.parse("∀n. ∀m. ∀k. plus(n, plus(m, k)) = plus(m, plus(n, k))")
    prover.prove(miningGoal, maxDepth = 20) match {
      case Right(_) => println("✓ OK: Complex arithmetic solved with dynamic lemmas")
      case Left(trace) => println(s"✗ FAIL: Still too hard - Reason: ${trace.reason}")
    }

    println("\n=== Phase 6: HIT DSL Test ===")
    try {
      val circleDSL = "HIT Circle { base, loop: base -> base }"
      val circleAlgebra = TestParser.parseHIT(circleDSL)
      prover.addHIT(circleAlgebra)
      
      val circleGoal = TestParser.parse("∀c:Circle. P(base) → P(c)")
      prover.prove(circleGoal, maxDepth = 10) match {
        case Right(_) => println("✓ OK: Circle defined via DSL and induction proved")
        case Left(trace) => println(s"✗ FAIL: Circle induction failed - Reason: ${trace.reason}")
      }
    } catch {
      case e: Exception => println(s"Error in HIT DSL: ${e.getMessage}")
    }
  }
}
