package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug.logger

object HoTTTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val hottRules = StandardRules.hott ++ StandardRules.equality
    val config = ProverConfig(rules = hottRules)
    val prover = new Prover(config)

    println("=== HoTT Test: Path Reflexivity ===")
    // Goal: path(Type, a, a)
    val goal1 = sym(Path)(sym(Type), v("a"), v("a"))
    val result1 = prover.prove(goal1)
    result1 match {
      case Right(res) =>
        println("Proof found:")
        println(res.tree.format())
      case Left(fail) =>
        println("Proof failed:")
        println(fail.format())
    }

    println("\n=== HoTT Test: Univalence (A ≃ B -> path(Type, A, B)) ===")
    // Goal: path(Type, A, B) given equiv(A, B)
    // Note: univalence rule expects Path(Type(?0), A, B)
    val goal2 = sym(Path)(typeLevel(1), v("A"), v("B"))
    val goal2Impl = sym(Implies)(sym("equiv")(v("A"), v("B")), goal2)
    val result2 = prover.prove(goal2Impl)
    result2 match {
      case Right(res) =>
        println("Proof found:")
        println(res.tree.format())
      case Left(fail) =>
        println("Proof failed:")
        println(fail.format())
    }

    println("\n=== HoTT Test: Type Hierarchy ===")
    val t0 = typeLevel(0)
    val t1 = typeLevel(1)
    val t2 = typeLevel(2)
    println(s"Type 0: $t0")
    println(s"Type 1: $t1")
    println(s"Type 2: $t2")

    // Test unification of types
    val metaType = sym("Type")(meta(1)) // Type(?1)
    println(s"Unifying $metaType with $t1")
    val substs = Unifier.unify(metaType, t1, Unifier.emptySubst)
    if (substs.nonEmpty) {
      println(s"Unification success: ${substs.head}")
      println(s"Applied: ${Unifier.applySubst(metaType, substs.head)}")
    } else {
      println("Unification failed")
    }

    println("\n=== HoTT Test: Path Reduction ===")
    val p = v("p")
    val q = v("q")
    val a = v("a")

    val testExprs = List(
      Expr.App(
        sym("inv"),
        List(Expr.App(sym(Refl), List(a)))
      ), // inv(refl) -> refl
      Expr.App(
        sym("inv"),
        List(Expr.App(sym("inv"), List(p)))
      ), // inv(inv(p)) -> p
      sym(Compose)(p, Expr.App(sym(Refl), List(a))), // p ∘ refl -> p
      sym(Compose)(
        Expr.App(sym("inv"), List(q)),
        Expr.App(sym("inv"), List(p))
      ) // inv(p ∘ q) simplified
    )

    testExprs.foreach { e =>
      println(s"Original: $e")
      println(s"Normalized: ${Rewriter.normalize(e)}")
    }

    // Testing inv(p ∘ q) -> inv(q) ∘ inv(p)
    val complexPath = Expr.App(sym("inv"), List(sym(Compose)(p, q)))
    println(s"Original: $complexPath")
    println(s"Normalized: ${Rewriter.normalize(complexPath)}")

    println("\n=== HoTT Test: Higher Path (Path between Paths) ===")
    // Goal: path(path(A, x, y), inv(inv(p)), p)
    // This should be provable by normalization (inv(inv(p)) -> p) and then path-reflexivity.
    val x = v("x")
    val y = v("y")
    val A = v("A")
    val pathType = sym(Path)(A, x, y)
    val higherGoal = sym(Path)(
      pathType,
      Expr.App(sym("inv"), List(Expr.App(sym("inv"), List(p)))),
      p
    )

    val resultHigher = prover.prove(higherGoal)
    resultHigher match {
      case Right(res) =>
        println("Proof found for higher path:")
        println(res.tree.format())
      case Left(fail) =>
        println("Proof failed for higher path:")
        println(fail.format())
    }

    println("\n=== HoTT Test: Higher Inductive Type (S1 Circle) ===")
    // S1 = base + loop: path(base, base)
    val base = sym("base")
    val loop = sym("loop")
    val s1Algebra = InitialAlgebra(
      "S1",
      List(
        ConstructorDef("base", Nil, ConstructorType.Point),
        ConstructorDef("loop", Nil, ConstructorType.Path(base, base))
      ),
      "x"
    )

    val configS1 = ProverConfig(rules = hottRules, algebras = List(s1Algebra))
    val proverS1 = new Prover(configS1)

    // Goal: ∀x:S1. P(x)
    val P = v("P")
    val goalS1 = sym(Forall)(v("x"), P(v("x")))

    // Prove P(base) by providing it in the context
    // This allows us to see the second subgoal for the path constructor 'loop'
    val goalS1WithHyp = sym(Implies)(P(base), goalS1)

    val resultS1 = proverS1.prove(goalS1WithHyp, maxDepth = 3)
    resultS1 match {
      case Right(res) =>
        println("Proof found for S1 induction:")
        println(res.tree.format())
      case Left(fail) =>
        println("S1 Induction subgoals (from failure trace):")
        println(fail.format())
    }

    println("\n=== HoTT Test: Path Lemma Generation ===")
    // Use config with LemmaGenerationMode.All to see it in action for Implies
    val configLemma = config.copy(lemmaMode = LemmaGenerationMode.All)
    val proverLemma = new Prover(configLemma)
    
    val a_val = v("a")
    val b_val = v("b")
    val A_val = v("A")
    val path_ab = sym(Path)(A_val, a_val, b_val)
    val goalLemma = sym(Implies)(path_ab, path_ab)
    val resultLemma = proverLemma.prove(goalLemma)

    resultLemma match {
      case Right(res) =>
        println("Proof found:")
        println(res.tree.format())
        res.generatedLemma match {
          case Some(lemma) =>
            println(s"Generated Lemma: $lemma")
          case None =>
            println("No lemma generated")
        }
      case Left(fail) =>
        println("Proof failed:")
        println(fail.format())
    }
  }
}
