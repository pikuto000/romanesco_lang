// ==========================================
// 数学的定理とプログラム検証のテストケース
// ==========================================

package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.TestParser
import romanesco.Solver.core.LogicSymbols._

// ==========================================
// Test Suite 1: 数学的定理
// ==========================================

object MathematicalTheoremsTests {

  def arithmeticIdentitiesTest(): Unit = {
    println("=== Mathematical Theorems: Arithmetic Identities ===")

    val extendedArithRules = List(
      // 乗算
      CatRule(
        "times_0_m",
        Expr.App(Expr.Sym("times"), List(Expr.Sym("0"), Expr.Var("m"))),
        Expr.Sym("0"),
        List(Expr.Var("m"))
      ),

      CatRule(
        "times_S_m",
        Expr.App(
          Expr.Sym("times"),
          List(
            Expr.App(Expr.Sym("S"), List(Expr.Var("n"))),
            Expr.Var("m")
          )
        ),
        Expr.App(
          Expr.Sym("plus"),
          List(
            Expr.Var("m"),
            Expr.App(Expr.Sym("times"), List(Expr.Var("n"), Expr.Var("m")))
          )
        ),
        List(Expr.Var("n"), Expr.Var("m"))
      ),

      // 指数
      CatRule(
        "exp_0",
        Expr.App(Expr.Sym("exp"), List(Expr.Var("b"), Expr.Sym("0"))),
        Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))),
        List(Expr.Var("b"))
      ),

      CatRule(
        "exp_S",
        Expr.App(
          Expr.Sym("exp"),
          List(
            Expr.Var("b"),
            Expr.App(Expr.Sym("S"), List(Expr.Var("n")))
          )
        ),
        Expr.App(
          Expr.Sym("times"),
          List(
            Expr.Var("b"),
            Expr.App(Expr.Sym("exp"), List(Expr.Var("b"), Expr.Var("n")))
          )
        ),
        List(Expr.Var("b"), Expr.Var("n"))
      ),

      // 階乗
      CatRule(
        "fact_0",
        Expr.App(Expr.Sym("fact"), List(Expr.Sym("0"))),
        Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))),
        Nil
      ),

      CatRule(
        "fact_S",
        Expr.App(
          Expr.Sym("fact"),
          List(Expr.App(Expr.Sym("S"), List(Expr.Var("n"))))
        ),
        Expr.App(
          Expr.Sym("times"),
          List(
            Expr.App(Expr.Sym("S"), List(Expr.Var("n"))),
            Expr.App(Expr.Sym("fact"), List(Expr.Var("n")))
          )
        ),
        List(Expr.Var("n"))
      ),

      // Min/Max
      CatRule(
        "min_0_n",
        Expr.App(Expr.Sym("min"), List(Expr.Sym("0"), Expr.Var("n"))),
        Expr.Sym("0"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "min_n_0",
        Expr.App(Expr.Sym("min"), List(Expr.Var("n"), Expr.Sym("0"))),
        Expr.Sym("0"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "min_same",
        Expr.App(Expr.Sym("min"), List(Expr.Var("n"), Expr.Var("n"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "max_0_n",
        Expr.App(Expr.Sym("max"), List(Expr.Sym("0"), Expr.Var("n"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "max_n_0",
        Expr.App(Expr.Sym("max"), List(Expr.Var("n"), Expr.Sym("0"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "max_same",
        Expr.App(Expr.Sym("max"), List(Expr.Var("n"), Expr.Var("n"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      )
    )

    val config = ProverConfig(
      rules =
        extendedArithRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(StandardRules.natAlgebra)
    )

    val prover = Prover(config)

    val cases = List(
      // 基本的な乗算の性質
      ("times(n, 0) = 0", "∀n. times(n, 0) = 0", 15, true),

      ("times(n, S(0)) = n", "∀n. times(n, S(0)) = n", 10, true),

      ("times commutative", "∀n. ∀m. times(n, m) = times(m, n)", 25, true),

      (
        "times distributive over plus",
        "∀n. ∀m. ∀k. times(n, plus(m, k)) = plus(times(n, m), times(n, k))",
        30,
        true
      ),

      (
        "times associative",
        "∀n. ∀m. ∀k. times(times(n, m), k) = times(n, times(m, k))",
        30,
        true
      ),

      // 指数の性質
      ("exp(b, 0) = 1", "∀b. exp(b, 0) = S(0)", 5, true),

      ("exp(b, 1) = b", "∀b. exp(b, S(0)) = b", 10, true),

      (
        "exp(b, n+m) = exp(b,n) * exp(b,m)",
        "∀b. ∀n. ∀m. exp(b, plus(n, m)) = times(exp(b, n), exp(b, m))",
        30,
        true
      ),

      // Min/Maxの性質
      ("min commutative", "∀n. ∀m. min(n, m) = min(m, n)", 20, true),

      ("max commutative", "∀n. ∀m. max(n, m) = max(m, n)", 20, true),

      ("min idempotent", "∀n. min(n, n) = n", 5, true),

      ("max idempotent", "∀n. max(n, n) = n", 5, true),

      (
        "Absorption law",
        "∀n. ∀m. plus(min(n, m), max(n, m)) = plus(n, m)",
        25,
        true
      ),

      // 階乗の基本性質
      ("fact(0) = 1", "fact(0) = S(0)", 5, true),

      ("fact(1) = 1", "fact(S(0)) = S(0)", 10, true),

      ("fact is positive", "∀n. ∃k. fact(n) = S(k)", 15, true)
    )

    runTestCases(cases, prover, "Arithmetic Identities")
  }

  def numberTheoryTest(): Unit = {
    println("=== Mathematical Theorems: Number Theory ===")

    val numberTheoryRules = List(
      // GCD
      CatRule(
        "gcd_n_0",
        Expr.App(Expr.Sym("gcd"), List(Expr.Var("n"), Expr.Sym("0"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "gcd_0_n",
        Expr.App(Expr.Sym("gcd"), List(Expr.Sym("0"), Expr.Var("n"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "gcd_same",
        Expr.App(Expr.Sym("gcd"), List(Expr.Var("n"), Expr.Var("n"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      // LCM
      CatRule(
        "lcm_n_0",
        Expr.App(Expr.Sym("lcm"), List(Expr.Var("n"), Expr.Sym("0"))),
        Expr.Sym("0"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "lcm_0_n",
        Expr.App(Expr.Sym("lcm"), List(Expr.Sym("0"), Expr.Var("n"))),
        Expr.Sym("0"),
        List(Expr.Var("n"))
      ),

      CatRule(
        "lcm_same",
        Expr.App(Expr.Sym("lcm"), List(Expr.Var("n"), Expr.Var("n"))),
        Expr.Var("n"),
        List(Expr.Var("n"))
      ),

      // Divisibility
      CatRule(
        "divides_refl",
        Expr.App(Expr.Sym("divides"), List(Expr.Var("n"), Expr.Var("n"))),
        Expr.Sym(True),
        List(Expr.Var("n"))
      ),

      CatRule(
        "divides_0",
        Expr.App(Expr.Sym("divides"), List(Expr.Var("n"), Expr.Sym("0"))),
        Expr.Sym(True),
        List(Expr.Var("n"))
      )
    )

    val config = ProverConfig(
      rules =
        numberTheoryRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(StandardRules.natAlgebra)
    )

    val prover = Prover(config)

    val cases = List(
      ("GCD commutative", "∀n. ∀m. gcd(n, m) = gcd(m, n)", 15, true),

      ("GCD with 0", "∀n. gcd(n, 0) = n", 5, true),

      ("GCD idempotent", "∀n. gcd(n, n) = n", 5, true),

      ("LCM commutative", "∀n. ∀m. lcm(n, m) = lcm(m, n)", 15, true),

      ("LCM with 0", "∀n. lcm(n, 0) = 0", 5, true),

      ("LCM idempotent", "∀n. lcm(n, n) = n", 5, true),

      (
        "GCD-LCM identity",
        "∀n. ∀m. times(gcd(n, m), lcm(n, m)) = times(n, m)",
        25,
        true
      ),

      ("Divisibility reflexive", "∀n. divides(n, n) = ⊤", 5, true),

      (
        "Divisibility transitive",
        "∀a. ∀b. ∀c. divides(a, b) ∧ divides(b, c) → divides(a, c)",
        20,
        true
      )
    )

    runTestCases(cases, prover, "Number Theory")
  }

  def runTestCases(
      cases: List[(String, String, Int, Boolean)],
      prover: Prover,
      category: String
  ): Unit = {
    var passed = 0
    var failed = 0

    cases.foreach { case (name, goalStr, depth, shouldSucceed) =>
      print(s"Case: $name ... ")
      val goal = TestParser.parse(goalStr)
      prover.prove(goal, maxDepth = depth) match {
        case Right(result) if shouldSucceed =>
          println("✓ OK (Solved)")
          passed += 1
        case Left(_) if !shouldSucceed =>
          println("✓ OK (Failed as expected)")
          passed += 1
        case Right(_) =>
          println("✗ FAIL (Should have failed)")
          failed += 1
        case Left(trace) =>
          println(s"✗ FAIL: ${trace.reason}")
          failed += 1
      }
    }

    println(s"\n[$category] Results: $passed passed, $failed failed\n")
  }
}

// ==========================================
// Test Suite 2: プログラム検証
// ==========================================

object ProgramVerificationTests {
  import MathematicalTheoremsTests.runTestCases

  def hoarLogicTest(): Unit = {
    println("=== Program Verification: Hoare Logic ===")

    val hoareRules = List(
      // {P} skip {P}
      CatRule(
        "hoare_skip",
        Expr.App(
          Expr.Sym("hoare"),
          List(Expr.Var("P"), Expr.Sym("skip"), Expr.Var("P"))
        ),
        Expr.Sym(True),
        List(Expr.Var("P"))
      ),

      // {P[e/x]} x:=e {P}
      CatRule(
        "hoare_assign",
        Expr.App(
          Expr.Sym("hoare"),
          List(
            Expr.App(
              Expr.Sym("subst"),
              List(Expr.Var("P"), Expr.Var("x"), Expr.Var("e"))
            ),
            Expr.App(Expr.Sym("assign"), List(Expr.Var("x"), Expr.Var("e"))),
            Expr.Var("P")
          )
        ),
        Expr.Sym(True),
        List(Expr.Var("P"), Expr.Var("x"), Expr.Var("e"))
      ),

      // {P} c1 {Q} ∧ {Q} c2 {R} → {P} c1;c2 {R}
      CatRule(
        "hoare_seq",
        Expr.App(
          Expr.Sym("hoare"),
          List(
            Expr.Var("P"),
            Expr.App(Expr.Sym("seq"), List(Expr.Var("c1"), Expr.Var("c2"))),
            Expr.Var("R")
          )
        ),
        Expr.App(
          Expr.Sym(And),
          List(
            Expr.App(
              Expr.Sym("hoare"),
              List(Expr.Var("P"), Expr.Var("c1"), Expr.Var("Q"))
            ),
            Expr.App(
              Expr.Sym("hoare"),
              List(Expr.Var("Q"), Expr.Var("c2"), Expr.Var("R"))
            )
          )
        ),
        List(
          Expr.Var("P"),
          Expr.Var("Q"),
          Expr.Var("R"),
          Expr.Var("c1"),
          Expr.Var("c2")
        )
      ),

      // {P ∧ b} c {P} → {P} while b do c {P ∧ ¬b}
      CatRule(
        "hoare_while",
        Expr.App(
          Expr.Sym("hoare"),
          List(
            Expr.Var("I"),
            Expr.App(Expr.Sym("while"), List(Expr.Var("b"), Expr.Var("c"))),
            Expr.App(
              Expr.Sym(And),
              List(Expr.Var("I"), Expr.App(Expr.Sym("¬"), List(Expr.Var("b"))))
            )
          )
        ),
        Expr.App(
          Expr.Sym("hoare"),
          List(
            Expr.App(Expr.Sym(And), List(Expr.Var("I"), Expr.Var("b"))),
            Expr.Var("c"),
            Expr.Var("I")
          )
        ),
        List(Expr.Var("I"), Expr.Var("b"), Expr.Var("c"))
      ),

      // {P'} c {Q'} ∧ P→P' ∧ Q'→Q → {P} c {Q}  (consequence rule)
      CatRule(
        "hoare_consequence",
        Expr.App(
          Expr.Sym("hoare"),
          List(Expr.Var("P"), Expr.Var("c"), Expr.Var("Q"))
        ),
        Expr.App(
          Expr.Sym(And),
          List(
            Expr.App(
              Expr.Sym("hoare"),
              List(Expr.Var("P'"), Expr.Var("c"), Expr.Var("Q'"))
            ),
            Expr.App(
              Expr.Sym(And),
              List(
                Expr
                  .App(Expr.Sym(Implies), List(Expr.Var("P"), Expr.Var("P'"))),
                Expr.App(Expr.Sym(Implies), List(Expr.Var("Q'"), Expr.Var("Q")))
              )
            )
          )
        ),
        List(
          Expr.Var("P"),
          Expr.Var("P'"),
          Expr.Var("Q"),
          Expr.Var("Q'"),
          Expr.Var("c")
        )
      )
    )

    val config = ProverConfig(
      rules = hoareRules ++ StandardRules.all,
      algebras = Nil
    )

    val prover = Prover(config)

    val cases = List(
      ("Skip preserves postcondition", "∀P. hoare(P, skip, P) = ⊤", 5, true),

      (
        "Assignment correctness",
        "∀P. ∀x. ∀e. hoare(subst(P, x, e), assign(x, e), P) = ⊤",
        5,
        true
      ),

      (
        "Sequential composition",
        "∀P. ∀Q. ∀R. ∀c1. ∀c2. " +
          "hoare(P, c1, Q) ∧ hoare(Q, c2, R) → hoare(P, seq(c1, c2), R)",
        15,
        true
      ),

      (
        "While loop invariant",
        "∀I. ∀b. ∀c. hoare(I ∧ b, c, I) → hoare(I, while(b, c), I ∧ ¬b)",
        15,
        true
      )
    )

    runTestCases(cases, prover, "Hoare Logic")
  }

  def resourceSafetyTest(): Unit = {
    println("=== Program Verification: Resource Safety ===")

    val resourceRules = List(
      // File handle safety
      CatRule(
        "file_opened",
        Expr.App(Expr.Sym("opened"), List(Expr.Var("f"))),
        Expr.Sym("resource"),
        List(Expr.Var("f"))
      ),

      CatRule(
        "file_closed",
        Expr.App(Expr.Sym("closed"), List(Expr.Var("f"))),
        Expr.Sym("unit"),
        List(Expr.Var("f"))
      ),

      // opened(f) ⊸ (closed(f) ⊕ (data * opened(f)))
      CatRule(
        "file_use",
        Expr.App(
          Expr.Sym(LImplies),
          List(
            Expr.App(Expr.Sym("opened"), List(Expr.Var("f"))),
            Expr.App(
              Expr.Sym(LPlus),
              List(
                Expr.App(Expr.Sym("closed"), List(Expr.Var("f"))),
                Expr.App(
                  Expr.Sym(SepAnd),
                  List(
                    Expr.Sym("data"),
                    Expr.App(Expr.Sym("opened"), List(Expr.Var("f")))
                  )
                )
              )
            )
          )
        ),
        Expr.Sym(True),
        List(Expr.Var("f"))
      ),

      // Memory safety
      CatRule(
        "alloc_ptr",
        Expr.App(Expr.Sym("allocated"), List(Expr.Var("p"))),
        Expr.Sym("resource"),
        List(Expr.Var("p"))
      ),

      CatRule(
        "freed_ptr",
        Expr.App(Expr.Sym("freed"), List(Expr.Var("p"))),
        Expr.Sym("unit"),
        List(Expr.Var("p"))
      ),

      // freed(p) ⊸ ⊥ (no double free)
      CatRule(
        "no_double_free",
        Expr.App(
          Expr.Sym(LImplies),
          List(
            Expr.App(Expr.Sym("freed"), List(Expr.Var("p"))),
            Expr.Sym(False)
          )
        ),
        Expr.Sym(True),
        List(Expr.Var("p"))
      ),

      // Lock safety
      CatRule(
        "lock_acquired",
        Expr.App(Expr.Sym("locked"), List(Expr.Var("m"))),
        Expr.Sym("resource"),
        List(Expr.Var("m"))
      ),

      // locked(m) * locked(m) ⊸ ⊥ (mutual exclusion)
      CatRule(
        "mutex_exclusive",
        Expr.App(
          Expr.Sym(LImplies),
          List(
            Expr.App(
              Expr.Sym(SepAnd),
              List(
                Expr.App(Expr.Sym("locked"), List(Expr.Var("m"))),
                Expr.App(Expr.Sym("locked"), List(Expr.Var("m")))
              )
            ),
            Expr.Sym(False)
          )
        ),
        Expr.Sym(True),
        List(Expr.Var("m"))
      )
    )

    val config = ProverConfig(
      rules = resourceRules ++ StandardRules.all,
      algebras = Nil
    )

    val prover = Prover(config)

    val cases = List(
      (
        "File must be closed or used",
        "∀f. opened(f) ⊸ (closed(f) ⊕ (data * opened(f)))",
        10,
        true
      ),

      ("No double free", "∀p. freed(p) ⊸ ⊥", 5, true),

      ("Mutex exclusion", "∀m. locked(m) * locked(m) ⊸ ⊥", 5, true),

      ("Use after free is invalid", "∀p. freed(p) → ¬valid(p)", 10, true)
    )

    runTestCases(cases, prover, "Resource Safety")
  }
}

// ==========================================
// メイン実行
// ==========================================

@main def runMathAndVerificationTests(): Unit = {
  println("=" * 60)
  println("Mathematical Theorems and Program Verification Tests")
  println("=" * 60)
  println()

  MathematicalTheoremsTests.arithmeticIdentitiesTest()
  println()

  MathematicalTheoremsTests.numberTheoryTest()
  println()

  ProgramVerificationTests.hoarLogicTest()
  println()

  ProgramVerificationTests.resourceSafetyTest()

  println()
  println("=" * 60)
  println("All tests completed!")
  println("=" * 60)
}
