// ==========================================
// SolverTests パッケージに追加するテストケース
// 既存の SolverTests ディレクトリに配置してください
// ==========================================

package romanesco.Solver.SolverTests

import romanesco.Solver.core._

// ==========================================
// Test Suite 1: 高度なリスト操作
// ==========================================

object AdvancedListTests {

  def lengthPreservationTest(): Unit = {
    println("=== List Length Preservation Test ===")

    val lengthRules = List(
      CatRule(
        "length_nil",
        Expr.App(Expr.Sym("length"), List(Expr.Sym("nil"))),
        Expr.Sym("0"),
        Nil
      ),
      CatRule(
        "length_cons",
        Expr.App(
          Expr.Sym("length"),
          List(
            Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Var("xs")))
          )
        ),
        Expr.App(
          Expr.Sym("S"),
          List(
            Expr.App(Expr.Sym("length"), List(Expr.Var("xs")))
          )
        ),
        List(Expr.Var("x"), Expr.Var("xs"))
      )
    )

    val cases = List(
      (
        "length(append(xs, ys)) = plus(length(xs), length(ys))",
        Expr.App(
          Expr.Sym("∀"),
          List(
            Expr.Var("xs"),
            Expr.App(
              Expr.Sym("∀"),
              List(
                Expr.Var("ys"),
                Expr.App(
                  Expr.Sym("="),
                  List(
                    Expr.App(
                      Expr.Sym("length"),
                      List(
                        Expr.App(
                          Expr.Sym("append"),
                          List(Expr.Var("xs"), Expr.Var("ys"))
                        )
                      )
                    ),
                    Expr.App(
                      Expr.Sym("plus"),
                      List(
                        Expr.App(Expr.Sym("length"), List(Expr.Var("xs"))),
                        Expr.App(Expr.Sym("length"), List(Expr.Var("ys")))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        20,
        true
      )
    )

    val config = ProverConfig(
      rules =
        lengthRules ++ StandardRules.listAppendRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(StandardRules.listAlgebra, StandardRules.natAlgebra)
    )

    runTestCases(cases, config, "List Length")
  }

  def mapFunctorTest(): Unit = {
    println("=== Map Functor Laws Test ===")

    val mapRules = List(
      CatRule(
        "map_nil",
        Expr.App(Expr.Sym("map"), List(Expr.Var("f"), Expr.Sym("nil"))),
        Expr.Sym("nil"),
        List(Expr.Var("f"))
      ),
      CatRule(
        "map_cons",
        Expr.App(
          Expr.Sym("map"),
          List(
            Expr.Var("f"),
            Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Var("xs")))
          )
        ),
        Expr.App(
          Expr.Sym("cons"),
          List(
            Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
            Expr.App(Expr.Sym("map"), List(Expr.Var("f"), Expr.Var("xs")))
          )
        ),
        List(Expr.Var("f"), Expr.Var("x"), Expr.Var("xs"))
      ),
      CatRule(
        "id_def",
        Expr.App(Expr.Sym("id"), List(Expr.Var("x"))),
        Expr.Var("x"),
        List(Expr.Var("x"))
      )
    )

    val cases = List(
      (
        "map(id, xs) = xs (Functor identity)",
        Expr.App(
          Expr.Sym("∀"),
          List(
            Expr.Var("xs"),
            Expr.App(
              Expr.Sym("="),
              List(
                Expr.App(Expr.Sym("map"), List(Expr.Sym("id"), Expr.Var("xs"))),
                Expr.Var("xs")
              )
            )
          )
        ),
        15,
        true
      )
    )

    val config = ProverConfig(
      rules = mapRules ++ StandardRules.all,
      algebras = List(StandardRules.listAlgebra)
    )

    runTestCases(cases, config, "Map Functor")
  }

  def runTestCases(
      cases: List[(String, Expr, Int, Boolean)],
      config: ProverConfig,
      category: String
  ): Unit = {
    val prover = Prover(config)

    cases.foreach { case (name, goal, depth, shouldSucceed) =>
      print(s"Case: $name ... ")
      prover.prove(goal, maxDepth = depth) match {
        case Right(result) if shouldSucceed =>
          println("✓ OK (Solved)")
          if (depth <= 15) println(result.tree.format(0))
        case Left(_) if !shouldSucceed =>
          println("✓ OK (Failed as expected)")
        case Right(_) =>
          println("✗ FAIL (Should have failed)")
        case Left(trace) =>
          println(s"✗ FAIL: ${trace.reason}")
      }
    }
  }
}

// ==========================================
// Test Suite 2: 算術の高度な性質
// ==========================================

object ArithmeticPropertiesTests {
  import AdvancedListTests.runTestCases

  def multiplicationTest(): Unit = {
    println("=== Multiplication Properties Test ===")

    val timesRules = List(
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
      // Derived rules (would be proved as lemmas)
      CatRule(
        "times_n_0",
        Expr.App(Expr.Sym("times"), List(Expr.Var("n"), Expr.Sym("0"))),
        Expr.Sym("0"),
        List(Expr.Var("n"))
      )
    )

    val cases = List(
      (
        "times(0, m) = 0",
        Expr.App(
          Expr.Sym("∀"),
          List(
            Expr.Var("m"),
            Expr.App(
              Expr.Sym("="),
              List(
                Expr.App(Expr.Sym("times"), List(Expr.Sym("0"), Expr.Var("m"))),
                Expr.Sym("0")
              )
            )
          )
        ),
        5,
        true
      ),

      (
        "times(n, 0) = 0",
        Expr.App(
          Expr.Sym("∀"),
          List(
            Expr.Var("n"),
            Expr.App(
              Expr.Sym("="),
              List(
                Expr.App(Expr.Sym("times"), List(Expr.Var("n"), Expr.Sym("0"))),
                Expr.Sym("0")
              )
            )
          )
        ),
        15,
        true
      ),

      (
        "times(S(0), n) = n",
        Expr.App(
          Expr.Sym("∀"),
          List(
            Expr.Var("n"),
            Expr.App(
              Expr.Sym("="),
              List(
                Expr.App(
                  Expr.Sym("times"),
                  List(
                    Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))),
                    Expr.Var("n")
                  )
                ),
                Expr.Var("n")
              )
            )
          )
        ),
        10,
        true
      )
    )

    val config = ProverConfig(
      rules = timesRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(StandardRules.natAlgebra)
    )

    runTestCases(cases, config, "Arithmetic")
  }
}

// ==========================================
// Test Suite 3: 帰納的述語
// ==========================================

object InductivePredicateTests {
  import AdvancedListTests.runTestCases

  def evenOddTest(): Unit = {
    println("=== Even/Odd Predicate Test ===")

    val evenOddRules = List(
      CatRule(
        "even_0",
        Expr.App(Expr.Sym("even"), List(Expr.Sym("0"))),
        Expr.Sym("True"),
        Nil
      ),
      CatRule(
        "even_SS",
        Expr.App(
          Expr.Sym("even"),
          List(
            Expr.App(
              Expr.Sym("S"),
              List(Expr.App(Expr.Sym("S"), List(Expr.Var("n"))))
            )
          )
        ),
        Expr.App(Expr.Sym("even"), List(Expr.Var("n"))),
        List(Expr.Var("n"))
      ),
      CatRule(
        "odd_1",
        Expr.App(
          Expr.Sym("odd"),
          List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
        ),
        Expr.Sym("True"),
        Nil
      ),
      CatRule(
        "odd_SS",
        Expr.App(
          Expr.Sym("odd"),
          List(
            Expr.App(
              Expr.Sym("S"),
              List(Expr.App(Expr.Sym("S"), List(Expr.Var("n"))))
            )
          )
        ),
        Expr.App(Expr.Sym("odd"), List(Expr.Var("n"))),
        List(Expr.Var("n"))
      )
    )

    val tests = List(
      (
        "even(0)",
        Expr.App(
          Expr.Sym("="),
          List(
            Expr.App(Expr.Sym("even"), List(Expr.Sym("0"))),
            Expr.Sym("True")
          )
        ),
        5,
        true
      ),
      (
        "even(2)",
        Expr.App(
          Expr.Sym("="),
          List(
            Expr.App(
              Expr.Sym("even"),
              List(
                Expr.App(
                  Expr.Sym("S"),
                  List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
                )
              )
            ),
            Expr.Sym("True")
          )
        ),
        10,
        true
      ),
      (
        "even(4)",
        Expr.App(
          Expr.Sym("="),
          List(
            Expr.App(
              Expr.Sym("even"),
              List(
                Expr.App(
                  Expr.Sym("S"),
                  List(
                    Expr.App(
                      Expr.Sym("S"),
                      List(
                        Expr.App(
                          Expr.Sym("S"),
                          List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
                        )
                      )
                    )
                  )
                )
              )
            ),
            Expr.Sym("True")
          )
        ),
        15,
        true
      ),
      (
        "odd(1)",
        Expr.App(
          Expr.Sym("="),
          List(
            Expr.App(
              Expr.Sym("odd"),
              List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
            ),
            Expr.Sym("True")
          )
        ),
        5,
        true
      ),
      (
        "odd(3)",
        Expr.App(
          Expr.Sym("="),
          List(
            Expr.App(
              Expr.Sym("odd"),
              List(
                Expr.App(
                  Expr.Sym("S"),
                  List(
                    Expr.App(
                      Expr.Sym("S"),
                      List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
                    )
                  )
                )
              )
            ),
            Expr.Sym("True")
          )
        ),
        15,
        true
      )
    )

    val config = ProverConfig(
      rules = evenOddRules ++ StandardRules.all,
      algebras = List(StandardRules.natAlgebra)
    )

    runTestCases(tests, config, "Even/Odd")
  }
}

// ==========================================
// メインテストランナー
// ==========================================

@main def runPhase1AdditionalTests(): Unit = {
  println("=" * 60)
  println("Phase 1 Additional Tests")
  println("=" * 60)
  println()

  AdvancedListTests.lengthPreservationTest()
  println()

  AdvancedListTests.mapFunctorTest()
  println()

  ArithmeticPropertiesTests.multiplicationTest()
  println()

  InductivePredicateTests.evenOddTest()

  println()
  println("=" * 60)
  println("Phase 1 Additional Tests done!")
  println("=" * 60)
}
