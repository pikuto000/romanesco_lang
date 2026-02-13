// ==========================================
// 高度で実用的な追加テストケース (Vector & Stream & Monad 強化版)
// ==========================================

package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.TestParser
import romanesco.Solver.core.LogicSymbols._
import romanesco.Utils.Debug
import romanesco.Utils.Debug.logger

// ==========================================
// Test Suite 1: 依存型の実用例
// ==========================================

object DependentTypeTests {
  import romanesco.Solver.core.Expr._

  def vectorTests(): Unit = {
    println("=== Dependent Type: Vector Tests ===")

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
        "vappend_nil",
        App(Sym("vappend"), List(Sym("vnil"), Var("v"))),
        Var("v"),
        List(Var("v"))
      ),

      CatRule(
        "vappend_cons",
        App(
          Sym("vappend"),
          List(
            App(Sym("vcons"), List(Var("x"), Var("xs"), Var("n"))),
            Var("ys")
          )
        ),
        App(
          Sym("vcons"),
          List(
            Var("x"),
            App(Sym("vappend"), List(Var("xs"), Var("ys"))),
            App(
              Sym("plus"),
              List(Var("n"), App(Sym("vlength"), List(Var("ys"))))
            )
          )
        ),
        List(Var("x"), Var("xs"), Var("n"), Var("ys"))
      ),

      CatRule(
        "vlength_nil",
        App(Sym("vlength"), List(Sym("vnil"))),
        Sym("0"),
        Nil
      ),

      CatRule(
        "vlength_cons",
        App(
          Sym("vlength"),
          List(
            App(Sym("vcons"), List(Var("x"), Var("xs"), Var("n")))
          )
        ),
        App(Sym("S"), List(Var("n"))),
        List(Var("x"), Var("xs"), Var("n"))
      ),

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

    val config = ProverConfig(
      rules = vecRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(vecAlgebra, StandardRules.natAlgebra),
      maxComplexity = 300
    )

    val prover = Prover(config)

    val cases = List(
      ("Vector head extraction", "∀n. ∀x. ∀xs. vhead(vcons(x, xs, n)) = x", 10),
      (
        "Vector length preservation",
        "∀v. ∀w. vlength(vappend(v, w)) = plus(vlength(v), vlength(w))",
        25
      ),
      ("Vector map identity", "∀v:Vec. ∀n. vmap(id, v) = v", 20)
    )

    runTestCases(cases, prover, "Dependent Vectors")
  }

  def runTestCases(
      cases: List[(String, String, Int)],
      prover: Prover,
      category: String,
      maxLogDepth: Int = 10
  ): Unit = {
    logger.switch(false)
    logger.setMaxDepth(maxLogDepth)
    cases.foreach { case (name, goalStr, depth) =>
      logger.info(s"Case: $name ...")
      logger.time(s"$category: $name") {
        val goal = TestParser.parse(goalStr)
        prover.prove(goal, maxDepth = depth, timeoutMs = 30000) match {
          case Right(result) =>
            println(s"✓ OK: $name")
          case Left(trace) =>
            println(s"✗ FAIL: $name - ${trace.reason}")
        }
      }
    }
  }
}

// ==========================================
// Test Suite 2: 算術とリスト (Phase 1)
// ==========================================

object Phase1Tests {
  import DependentTypeTests.runTestCases

  def arithmeticTests(): Unit = {
    println("=== Phase 1: Advanced Arithmetic ===")
    val prover = Prover(
      ProverConfig(
        rules = StandardRules.all,
        algebras = StandardRules.defaultAlgebras
      )
    )

    val cases = List(
      ("Plus Commutative", "∀n. ∀m. plus(n, m) = plus(m, n)", 20),
      (
        "Plus Associative",
        "∀n. ∀m. ∀k. plus(plus(n, m), k) = plus(n, plus(m, k))",
        20
      )
    )
    runTestCases(cases, prover, "Arithmetic")
  }

  def listTests(): Unit = {
    println("=== Phase 1: Advanced Lists ===")
    val prover = Prover(
      ProverConfig(
        rules = StandardRules.all,
        algebras = StandardRules.defaultAlgebras
      )
    )

    val cases = List(
      (
        "Append Associative",
        "∀xs. ∀ys. ∀zs. append(append(xs, ys), zs) = append(xs, append(ys, zs))",
        20
      ),
      ("Reverse Reverse", "∀xs. reverse(reverse(xs)) = xs", 25),
      (
        "Map Composition",
        "∀f. ∀g. ∀xs. map(compose(f, g), xs) = map(f, map(g, xs))",
        20
      )
    )
    runTestCases(cases, prover, "Lists")
  }
}

// ==========================================
// Test Suite 3: 到達可能性とストリーム
// ==========================================

object AdvancedInductivePredicateTests {
  import DependentTypeTests.runTestCases
  import romanesco.Solver.core.Expr._

  def reachabilityTest(): Unit = {
    println("=== Inductive Predicate: Graph Reachability ===")

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

    val config = ProverConfig(
      rules = graphRules ++ StandardRules.all,
      algebras = Nil,
      maxComplexity = 200
    )

    val prover = Prover(config)

    val cases = List(
      ("Reflexivity", "∀x. reachable(x, x) = ⊤", 5),
      ("Edge implies reach", "∀x. ∀y. edge(x, y) → reachable(x, y)", 10)
    )

    runTestCases(cases, prover, "Reachability")
  }
}

object CoinductiveTests {
  import DependentTypeTests.runTestCases
  import romanesco.Solver.core.Expr._

  def streamTest(): Unit = {
    println("=== Coinductive: Infinite Streams ===")

    val streamRules = List(
      CatRule(
        "bisim_def",
        App(Sym("bisim"), List(Var("s1"), Var("s2"))),
        App(
          Sym(And),
          List(
            App(Sym("="), List(head(Var("s1")), head(Var("s2")))),
            App(
              Sym(Next),
              List(App(Sym("bisim"), List(tail(Var("s1")), tail(Var("s2")))))
            )
          )
        ),
        List(Var("s1"), Var("s2"))
      ),

      CatRule(
        "repeat_unfold",
        App(Sym("repeat"), List(Var("x"))),
        App(
          Sym("cons_stream"),
          List(Var("x"), App(Sym("repeat"), List(Var("x"))))
        ),
        List(Var("x"))
      ),

      CatRule(
        "head_repeat",
        App(Sym("head"), List(App(Sym("repeat"), List(Var("x"))))),
        Var("x"),
        List(Var("x"))
      ),

      CatRule(
        "tail_repeat",
        App(Sym("tail"), List(App(Sym("repeat"), List(Var("x"))))),
        App(Sym("repeat"), List(Var("x"))),
        List(Var("x"))
      )
    )

    val config = ProverConfig(
      rules = streamRules ++ StandardRules.all,
      algebras = Nil
    )

    val prover = Prover(config)

    val cases = List(
      ("Stream head of repeat", "∀x. head(repeat(x)) = x", 10),
      ("Stream bisimulation (self)", "∀x. bisim(repeat(x), repeat(x))", 30)
    )

    runTestCases(cases, prover, "Streams", maxLogDepth = 5)
  }

  private def head(s: Expr) = App(Sym("head"), List(s))
  private def tail(s: Expr) = App(Sym("tail"), List(s))
}

object MonadLawsTests {
  import DependentTypeTests.runTestCases
  import romanesco.Solver.core.Expr._

  def runTests(): Unit = {
    println("=== Category Theory: Monad Laws ===")

    val monadRules = List(
      CatRule(
        "return_list_def",
        sym("return_list")(v("x")),
        App(Sym("cons"), List(v("x"), Sym("nil"))),
        List(v("x"))
      ),
      CatRule(
        "bind_list_nil",
        sym("bind_list")(Sym("nil"), v("f")),
        Sym("nil"),
        List(v("f"))
      ),
      CatRule(
        "bind_list_cons",
        sym("bind_list")(App(Sym("cons"), List(v("x"), v("xs"))), v("f")),
        App(
          Sym("append"),
          List(v("f")(v("x")), sym("bind_list")(v("xs"), v("f")))
        ),
        List(v("x"), v("xs"), v("f"))
      ),

      CatRule(
        "return_maybe_def",
        sym("return_maybe")(v("x")),
        App(Sym("just"), List(v("x"))),
        List(v("x"))
      ),
      CatRule(
        "bind_maybe_nothing",
        sym("bind_maybe")(Sym("nothing"), v("f")),
        Sym("nothing"),
        List(v("f"))
      ),
      CatRule(
        "bind_maybe_just",
        sym("bind_maybe")(App(Sym("just"), List(v("x"))), v("f")),
        v("f")(v("x")),
        List(v("x"), v("f"))
      )
    )

    val prover = Prover(
      ProverConfig(
        rules = monadRules ++ StandardRules.all,
        algebras = StandardRules.defaultAlgebras
      )
    )

    val cases = List(
      (
        "List Monad: Left Identity",
        "∀a. ∀f. bind_list(return_list(a), f) = f(a)",
        15
      ),
      ("List Monad: Right Identity", "∀m. bind_list(m, return_list) = m", 20),
      (
        "List Monad: Associativity",
        "∀m. ∀f. ∀g. bind_list(bind_list(m, f), g) = bind_list(m, λx. bind_list(f(x), g))",
        25
      ),

      (
        "Maybe Monad: Left Identity",
        "∀a. ∀f. bind_maybe(return_maybe(a), f) = f(a)",
        10
      ),
      (
        "Maybe Monad: Right Identity",
        "∀m. bind_maybe(m, return_maybe) = m",
        15
      ),
      (
        "Maybe Monad: Associativity",
        "∀m. ∀f. ∀g. bind_maybe(bind_maybe(m, f), g) = bind_maybe(m, λx. bind_maybe(f(x), g))",
        20
      )
    )

    runTestCases(cases, prover, "Monad Laws", maxLogDepth = 5)
  }
}

// ==========================================
// メイン実行
// ==========================================

@main def runAdvancedTestsSuite(): Unit = {
  println("=" * 60)
  println("Advanced Test Suite")
  println("=" * 60)

  logger.switch(false)
  logger.setLevel(Debug.logger.Level.INFO)
  logger.setMaxDepth(10)

  DependentTypeTests.vectorTests()
  println()
  AdvancedInductivePredicateTests.reachabilityTest()
  println()
  CoinductiveTests.streamTest()
  println()
  MonadLawsTests.runTests()

  println("=" * 60)
}

@main def runPhase1TestsSuite(): Unit = {
  println("=" * 60)
  println("Phase 1 Additional Tests")
  println("=" * 60)

  Phase1Tests.arithmeticTests()
  println()
  Phase1Tests.listTests()

  println("=" * 60)
}

@main def runAllAdditionalTestsSuite(): Unit = {
  runPhase1TestsSuite()
  println()
  runAdvancedTestsSuite()
}
