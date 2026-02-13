// ==========================================
// 高度で実用的な追加テストケース (Vector & Stream 強化版)
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
        ConstructorDef("vcons", 
          List(ArgType.Constant, ArgType.Recursive, ArgType.Constant),
          ConstructorType.Point)
      )
    )
    
    val vecRules = List(
      CatRule("vappend_nil",
        App(Sym("vappend"), List(Sym("vnil"), Var("v"))),
        Var("v"),
        List(Var("v"))),
      
      CatRule("vappend_cons",
        App(Sym("vappend"), List(
          App(Sym("vcons"), List(Var("x"), Var("xs"), Var("n"))),
          Var("ys")
        )),
        App(Sym("vcons"), List(
          Var("x"),
          App(Sym("vappend"), List(Var("xs"), Var("ys"))),
          App(Sym("plus"), List(Var("n"), App(Sym("vlength"), List(Var("ys")))))
        )),
        List(Var("x"), Var("xs"), Var("n"), Var("ys"))),
      
      CatRule("vlength_nil",
        App(Sym("vlength"), List(Sym("vnil"))),
        Sym("0"),
        Nil),
      
      CatRule("vlength_cons",
        App(Sym("vlength"), List(
          App(Sym("vcons"), List(Var("x"), Var("xs"), Var("n")))
        )),
        App(Sym("S"), List(Var("n"))),
        List(Var("x"), Var("xs"), Var("n")))
    )
    
    val config = ProverConfig(
      rules = vecRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(vecAlgebra, StandardRules.natAlgebra),
      maxComplexity = 300
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("Vector head extraction",
       "∀n. ∀x. ∀xs. vhead(vcons(x, xs, n)) = x",
       10),
      ("Vector length preservation",
       "∀v. ∀w. vlength(vappend(v, w)) = plus(vlength(v), vlength(w))",
       25)
    )
    
    runTestCases(cases, prover, "Dependent Vectors")
  }

  def runTestCases(
    cases: List[(String, String, Int)],
    prover: Prover,
    category: String,
    maxLogDepth: Int = 10
  ): Unit = {
    logger.switch(true)
    logger.setMaxDepth(maxLogDepth)
    cases.foreach { case (name, goalStr, depth) =>
      logger.info(s"Case: $name ...")
      logger.time(s"$category: $name") {
        val goal = TestParser.parse(goalStr)
        prover.prove(goal, maxDepth = depth) match {
          case Right(result) =>
            logger.info(s"✓ OK: $name")
          case Left(trace) =>
            logger.warn(s"✗ FAIL: $name - ${trace.reason}")
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
    val prover = Prover(ProverConfig(rules = StandardRules.all, algebras = StandardRules.defaultAlgebras))
    
    val cases = List(
      ("Plus Commutative", "∀n. ∀m. plus(n, m) = plus(m, n)", 20),
      ("Plus Associative", "∀n. ∀m. ∀k. plus(plus(n, m), k) = plus(n, plus(m, k))", 20)
    )
    runTestCases(cases, prover, "Arithmetic")
  }

  def listTests(): Unit = {
    println("=== Phase 1: Advanced Lists ===")
    val prover = Prover(ProverConfig(rules = StandardRules.all, algebras = StandardRules.defaultAlgebras))
    
    val cases = List(
      ("Append Associative", "∀xs. ∀ys. ∀zs. append(append(xs, ys), zs) = append(xs, append(ys, zs))", 20),
      ("Reverse Reverse", "∀xs. reverse(reverse(xs)) = xs", 25),
      ("Map Composition", "∀f. ∀g. ∀xs. map(compose(f, g), xs) = map(f, map(g, xs))", 20)
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
      CatRule("reach_refl",
        App(Sym("reachable"), List(Var("x"), Var("x"))),
        Sym(True),
        List(Var("x"))),
      
      CatRule("reach_edge",
        App(Sym("reachable"), List(Var("x"), Var("y"))),
        App(Sym("edge"), List(Var("x"), Var("y"))),
        List(Var("x"), Var("y"))),
      
      CatRule("reach_trans",
        App(Sym("reachable"), List(Var("x"), Var("z"))),
        App(Sym(And), List(
          App(Sym("reachable"), List(Var("x"), Var("y"))),
          App(Sym("reachable"), List(Var("y"), Var("z")))
        )),
        List(Var("x"), Var("y"), Var("z")))
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
      // bisim(s1, s2) を G(head(s1) = head(s2)) として定義
      CatRule("bisim_is_G",
        App(Sym("bisim"), List(Var("s1"), Var("s2"))),
        App(Sym(Globally), List(App(Sym("="), List(head(Var("s1")), head(Var("s2")))))),
        List(Var("s1"), Var("s2"))),
      
      CatRule("repeat_unfold",
        App(Sym("repeat"), List(Var("x"))),
        App(Sym("cons_stream"), List(Var("x"), App(Sym("repeat"), List(Var("x"))))),
        List(Var("x")))
    )
    
    val config = ProverConfig(
      rules = streamRules ++ StandardRules.all,
      algebras = Nil
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("Stream head of repeat", "∀x. head(repeat(x)) = x", 10),
      ("Stream bisimulation (self)", "∀x. bisim(repeat(x), repeat(x))", 25)
    )
    
    // タイムアウトを30秒に延長
    logger.switch(true)
    cases.foreach { case (name, goalStr, depth) =>
      logger.info(s"Case: $name ...")
      logger.time(s"Streams: $name") {
        val goal = TestParser.parse(goalStr)
        prover.prove(goal, maxDepth = depth, timeoutMs = 30000) match {
          case Right(result) =>
            logger.info(s"✓ OK: $name")
          case Left(trace) =>
            logger.warn(s"✗ FAIL: $name - ${trace.reason}")
        }
      }
    }
  }
  
  private def head(s: Expr) = App(Sym("head"), List(s))
  private def tail(s: Expr) = App(Sym("tail"), List(s))
}

// ==========================================
// メイン実行
// ==========================================

@main def runAdvancedTestsSuite(): Unit = {
  println("=" * 60)
  println("Advanced Test Suite")
  println("=" * 60)
  
  logger.switch(true)
  logger.setLevel(Debug.logger.Level.INFO)
  logger.setMaxDepth(10)
  
  DependentTypeTests.vectorTests()
  println()
  AdvancedInductivePredicateTests.reachabilityTest()
  println()
  CoinductiveTests.streamTest()
  
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
