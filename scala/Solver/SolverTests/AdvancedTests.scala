// ==========================================
// 高度で実用的な追加テストケース
// ==========================================
// 現在のテストスイートをさらに拡充する高度なテストケース集

package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.TestParser
import romanesco.Solver.core.LogicSymbols._

// ==========================================
// Test Suite 1: 依存型の実用例
// ==========================================

object DependentTypeTests {
  
  def vectorTests(): Unit = {
    println("=== Dependent Type: Vector Tests ===")
    
    // ベクトル代数の定義
    val vecAlgebra = InitialAlgebra(
      name = "Vec",
      varPrefix = "v",
      constructors = List(
        ConstructorDef("vnil", Nil, ConstructorType.Point),  // Vec A 0
        ConstructorDef("vcons", 
          List(ArgType.Constant, ArgType.Recursive, ArgType.Constant),
          ConstructorType.Point)  // Vec A (S n)
      )
    )
    
    val vecRules = List(
      // vappend: Vec A n → Vec A m → Vec A (n+m)
      CatRule("vappend_nil",
        Expr.App(Expr.Sym("vappend"), List(Expr.Sym("vnil"), Expr.Var("v"))),
        Expr.Var("v"),
        List(Expr.Var("v"))),
      
      CatRule("vappend_cons",
        Expr.App(Expr.Sym("vappend"), List(
          Expr.App(Expr.Sym("vcons"), List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n"))),
          Expr.Var("ys")
        )),
        Expr.App(Expr.Sym("vcons"), List(
          Expr.Var("x"),
          Expr.App(Expr.Sym("vappend"), List(Expr.Var("xs"), Expr.Var("ys"))),
          Expr.App(Expr.Sym("plus"), List(Expr.Var("n"), Expr.Var("m")))
        )),
        List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n"), Expr.Var("ys"), Expr.Var("m"))),
      
      // vlength: Vec A n → Nat (should return n)
      CatRule("vlength_nil",
        Expr.App(Expr.Sym("vlength"), List(Expr.Sym("vnil"))),
        Expr.Sym("0"),
        Nil),
      
      CatRule("vlength_cons",
        Expr.App(Expr.Sym("vlength"), List(
          Expr.App(Expr.Sym("vcons"), List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n")))
        )),
        Expr.App(Expr.Sym("S"), List(Expr.Var("n"))),
        List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n"))),
      
      // vhead: Vec A (S n) → A (型安全なhead)
      CatRule("vhead_cons",
        Expr.App(Expr.Sym("vhead"), List(
          Expr.App(Expr.Sym("vcons"), List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n")))
        )),
        Expr.Var("x"),
        List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n"))),
      
      // vmap: (A → B) → Vec A n → Vec B n
      CatRule("vmap_nil",
        Expr.App(Expr.Sym("vmap"), List(Expr.Var("f"), Expr.Sym("vnil"))),
        Expr.Sym("vnil"),
        List(Expr.Var("f"))),
      
      CatRule("vmap_cons",
        Expr.App(Expr.Sym("vmap"), List(
          Expr.Var("f"),
          Expr.App(Expr.Sym("vcons"), List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("n")))
        )),
        Expr.App(Expr.Sym("vcons"), List(
          Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
          Expr.App(Expr.Sym("vmap"), List(Expr.Var("f"), Expr.Var("xs"))),
          Expr.Var("n")
        )),
        List(Expr.Var("f"), Expr.Var("x"), Expr.Var("xs"), Expr.Var("n")))
    )
    
    val config = ProverConfig(
      rules = vecRules ++ StandardRules.natPlusRules ++ StandardRules.all,
      algebras = List(vecAlgebra, StandardRules.natAlgebra)
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("Vector length preservation",
       "∀n. ∀m. ∀v. vlength(vappend(v, w)) = plus(n, m)",
       20),
      
      ("Vector map preserves length",
       "∀n. ∀f. ∀v. vlength(vmap(f, v)) = n",
       15),
      
      ("Vector append associativity",
       "∀v1. ∀v2. ∀v3. vappend(vappend(v1, v2), v3) = vappend(v1, vappend(v2, v3))",
       20)
    )
    
    runTestCases(cases, prover, "Dependent Vectors")
  }

  def runTestCases(
    cases: List[(String, String, Int)],
    prover: Prover,
    category: String
  ): Unit = {
    cases.foreach { case (name, goalStr, depth) =>
      print(s"Case: $name ... ")
      val goal = TestParser.parse(goalStr)
      prover.prove(goal, maxDepth = depth) match {
        case Right(result) =>
          println("✓ OK")
        case Left(trace) =>
          println(s"✗ FAIL: ${trace.reason}")
      }
    }
  }
}

// ==========================================
// Test Suite 2: 圏論の性質
// ==========================================

object CategoryTheoryTests {
  import DependentTypeTests.runTestCases

  def functorLawsTest(): Unit = {
    println("=== Category Theory: Functor Laws ===")
    
    val functorRules = List(
      // List Functor
      CatRule("fmap_list_nil",
        Expr.App(Expr.Sym("fmap"), List(Expr.Var("f"), Expr.Sym("nil"))),
        Expr.Sym("nil"),
        List(Expr.Var("f"))),
      
      CatRule("fmap_list_cons",
        Expr.App(Expr.Sym("fmap"), List(
          Expr.Var("f"),
          Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Var("xs")))
        )),
        Expr.App(Expr.Sym("cons"), List(
          Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
          Expr.App(Expr.Sym("fmap"), List(Expr.Var("f"), Expr.Var("xs")))
        )),
        List(Expr.Var("f"), Expr.Var("x"), Expr.Var("xs"))),
      
      // Maybe Functor
      CatRule("fmap_nothing",
        Expr.App(Expr.Sym("fmap"), List(Expr.Var("f"), Expr.Sym("nothing"))),
        Expr.Sym("nothing"),
        List(Expr.Var("f"))),
      
      CatRule("fmap_just",
        Expr.App(Expr.Sym("fmap"), List(
          Expr.Var("f"),
          Expr.App(Expr.Sym("just"), List(Expr.Var("x")))
        )),
        Expr.App(Expr.Sym("just"), List(Expr.App(Expr.Var("f"), List(Expr.Var("x"))))),
        List(Expr.Var("f"), Expr.Var("x"))),
      
      // Identity and composition
      CatRule("id_def",
        Expr.App(Expr.Sym("id"), List(Expr.Var("x"))),
        Expr.Var("x"),
        List(Expr.Var("x"))),
      
      CatRule("compose_def",
        Expr.App(Expr.App(Expr.Sym("compose"), List(Expr.Var("f"), Expr.Var("g"))), List(Expr.Var("x"))),
        Expr.App(Expr.Var("f"), List(Expr.App(Expr.Var("g"), List(Expr.Var("x"))))),
        List(Expr.Var("f"), Expr.Var("g"), Expr.Var("x")))
    )
    
    val config = ProverConfig(
      rules = functorRules ++ StandardRules.all,
      algebras = List(StandardRules.listAlgebra)
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("List Functor identity",
       "∀xs. fmap(id, xs) = xs",
       15),
      
      ("List Functor composition",
       "∀f. ∀g. ∀xs. fmap(compose(f, g), xs) = compose(fmap(f), fmap(g))(xs)",
       20),
      
      ("Maybe Functor identity",
       "∀m. fmap(id, m) = m",
       10),
      
      ("Maybe Functor composition",
       "∀f. ∀g. ∀m. fmap(compose(f, g), m) = fmap(f, fmap(g, m))",
       15)
    )
    
    runTestCases(cases, prover, "Functor Laws")
  }
  
  def monadLawsTest(): Unit = {
    println("=== Category Theory: Monad Laws ===")
    
    val monadRules = List(
      // List Monad
      CatRule("return_list",
        Expr.App(Expr.Sym("return"), List(Expr.Var("x"))),
        Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Sym("nil"))),
        List(Expr.Var("x"))),
      
      CatRule("bind_nil",
        Expr.App(Expr.Sym("bind"), List(Expr.Sym("nil"), Expr.Var("f"))),
        Expr.Sym("nil"),
        List(Expr.Var("f"))),
      
      CatRule("bind_cons",
        Expr.App(Expr.Sym("bind"), List(
          Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Var("xs"))),
          Expr.Var("f")
        )),
        Expr.App(Expr.Sym("append"), List(
          Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
          Expr.App(Expr.Sym("bind"), List(Expr.Var("xs"), Expr.Var("f")))
        )),
        List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("f"))),
      
      // Maybe Monad
      CatRule("return_maybe",
        Expr.App(Expr.Sym("return_maybe"), List(Expr.Var("x"))),
        Expr.App(Expr.Sym("just"), List(Expr.Var("x"))),
        List(Expr.Var("x"))),
      
      CatRule("bind_maybe_nothing",
        Expr.App(Expr.Sym("bind_maybe"), List(Expr.Sym("nothing"), Expr.Var("f"))),
        Expr.Sym("nothing"),
        List(Expr.Var("f"))),
      
      CatRule("bind_maybe_just",
        Expr.App(Expr.Sym("bind_maybe"), List(
          Expr.App(Expr.Sym("just"), List(Expr.Var("x"))),
          Expr.Var("f")
        )),
        Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
        List(Expr.Var("x"), Expr.Var("f")))
    )
    
    val config = ProverConfig(
      rules = monadRules ++ StandardRules.listAppendRules ++ StandardRules.all,
      algebras = List(StandardRules.listAlgebra)
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("List Monad - left identity",
       "∀a. ∀f. bind(return(a), f) = f(a)",
       15),
      
      ("List Monad - right identity",
       "∀m. bind(m, return) = m",
       15),
      
      ("List Monad - associativity",
       "∀m. ∀f. ∀g. bind(bind(m, f), g) = bind(m, λx. bind(f(x), g))",
       25),
      
      ("Maybe Monad - left identity",
       "∀a. ∀f. bind_maybe(return_maybe(a), f) = f(a)",
       10),
      
      ("Maybe Monad - right identity",
       "∀m. bind_maybe(m, return_maybe) = m",
       10)
    )
    
    runTestCases(cases, prover, "Monad Laws")
  }
}

// ==========================================
// Test Suite 3: 高度な帰納的述語
// ==========================================

object AdvancedInductivePredicateTests {
  import DependentTypeTests.runTestCases

  def reachabilityTest(): Unit = {
    println("=== Inductive Predicate: Graph Reachability ===")
    
    val graphRules = List(
      // reachable(x, x) - 反射律
      CatRule("reach_refl",
        Expr.App(Expr.Sym("reachable"), List(Expr.Var("x"), Expr.Var("x"))),
        Expr.Sym(True),
        List(Expr.Var("x"))),
      
      // edge(x, y) → reachable(x, y) - 直接辺
      CatRule("reach_edge",
        Expr.App(Expr.Sym("reachable"), List(Expr.Var("x"), Expr.Var("y"))),
        Expr.App(Expr.Sym("edge"), List(Expr.Var("x"), Expr.Var("y"))),
        List(Expr.Var("x"), Expr.Var("y"))),
      
      // edge(x, y) ∧ reachable(y, z) → reachable(x, z) - 推移律
      CatRule("reach_trans",
        Expr.App(Expr.Sym("reachable"), List(Expr.Var("x"), Expr.Var("z"))),
        Expr.App(Expr.Sym(And), List(
          Expr.App(Expr.Sym("edge"), List(Expr.Var("x"), Expr.Var("y"))),
          Expr.App(Expr.Sym("reachable"), List(Expr.Var("y"), Expr.Var("z")))
        )),
        List(Expr.Var("x"), Expr.Var("y"), Expr.Var("z")))
    )
    
    val config = ProverConfig(
      rules = graphRules ++ StandardRules.all,
      algebras = Nil
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("Reflexivity",
       "∀x. reachable(x, x) = ⊤",
       5),
      
      ("Path of length 2",
       "∀x. ∀y. ∀z. edge(x, y) ∧ edge(y, z) → reachable(x, z)",
       15),
      
      ("Transitivity composition",
       "∀x. ∀y. ∀z. ∀w. reachable(x, y) ∧ reachable(y, z) ∧ reachable(z, w) → reachable(x, w)",
       20)
    )
    
    runTestCases(cases, prover, "Reachability")
  }
  
  def sortedListTest(): Unit = {
    println("=== Inductive Predicate: Sorted Lists ===")
    
    val sortedRules = List(
      // sorted(nil)
      CatRule("sorted_nil",
        Expr.App(Expr.Sym("sorted"), List(Expr.Sym("nil"))),
        Expr.Sym(True),
        Nil),
      
      // sorted(cons(x, nil))
      CatRule("sorted_single",
        Expr.App(Expr.Sym("sorted"), List(
          Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Sym("nil")))
        )),
        Expr.Sym(True),
        List(Expr.Var("x"))),
      
      // x ≤ y ∧ sorted(cons(y, ys)) → sorted(cons(x, cons(y, ys)))
      CatRule("sorted_cons",
        Expr.App(Expr.Sym("sorted"), List(
          Expr.App(Expr.Sym("cons"), List(
            Expr.Var("x"),
            Expr.App(Expr.Sym("cons"), List(Expr.Var("y"), Expr.Var("ys")))
          ))
        )),
        Expr.App(Expr.Sym(And), List(
          Expr.App(Expr.Sym("≤"), List(Expr.Var("x"), Expr.Var("y"))),
          Expr.App(Expr.Sym("sorted"), List(
            Expr.App(Expr.Sym("cons"), List(Expr.Var("y"), Expr.Var("ys")))
          ))
        )),
        List(Expr.Var("x"), Expr.Var("y"), Expr.Var("ys")))
    )
    
    val config = ProverConfig(
      rules = sortedRules ++ StandardRules.all,
      algebras = List(StandardRules.listAlgebra)
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("Empty list is sorted",
       "sorted(nil) = ⊤",
       5),
      
      ("Singleton is sorted",
       "∀x. sorted(cons(x, nil)) = ⊤",
       5),
      
      ("Sorted preserves order",
       "∀x. ∀y. ∀ys. x ≤ y → sorted(cons(y, ys)) → sorted(cons(x, cons(y, ys)))",
       15)
    )
    
    runTestCases(cases, prover, "Sorted Lists")
  }
}

// ==========================================
// Test Suite 4: 共帰納的データ構造
// ==========================================

object CoinductiveTests {
  import DependentTypeTests.runTestCases

  def streamTest(): Unit = {
    println("=== Coinductive: Infinite Streams ===")
    
    val streamRules = List(
      // bisim(s1, s2) if head(s1) = head(s2) ∧ bisim(tail(s1), tail(s2))
      CatRule("bisim_def",
        Expr.App(Expr.Sym("bisim"), List(Expr.Var("s1"), Expr.Var("s2"))),
        Expr.App(Expr.Sym(And), List(
          Expr.App(Expr.Sym("="), List(
            Expr.App(Expr.Sym("head"), List(Expr.Var("s1"))),
            Expr.App(Expr.Sym("head"), List(Expr.Var("s2")))
          )),
          Expr.App(Expr.Sym("bisim"), List(
            Expr.App(Expr.Sym("tail"), List(Expr.Var("s1"))),
            Expr.App(Expr.Sym("tail"), List(Expr.Var("s2")))
          ))
        )),
        List(Expr.Var("s1"), Expr.Var("s2"))),
      
      // repeat(x) = cons_stream(x, repeat(x))
      CatRule("repeat_unfold",
        Expr.App(Expr.Sym("repeat"), List(Expr.Var("x"))),
        Expr.App(Expr.Sym("cons_stream"), List(
          Expr.Var("x"),
          Expr.App(Expr.Sym("repeat"), List(Expr.Var("x")))
        )),
        List(Expr.Var("x"))),
      
      // head(cons_stream(x, s)) = x
      CatRule("head_cons",
        Expr.App(Expr.Sym("head"), List(
          Expr.App(Expr.Sym("cons_stream"), List(Expr.Var("x"), Expr.Var("s")))
        )),
        Expr.Var("x"),
        List(Expr.Var("x"), Expr.Var("s"))),
      
      // tail(cons_stream(x, s)) = s
      CatRule("tail_cons",
        Expr.App(Expr.Sym("tail"), List(
          Expr.App(Expr.Sym("cons_stream"), List(Expr.Var("x"), Expr.Var("s")))
        )),
        Expr.Var("s"),
        List(Expr.Var("x"), Expr.Var("s"))),
      
      // map_stream(f, cons_stream(x, s)) = cons_stream(f(x), map_stream(f, s))
      CatRule("map_stream_cons",
        Expr.App(Expr.Sym("map_stream"), List(
          Expr.Var("f"),
          Expr.App(Expr.Sym("cons_stream"), List(Expr.Var("x"), Expr.Var("s")))
        )),
        Expr.App(Expr.Sym("cons_stream"), List(
          Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
          Expr.App(Expr.Sym("map_stream"), List(Expr.Var("f"), Expr.Var("s")))
        )),
        List(Expr.Var("f"), Expr.Var("x"), Expr.Var("s")))
    )
    
    val config = ProverConfig(
      rules = streamRules ++ StandardRules.all,
      algebras = Nil
    )
    
    val prover = Prover(config)
    
    val cases = List(
      ("Constant stream unfolds",
       "∀x. bisim(repeat(x), cons_stream(x, repeat(x)))",
       15),
      
      ("Stream map fusion",
       "∀f. ∀g. ∀s. bisim(map_stream(compose(f, g), s), map_stream(f, map_stream(g, s)))",
       20),
      
      ("Stream head of repeat",
       "∀x. head(repeat(x)) = x",
       10)
    )
    
    runTestCases(cases, prover, "Streams")
  }
}

// ==========================================
// メイン実行
// ==========================================

@main def runAdvancedTests(): Unit = {
  println("=" * 60)
  println("Advanced Test Suite")
  println("=" * 60)
  println()
  
  DependentTypeTests.vectorTests()
  println()
  
  CategoryTheoryTests.functorLawsTest()
  println()
  
  CategoryTheoryTests.monadLawsTest()
  println()
  
  AdvancedInductivePredicateTests.reachabilityTest()
  println()
  
  AdvancedInductivePredicateTests.sortedListTest()
  println()
  
  CoinductiveTests.streamTest()
  
  println()
  println("=" * 60)
  println("Advanced tests completed!")
  println("=" * 60)
}
