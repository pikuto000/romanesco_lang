// ==========================================
// 既存テストスイートに追加できるテストケース
// コピー&ペーストで動作します
// ==========================================

package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver._

// ==========================================
// Test 1: フィボナッチ数列
// ==========================================

@main def testFibonacci(): Unit = {
  println("\n=== Fibonacci Sequence Test ===\n")

  val fibRules = List(
    CatRule(
      "fib_0",
      Expr.App(Expr.Sym("fib"), List(Expr.Sym("0"))),
      Expr.Sym("0"),
      Nil
    ),
    CatRule(
      "fib_1",
      Expr.App(
        Expr.Sym("fib"),
        List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
      ),
      Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))),
      Nil
    ),
    CatRule(
      "fib_rec",
      Expr.App(
        Expr.Sym("fib"),
        List(
          Expr.App(
            Expr.Sym("S"),
            List(Expr.App(Expr.Sym("S"), List(Expr.Var("n"))))
          )
        )
      ),
      Expr.App(
        Expr.Sym("plus"),
        List(
          Expr.App(
            Expr.Sym("fib"),
            List(Expr.App(Expr.Sym("S"), List(Expr.Var("n"))))
          ),
          Expr.App(Expr.Sym("fib"), List(Expr.Var("n")))
        )
      ),
      List(Expr.Var("n"))
    )
  )

  val config = ProverConfig(
    rules = fibRules ++ StandardRules.all,
    algebras = List(StandardRules.nat)
  )
  val prover = Prover(config)

  // Test: fib(3) = 2
  val goal = Expr.App(
    Expr.Sym("="),
    List(
      Expr.App(
        Expr.Sym("fib"),
        List(
          Expr.App(
            Expr.Sym("S"),
            List(
              Expr.App(
                Expr.Sym("S"),
                List(
                  Expr.App(Expr.Sym("S"), List(Expr.Sym("0")))
                )
              )
            )
          )
        )
      ),
      Expr.App(
        Expr.Sym("S"),
        List(Expr.App(Expr.Sym("S"), List(Expr.Sym("0"))))
      )
    )
  )

  prover.prove(goal, maxDepth = 15) match {
    case Right(result) =>
      println("✓ Proof found for fib(3) = 2:")
      println(result.tree.format(0))
    case Left(trace) =>
      println(s"✗ Proof failed: ${trace.reason}")
  }
}

// ==========================================
// Test 2: リストのlength保存
// ==========================================

@main def testListLength(): Unit = {
  println("\n=== List Length Preservation Test ===\n")

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

  val config = ProverConfig(
    rules = lengthRules ++ StandardRules.all,
    algebras = List(StandardRules.list, StandardRules.nat)
  )
  val prover = Prover(config)

  // Goal: ∀xs. ∀ys. length(append(xs, ys)) = plus(length(xs), length(ys))
  val goal = Expr.App(
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
  )

  prover.prove(goal, maxDepth = 20) match {
    case Right(result) =>
      println("✓ Proof found:")
      println(result.tree.format(0))
    case Left(trace) =>
      println(s"✗ Proof failed: ${trace.reason}")
  }
}

// ==========================================
// Test 3: Map関数の性質
// ==========================================

@main def testMapFusion2(): Unit = {
  println("\n=== Map Fusion Test ===\n")

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
      "compose_def",
      Expr.App(
        Expr.App(Expr.Sym("compose"), List(Expr.Var("f"), Expr.Var("g"))),
        List(Expr.Var("x"))
      ),
      Expr
        .App(Expr.Var("f"), List(Expr.App(Expr.Var("g"), List(Expr.Var("x"))))),
      List(Expr.Var("f"), Expr.Var("g"), Expr.Var("x"))
    ),
    CatRule(
      "id_def",
      Expr.App(Expr.Sym("id"), List(Expr.Var("x"))),
      Expr.Var("x"),
      List(Expr.Var("x"))
    )
  )

  val config = ProverConfig(
    rules = mapRules,
    algebras = List(StandardRules.list)
  )
  val prover = Prover(config)

  // Goal: ∀xs. map(id, xs) = xs
  val goal = Expr.App(
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
  )

  prover.prove(goal, maxDepth = 15) match {
    case Right(result) =>
      println("✓ Map identity proof found:")
      println(result.tree.format(0))
    case Left(trace) =>
      println(s"✗ Proof failed: ${trace.reason}")
  }
}

// ==========================================
// Test 4: 木のMirror involution
// ==========================================

@main def testTreeMirror(): Unit = {
  println("\n=== Tree Mirror Involution Test ===\n")

  val treeAlgebra = romanesco.Solver.core.InitialAlgebra(
    name = "Tree",
    varPrefix = "t",
    constructors = List(
      ConstructorDef("leaf", Nil, ConstructorType.Point),
      ConstructorDef(
        "node",
        List(ArgType.Recursive, ArgType.Constant, ArgType.Recursive),
        ConstructorType.Point
      )
    )
  )

  val mirrorRules = List(
    CatRule(
      "mirror_leaf",
      Expr.App(Expr.Sym("mirror"), List(Expr.Sym("leaf"))),
      Expr.Sym("leaf"),
      Nil
    ),
    CatRule(
      "mirror_node",
      Expr.App(
        Expr.Sym("mirror"),
        List(
          Expr.App(
            Expr.Sym("node"),
            List(Expr.Var("l"), Expr.Var("x"), Expr.Var("r"))
          )
        )
      ),
      Expr.App(
        Expr.Sym("node"),
        List(
          Expr.App(Expr.Sym("mirror"), List(Expr.Var("r"))),
          Expr.Var("x"),
          Expr.App(Expr.Sym("mirror"), List(Expr.Var("l")))
        )
      ),
      List(Expr.Var("l"), Expr.Var("x"), Expr.Var("r"))
    )
  )

  val config = ProverConfig(
    rules = mirrorRules,
    algebras = List(treeAlgebra)
  )
  val prover = Prover(config)

  // Goal: ∀t. mirror(mirror(t)) = t
  val goal = Expr.App(
    Expr.Sym("∀"),
    List(
      Expr.Var("t"),
      Expr.App(
        Expr.Sym("="),
        List(
          Expr.App(
            Expr.Sym("mirror"),
            List(
              Expr.App(Expr.Sym("mirror"), List(Expr.Var("t")))
            )
          ),
          Expr.Var("t")
        )
      )
    )
  )

  prover.prove(goal, maxDepth = 15) match {
    case Right(result) =>
      println("✓ Mirror involution proof found:")
      println(result.tree.format(0))
    case Left(trace) =>
      println(s"✗ Proof failed: ${trace.reason}")
  }
}

// ==========================================
// Test 5: Even/Odd述語
// ==========================================

@main def testEvenOdd(): Unit = {
  println("\n=== Even/Odd Predicate Test ===\n")

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

  val config = ProverConfig(
    rules = evenOddRules ++ StandardRules.all,
    algebras = List(StandardRules.nat)
  )
  val prover = Prover(config)

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
      5
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
                        List(
                          Expr.App(Expr.Sym("S"), List(Expr.Sym("0")))
                        )
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
      15
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
                    List(
                      Expr.App(Expr.Sym("S"), List(Expr.Sym("0")))
                    )
                  )
                )
              )
            )
          ),
          Expr.Sym("True")
        )
      ),
      15
    )
  )

  tests.foreach { case (name, goal, depth) =>
    print(s"Testing: $name ... ")
    prover.prove(goal, maxDepth = depth) match {
      case Right(_) => println("✓ OK")
      case Left(_)  => println("✗ FAIL")
    }
  }
}

// ==========================================
// Test 6: 乗算の交換律
// ==========================================

@main def testMultiplication(): Unit = {
  println("\n=== Multiplication Commutativity Test ===\n")

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
    // Helper lemmas
    CatRule(
      "times_n_0",
      Expr.App(Expr.Sym("times"), List(Expr.Var("n"), Expr.Sym("0"))),
      Expr.Sym("0"),
      List(Expr.Var("n"))
    ),
    CatRule(
      "times_n_Sm",
      Expr.App(
        Expr.Sym("times"),
        List(
          Expr.Var("n"),
          Expr.App(Expr.Sym("S"), List(Expr.Var("m")))
        )
      ),
      Expr.App(
        Expr.Sym("plus"),
        List(
          Expr.Var("n"),
          Expr.App(Expr.Sym("times"), List(Expr.Var("n"), Expr.Var("m")))
        )
      ),
      List(Expr.Var("n"), Expr.Var("m"))
    )
  )

  val config = ProverConfig(
    rules = timesRules ++ StandardRules.all,
    algebras = List(StandardRules.nat),
    generateLemmas = true // 補題自動生成を有効化
  )
  val prover = Prover(config)

  // Goal: ∀n. ∀m. times(n, m) = times(m, n)
  val goal = Expr.App(
    Expr.Sym("∀"),
    List(
      Expr.Var("n"),
      Expr.App(
        Expr.Sym("∀"),
        List(
          Expr.Var("m"),
          Expr.App(
            Expr.Sym("="),
            List(
              Expr.App(Expr.Sym("times"), List(Expr.Var("n"), Expr.Var("m"))),
              Expr.App(Expr.Sym("times"), List(Expr.Var("m"), Expr.Var("n")))
            )
          )
        )
      )
    )
  )

  prover.prove(goal, maxDepth = 25) match {
    case Right(result) =>
      println("✓ Multiplication commutativity proof found:")
      println(result.tree.format(0))
      result.generatedLemma.foreach(l => println(s"\nGenerated lemma: $l"))
    case Left(trace) =>
      println(s"✗ Proof failed: ${trace.reason}")
  }
}

// ==========================================
// Test 7: Monad左単位律
// ==========================================

@main def testMonadLeftIdentity(): Unit = {
  println("\n=== Monad Left Identity Test ===\n")

  val monadRules = List(
    CatRule(
      "return_list",
      Expr.App(Expr.Sym("return"), List(Expr.Var("x"))),
      Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Sym("nil"))),
      List(Expr.Var("x"))
    ),
    CatRule(
      "bind_nil",
      Expr.App(Expr.Sym("bind"), List(Expr.Sym("nil"), Expr.Var("f"))),
      Expr.Sym("nil"),
      List(Expr.Var("f"))
    ),
    CatRule(
      "bind_cons",
      Expr.App(
        Expr.Sym("bind"),
        List(
          Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Var("xs"))),
          Expr.Var("f")
        )
      ),
      Expr.App(
        Expr.Sym("append"),
        List(
          Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
          Expr.App(Expr.Sym("bind"), List(Expr.Var("xs"), Expr.Var("f")))
        )
      ),
      List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("f"))
    ),
    CatRule(
      "append_nil_r",
      Expr.App(Expr.Sym("append"), List(Expr.Var("xs"), Expr.Sym("nil"))),
      Expr.Var("xs"),
      List(Expr.Var("xs"))
    )
  )

  val config = ProverConfig(
    rules = monadRules ++ StandardRules.all,
    algebras = List(StandardRules.list)
  )
  val prover = Prover(config)

  // Goal: ∀a. ∀f. bind(return(a), f) = f(a)
  val goal = Expr.App(
    Expr.Sym("∀"),
    List(
      Expr.Var("a"),
      Expr.App(
        Expr.Sym("∀"),
        List(
          Expr.Var("f"),
          Expr.App(
            Expr.Sym("="),
            List(
              Expr.App(
                Expr.Sym("bind"),
                List(
                  Expr.App(Expr.Sym("return"), List(Expr.Var("a"))),
                  Expr.Var("f")
                )
              ),
              Expr.App(Expr.Var("f"), List(Expr.Var("a")))
            )
          )
        )
      )
    )
  )

  prover.prove(goal, maxDepth = 15) match {
    case Right(result) =>
      println("✓ Monad left identity proof found:")
      println(result.tree.format(0))
    case Left(trace) =>
      println(s"✗ Proof failed: ${trace.reason}")
  }
}

// ==========================================
// まとめて実行
// ==========================================

@main def runAllAdditionalTests(): Unit = {
  println("=" * 60)
  println("Running All Additional Tests")
  println("=" * 60)

  testFibonacci()
  testListLength()
  testMapFusion2()
  testTreeMirror()
  testEvenOdd()
  testMultiplication()
  testMonadLeftIdentity()

  println("\n" + "=" * 60)
  println("All additional tests completed!")
  println("=" * 60)
}
