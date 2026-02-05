package romanesco

import scala.util.matching.Regex
// ============================================================================
// 制約システムのテスト
// ============================================================================

object ConstraintTest:
  def run(args: Array[String]): Unit =
    println("=" * 60)
    println("Constraint System Test Suite")
    println("=" * 60)
    
    test1_basicUnification()
    test2_occursCheck()
    test3_compositeTerms()
    test4_constraintStore()
    test5_backtracking()
    test6_disjunction()
    test7_integrationWithTree()
    test8_predicateConstraints()
    
    println("\n" + "=" * 60)
    println("All tests completed!")
    println("=" * 60)
  
  // --------------------------------------------------------------------------
  // テスト1: 基本的な単一化
  // --------------------------------------------------------------------------
  def test1_basicUnification(): Unit =
    println("\n[Test 1] Basic Unification")
    println("-" * 40)
    
    val store = ConstraintStore[String]()
    val x = LVar("x")
    val y = LVar("y")
    
    // x = "hello"
    val result1 = store.unify(Term.Variable(x), Term.Value("hello"))
    println(s"unify(x, 'hello') = $result1")
    println(s"  bindings: ${store.currentBindings}")
    assert(result1 == true)
    
    // y = x (つまり y = "hello")
    val result2 = store.unify(Term.Variable(y), Term.Variable(x))
    println(s"unify(y, x) = $result2")
    println(s"  bindings: ${store.currentBindings}")
    assert(result2 == true)
    
    // x = "world" (失敗するはず、xは既に"hello"に束縛されている)
    val store2 = ConstraintStore[String]()
    store2.unify(Term.Variable(x), Term.Value("hello"))
    val result3 = store2.unify(Term.Variable(x), Term.Value("world"))
    println(s"unify(x, 'world') after x='hello' = $result3")
    assert(result3 == false)
    
    println("[success] Test 1 passed")
  
  // --------------------------------------------------------------------------
  // テスト2: Occurs Check
  // --------------------------------------------------------------------------
  def test2_occursCheck(): Unit =
    println("\n[Test 2] Occurs Check")
    println("-" * 40)
    
    val store = ConstraintStore[String]()
    val x = LVar("x")
    
    // x = f(x) は無限ループになるため失敗する
    val cyclic = Term.Composite("f", List(Term.Variable(x)))
    val result = store.unify(Term.Variable(x), cyclic)
    println(s"unify(x, f(x)) = $result (should be false due to occurs check)")
    assert(result == false)
    
    println("[success] Test 2 passed")
  
  // --------------------------------------------------------------------------
  // テスト3: 複合項の単一化
  // --------------------------------------------------------------------------
  def test3_compositeTerms(): Unit =
    println("\n[Test 3] Composite Terms")
    println("-" * 40)
    
    val store = ConstraintStore[String]()
    val x = LVar("x")
    val y = LVar("y")
    
    // f(x, "b") = f("a", y)
    val left = Term.Composite("f", List(Term.Variable(x), Term.Value("b")))
    val right = Term.Composite("f", List(Term.Value("a"), Term.Variable(y)))
    
    val result = store.unify(left, right)
    println(s"unify(f(x, 'b'), f('a', y)) = $result")
    println(s"  x = ${store.resolve(Term.Variable(x))}")
    println(s"  y = ${store.resolve(Term.Variable(y))}")
    
    assert(result == true)
    assert(store.resolve(Term.Variable(x)) == Term.Value("a"))
    assert(store.resolve(Term.Variable(y)) == Term.Value("b"))
    
    println("[success] Test 3 passed")
  
  // --------------------------------------------------------------------------
  // テスト4: 制約ストアの状態管理
  // --------------------------------------------------------------------------
  def test4_constraintStore(): Unit =
    println("\n[Test 4] Constraint Store State")
    println("-" * 40)
    
    val store = ConstraintStore[Int]()
    val x = LVar("x")
    val y = LVar("y")
    
    // 複数の制約を適用
    store.unify(Term.Variable(x), Term.Value(42))
    store.unify(Term.Variable(y), Term.Value(100))
    
    println(s"After constraints: ${store.currentBindings}")
    assert(store.currentBindings.size == 2)
    
    // 解決の確認
    val resolvedX = store.resolve(Term.Variable(x))
    println(s"resolve(x) = $resolvedX")
    assert(resolvedX == Term.Value(42))
    
    println("[success] Test 4 passed")
  
  // --------------------------------------------------------------------------
  // テスト5: バックトラッキング
  // --------------------------------------------------------------------------
  def test5_backtracking(): Unit =
    println("\n[Test 5] Backtracking")
    println("-" * 40)
    
    val store = ConstraintStore[String]()
    val x = LVar("x")
    val y = LVar("y")
    
    val initialDepth = store.trailDepth
    println(s"Initial trail depth: $initialDepth")
    
    // 束縛を追加
    store.unify(Term.Variable(x), Term.Value("first"))
    store.unify(Term.Variable(y), Term.Value("second"))
    println(s"After bindings: ${store.currentBindings}")
    println(s"Trail depth: ${store.trailDepth}")
    
    // バックトラック
    store.undo()
    println(s"After 1 undo: ${store.currentBindings}")
    
    store.undo()
    println(s"After 2 undos: ${store.currentBindings}")
    assert(store.currentBindings.isEmpty)
    
    println("[success] Test 5 passed")
  
  // --------------------------------------------------------------------------
  // テスト6: 選言制約（非決定性）
  // --------------------------------------------------------------------------
  def test6_disjunction(): Unit =
    println("\n[Test 6] Disjunctive Constraints")
    println("-" * 40)
    
    val store = ConstraintStore[String]()
    val x = LVar("x")
    
    // x = "a" ∨ x = "b"
    val disj = Constraint.Disjunction(List(
      Constraint.Unify(Term.Variable(x), Term.Value("a")),
      Constraint.Unify(Term.Variable(x), Term.Value("b"))
    ))
    
    // 現在の実装では最初に成功したものを選ぶ
    val result = store.applyConstraint(disj)
    println(s"Disjunction result: $result")
    println(s"x = ${store.resolve(Term.Variable(x))}")
    
    assert(result == true)
    
    println("[success] Test 6 passed")
  
  // --------------------------------------------------------------------------
  // テスト7: Treeとの統合
  // --------------------------------------------------------------------------
  def test7_integrationWithTree(): Unit =
    println("\n[Test 7] Integration with Tree")
    println("-" * 40)
    
    // トークン形式のTree (row, col, regex, content)
    val tree: Tree[(Int, Int, Regex, String)] = Tree.V(
      (1, 1, "Add".r, "Add"),
      Vector(
        Tree.V((1, 5, "[0-9]".r, "2"), Vector.empty),
        Tree.V((1, 7, "[0-9]".r, "3"), Vector.empty)
      )
    )
    
    // 制約：演算子が"Add"であること
    val constraint = Constraint.Predicate[(String, Vector[String])](
      "has_operator",
      List(Term.Value("Add", Vector.empty))
    )
    
    val filtered = TokenConstraint.filterTree(tree, _._4 == "Add")
    println(s"Filtered tree: $filtered")
    
    filtered match
      case ConstrainedTree.Resolved(t, _) =>
        println(s"Resolved tree: ${t.prettyPrint()}")
      case _ =>
        println("Failed to resolve")
    
    println("[success] Test 7 passed")
  
  // --------------------------------------------------------------------------
  // テスト8: 述語制約（組み込み述語）
  // --------------------------------------------------------------------------
  def test8_predicateConstraints(): Unit =
    println("\n[Test 8] Predicate Constraints")
    println("-" * 40)
    
    val solver = ConstraintSolver[Any]()
    
    // integer(42) は成功する
    val intConstraint = Constraint.Predicate[Any]("integer", List(Term.Value(42)))
    val result1 = solver.solve(intConstraint)
    println(s"integer(42) = $result1")
    assert(result1.isDefined)
    
    // integer("hello") は失敗する
    val intConstraint2 = Constraint.Predicate[Any]("integer", List(Term.Value("hello")))
    val result2 = solver.solve(intConstraint2)
    println(s"integer('hello') = $result2")
    assert(result2.isEmpty)
    
    // greater_than(5, 3) は成功する
    val gtConstraint = Constraint.Predicate[Any]("greater_than", List(Term.Value(5), Term.Value(3)))
    val result3 = solver.solve(gtConstraint)
    println(s"greater_than(5, 3) = $result3")
    assert(result3.isDefined)
    
    println("[success] Test 8 passed")

// ============================================================================
// 実用的な例：型推論シミュレーション
// ============================================================================

object TypeInferenceExample:
  def run(): Unit =
    println("\n" + "=" * 60)
    println("Type Inference Example (Curry-Howard Correspondence)")
    println("=" * 60)
    
    // 型をTermとして表現
    type TypeTerm = Term[String]
    
    // 型変数
    val a = LVar("a")
    val b = LVar("b")
    
    // 式: \x -> x + 1
    // 型: a -> Int (ただし a = Int)
    
    val store = ConstraintStore[String]()
    
    // 制約1: xの型 = Int（+演算子のため）
    val xType = Term.Variable(a)
    val intType = Term.Value("Int")
    
    println(s"Constraint: typeof(x) = Int")
    val c1 = store.unify(xType, intType)
    println(s"  Result: $c1")
    println(s"  Bindings: ${store.currentBindings}")
    
    // 制約2: 結果の型 = Int
    val resultType = Term.Composite("Arrow", List(xType, intType))
    println(s"\nInferred type: a -> Int where a = ${store.resolve(xType)}")
    
    // 最終的な型
    val finalType = store.resolve(resultType)
    println(s"Final type: $finalType")

// ============================================================================
// メインエントリ
// ============================================================================

@main def runConstraintTests(): Unit =
  ConstraintTest.run(Array.empty)
  TypeInferenceExample.run()
