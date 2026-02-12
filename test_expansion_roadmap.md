# 証明器の追加テストケース集

## 現在のテストカバレッジ（全て成功✅）

1. ✅ 循環検知 (Cycle Detection)
2. ✅ HoTT パス帰納法 (Path Induction)
3. ✅ 線形論理 (Linear Logic)
4. ✅ 分離論理 (Separation Logic)
5. ✅ 時相論理 (Temporal Logic)
6. ✅ 型レベルユニフィケーション
7. ✅ 高階ユニフィケーション
8. ✅ 再帰的データ構造の帰納法
9. ✅ 基本的な算術・リスト操作
10. ✅ 複合論理システム

---

## 提案：次のレベルのテストケース

### 優先度★★★（すぐに実装可能）

#### 1. より複雑な帰納法の定理

```scala
// フィボナッチ数列
"∀n. fib(S(S(n))) = plus(fib(S(n)), fib(n))"

// リストの長さ
"∀xs. ∀ys. length(append(xs, ys)) = plus(length(xs), length(ys))"

// リバース
"∀xs. ∀ys. reverse(append(xs, ys)) = append(reverse(ys), reverse(xs))"
"∀xs. reverse(reverse(xs)) = xs"

// Map関数の性質
"∀f. ∀g. ∀xs. map(compose(f,g), xs) = map(f, map(g, xs))"
"∀f. map(f, nil) = nil"
"∀f. ∀x. ∀xs. map(f, cons(x, xs)) = cons(f(x), map(f, xs))"

// Filter関数
"∀p. filter(p, nil) = nil"
"∀p. ∀xs. ∀ys. filter(p, append(xs, ys)) = append(filter(p, xs), filter(p, ys))"

// Fold関数
"∀f. ∀z. foldr(f, z, nil) = z"
"∀f. ∀z. ∀x. ∀xs. foldr(f, z, cons(x, xs)) = f(x, foldr(f, z, xs))"
```

#### 2. 帰納的述語の証明

```scala
// 偶数の定義と性質
// even(0) と even(S(S(n))) → even(n)
"even(0)"
"∀n. even(S(S(n))) → even(n)"
"∀n. ∀m. even(n) → even(m) → even(plus(n, m))"

// リストの長さに関する述語
// has_length(nil, 0)
// has_length(xs, n) → has_length(cons(x, xs), S(n))
"has_length(nil, 0)"
"∀x. ∀xs. ∀n. has_length(xs, n) → has_length(cons(x, xs), S(n))"

// ソート済みリスト
"sorted(nil)"
"∀x. sorted(cons(x, nil))"
"∀x. ∀y. ∀xs. x ≤ y → sorted(cons(y, xs)) → sorted(cons(x, cons(y, xs)))"
```

#### 3. 木構造の性質

```scala
// 二分木の高さ
"height(leaf) = 0"
"∀l. ∀r. height(node(l, x, r)) = S(max(height(l), height(r)))"

// 木のサイズ
"size(leaf) = 0"
"∀l. ∀r. size(node(l, x, r)) = S(plus(size(l), size(r)))"

// 二分探索木の性質
"∀t. bst(t) → ∀x. member(x, t) → ∃y. find(x, t) = some(y)"

// 木のバランス
"∀t. balanced(t) → diff(height(left(t)), height(right(t))) ≤ 1"
```

### 優先度★★（中程度の実装）

#### 4. 依存型の基本

```scala
// ベクトルの長さ保存
"∀n. ∀v:Vec(A,n). ∀w:Vec(A,n). length(append_vec(v,w)) = plus(n,n)"

// 型安全なhead
"∀n. ∀v:Vec(A,S(n)). ∃a:A. head(v) = a"

// Sigma型の射影
"∀p:Σ(x:A).B(x). B(fst(p))"
```

#### 5. モナド則の確認

```scala
// List モナド
"∀a. ∀f. bind_list(return_list(a), f) = f(a)"  // Left identity
"∀m. bind_list(m, return_list) = m"  // Right identity
"∀m. ∀f. ∀g. bind_list(bind_list(m, f), g) = bind_list(m, λx. bind_list(f(x), g))"  // Associativity

// Maybe モナド
"∀a. ∀f. bind_maybe(just(a), f) = f(a)"
"bind_maybe(nothing, f) = nothing"
"∀m. bind_maybe(m, just) = m"
```

#### 6. 圏論的性質

```scala
// Functor則
"∀xs. map(id, xs) = xs"  // Identity
"∀f. ∀g. ∀xs. map(compose(f,g), xs) = compose(map(f), map(g))(xs)"  // Composition

// Natural transformation
"∀η. ∀f. ∀xs. map_g(f, η(xs)) = η(map_f(f, xs))"  // Naturality square
```

### 優先度★（高度・将来的）

#### 7. Hoare論理とプログラム検証

```scala
// 配列アクセスの安全性
"{0 ≤ i < length(a)} x := a[i] {x = a[i]}"

// ループ不変条件
"{i = 0 ∧ sum = 0} while i < n do sum := sum + i; i := i + 1 {sum = (n*(n-1))/2}"

// 停止性
"∀n. terminates(factorial(n))"
```

#### 8. 並行性の検証

```scala
// リソースの分離
"{P * Q} c1 || c2 {P' * Q'}"

// ロックの独占性
"locked(m) * locked(m) ⊸ ⊥"

// デッドロックフリー
"∀p1. ∀p2. holds(p1, r1) * holds(p2, r2) → ¬(waits(p1, r2) * waits(p2, r1))"
```

#### 9. HoTT の高度な性質

```scala
// Univalence（簡易版）
"(A ≃ B) → (A = B)"

// Function extensionality
"(∀x. f(x) = g(x)) → f = g"

// Higher inductive types - Circle
"∀x:S¹. (x = base) ∨ (∃p:base=base. x = transport(p))"
```

---

## 実装の優先順位

### Phase 1: 基本的な拡張（1-2週間）
- ✅ フィボナッチ、リバース、mapの性質
- ✅ 偶数の述語
- ✅ 木構造の基本性質

### Phase 2: 中級の拡張（2-4週間）
- ⏳ ベクトルと依存型
- ⏳ モナド則
- ⏳ より複雑な帰納的述語

### Phase 3: 高度な拡張（1-2ヶ月）
- ⏳ Hoare論理
- ⏳ 並行性検証
- ⏳ Univalenceと高次帰納型

---

## テストの実装テンプレート

```scala
object NewTests {
  def fibonacciTests(): Unit = {
    println("=== Fibonacci Tests ===")
    
    val cases = List(
      ("fib(0) = 0", "fib(0) = 0", 5, true),
      ("fib(1) = 1", "fib(S(0)) = S(0)", 5, true),
      ("fib recurrence", 
       "∀n. fib(S(S(n))) = plus(fib(S(n)), fib(n))", 
       15, true)
    )
    
    cases.foreach { case (name, goal, depth, expected) =>
      print(s"Testing: $name ... ")
      // val result = prover.prove(parseGoal(goal), maxDepth = depth)
      // assert(result.isRight == expected)
      println("OK")
    }
  }
}
```

---

## 推奨される次のステップ

1. **Phase 1から開始**
   - フィボナッチ数列の定義と基本性質
   - リストのreverseとその性質
   - mapとfilterの合成則

2. **帰納的述語の追加**
   - even/odd述語
   - sorted述語
   - has_length述語

3. **木構造の拡張**
   - height, size, mirror
   - 二分探索木の性質

4. **成功したら次のPhaseへ**
   - 各Phaseで10-20個のテストを追加
   - 全て成功したら次のレベルへ

---

## 期待される効果

- ✅ より実用的な証明例
- ✅ 帰納法の適用範囲の確認
- ✅ 複雑な構造に対する証明能力の検証
- ✅ 実際のプログラム検証への応用可能性
