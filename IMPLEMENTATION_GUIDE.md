# 証明器テストスイート拡張ガイド

## 🎉 現在の状況

**全テスト成功！おめでとうございます！**

現在実装されているテスト：
- ✅ 基本的な帰納法（自然数・リスト）
- ✅ HoTT（パス帰納法・キューブ近似）
- ✅ 線形論理（リソース消費）
- ✅ 分離論理（ヒープ管理）
- ✅ 時相論理（余帰納法）
- ✅ 型レベルユニフィケーション
- ✅ 高階ユニフィケーション
- ✅ 複合論理システム

---

## 📦 提供したファイル

### 1. `ReadyToUseTests.scala` ⭐ **推奨：最初に試す**
**すぐに動作する完全なテスト集**

含まれるテスト：
- フィボナッチ数列（基本ケース + 漸化式）
- リストのlength保存
- Map関数の恒等律
- 木のMirror involution
- Even/Odd述語
- 乗算の交換律
- Monad左単位律

**使い方：**
```bash
# プロジェクトルートに配置
cp ReadyToUseTests.scala C:\Users\Cocoa\romanesco\scala\

# 実行（個別）
sbt "run romanesco.Solver.testFibonacci"
sbt "run romanesco.Solver.testListLength"
sbt "run romanesco.Solver.testMapFusion"

# 全て実行
sbt "run romanesco.Solver.runAllAdditionalTests"
```

### 2. `IntegratedTests.scala` ⭐ **推奨：既存構造に統合**
**既存のSolverTestsパッケージに統合できる形式**

含まれるテスト：
- 高度なリスト操作（length, map）
- 算術性質（乗算）
- 帰納的述語（even/odd）

**使い方：**
```bash
# SolverTestsディレクトリに配置
cp IntegratedTests.scala C:\Users\Cocoa\romanesco\scala\Solver\SolverTests\

# 実行
sbt "run romanesco.Solver.SolverTests.runPhase1AdditionalTests"
```

### 3. `Phase1Tests.scala`
**より包括的なテスト集（実装参考用）**

含まれるテスト：
- フィボナッチ数列
- 高度なリスト操作（map, filter, flatten）
- 木構造（height, size, mirror）
- 帰納的述語
- Monad則
- 算術性質

### 4. `test_expansion_roadmap.md`
**段階的な拡張計画とテストアイデア集**

---

## 🚀 実装の推奨手順

### Step 1: すぐに試す（5-10分）

```bash
# ReadyToUseTests.scalaをコピー
cp ReadyToUseTests.scala C:\Users\Cocoa\romanesco\scala\

# フィボナッチテストを実行
sbt "run romanesco.Solver.testFibonacci"

# Map恒等律テストを実行
sbt "run romanesco.Solver.testMapFusion"

# Tree Mirrorテストを実行
sbt "run romanesco.Solver.testTreeMirror"
```

**期待される出力：**
```
=== Fibonacci Sequence Test ===

✓ Proof found for fib(3) = 2:
[証明木が表示される]

=== Map Fusion Test ===

✓ Map identity proof found:
[証明木が表示される]
```

### Step 2: 既存テストに統合（10-20分）

```bash
# IntegratedTests.scalaをSolverTestsディレクトリに配置
cp IntegratedTests.scala C:\Users\Cocoa\romanesco\scala\Solver\SolverTests\

# 実行
sbt "run romanesco.Solver.SolverTests.runPhase1AdditionalTests"
```

### Step 3: カスタマイズと拡張（自由に）

提供したテストを元に、自分のテストを追加：

```scala
// 例：独自のデータ構造
val myAlgebra = AlgebraDef(
  name = "MyType",
  varPrefix = "x",
  constructors = List(...)
)

// 例：独自のルール
val myRules = List(
  CatRule("my_rule", lhs, rhs, universals)
)
```

---

## 📋 推奨される次のテストケース

### 優先度 ★★★（すぐに実装可能）

#### 1. より複雑なフィボナッチ性質
```scala
// Cassini's identity: fib(n-1)*fib(n+1) - fib(n)² = (-1)ⁿ
// (難易度: 高)
```

#### 2. リストのflattening
```scala
// flatten(map(f, xss)) = map(f, flatten(xss))
// flatten . map (map f) = map f . flatten
```

#### 3. 木の高さとサイズの関係
```scala
// ∀t. size(t) ≥ height(t)
// balanced(t) → size(t) ≥ 2^(height(t)) - 1
```

#### 4. ソート済みリスト
```scala
sorted(nil)
sorted(cons(x, nil))
∀x y xs. x ≤ y → sorted(cons(y, xs)) → sorted(cons(x, cons(y, xs)))
```

### 優先度 ★★（中程度の難易度）

#### 5. Monad結合律
```scala
// bind(bind(m, f), g) = bind(m, λx. bind(f(x), g))
// (既存のbind実装に依存)
```

#### 6. 依存型の基本
```scala
// ベクトルの長さ保存
// ∀n. ∀v:Vec(A,n). ∀w:Vec(A,m). length(append(v,w)) = n+m
```

#### 7. 到達可能性述語
```scala
reachable(x, x)  // 反射律
edge(x, y) → reachable(y, z) → reachable(x, z)  // 推移律
```

### 優先度 ★（長期的・研究的）

#### 8. Univalence（簡易版）
```scala
// (A ≃ B) → path(Type, A, B)
```

#### 9. 並行性検証
```scala
// {P * Q} c1 || c2 {P' * Q'}
```

#### 10. Hoare論理
```scala
// {P} c {Q}
// {P[e/x]} x:=e {P}
```

---

## 🎯 実装時のヒント

### ヒント1: 小さく始める

最初は単純なケースから：
```scala
// ❌ いきなり複雑な定理
"∀n. fib(n)² + fib(n+1)² = fib(2n+1)"

// ✅ まず基本ケース
"fib(0) = 0"
"fib(1) = 1"
"fib(2) = 1"
```

### ヒント2: 補題を活用

複雑な証明は補題に分解：
```scala
val config = ProverConfig(
  rules = baseRules ++ lemmaRules,
  generateLemmas = true  // 自動補題生成を有効化
)
```

### ヒント3: 深さ制限を調整

証明が見つからない場合：
```scala
// 深さを増やす
prover.prove(goal, maxDepth = 25)  // デフォルト: 30

// タイムアウトを延長
prover.prove(goal, timeoutMs = 30000)  // デフォルト: 15000
```

### ヒント4: ログを活用

デバッグ時：
```scala
// Debug.scalaで有効化
logger.switch(true)
logger.setMaxDepth(5)  // ログ出力の深さ
```

---

## 📊 期待される成果

### Phase 1完了時（1-2週間）
- ✅ 10-15個の新しいテストケース
- ✅ フィボナッチ、リスト高度操作、木構造
- ✅ 帰納的述語（even/odd, sorted）

### Phase 2完了時（2-4週間）
- ✅ 30-40個のテストケース
- ✅ Monad則完全実装
- ✅ 依存型の基本
- ✅ より複雑な帰納的証明

### Phase 3完了時（1-2ヶ月）
- ✅ 50+個のテストケース
- ✅ プログラム検証の基本
- ✅ 並行性検証
- ✅ HoTTの高度な性質

---

## 🐛 トラブルシューティング

### 問題: 証明が見つからない

**解決策：**
1. `maxDepth`を増やす（15 → 20 → 25）
2. 補題ルールを追加
3. ログを有効化して失敗箇所を確認

### 問題: コンパイルエラー

**解決策：**
1. パッケージ宣言を確認
2. import文を確認
3. `sbt clean compile`で再コンパイル

### 問題: 証明が遅い

**解決策：**
1. `generateLemmas = true`で補題を自動生成
2. よく使うルールをルールリストの前方に配置
3. `maxComplexity`を制限

---

## 📚 参考資料

### 既存のテストから学ぶ
```bash
# 現在のテストを確認
ls C:\Users\Cocoa\romanesco\scala\Solver\SolverTests\
cat C:\Users\Cocoa\romanesco\scala\Solver\SolverTests\CycleTest.scala
```

### ドキュメント
- `proof_system_analysis.md` - 問題分析と修正内容
- `modification_summary.md` - 実装した修正のまとめ

---

## ✅ チェックリスト

### 準備
- [ ] `ReadyToUseTests.scala`を配置
- [ ] sbtでコンパイル成功を確認
- [ ] 既存テストが全て成功することを確認

### Phase 1実装
- [ ] フィボナッチテストを実行
- [ ] Map恒等律テストを実行
- [ ] Tree Mirrorテストを実行
- [ ] Even/Oddテストを実行
- [ ] 独自のテストを1つ追加

### Phase 2実装（オプション）
- [ ] Monad則テストを追加
- [ ] 乗算の性質テストを拡充
- [ ] ソート済みリスト述語を実装

---

## 🎓 次のステップ

1. **今すぐ試す**: `ReadyToUseTests.scala`を実行
2. **理解する**: 動作したテストのコードを読む
3. **カスタマイズ**: 自分のテストケースを追加
4. **共有する**: 成功したテストを報告

**Happy Proving! 🎉**
