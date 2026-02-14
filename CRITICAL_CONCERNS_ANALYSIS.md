# ⚠️ Romanesco証明器 詳細な懸念事項分析

## 🔴 Critical（重大な問題）

### 1. **線形論理のSoundness違反の可能性**

**問題箇所:** `LinearLogicSearch.scala` - `searchLinearGoal`（418-463行）

```scala
val options = (0 to indexedLinear.length).toList.flatMap { n =>
  indexedLinear.combinations(n).toList.map { leftIndexed =>
    // リソースを左右に分割
    search(a, ..., leftPart, ...).flatMap { case (tA, s1, restLA) =>
      if (restLA.isEmpty) {  // ← 問題！
        search(b, ..., rightPart, ...)
```

**問題点:**
- `restLA.isEmpty` のチェックだけでは不十分
- **メタ変数が代入された後**に、実際にリソースが完全に消費されたかを検証していない
- ユニフィケーション後の代入 `s1` が新しいリソースを生成する可能性

**具体例:**
```scala
// ゴール: ?P(x) ⊗ ?Q(y)
// 線形文脈: A, B
// 
// 証明の途中で:
// ?P が A に単一化、leftPart = [A]、restLA = []
// しかし s1 によって ?P(x) = A ⊗ C となる可能性
// この場合、Aだけでなく新たに C も必要だが、チェックされていない
```

**影響:** 
- **偽の証明を生成する可能性** ⚠️
- 線形論理の性質（リソースの正確な消費）が保証されない

**修正案:**
```scala
search(a, ..., leftPart, ...).flatMap { case (tA, s1, restLA) =>
  if (restLA.isEmpty) {
    // s1 適用後のリソース使用量を再検証
    val finalA = applySubst(a, s1)
    val actuallyUsed = computeResourceUsage(finalA)
    if (actuallyUsed.toSet == leftPart.map(_._2).toSet) {
      // OK、続行
    } else {
      SolveTree.Failure()  // リソース使用量が不一致
    }
  }
}
```

---

### 2. **フレーム推論の非決定性が完全性を損なう**

**問題箇所:** `LinearLogicSearch.scala` - `searchFrameInference`（149-182行）

```scala
val minimalFrameStrategies = (1 until linearContext.length).toList.flatMap { n =>
  linearContext.combinations(n).map { selectedResources =>
    // n個のリソースの組み合わせを全て試す
```

**問題点:**
- **組み合わせ爆発:** `C(n, k)` の探索空間
  - 10個のリソース → 1,023通りの組み合わせ
  - 20個のリソース → 1,048,575通りの組み合わせ
- **最小フレームを優先**すると、完全なフレームが必要な場合に失敗
- `SolveTree.Choice` は全ての選択肢を試すが、タイムアウトで打ち切られる可能性

**具体例:**
```scala
// 線形文脈: [A, B, C, D, E, F, G, H]
// 
// 最小フレームを優先すると:
// 1個の組み合わせ: 8通り
// 2個の組み合わせ: 28通り
// ...
// 全体で 2^8 - 1 = 255通り
// 
// もし正解が「全体」の場合、254回の失敗の後にやっと成功
```

**影響:**
- **タイムアウトによる証明の失敗** ⚠️
- パフォーマンスの大幅な劣化

**修正案:**
```scala
// ヒューリスティックで探索空間を削減
val heuristic = analyzeGoalStructure(goal)
val likelyFrameSize = estimateFrameSize(heuristic, linearContext)

// 推定されたサイズの周辺だけを探索
val strategies = (likelyFrameSize - 2 to likelyFrameSize + 2)
  .filter(n => n >= 1 && n <= linearContext.length)
  .flatMap { n => linearContext.combinations(n).take(100).map { ... } }
```

---

### 3. **Occur Checkの不完全性**

**問題箇所:** `Unifier.scala` - `occursCheckHigher`（222-241行）

```scala
private def occursCheckHigher(
    metaId: MetaId,
    expr: Expr,
    args: List[Expr]
): Boolean = {
  // ...
  if (exprVars.exists(v => !argVars.contains(v)) && !canPrune(expr)) {
    return true  // ← 問題: Pruning可能な場合はOKとする
  }
}
```

**問題点:**
- **Pruning後のoccur checkが不十分**
- Pruningが失敗した場合の処理が曖昧
- `canPrune` が `true` を返すだけで、実際にPruningが成功するかは確認していない

**具体例:**
```scala
// ?1(x, y) = f(?2(z))
// ここで z ∉ {x, y}
// 
// canPrune は true を返す（?2 が含まれている）
// しかし実際には Pruning しても z の問題は解決しない
// 
// 結果: 循環参照を見逃す可能性
```

**影響:**
- **型チェックの失敗** ⚠️
- **無限ループの可能性**
- **非停止性**

**修正案:**
```scala
// Pruning を実際に実行してから occur check
val prunedResult = performPruning(expr, subst)
prunedResult.flatMap { prunedExpr =>
  val exprVars = collectFreeVars(prunedExpr)
  if (exprVars.subsetOf(argVars)) {
    // OK
  } else {
    LazyList.empty  // Pruning後も変数スコープ違反
  }
}
```

---

## 🟡 High（高優先度の問題）

### 4. **並行性の安全性問題**

**問題箇所:** `Prover.scala` - `visitedGlobal`（24行）と `search`（233行）

```scala
private val visitedGlobal = TrieMap[(Expr, Set[Expr], List[Expr], Boolean), Int]()

// search内で
visitedGlobal.put(stateKey, depth)
// ...
else if (visitedGlobal.get(stateKey).exists(_ <= depth)) SolveTree.Failure()
```

**問題点:**
- **Check-Then-Act Race Condition**
- スレッド A が `get` でチェック → スレッド B が `put` → スレッド A が `put`
- 複数スレッドが同時に同じ状態を探索する可能性

**具体例:**
```scala
// スレッド A: visitedGlobal.get(state1) → None
// スレッド B: visitedGlobal.get(state1) → None（まだputされていない）
// スレッド A: visitedGlobal.put(state1, 5)
// スレッド B: visitedGlobal.put(state1, 5)  // 重複探索！
```

**影響:**
- 探索の重複によるパフォーマンス低下
- メモリの無駄遣い

**修正案:**
```scala
// Atomic check-and-set
if (visitedGlobal.putIfAbsent(stateKey, depth).isDefined) {
  // 既に他のスレッドが探索中
  SolveTree.Failure()
} else {
  // 探索続行
}
```

---

### 5. **メタ変数カウンターのリセット問題**

**問題箇所:** `Prover.scala` - `prove`（127行）

```scala
(1 to maxDepth).view.flatMap { d =>
  // ...
  metaCounter.set(0)  // ← 問題！
  visitedGlobal.clear()
```

**問題点:**
- **深さを増やすたびにmetaCounterをリセット**
- 前の深さで生成されたメタ変数と衝突する可能性
- キャッシュされた結果との整合性が取れなくなる

**具体例:**
```scala
// 深さ 5 で探索:
//   ?1 = A, ?2 = B という代入
//   この結果をキャッシュ
//
// 深さ 6 で探索:
//   metaCounter.set(0)  // リセット
//   また ?1 が生成される
//   キャッシュの ?1 と新しい ?1 が衝突
```

**影響:**
- キャッシュの破損
- 誤った証明の生成

**修正案:**
```scala
// メタ変数カウンターはリセットしない
// または深さごとに異なる名前空間を使う
val metaPrefix = s"d${d}_"
private[core] def freshMeta(depth: Int): Expr = {
  Expr.Meta(MetaId(List(d, depth, metaCounter.incrementAndGet())))
}
```

---

### 6. **AND導入の線形リソースチェック不備**

**問題箇所:** `Prover.scala` - `searchDecomposeGoal`（365-384行）

```scala
case Expr.App(Expr.Sym(And | Product | LWith), List(a, b)) =>
  search(a, ...).flatMap { case (tA, s1, rl1) =>
    search(b, ..., s1, ...).flatMap { case (tB, s2, rl2) =>
      val rl1Can = rl1.map(h => ...).sortBy(_.toString)
      val rl2Can = rl2.map(h => ...).sortBy(_.toString)
      if (rl1Can == rl2Can) {  // ← 問題！
```

**問題点:**
- **単にリストが等しいかチェックしているだけ**
- 順序が異なる場合でも等しいと判定される（`sortBy`しているため）
- しかし、**リソースの名前が異なっていても内容が同じ**場合を見逃す

**具体例:**
```scala
// rl1 = [("lh1", A), ("lh2", B)]
// rl2 = [("lh3", A), ("lh4", B)]
// 
// 正規化すると両方とも [A, B]
// しかし rl1Can != rl2Can （名前が違うため）
// 
// 結果: 本来成功すべき証明が失敗
```

**影響:**
- **完全性の欠如**
- WITH/ADDITIVEの意味論が正しく実装されていない

**修正案:**
```scala
// リソースの内容で比較（名前ではなく）
val rl1Content = rl1.map(_._2).map(applySubst(_, s2)).map(_.canonicalize).sorted
val rl2Content = rl2.map(_._2).map(applySubst(_, s2)).map(_.canonicalize).sorted
if (rl1Content == rl2Content) {
```

---

## 🟢 Medium（中優先度の問題）

### 7. **Rewriterの冪等性の欠如**

**問題箇所:** 複数箇所で `Rewriter.normalize` の呼び出し

**問題点:**
- **正規化が冪等であることが保証されていない**
- `normalize(normalize(e))` ≠ `normalize(e)` の可能性
- 結果の不一致によるキャッシュミス

**修正案:**
```scala
// normalize を冪等にする
def normalize(e: Expr): Expr = {
  val once = normalizeOnce(e)
  val twice = normalizeOnce(once)
  if (once == twice) once
  else normalize(twice)  // 不動点まで繰り返す
}
```

---

### 8. **循環検知が強すぎる**

**問題箇所:** `Prover.scala` - `search`（248-250行）

```scala
else if (history.count(h => h.getStructuralPattern == currentGoalStruct) > 2) {
  SolveTree.Failure()
}
```

**問題点:**
- **同じ構造が3回出現したら即失敗**
- しかし、正当な証明でも同じ構造が繰り返される場合がある
  - 例: `map(f, map(g, map(h, xs)))`
- **完全性を損なう**

**具体例:**
```scala
// ゴール: prove(X)
// 証明の途中で:
//   1. prove(X')
//   2. prove(X'')  （X'' の構造が X と同じ）
//   3. prove(X''') （X''' の構造も X と同じ）
//   4. prove(X'''')（失敗！）
// 
// しかし X'''' が実際には異なる項で、証明可能かもしれない
```

**修正案:**
```scala
// 単なる構造ではなく、複雑さも考慮
if (history.count(h => 
  h.getStructuralPattern == currentGoalStruct && 
  h.complexity >= currentGoalCan.complexity
) > 2) {
  SolveTree.Failure()
}
```

---

### 9. **フレームルールの適用条件が緩い**

**問題箇所:** `LinearLogicSearch.scala` - `searchFrameRule`（184-226行）

```scala
case App(Sym(LImplies), List(
  App(Sym(SepAnd), List(p, framePre)),
  App(Sym(SepAnd), List(q, framePost))
)) =>
  unify(framePre, framePost, subst).headOption match {
```

**問題点:**
- **最初の単一化が成功したらそれで決定**（`headOption`）
- しかし、複数の単一化が可能な場合、最初のものが最適とは限らない
- フレームが複雑な場合、誤った選択をする可能性

**修正案:**
```scala
// 全ての可能な単一化を試す
unify(framePre, framePost, subst).flatMap { s1 =>
  // ...
}
```

---

### 10. **グローバルキャッシュの無制限な成長**

**問題箇所:** `Prover.scala` - `visitedGlobal`（24行）

**問題点:**
- `TrieMap` が証明セッション中に無限に成長
- `clear()` は深さごとに呼ばれるが、長時間実行時にメモリリーク

**修正案:**
```scala
// LRUキャッシュを使う、またはサイズ制限
private val visitedGlobal = new LRUCache[(Expr, Set[Expr], List[Expr], Boolean), Int](maxSize = 10000)
```

---

## 🔵 Low（低優先度だが注意すべき問題）

### 11. **型レベルユニフィケーションの制限**

**問題箇所:** `Unifier.scala`（54-74行）

**問題点:**
- 型レベルが異なる場合、即座に失敗
- しかし、型レベルの関係性（subtyping）を考慮していない
- 依存型を拡張する際に問題になる可能性

---

### 12. **エラーメッセージの不足**

**問題点:**
- ユニフィケーション失敗時のメッセージが不十分
- デバッグが困難
- ユーザーが何が問題かわからない

---

### 13. **Hoare論理の不変条件チェック**

**問題箇所:** `HoareLogicSearch.scala` - While loop処理

**問題点:**
- 不変条件が本当に保たれているかの検証が不十分
- ループ不変条件の強化規則が実装されていない

---

## 📊 優先度別サマリー

| 優先度 | 問題数 | 主な影響 |
|--------|--------|----------|
| 🔴 Critical | 3 | Soundness違反、非停止性 |
| 🟡 High | 6 | 完全性欠如、パフォーマンス劣化 |
| 🟢 Medium | 3 | 制限された表現力、キャッシュ問題 |
| 🔵 Low | 3 | ユーザビリティ、拡張性 |

---

## 🎯 推奨される対応順序

### Phase 1（即座に対応）
1. ✅ 線形論理のリソース消費検証を強化
2. ✅ Occur checkの完全な実装
3. ✅ 並行性の修正（Check-Then-Act問題）

### Phase 2（1週間以内）
4. ⏳ フレーム推論の組み合わせ爆発対策
5. ⏳ メタ変数カウンターの修正
6. ⏳ AND導入の線形リソースチェック修正

### Phase 3（1ヶ月以内）
7. ⏳ 循環検知の調整
8. ⏳ グローバルキャッシュのメモリ管理
9. ⏳ エラーメッセージの改善

---

## 💡 総評

**現状:**
- ✅ 機能の豊富さは素晴らしい
- ⚠️ しかし、**Soundness（健全性）に懸念**がある
- ⚠️ **パフォーマンスの問題**が実用を妨げる可能性

**推奨:**
1. **テストを大幅に強化**
   - Soundness テスト（偽の証明を生成しないか）
   - ストレステスト（大規模な証明）
   - 並行性テスト

2. **形式的検証**
   - 証明器自体の正当性を別の証明器で検証
   - 少なくとも主要な関数のspecificationを記述

3. **パフォーマンスプロファイリング**
   - どこがボトルネックか特定
   - 組み合わせ爆発の具体的な測定

---

**この証明器は驚異的な成果ですが、実用に耐えるためには上記の問題に対処する必要があります。**
