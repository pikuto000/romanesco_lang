# Zig移植 TODO

Scala実装との比較に基づく未実装・不完全な機能のリスト。

---

## P0 — 安全性・正確性

### 宇宙レベルチェック (`unifier.zig`)
- Scala: `Unifier.scala` L57-77 でType階層の整合性を検証
- `Type(0)` と `Type(1)` のユニフィケーション時にレベルを比較する処理が未実装
- 現状: `Type(0) ∈ Type(0)` のようなRussellのパラドックスを構築できてしまう

### 発散検出 (`search.zig`)
- Scala: `Prover.scala` L400-441 の `isEmbedding()` で無限ループを検出
- ゴールの複雑度が単調増加しているケースを検出して探索を打ち切る処理が未実装
- 現状: 発散するゴールで無限ループになる可能性がある

### グローバルキャッシュ (`search.zig`)
- Scala: `Prover.scala` L24-31 の `globalFailureCache` / `globalLemmaCache` (TrieMap)
- 失敗したゴールと成功した証明をキャッシュして再探索を回避する処理が未実装
- 現状: 同じゴールを反復深化のたびに再証明している（指数的な遅さ）

---

## P1 — 機能充実

### 余帰納法 (`search.zig`)
- Scala: `Prover.scala` L356-376 の `guarded` フラグによる余帰納的証明
- 循環・再帰的な命題（coinductive）を証明する機能が未実装

### ラムダ抽象のユニフィケーション (`unifier.zig`)
- Scala: `Unifier.scala` L46-48 の `Lam(v1, b1)` vs `Lam(v2, b2)` のケース
- α同値なラムダ式をユニファイする処理が未実装

### 標準ルールの追加 (`rules.zig`)
現在 ~25本。Scalaの ~155本に対して以下が不足:

| カテゴリ | Scala | Zig | 不足 |
|---------|-------|-----|------|
| モーダル論理 | 12本 | 1本 | K, 4, 5, duality, dist-tensor 等 |
| 時相論理 | 5本 | 1本 | G展開, U展開, X-step 等 |
| HoTT/Cubical | 11本+ | 1本 | path-inv, univalence, hcomp, 面制約 等 |
| リソース規則 | 6本 | 0本 | file-open/close, memory-alloc, lock 等 |
| ベクタ演算 | 数本 | 0本 | vlength, vhead, vappend 等 |

### ルールインスタンス化キャッシュ (`search.zig` or `rules.zig`)
- Scala: `Rewriter.scala` L11-32 の `RuleInstantiator` オブジェクト
- ルールの変数→メタ変数置換結果をキャッシュする処理が未実装

---

## P2 — プラグイン完全移植

### `plugins/forward.zig` — 前方推論の強化
- Scala: `ForwardReasoningSearch.scala`
- 現状: Modus Ponensのみ
- 不足: ∀除去、∃除去、飽和ループ、基礎項の収集

### `plugins/induction.zig` — 帰納法の完全実装
- Scala: `InductionPlugin.scala`
- 現状: 簡略版（代数的構造への対応が不完全）
- 不足: コンストラクタごとの完全なケース生成、帰納法仮定の正確な追跡

### `plugins/rewrite_plugin.zig` — 書換えプラグインの強化
- Scala: `RewritePlugin.scala`
- 現状: ゴール正規化のみ
- 不足: 量化子削除、パターン変数置換、再帰的 find-and-replace

---

## P2 — Rewriterルール追加 (`rewriter.zig`)

Scala の `Rewriter.scala` には組込み簡約規則が ~100本あるが、Zig版はAC正規化のみ:
- λβ簡約 (`(λx.B)(a)` → `B[x/a]`)
- 圏論公理（既存の rules.zig と統合検討）
- モナド則 (`bind`, `return`, `>>=`)
- ファンクタ則 (`fmap`)
- ベクタ演算簡約

---

## P3 — その他

### 補題のディスク永続化
- Scala: `LemmaManager.scala` の `saveLemmas()` / `loadLemmas()`
- 証明済み補題をタブ区切りテキストで保存・読み込みする機能が未実装

### Sugar/DSL層
- Scala: `Sugar.scala` の演算子オーバーロード (`e1 → e2`, `e1 ∧ e2` 等)
- Zigでは式構築が冗長（`app2(arena, impl, a, b)` のような記述）
- Zigの`comptime`を使った改善を検討
