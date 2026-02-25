# 計画: rewriter.zig の疎結合化（RewriteHook導入）

## 問題

`rewriter.zig` の `builtinRules()` が全ドメインの簡約規則を直接知っている。

```zig
fn builtinRules(e: *const Expr, arena: Allocator) !*const Expr {
    // transport の積型・余積型分解 (HoTT)
    // hcomp / fill / comp (Cubical)
    // face制約 ∧ᶠ / ∨ᶠ / ¬ᶠ (Cubical)
    // fmap / bind (List/Maybeモナド)
    // vlength / vhead / vcons (ベクタ)
    // ...
}
```

**結果:** 新しい論理体系や代数構造を追加するたびに `rewriter.zig` を直接変更しなければならない。プラグインが自分の簡約規則を「所有」できない。

---

## 解決策: `RewriteHook` 関数ポインタテーブル

### 新しいインターフェース

```zig
// rewriter.zig に追加
pub const RewriteHook = *const fn (
    e: *const Expr,
    arena: Allocator,
) error{OutOfMemory}!?*const Expr;
// null = この規則はマッチしなかった
// non-null = 簡約結果
```

### normalize のシグネチャ変更

```zig
// 新しいメイン関数
pub fn normalizeWithHooks(
    e: *const Expr,
    rules: []const CatRule,
    hooks: []const RewriteHook,
    arena: Allocator,
) error{OutOfMemory}!*const Expr

// 後方互換ラッパー（既存コードを壊さない）
pub fn normalize(
    e: *const Expr,
    rules: []const CatRule,
    arena: Allocator,
) error{OutOfMemory}!*const Expr {
    return normalizeWithHooks(e, rules, &.{}, arena);
}
```

### rewriteRule 内での hooks 適用

```zig
fn rewriteRule(
    e: *const Expr,
    rules: []const CatRule,
    hooks: []const RewriteHook,
    arena: Allocator,
) !*const Expr {
    // ... 既存のユーザ定義ルール適用 ...

    // hooks を順に試す
    for (hooks) |hook| {
        if (try hook(e, arena)) |result| return result;
    }

    return builtinRules(e, arena);  // コアのみ
}
```

---

## builtinRules に残すもの（普遍的コア）

| 規則 | 根拠 |
|------|------|
| β簡約 `(λv. body)(arg) → body[v/arg]` | 型理論の基礎、全体系に共通 |
| `f ∘ id → f`、`id ∘ f → f` | 圏論的基盤（全プラグインが前提） |
| `(f ∘ g) ∘ h → f ∘ (g ∘ h)` | 結合律の正規化 |
| `pi1 ∘ pair(f,g) → f`、`pi2 ∘ pair(f,g) → g` | 積の射影 |
| `A ∧ ⊤ → A`、`A ∧ ⊥ → ⊥` | AC正規化と一体 |
| `A ∨ ⊥ → A`、`A ∨ ⊤ → ⊤` | AC正規化と一体 |
| `A = A → ⊤` | ユニフィケーションと一体 |
| `¬A → A → ⊥` | 全体系で共通の展開 |
| `id(x) → x` | 普遍的 |

---

## 各プラグインへの移動先

### `plugins/hott.zig` → `rewriteHook`

```zig
pub fn rewriteHook(e: *const Expr, arena: Allocator) !?*const Expr {
    if (e.* != .app) return null;
    const hn = e.app.head.sym_or_null() orelse return null;

    // inv(refl) → refl、inv(inv(p)) → p
    if (std.mem.eql(u8, hn, "inv")) return rewriteInv(e, arena);

    // compose/concat(refl, p) → p 等
    if (std.mem.eql(u8, hn, syms.Compose) or
        std.mem.eql(u8, hn, syms.Concat)) return rewriteConcat(e, arena);

    // transport(_, refl, x) → x、積型/余積型分解等
    if (std.mem.eql(u8, hn, syms.Transport)) return rewriteTransport(e, arena);

    return null;
}
```

### `plugins/cubical.zig` → `rewriteHook`

```zig
pub fn rewriteHook(e: *const Expr, arena: Allocator) !?*const Expr {
    if (e.* != .app) return null;
    const hn = e.app.head.sym_or_null() orelse return null;

    // face制約: ∧ᶠ / ∨ᶠ / ¬ᶠ
    if (std.mem.eql(u8, hn, syms.FaceAnd)) return rewriteFaceAnd(e, arena);
    if (std.mem.eql(u8, hn, syms.FaceOr))  return rewriteFaceOr(e, arena);
    if (std.mem.eql(u8, hn, syms.FaceNeg)) return rewriteFaceNeg(e, arena);

    // hcomp / comp / fill
    if (std.mem.eql(u8, hn, syms.HComp)) return rewriteHComp(e, arena);
    if (std.mem.eql(u8, hn, syms.Comp))  return rewriteComp(e, arena);
    if (std.mem.eql(u8, hn, syms.Fill))  return rewriteFill(e, arena);

    return null;
}
```

### `plugins/algebra.zig` → `rewriteHook`

```zig
pub fn rewriteHook(e: *const Expr, arena: Allocator) !?*const Expr {
    // fmap / bind (List/Maybe モナド)
    // vlength / vhead / vcons
    // head / tail of cons_stream
    ...
    return null;
}
```

---

## 利用側（prover.zig / ProverConfig）

```zig
pub const ProverConfig = struct {
    rules: []const CatRule,
    rewrite_hooks: []const rewriter.RewriteHook,  // ← 追加
    // ...
};

// HoTT + Cubical 使用時の構成例
const config = ProverConfig{
    .rules = standard_rules,
    .rewrite_hooks = &.{
        hott.rewriteHook,
        cubical.rewriteHook,
        algebra.rewriteHook,
    },
};
```

---

## 移行手順

### Step 1: インターフェース追加（既存コードを壊さない）

1. `RewriteHook` 型を `rewriter.zig` に追加
2. `normalizeWithHooks()` を実装（`hooks` を受け取る）
3. `normalize()` を `normalizeWithHooks(e, rules, &.{}, arena)` のラッパーにする
4. テスト: 全テストが引き続きパスすること

### Step 2: HoTT規則の切り出し

1. `builtinRules()` から transport/inv/concat 関連を削除
2. `plugins/hott.zig` に `rewriteHook` として移植
3. `ProverConfig` に `rewrite_hooks` フィールド追加
4. HoTTテストがパスすること

### Step 3: Cubical規則の切り出し

1. `builtinRules()` から face制約/hcomp/fill を削除
2. `plugins/cubical.zig` に `rewriteHook` として移植
3. Cubicalテストがパスすること

### Step 4: Algebra/モナド規則の切り出し

1. fmap/bind/vlength 等を `plugins/algebra.zig` へ
2. 全テストパスを確認

### Step 5: builtinRules の最終確認

残るのはコアのみであることを確認し、不要なコメントを整理。

---

## 期待効果

| 項目 | 改善内容 |
|------|---------|
| 疎結合性 | 新論理体系追加時に `rewriter.zig` を変更不要 |
| プラグイン自律性 | 各プラグインが自分の簡約規則を所有・管理 |
| テスタビリティ | 各プラグインのフックを単体テスト可能 |
| 段階的導入 | 後方互換ラッパーにより一気に移行不要 |

---

## 対象ファイル

| ファイル | 変更内容 |
|---------|---------|
| `src/prover/rewriter.zig` | `RewriteHook`型追加、`normalizeWithHooks()`追加、`builtinRules()`をコアのみに縮小 |
| `src/prover/prover.zig` | `ProverConfig` に `rewrite_hooks` フィールド追加 |
| `src/prover/plugins/hott.zig` | `rewriteHook` 関数を追加 |
| `src/prover/plugins/cubical.zig` | `rewriteHook` 関数を追加 |
| `src/prover/plugins/algebra.zig` | `rewriteHook` 関数を追加 |
