# romanesco

多論理体系対応の定理証明器と、その証明から生成されるコードを実行するJITランタイム。

Scala 3（定理証明エンジン）とZig（実行ランタイム）の2言語で構成されており、外部ライブラリ依存なし。

---

## アーキテクチャ概要

```
証明ゴール（ユーザ入力）
        ↓
  Scala 証明エンジン
  （プラグインベース探索）
        ↓ バイトコード生成
  Zig VM（Tier 0 インタプリタ）
        ↓ プロファイリングデータ
  投機的JIT（Tier N）
        ↓ 解析・最適化
  コード生成 → LLVM IR
        ↓ clang -O3
  ネイティブコード（.dll/.so）
        ↓
    実行結果
```

---

## コンポーネント

### Scala 証明エンジン（`scala/`）

カテゴリ理論のCurry-Howard-Lambek対応を基盤とした証明探索エンジン。

| ファイル | 役割 |
|---------|------|
| `Solver/Core.scala` | AST定義（`Expr`: Var/Meta/Sym/App）、Goal、SearchNode |
| `Solver/Prover.scala` | 反復深化探索、サイクル/発散検出、失敗キャッシュ、補題キャッシュ |
| `Solver/Unifier.scala` | メタ変数ユニフィケーション（宇宙階層チェック付き） |
| `Solver/Rewriter.scala` | AC正規化、冪等性、書き換えキャッシュ |
| `Solver/Repl.scala` | インタラクティブ証明シェル（undo/abort対応） |

**対応論理体系（プラグイン、15種）:**

| プラグイン | 論理体系 |
|-----------|---------|
| StandardPlugins | 古典論理・直観主義論理（EM/DNE対応） |
| LinearLogicSearch | 線形論理・分離論理（フレーム推論付き） |
| HoareLogicSearch | Hoare論理（skip/assign/seq/if/while） |
| HoTTSearch | ホモトピー型理論（path induction、HIT） |
| CubicalPlugin | Cubical型理論（hcomp/transport、face制約） |
| ModalLogicSearch | 様相論理（◇/□演算子） |
| TemporalLogicSearch | 時相論理（Globally/Finally/Until） |
| InductionPlugin | 帰納法・余帰納法 |
| AlgebraPlugin | 代数的構造（自然数、リスト、木） |
| ForwardReasoningSearch | 前向き連鎖推論 |
| PersistentLogicSearch | 永続リソース追跡 |

### Zig ランタイム（`Zig/src/Runtime/`）

型無しビット列設計のレジスタVMとJITコンパイルスタック。

```
Value = union { bits: u64, wide: []u64, pair, inl, inr, closure, unit }
                    ↑
       int/float/pointerを型区別せず生ビット列として保持
```

| ファイル | 役割 |
|---------|------|
| `vm.zig` | レジスタVM本体、bigint演算（128bit+）、ProfilingVM |
| `analyzer.zig` | レンジ解析・タグ型推論（bits/wide/unit/pair/sum/closure） |
| `optimizer.zig` | 定数畳み込み・DCE・レジスタ割り当て（Arena化済み） |
| `codegen.zig` | LLVM IR生成、SIMD intrinsics（AVX2/Neon）、ガード省略最適化 |
| `lifter.zig` | LLVM IR → VMバイトコード変換（CFG構築） |
| `decompiler.zig` | LLVM IRからOp命令を逆コンパイル |
| `loader.zig` | バイトコードバイナリ形式のロード（RBC形式） |
| `ir_parser.zig` | LLVM IR構造パーサ |
| `jit.zig` | clang経由JITコンパイル・ネイティブ関数リンク |
| `aot.zig` | スタンドアロン実行可能ファイルのAOTコンパイル |
| `speculative.zig` | 投機的ティア実行エンジン（deoptimization対応） |
| `tests.zig` | 統合テストスイート（51テスト全パス） |

### Zig 証明エンジン移植（`Zig/src/prover/`）

Scalaプローバーをそのままパフォーマンス重視でZigに移植したもの。
推論ルール70本超、プラグイン15種。

---

## ビルドと実行

### Scala（定理証明器）

```bash
# ビルド
sbt compile

# 全テスト実行
.\run.ps1

# 単一テスト
sbt "runMain romanesco.Solver.SolverTests.HoTTTest"

# インタラクティブREPL
.\run.ps1 repl
```

### Zig（ランタイム）

```bash
cd Zig

# テスト（全51ケース）
zig test src/Runtime/tests.zig

# ビルド
zig build
```

---

## 実装済み機能

### 証明探索

- 反復深化探索（深さ制限BFS）
- サイクル検出・発散検出（`isEmbedding`によるゴール埋め込みチェック）
- 失敗キャッシュ（集合包含関係を考慮）・補題キャッシュ
- ウォッチドッグタイムアウト

### 論理体系

- 古典論理（排中律・二重否定除去）
- 直観主義論理（introduction/elimination規則）
- 線形論理・分離論理（フレーム推論）
- Hoare論理（プログラム検証）
- 様相論理・時相論理
- HoTT（パス型・等号・higher inductive type）
- Cubical型理論（hcomp/transport、De Morgan面制約）
- 帰納法・余帰納法

### ランタイム

- 型無しレジスタVM（int/float/pointerを `bits: u64` で統一）
- 多倍長整数演算（128bit以上、加減乗除・ビット演算・比較・シフト）
- 投機的JIT（tiered execution、ガード付きdeopt）
- AOTコンパイル（スタンドアロンバイナリ生成）
- SIMD対応コード生成（AVX2/Neon自動検出）
- プロファイル誘導最適化（呼び出しターゲット推論、安定値推論）

### バイトコード最適化

- 定数畳み込み（ArenaAllocatorで中間状態を一括管理）
- デッドコード除去
- レジスタ割り当て最適化
- エスケープ解析（スタック割り当て最適化）
- タグ型推論によるランタイムガード省略（`isProvenBits`）

---

## 開発状況

| カテゴリ | 状況 |
|---------|------|
| Scalaソース | 27ファイル・約4,600行 |
| Zigソース | 46ファイル・約12,200行 |
| テスト（Scala） | 40テストファイル |
| テスト（Zig） | 51ケース・全パス |
| 推論ルール | 70本超 |
| VMオペコード | 30種超 |
