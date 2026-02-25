# 汎用 LLVM IR → Romanesco バイトコード変換器 実装計画

作成日: 2026-02-25

## 概要

現在の `decompiler.zig` は Romanesco 自身の `codegen.zig` が生成した IR パターンにのみ対応している。
本計画では、C/Rust 等の他言語コンパイラが生成した **汎用 LLVM IR** を Romanesco バイトコード (`LoadedProgram`) に変換する
`lifter.zig` を新たに実装することを目的とする。

---

## フェーズ一覧

| # | フェーズ | 内容 | 依存 |
|---|---|---|---|
| 0 | 共通 IR パーサー | `ir_parser.zig` の実装・decompiler の移行 | なし |
| 1 | VM 拡張 | 新 opcode・Value 型の追加 | なし |
| 2 | バイトコードローダー拡張 | 新 opcode のシリアライズ対応 | フェーズ1 |
| 3 | Lifter 本体 | LLVM IR → バイトコード変換器 | フェーズ0・1 |
| 4 | JIT/AOT 統合 | Lifter を既存パイプラインに接続 | フェーズ3 |
| 5 | 最適化器拡張 | 新 opcode を optimizer.zig に対応 | フェーズ1 |

---

## フェーズ 0: 共通 IR パーサー (`ir_parser.zig`)

### 目的

`decompiler.zig` と `lifter.zig` が共通して必要とする IR テキストの字句解析・構造分解を
独立モジュールとして実装する。既存の `decompiler.zig` もこれを利用するよう移行する。

### 公開 API

```zig
/// LLVM IR テキストを関数単位に分解したもの
pub const ParsedModule = struct {
    functions: []ParsedFunction,

    pub fn deinit(self: *ParsedModule, allocator: Allocator) void
};

/// 1つの関数定義
pub const ParsedFunction = struct {
    name: []const u8,           // "factorial", "__block_1" 等
    params: []Param,            // 引数 (型・レジスタ名)
    ret_type: []const u8,       // "i64", "void", "double" 等
    blocks: []ParsedBlock,      // 基本ブロックの配列
    is_declare: bool,           // declare（外部宣言）か define か
};

/// 1つの基本ブロック
pub const ParsedBlock = struct {
    label: []const u8,          // "entry", "loop", "exit" 等
    instructions: []Instruction,
};

/// 1つの命令（トークン分解済み）
pub const Instruction = struct {
    result: ?[]const u8,        // "%x" など（void 命令は null）
    opcode: []const u8,         // "add", "call", "br", "phi" 等
    operands: [][]const u8,     // トークン列（型・値・ラベル）
    raw: []const u8,            // 元のテキスト行（デバッグ用）
};

pub const Param = struct {
    type_name: []const u8,
    name: []const u8,
};

pub const IRParser = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) IRParser

    /// IR テキスト全体をパースして ParsedModule を返す
    pub fn parse(self: *IRParser, ir_text: []const u8) !ParsedModule
};
```

### decompiler.zig の移行方針

- 現在 `decompiler.zig` 内にある行分割・関数境界検出・ラベル認識のロジックを `ir_parser.zig` に移行
- `decompiler.zig` は `IRParser.parse()` の結果を受け取り、`@rt_*` パターンマッチングに専念する構成に変更
- 外部インターフェース（`Decompiler.decompile()`）は変更しない

---

## フェーズ 1: VM 拡張 (`vm.zig`)

### 1-1. Value 型への `float` 追加

```zig
pub const ValueTag = enum(u64) {
    closure = 1,
    pair    = 2,
    inl     = 3,
    inr     = 4,
    unit    = 5,
    int     = 6,
    float   = 7,   // 追加: f64 の IEEE 754 ビットパターンを u64 で保持
};

pub const Value = union(ValueTag) {
    // ...既存フィールド...
    float: u64,    // @bitCast(f64) した値
};
```

**方針**: `f64` をソフトウェア実装せず、`@bitCast` でビットパターンを `u64` に変換して保持する。
演算時は `@bitCast` で `f64` に戻してCPUのFPUを利用する。コストは実質ゼロ。

**FPU 非搭載環境について**: `u64` によるビットパターン保持はアーキテクチャ非依存であるため、
値の格納・転送は整数操作のみで完結する。実際の演算（`lf + rf` 等の Zig コード）を
FPU なしターゲット向けにコンパイルすると、Zig（LLVM バックエンド）が自動的に
`compiler-rt` のソフトウェア float ルーチン（`__adddf3`、`__subdf3` 等）へ置き換える。
JIT/AOT で生成される LLVM IR 中の `fadd double` 命令も、clang がターゲットに応じて
同様に処理するため、**明示的な対処は不要**。
ただしソフトウェア float はハードウェア FPU に比べて数十〜数百倍遅くなる点に注意。

### 1-2. Op への新 opcode 追加

#### 整数比較（結果は `inl`=真 / `inr`=偽 の Value になる）

```zig
icmp_eq:  struct { dst: u32, lhs: u32, rhs: u32 },  // ==
icmp_ne:  struct { dst: u32, lhs: u32, rhs: u32 },  // !=
icmp_slt: struct { dst: u32, lhs: u32, rhs: u32 },  // signed <
icmp_sle: struct { dst: u32, lhs: u32, rhs: u32 },  // signed <=
icmp_sgt: struct { dst: u32, lhs: u32, rhs: u32 },  // signed >
icmp_sge: struct { dst: u32, lhs: u32, rhs: u32 },  // signed >=
icmp_ult: struct { dst: u32, lhs: u32, rhs: u32 },  // unsigned <
icmp_ule: struct { dst: u32, lhs: u32, rhs: u32 },  // unsigned <=
```

#### 整数拡張演算

```zig
div:  struct { dst: u32, lhs: u32, rhs: u32 },  // 符号付き除算
udiv: struct { dst: u32, lhs: u32, rhs: u32 },  // 符号なし除算
rem:  struct { dst: u32, lhs: u32, rhs: u32 },  // 符号付き剰余
urem: struct { dst: u32, lhs: u32, rhs: u32 },  // 符号なし剰余
and_op: struct { dst: u32, lhs: u32, rhs: u32 }, // ビット AND
or_op:  struct { dst: u32, lhs: u32, rhs: u32 }, // ビット OR
xor_op: struct { dst: u32, lhs: u32, rhs: u32 }, // ビット XOR
shl:    struct { dst: u32, lhs: u32, rhs: u32 }, // 左シフト
lshr:   struct { dst: u32, lhs: u32, rhs: u32 }, // 論理右シフト
ashr:   struct { dst: u32, lhs: u32, rhs: u32 }, // 算術右シフト
```

#### 浮動小数点演算

```zig
fadd: struct { dst: u32, lhs: u32, rhs: u32 },
fsub: struct { dst: u32, lhs: u32, rhs: u32 },
fmul: struct { dst: u32, lhs: u32, rhs: u32 },
fdiv: struct { dst: u32, lhs: u32, rhs: u32 },
frem: struct { dst: u32, lhs: u32, rhs: u32 },
fcmp_eq:  struct { dst: u32, lhs: u32, rhs: u32 }, // 結果は inl/inr
fcmp_lt:  struct { dst: u32, lhs: u32, rhs: u32 },
fcmp_le:  struct { dst: u32, lhs: u32, rhs: u32 },
fcmp_gt:  struct { dst: u32, lhs: u32, rhs: u32 },
fcmp_ge:  struct { dst: u32, lhs: u32, rhs: u32 },
```

#### 型変換

```zig
int_to_float: struct { dst: u32, src: u32 },   // i64 → f64
float_to_int: struct { dst: u32, src: u32 },   // f64 → i64 (truncate)
load_float:   struct { dst: u32, val: u64 },   // f64 定数ロード (ビットパターン)
```

### 1-3. VM 実行ループへの追加

各 opcode の実装例:

```zig
.fadd => |a| {
    const lf: f64 = @bitCast(regs[a.lhs].float);
    const rf: f64 = @bitCast(regs[a.rhs].float);
    regs[a.dst] = .{ .float = @bitCast(lf + rf) };
},
.icmp_slt => |a| {
    const l: i64 = @bitCast(regs[a.lhs].int);
    const r: i64 = @bitCast(regs[a.rhs].int);
    if (l < r) {
        const v = try self.allocator.create(Value);
        v.* = .unit;
        regs[a.dst] = .{ .inl = v };
    } else {
        const v = try self.allocator.create(Value);
        v.* = .unit;
        regs[a.dst] = .{ .inr = v };
    }
},
```

---

## フェーズ 2: バイトコードローダー拡張 (`loader.zig`)

新 opcode 用のバイトコードオペコードを割り当て、`loadFromFile` のデコードテーブルを拡張する。

```
0x11  icmp_eq   dst, lhs, rhs
0x12  icmp_ne   dst, lhs, rhs
0x13  icmp_slt  dst, lhs, rhs
0x14  icmp_sle  dst, lhs, rhs
0x15  icmp_sgt  dst, lhs, rhs
0x16  icmp_sge  dst, lhs, rhs
0x17  icmp_ult  dst, lhs, rhs
0x18  icmp_ule  dst, lhs, rhs
0x19  div       dst, lhs, rhs
0x1A  udiv      dst, lhs, rhs
0x1B  rem       dst, lhs, rhs
0x1C  urem      dst, lhs, rhs
0x1D  and_op    dst, lhs, rhs
0x1E  or_op     dst, lhs, rhs
0x1F  xor_op    dst, lhs, rhs
0x20  shl       dst, lhs, rhs
0x21  lshr      dst, lhs, rhs
0x22  ashr      dst, lhs, rhs
0x23  fadd      dst, lhs, rhs
0x24  fsub      dst, lhs, rhs
0x25  fmul      dst, lhs, rhs
0x26  fdiv      dst, lhs, rhs
0x27  frem      dst, lhs, rhs
0x28  fcmp_eq   dst, lhs, rhs
0x29  fcmp_lt   dst, lhs, rhs
0x2A  fcmp_le   dst, lhs, rhs
0x2B  fcmp_gt   dst, lhs, rhs
0x2C  fcmp_ge   dst, lhs, rhs
0x2D  int_to_float  dst, src
0x2E  float_to_int  dst, src
0x2F  load_float    dst, val(u64 8バイト)
```

---

## フェーズ 3: Lifter 本体 (`lifter.zig`)

### 3-1. アーキテクチャ概要

```
LLVM IR テキスト
      ↓
  [1] パーサー        IRParser: 関数・基本ブロック・命令をパース
      ↓
  [2] CFG 構築       ControlFlowGraph: 基本ブロック間のエッジ・支配木
      ↓
  [3] ループ検出      LoopDetector: 後退辺を検出してループを特定
      ↓
  [4] Lambda Lifting  LoopLifter: ループ本体を再帰クロージャに変換
      ↓
  [5] PHI 除去        PhiEliminator: PHI ノード → move 命令に変換
      ↓
  [6] 命令変換        InstructionMapper: LLVM IR 命令 → Romanesco Op
      ↓
  LoadedProgram ([][]Op)
```

### 3-2. 対応する LLVM IR のサブセット

#### 完全対応

| LLVM IR | Romanesco Op |
|---|---|
| `add/sub/mul nsw i64 %a, %b` | `add/sub/mul` |
| `sdiv/udiv i64 %a, %b` | `div/udiv` |
| `srem/urem i64 %a, %b` | `rem/urem` |
| `and/or/xor i64 %a, %b` | `and_op/or_op/xor_op` |
| `shl/lshr/ashr i64 %a, %b` | `shl/lshr/ashr` |
| `fadd/fsub/fmul/fdiv double %a, %b` | `fadd/fsub/fmul/fdiv` |
| `icmp slt/sle/eq/... i64 %a, %b` | `icmp_slt/...` |
| `fcmp olt/ole/oeq/... double %a, %b` | `fcmp_lt/...` |
| `br i1 %c, label %t, label %f` | `case_op` (after icmp result) |
| `br label %bb` | 次ブロックへの無条件ジャンプ |
| `ret i64 %v` | `ret` |
| `ret void` | `load_const .unit` + `ret` |
| `call i64 @f(%a, %b)` | `call` |
| `phi` | `move`（PHI 除去後） |
| `sitofp / fptosi` | `int_to_float / float_to_int` |
| `zext i1 to i64` | inl/inr → int 変換 |

#### 対応不可（エラーを返す）

| LLVM IR | 理由 |
|---|---|
| `load/store` | Value モデルと非互換（将来フェーズで検討） |
| `getelementptr` | ポインタ演算なし |
| `alloca` | スタックモデルなし |
| ベクター型 `<4 x i64>` | VM に対応なし |
| `i8/i16/i32` (非 i64) | 自動 `sext` 可能だが要検討 |
| `f32` | 自動 `fpext` で f64 に昇格可能 |
| `invoke/landingpad` | 例外なし |
| `atomicrmw/cmpxchg` | 並行性なし |

### 3-3. ループ変換（Lambda Lifting）

LLVM IR のループ（後退辺を持つ CFG）を再帰クロージャに変換する。

**変換前（LLVM IR）:**
```llvm
define i64 @sum(i64 %n) {
entry:
  br label %loop
loop:
  %i   = phi i64 [0, %entry], [%i1, %loop]
  %acc = phi i64 [0, %entry], [%acc1, %loop]
  %cond = icmp slt i64 %i, %n
  br i1 %cond, label %body, label %exit
body:
  %acc1 = add i64 %acc, %i
  %i1   = add i64 %i, 1
  br label %loop
exit:
  ret i64 %acc
}
```

**変換後（Romanesco バイトコード疑似表記）:**
```
; ループ本体を別ブロック __loop_0 として抽出
; __loop_0(i, acc): captures=[n]
;   icmp_slt r_cond, r_i, r_n
;   case_op r_cond:
;     inl: add r_acc1, r_acc, r_i
;          add r_i1, r_i, const_1
;          call self, [r_i1, r_acc1]   ← 末尾再帰
;     inr: ret r_acc

; メイン @sum(n):
;   load_const r_0, 0       ; i=0
;   load_const r_1, 0       ; acc=0
;   make_closure r_f, __loop_0, captures=[r_n], arity=2
;   call r_result, r_f, [r_0, r_1]
;   ret r_result
```

**変換アルゴリズム:**

1. CFG を構築し後退辺（back edge）を検出
2. ループヘッダーの PHI ノードをループパラメータとして抽出
3. ループ本体を新しいブロックとして分離
4. ループ内の後退辺を自己再帰呼び出しに置き換え
5. ループ出口での値を `ret` に変換
6. 呼び出し元でクロージャを生成して初期値で呼び出す

### 3-4. SSA / PHI ノード除去

ループ外の PHI ノード（if-then-else の合流点）は、各前駆ブロックの末尾に `move` を挿入することで除去する。

```
; 変換前
bb_merge:
  %x = phi i64 [%a, %bb_true], [%b, %bb_false]

; 変換後
bb_true:
  move r_x, r_a    ← 追加
  br label %bb_merge
bb_false:
  move r_x, r_b    ← 追加
  br label %bb_merge
bb_merge:
  ; %x は既に r_x にある
```

### 3-5. 型マッピング

| LLVM IR 型 | Value タグ | 備考 |
|---|---|---|
| `i64` | `int` | そのまま |
| `i32` / `i16` / `i8` | `int` | `sext` して i64 に |
| `i1` | `inl` (真) / `inr` (偽) | icmp の結果 |
| `double` | `float` | ビットパターン保持 |
| `float` | `float` | `fpext` で double に昇格 |
| `void` | `unit` | |
| `ptr` → 関数ポインタ | `closure` | シグネチャが必要 |
| `ptr` → データ | **対応不可** | フェーズ4以降で検討 |

### 3-6. 公開 API

```zig
pub const Lifter = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Lifter

    /// LLVM IR テキストを受け取り LoadedProgram を返す
    /// 対応外の命令が含まれる場合は LiftError を返す
    pub fn lift(self: *Lifter, ir_text: []const u8) LiftError!loader.LoadedProgram
};

pub const LiftError = error{
    OutOfMemory,
    ParseError,           // IR のパース失敗
    UnsupportedInstruction, // 未対応命令（load/store/vector 等）
    UnsupportedType,      // 未対応型（ptr to data 等）
    NoFunctions,          // 変換対象の関数が見つからない
};
```

---

## フェーズ 4: JIT/AOT 統合

### `jit.zig` への追加

```zig
/// 外部 LLVM IR を Lifter でバイトコード化し、JIT 実行する
pub fn liftAndRun(
    self: *JIT,
    ir_text: []const u8,
    entry_name: []const u8,
    vm_regs: []Value,
) !u64
```

### `aot.zig` への追加

```zig
/// 外部 LLVM IR を Lifter でバイトコード化し、AOT コンパイルする
pub fn liftAndCompile(
    self: *AOTCompiler,
    ir_text: []const u8,
    output_path: []const u8,
    cpu: vm.CpuFeatures,
) !void
```

---

## フェーズ 5: 最適化器拡張 (`optimizer.zig`)

`foldConstants` と `eliminateDeadCode` に新 opcode を追加する。

```zig
// foldConstants: float 定数畳み込み
.fadd => |a| {
    if (constants.get(a.lhs)) |l| if (constants.get(a.rhs)) |r| {
        if (l == .float and r == .float) {
            const lf: f64 = @bitCast(l.float);
            const rf: f64 = @bitCast(r.float);
            const val = Value{ .float = @bitCast(lf + rf) };
            // load_const に置き換え
        }
    }
},
```

---

## decompiler.zig との関係

### 統合しない理由

`decompiler.zig` は `codegen.zig` が生成する IR の**構造を前提としたパターンマッチング**で動作する。
たとえば `call @rt_make_pair` を見た瞬間に `make_pair` opcode へ直接変換できる（1命令）。

一方、汎用 lifter は `@rt_make_pair` を「未知の外部関数呼び出し」として扱うため、
そのまま統合すると `make_closure` + `call` に変換されてしまい、roundtrip が壊れる。

```
decompiler（現状）:  call @rt_make_pair → make_pair      ✓ 1命令・意味保存
lifter（汎用）:      call @rt_make_pair → 外部関数扱い   ✗ 意味が変わる
```

### 共通化する部分: `ir_parser.zig`（新規）

IRテキストの字句解析・構造分解は両者で重複するため、共通ユーティリティとして切り出す。

```
ir_parser.zig        （新規・共通）
  ├─ 行分割・コメント除去
  ├─ 関数定義の境界検出
  ├─ 基本ブロックの識別
  └─ 命令のトークン分解（opcode・オペランド）

decompiler.zig       （既存・修正）
  └─ ir_parser を利用 + @rt_* パターンマッチング

lifter.zig           （新規）
  └─ ir_parser を利用 + CFG/SSA/ループ解析
```

フェーズ一覧に `ir_parser.zig` の実装をフェーズ 0 として追加する（後述）。

### 将来の統合可能性

lifter に「`@rt_*` をイントリンシックとして認識する Romanesco モード」を実装した段階で、
decompiler を lifter に統合することを改めて検討する。それまでは両者を独立モジュールとして共存させる。

| ファイル | 役割 |
|---|---|
| `ir_parser.zig` | IR テキストの字句解析・構造分解（共通） |
| `decompiler.zig` | **Romanesco 専用**: `codegen.zig` 出力の逆変換（既存） |
| `lifter.zig` | **汎用**: 任意の LLVM IR → バイトコード（今回新規） |

---

## 未解決課題（将来フェーズ）

- **`load/store` 対応**: Romanesco の Value モデルに「参照型」を追加するか、ヒープ上の Value 配列として表現する
- **`i32` 以下の整数**: 現状 `sext` で i64 に昇格するが、意味論的な差異に注意
- **可変長引数関数**: `printf` 等の varargs は現状対応不可
- **相互再帰の Lambda Lifting**: 複数関数にまたがるループは要検討
- **デバッグ情報の保持**: 元の行番号・変数名の Romanesco バイトコードへの埋め込み
- **FPU 非搭載環境でのパフォーマンス**: ソフトウェア float は自動で機能するが低速。
  FPU なし環境で float 演算を多用する場合は、固定小数点数への手動変換や
  float 命令の使用を制限するリンターの導入を検討する
