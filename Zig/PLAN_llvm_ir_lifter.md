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

### 1-1. Value 型の拡張

**設計方針（型無し）**: 数値 Value は「意味を持たないビット列」とする。
整数か浮動小数点かの区別は Value タグに持たせず、**opcode が解釈を決める**。
構造型（closure / pair / inl / inr / unit）は計算モデルの基盤であるため型タグを保持する。

```zig
pub const ValueTag = enum(u64) {
    closure = 1,
    pair    = 2,
    inl     = 3,
    inr     = 4,
    unit    = 5,
    bits    = 6,   // ≤64ビットのビット列（int/float/pointer等を区別しない）
    wide    = 7,   // >64ビットのビット列（ヒープ上のリム配列）
};

pub const Value = union(ValueTag) {
    // ...既存フィールド...
    bits: u64,    // ビットパターン（意味はopcodeが決める）
    wide: []u64,  // 多倍長ビットパターン（リトルエンディアンのリム配列）
};
```

同じビット列を異なるopcodeが異なる意味で使う例:
```
load_bits { val=0x4048F5C28F5C28F6 } → Value.bits（f64の3.14のビットパターン）
load_bits { val=42 }                 → Value.bits（整数42のビットパターン）

ibin { op=.add, width=64 }  → bits を64bit整数として加算
fadd                         → bits を f64 として加算
ibin { op=.add, width=32 }  → bits の下位32ビットを整数として加算
```

**Valueのサイズについて**: `wide` の payload は `[]u64`（スライス = ポインタ+長さ = 16バイト）。
これが最大 payload サイズになるため、Value 全体は **24バイト** になる（現状16バイトから増加）。
`bits` やポインタ系タグのサイズは変わらないが、union 全体のサイズは wide に揃う点に注意。

**wide のリム構造**:
```
width=80の例: limbCount = ceil(80/64) = 2
limbs[0] = 下位64ビット
limbs[1] = 上位16ビット（上位48ビットはゼロ）
```

**float の扱い**: `f64` は `@bitCast` でビットパターンを `u64` に変換して `Value.bits` に格納する。
`fadd` 等のopcodeが実行時に `@bitCast` で `f64` に戻してCPUのFPUを利用する。コストは実質ゼロ。

**FPU 非搭載環境について**: `u64` によるビットパターン保持はアーキテクチャ非依存であるため、
値の格納・転送は整数操作のみで完結する。実際の演算（`lf + rf` 等の Zig コード）を
FPU なしターゲット向けにコンパイルすると、Zig（LLVM バックエンド）が自動的に
`compiler-rt` のソフトウェア float ルーチン（`__adddf3`、`__subdf3` 等）へ置き換える。
JIT/AOT で生成される LLVM IR 中の `fadd double` 命令も、clang がターゲットに応じて
同様に処理するため、**明示的な対処は不要**。
ただしソフトウェア float はハードウェア FPU に比べて数十〜数百倍遅くなる点に注意。

### 1-2. Op への新 opcode 追加

#### 任意ビット幅整数演算（`ibin`）

`add/sub/mul`（既存、width=64相当）はそのまま残し後方互換を保つ。
新規コードおよびLiftedコードには `ibin` を使う。

```zig
pub const IntWidth = u16;  // 1〜65535ビット（実用上1〜64を主用途とする）

pub const IBinOp = enum {
    add, sub, mul,
    sdiv, udiv,   // 符号付き/なし除算
    srem, urem,   // 符号付き/なし剰余
    and_, or_, xor_,
    shl, lshr, ashr,
};

ibin: struct { dst: u32, lhs: u32, rhs: u32, op: IBinOp, width: IntWidth },

// ビット列定数ロード（width ≤ 64、int/float どちらにも使う）
load_bits: struct { dst: u32, val: u64, width: IntWidth },

// ビット列定数ロード（width > 64）
load_wide: struct { dst: u32, limbs: []const u64, width: IntWidth },
```

`ibin` の後方互換関係:
```
add { dst, lhs, rhs }  ≡  ibin { dst, lhs, rhs, op=.add, width=64 }
sub { dst, lhs, rhs }  ≡  ibin { dst, lhs, rhs, op=.sub, width=64 }
mul { dst, lhs, rhs }  ≡  ibin { dst, lhs, rhs, op=.mul, width=64 }
```

#### 任意ビット幅整数比較（`icmp`）

結果は `inl`=真 / `inr`=偽 の Value になる。

```zig
pub const ICmpPred = enum {
    eq, ne,
    slt, sle, sgt, sge,   // 符号付き
    ult, ule, ugt, uge,   // 符号なし
};

icmp: struct { dst: u32, lhs: u32, rhs: u32, pred: ICmpPred, width: IntWidth },
```

#### ビット幅変換

整数としての意味的な変換。結果はいずれも `Value.bits` または `Value.wide`。

```zig
sext:  struct { dst: u32, src: u32, from: IntWidth, to: IntWidth },  // 符号拡張
zext:  struct { dst: u32, src: u32, from: IntWidth, to: IntWidth },  // ゼロ拡張
trunc: struct { dst: u32, src: u32, from: IntWidth, to: IntWidth },  // 切り捨て
```

#### 数値解釈の変換（bits の再解釈）

同じビット列を「整数↔浮動小数点」として意味変換する。

```zig
// bits を整数として解釈し f64 に変換 → 結果は Value.bits（f64 ビットパターン）
itof: struct { dst: u32, src: u32, width: IntWidth, signed: bool },

// bits を f64 として解釈し整数に変換 → 結果は Value.bits（整数ビットパターン）
ftoi: struct { dst: u32, src: u32, width: IntWidth, signed: bool },

// bits を単純に再解釈するだけ（意味の変換なし、LLVM の bitcast 相当）
// → load_bits で直接ビットパターンを指定するだけでよいため opcode 不要
```

#### 浮動小数点演算

`bits` タグの値を f64 ビットパターンとして解釈して演算する。結果も `Value.bits`。

```zig
pub const FCmpPred = enum { oeq, one, olt, ole, ogt, oge, ord, uno };

fadd: struct { dst: u32, lhs: u32, rhs: u32 },
fsub: struct { dst: u32, lhs: u32, rhs: u32 },
fmul: struct { dst: u32, lhs: u32, rhs: u32 },
fdiv: struct { dst: u32, lhs: u32, rhs: u32 },
frem: struct { dst: u32, lhs: u32, rhs: u32 },
fcmp: struct { dst: u32, lhs: u32, rhs: u32, pred: FCmpPred },  // 結果は inl/inr
```

### 1-3. VM 実行ループへの追加

#### bigint ヘルパー関数

```zig
fn limbCount(width: IntWidth) usize {
    return (@as(usize, width) + 63) / 64;
}

/// 上位リムの余分なビットをゼロにする（演算後に必ず呼ぶ）
fn maskTopLimb(limbs: []u64, width: IntWidth) void {
    const top = (width - 1) / 64;
    const used = @as(u6, @intCast((width - 1) % 64 + 1));
    const mask = if (used == 64) ~@as(u64, 0)
                 else (@as(u64, 1) << used) - 1;
    limbs[top] &= mask;
}

/// リムに符号拡張を適用する（符号付き演算前に使用）
fn signExtendLimbs(limbs: []u64, width: IntWidth) void {
    const top = (width - 1) / 64;
    const bit = @as(u6, @intCast((width - 1) % 64));
    const sign = (limbs[top] >> bit) & 1;
    if (sign == 1) {
        // 上位ビットを 1 で埋める
        const mask = ~((@as(u64, 1) << (bit + 1)) - 1);
        limbs[top] |= mask;
        for (top + 1..limbs.len) |i| limbs[i] = ~@as(u64, 0);
    }
}

/// 多倍長加算（キャリー伝搬）
fn bigintAdd(allocator: Allocator, lhs: []const u64, rhs: []const u64, width: IntWidth) ![]u64 {
    const n = limbCount(width);
    const dst = try allocator.alloc(u64, n);
    var carry: u1 = 0;
    for (0..n) |i| {
        const r = @addWithOverflow(lhs[i], rhs[i]);
        const r2 = @addWithOverflow(r[0], carry);
        dst[i] = r2[0];
        carry = r[1] | r2[1];
    }
    maskTopLimb(dst, width);
    return dst;
}

/// 演算難度ごとの実装優先度
/// add/sub/and/or/xor/shl/lshr : 低（キャリー/ボロー伝搬）
/// ashr                         : 中（符号ビット伝搬）
/// mul                          : 中（スクールブック法 O(n²)）
/// sdiv/udiv/srem/urem          : 高（Knuth Algorithm D 等）→ 後期フェーズで実装
```

#### `ibin` opcode の実装例

```zig
.ibin => |o| {
    // width ≤ 64: bits タグ、> 64: wide タグで分岐
    if (o.width <= 64) {
        const l = regs[o.lhs].bits;
        const r = regs[o.rhs].bits;
        const mask = if (o.width == 64) ~@as(u64, 0)
                     else (@as(u64, 1) << @intCast(o.width)) - 1;
        const raw: u64 = switch (o.op) {
            .add  => l +% r,
            .sub  => l -% r,
            .mul  => l *% r,
            .and_ => l  & r,
            .or_  => l  | r,
            .xor_ => l  ^ r,
            .lshr => l >> @intCast(r & 63),
            .shl  => l << @intCast(r & 63),
            .ashr => blk: {
                const sl: i64 = signExtend64(l, o.width);
                break :blk @bitCast(sl >> @intCast(r & 63));
            },
            .sdiv => blk: {
                const sl: i64 = signExtend64(l, o.width);
                const sr: i64 = signExtend64(r, o.width);
                break :blk @bitCast(@divTrunc(sl, sr));
            },
            .udiv => l / r,
            .srem => blk: {
                const sl: i64 = signExtend64(l, o.width);
                const sr: i64 = signExtend64(r, o.width);
                break :blk @bitCast(@rem(sl, sr));
            },
            .urem => l % r,
        };
        regs[o.dst] = .{ .bits = raw & mask };
    } else {
        // width > 64: wide タグ（多倍長演算）
        const result = try bigintBinOp(self.allocator, regs[o.lhs].wide,
                                       regs[o.rhs].wide, o.op, o.width);
        regs[o.dst].deinit(self.allocator);
        regs[o.dst] = .{ .wide = result };
    }
},
```

#### `fadd` / `icmp` の実装例

`bits` タグの値を opcode の意図に従って解釈する。型タグは参照しない。

```zig
.fadd => |a| {
    // bits を f64 ビットパターンとして解釈
    const lf: f64 = @bitCast(regs[a.lhs].bits);
    const rf: f64 = @bitCast(regs[a.rhs].bits);
    regs[a.dst] = .{ .bits = @bitCast(lf + rf) };
},
.icmp => |a| {
    const cond: bool = switch (a.pred) {
        .slt => signExtend64(regs[a.lhs].bits, a.width) <
                signExtend64(regs[a.rhs].bits, a.width),
        .ult => regs[a.lhs].bits < regs[a.rhs].bits,
        .eq  => regs[a.lhs].bits == regs[a.rhs].bits,
        // ...
    };
    const v = try self.allocator.create(Value);
    v.* = .unit;
    regs[a.dst] = if (cond) .{ .inl = v } else .{ .inr = v };
},
```

#### `deinit` への `wide` 追加

```zig
pub fn deinit(self: Value, allocator: Allocator) void {
    switch (self) {
        .wide => |limbs| allocator.free(limbs),
        // ...既存...
    }
}
```

---

## フェーズ 2: バイトコードローダー拡張 (`loader.zig`)

新 opcode 用のバイトコードオペコードを割り当て、`loadFromFile` のデコードテーブルを拡張する。

```
; --- 任意ビット幅整数演算 ---
; ibin の op フィールドは別途 u8 でエンコード（IBinOp の序数）
0x11  ibin          dst(u32), lhs(u32), rhs(u32), op(u8), width(u16)

; icmp の pred フィールドは u8 でエンコード（ICmpPred の序数）
0x12  icmp          dst(u32), lhs(u32), rhs(u32), pred(u8), width(u16)

; ビット列定数ロード（width ≤ 64、int/float 共用）
0x13  load_bits     dst(u32), val(u64), width(u16)

; ビット列定数ロード（width > 64）: limb_count = ceil(width/64) 個の u64 が続く
0x14  load_wide     dst(u32), width(u16), limb_count(u32), limbs(u64 × limb_count)

; ビット幅変換
0x15  sext          dst(u32), src(u32), from(u16), to(u16)
0x16  zext          dst(u32), src(u32), from(u16), to(u16)
0x17  trunc         dst(u32), src(u32), from(u16), to(u16)

; 数値解釈変換（bits を整数↔f64 として変換）
0x18  itof          dst(u32), src(u32), width(u16), signed(u8)
0x19  ftoi          dst(u32), src(u32), width(u16), signed(u8)

; --- 浮動小数点演算（bits を f64 ビットパターンとして解釈）---
; fcmp の pred フィールドは u8 でエンコード（FCmpPred の序数）
0x20  fadd          dst(u32), lhs(u32), rhs(u32)
0x21  fsub          dst(u32), lhs(u32), rhs(u32)
0x22  fmul          dst(u32), lhs(u32), rhs(u32)
0x23  fdiv          dst(u32), lhs(u32), rhs(u32)
0x24  frem          dst(u32), lhs(u32), rhs(u32)
0x25  fcmp          dst(u32), lhs(u32), rhs(u32), pred(u8)
```

エンコードのポイント:
- `width` は u16（2バイト）でエンコードし、最大65535ビットを表現可能
- `load_bigint` は可変長のため `limb_count` フィールドを持つ
- 既存の `0x01`〜`0x10` は変更なし（後方互換）

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

| LLVM IR | Romanesco Op | 備考 |
|---|---|---|
| `add/sub/mul nsw iN %a, %b` | `ibin { op, width=N }` | N=64 なら既存 `add/sub/mul` も可 |
| `sdiv/udiv iN %a, %b` | `ibin { op=.sdiv/.udiv, width=N }` | |
| `srem/urem iN %a, %b` | `ibin { op=.srem/.urem, width=N }` | |
| `and/or/xor iN %a, %b` | `ibin { op=.and_/.or_/.xor_, width=N }` | |
| `shl/lshr/ashr iN %a, %b` | `ibin { op=.shl/.lshr/.ashr, width=N }` | |
| `fadd/fsub/fmul/fdiv double` | `fadd/fsub/fmul/fdiv` | |
| `icmp slt/eq/ult/... iN %a, %b` | `icmp { pred, width=N }` | |
| `fcmp olt/oeq/... double` | `fcmp { pred }` | |
| `sext iN %x to iM` | `sext { from=N, to=M }` | |
| `zext iN %x to iM` | `zext { from=N, to=M }` | |
| `trunc iN %x to iM` | `trunc { from=N, to=M }` | |
| `sitofp iN %x to double` | `itof { width=N, signed=true }` | bits→f64 |
| `uitofp iN %x to double` | `itof { width=N, signed=false }` | |
| `fptosi double %x to iN` | `ftoi { width=N, signed=true }` | f64→bits |
| `fptoui double %x to iN` | `ftoi { width=N, signed=false }` | |
| `bitcast double %x to i64` | opcode 不要 | bits は既に同じ表現 |
| `br i1 %c, label %t, label %f` | `case_op` | icmp 結果（inl/inr）を scrutinee に |
| `br label %bb` | 次ブロックへの無条件ジャンプ | |
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
- **bigint の sdiv/udiv/srem/urem**: Knuth Algorithm D 等の実装が必要で複雑度が高い。
  フェーズ1では add/sub/mul/and/or/xor/shl/lshr/ashr のみ実装し、除算は後期フェーズとする
- **width > 64 の bigint パフォーマンス**: ヒープ確保が演算ごとに発生するため低速。
  アリーナアロケータの活用や、スタック上に小リム数を確保する最適化を将来検討する
