# Plan to implement: 多倍長整数（BigInt/Wide）対応

## Context

前フェーズ（新opcode LLVM IR生成 + benchmark.zig）は完了済み。
現在の残課題:
- `vm.zig`: `ibin` width > 64 で `add`/`sub` 以外が `error.UnsupportedBigIntOp` になる。
- `lifter.zig`: `ibin`/`icmp` の width が 64 固定になっている。
- `codegen.zig`: `wide` 型（width > 64）の定数ロードおよび演算の LLVM IR 生成が未実装。

---

## Phase 1: vm.zig — 演算拡張

対象ファイル: `Zig/src/Runtime/vm.zig`

### 1-1. `bigintMul` 追加

`bigintSub` の直後に追加。

```zig
fn bigintMul(self: *VM, lhs: []const u64, rhs: []const u64, width: IntWidth) ![]u64 {
    const n = self.limbCount(width);
    const dst = try self.allocator.alloc(u64, n);
    @memset(dst, 0);

    for (0..lhs.len) |i| {
        var carry: u64 = 0;
        for (0..rhs.len) |j| {
            if (i + j >= n) break;
            const prod = @as(u128, lhs[i]) * @as(u128, rhs[j]) +
                        @as(u128, dst[i + j]) + @as(u128, carry);
            dst[i + j] = @truncate(prod);
            carry = @truncate(prod >> 64);
        }
    }
    self.maskTopLimb(dst, width);
    return dst;
}
```

### 1-2. `bigintBitwise` 追加

`bigintMul` の直後に追加。

```zig
fn bigintBitwise(self: *VM, lhs: []const u64, rhs: []const u64, op: IBinOp, width: IntWidth) ![]u64 {
    const n = self.limbCount(width);
    const dst = try self.allocator.alloc(u64, n);
    for (0..n) |i| {
        const l = if (i < lhs.len) lhs[i] else 0;
        const r = if (i < rhs.len) rhs[i] else 0;
        dst[i] = switch (op) {
            .and_ => l & r,
            .or_  => l | r,
            .xor_ => l ^ r,
            else  => unreachable,
        };
    }
    self.maskTopLimb(dst, width);
    return dst;
}
```

### 1-3. `ibin` width > 64 ブランチ拡張

現在の `else => return error.UnsupportedBigIntOp` を以下に置換:

```zig
.mul  => try self.bigintMul(l, r, o.width),
.and_ => try self.bigintBitwise(l, r, .and_, o.width),
.or_  => try self.bigintBitwise(l, r, .or_, o.width),
.xor_ => try self.bigintBitwise(l, r, .xor_, o.width),
else  => return error.UnsupportedBigIntOp,  // div/rem/shift は継続未実装
```

---

## Phase 2: lifter.zig — width の動的解析

対象ファイル: `Zig/src/Runtime/lifter.zig`

### 2-1. `parseIntWidth` ヘルパー追加

`liftInstructionLocal` の前に追加:

```zig
fn parseIntWidth(type_str: []const u8) u16 {
    if (type_str.len < 2 or type_str[0] != 'i') return 64;
    return std.fmt.parseInt(u16, type_str[1..], 10) catch 64;
}
```

### 2-2. `ibin` width 修正

`add i128 %a, %b` → `operands[0] = "i128"`

行253付近（width = 64 固定の箇所）を変更:
```zig
// Before:
.{ .ibin = .{ ..., .width = 64 } }

// After:
const ibin_width = parseIntWidth(insn.operands[0]);
.{ .ibin = .{ ..., .width = ibin_width } }
```

### 2-3. `icmp` width 修正

`icmp slt i64 %a, %b` → `operands[1] = "i64"`

行301付近（width = 64 固定の箇所）を変更:
```zig
// Before:
.{ .icmp = .{ ..., .width = 64 } }

// After:
const icmp_width = parseIntWidth(insn.operands[1]);
.{ .icmp = .{ ..., .width = icmp_width } }
```

---

## Phase 3: codegen.zig — wide Value の LLVM IR 生成

対象ファイル: `Zig/src/Runtime/codegen.zig`

### Value のメモリレイアウト（確認済み）

`%Value = { i64 tag, ptr payload }`
- `bits` (tag=6): payload = `inttoptr i64` の値
- `wide` (tag=7): payload = `malloc(8 + n*8)` の先頭ポインタ
    - `[0..8]`: リム数 (i64)
    - `[8..]`: リム配列 (i64 × n)

### 3-1. `runtime_implementation` への追加

1. **`@llvm.memcpy` 宣言**（`declare ptr @malloc` の近くに追加）:
   `declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)`

2. **`rt_cleanup_value` の `wide` サポート**（`i64 4, label %free_sum` の後に追加）:
   `i64 7, label %free_wide`
   そして `free_cl:` の前に:
   ```llvm
   free_wide:
     call void @free(ptr %ptr)
     br label %done
   ```

3. **`rt_make_wide` 追加**（`rt_get_int` の後に追加）:
   ```llvm
   define void @rt_make_wide(ptr %out, ptr %limbs, i64 %count) {
   entry:
     %old = load %Value, ptr %out
     call void @rt_cleanup_value(%Value %old)
     %data_bytes = mul i64 %count, 8
     %total = add i64 %data_bytes, 8
     %mem = call ptr @malloc(i64 %total)
     store i64 %count, ptr %mem
     %dst_limbs = getelementptr i8, ptr %mem, i64 8
     call void @llvm.memcpy.p0.p0.i64(ptr %dst_limbs, ptr %limbs, i64 %data_bytes, i1 false)
     %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
     store i64 7, ptr %p1
     %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
     store ptr %mem, ptr %p2
     ret void
   }
   ```

4. **`rt_get_wide_limb` 追加**（`rt_make_wide` の直後）:
   ```llvm
   define i64 @rt_get_wide_limb(ptr %v, i64 %idx) {
   entry:
     %p2 = getelementptr %Value, ptr %v, i32 0, i32 1
     %mem = load ptr, ptr %p2
     %limbs_base = getelementptr i8, ptr %mem, i64 8
     %limb_ptr = getelementptr i64, ptr %limbs_base, i64 %idx
     %val = load i64, ptr %limb_ptr
     ret i64 %val
   }
   ```

### 3-2. `load_wide` case の修正

```zig
.load_wide => |o| {
    const w = o.width;
    const lc = (w + 63) / 64;  // limbCount
    // スタックにリム配列を構築
    try writer.print("  %lw_arr_{d} = alloca [{d} x i64]\n", .{o.dst, lc});
    for (0..lc) |li| {
        const val: i64 = @bitCast(if (li < o.limbs.len) o.limbs[li] else 0);
        const gep = next_temp(&temp_counter);
        try writer.print("  %v{d} = getelementptr [{d} x i64], ptr %lw_arr_{d}, i32 0, i32 {d}\n",
            .{gep, lc, o.dst, li});
        try writer.print("  store i64 {d}, ptr %v{d}\n", .{val, gep});
    }
    try writer.print("  %lw_dst_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, o.dst});
    try writer.print("  call void @rt_make_wide(ptr %lw_dst_{d}, ptr %lw_arr_{d}, i64 {d})\n",
        .{o.dst, o.dst, lc});
},
```

### 3-3. `ibin` case の w > 64 分岐追加

現在の `else` ブランチ（`%v{res} = {llvm_op} i64 %v{lv}, %v{rv}`）の前に分岐を追加:

```zig
if (w <= 64) {
    // 既存の w < 64 / w == 64 実装をそのまま維持
    ...
} else {
    // LLVM ネイティブ iN 型で演算（N = w）
    const lc = (w + 63) / 64;

    // lhs をリムから iN に再構築
    var lhs_acc = next_temp(&temp_counter);
    try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_lhs_{d}, i64 0)\n", .{lhs_acc, o.lhs});
    const lhs_e0 = next_temp(&temp_counter);
    try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{lhs_e0, lhs_acc, w});
    lhs_acc = lhs_e0;
    for (1..lc) |li| {
        const lr = next_temp(&temp_counter); const le = next_temp(&temp_counter);
        const ls = next_temp(&temp_counter); const la = next_temp(&temp_counter);
        try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_lhs_{d}, i64 {d})\n", .{lr, o.lhs, li});
        try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{le, lr, w});
        try writer.print("  %v{d} = shl i{d} %v{d}, {d}\n", .{ls, w, le, li * 64});
        try writer.print("  %v{d} = or i{d} %v{d}, %v{d}\n", .{la, w, lhs_acc, ls});
        lhs_acc = la;
    }
    // rhs を同様に再構築
    var rhs_acc = next_temp(&temp_counter);
    try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_rhs_{d}, i64 0)\n", .{rhs_acc, o.rhs});
    const rhs_e0 = next_temp(&temp_counter);
    try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{rhs_e0, rhs_acc, w});
    rhs_acc = rhs_e0;
    for (1..lc) |li| {
        const rr = next_temp(&temp_counter); const re = next_temp(&temp_counter);
        const rs = next_temp(&temp_counter); const ra = next_temp(&temp_counter);
        try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_rhs_{d}, i64 {d})\n", .{rr, o.rhs, li});
        try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{re, rr, w});
        try writer.print("  %v{d} = shl i{d} %v{d}, {d}\n", .{rs, w, re, li * 64});
        try writer.print("  %v{d} = or i{d} %v{d}, %v{d}\n", .{ra, w, rhs_acc, rs});
        rhs_acc = ra;
    }
    // 演算
    const op_res = next_temp(&temp_counter);
    try writer.print("  %v{d} = {s} i{d} %v{d}, %v{d}\n", .{op_res, llvm_op, w, lhs_acc, rhs_acc});
    // 結果リムをスタック配列に抽出
    try writer.print("  %ibin_wide_{d} = alloca [{d} x i64]\n", .{o.dst, lc});
    for (0..lc) |li| {
        const to_trunc = if (li == 0) op_res else blk: {
            const s = next_temp(&temp_counter);
            try writer.print("  %v{d} = lshr i{d} %v{d}, {d}\n", .{s, w, op_res, li * 64});
            break :blk s;
        };
        const tr = next_temp(&temp_counter);
        const gp = next_temp(&temp_counter);
        try writer.print("  %v{d} = trunc i{d} %v{d} to i64\n", .{tr, w, to_trunc});
        try writer.print("  %v{d} = getelementptr [{d} x i64], ptr %ibin_wide_{d}, i32 0, i32 {d}\n",
            .{gp, lc, o.dst, li});
        try writer.print("  store i64 %v{d}, ptr %v{d}\n", .{tr, gp});
    }
    try writer.print("  %ibin_dst_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, o.dst});
    try writer.print("  call void @rt_make_wide(ptr %ibin_dst_{d}, ptr %ibin_wide_{d}, i64 {d})\n",
        .{o.dst, o.dst, lc});
}
```

---

## Phase 4: テスト追加

### `vm.zig` テスト（ファイル末尾）

- `test "bigintMul: 3 * 5 = 15" { ... }`
- `test "bigintMul: 2^64 * 1" { ... }` // 128ビット乗算の繰り上がりテスト
- `test "bigintBitwise: and/or/xor 128bit" { ... }`
- `test "ibin wide add i128: (2^64+1) + 1" { ... }`
- `test "ibin wide mul i128: 3 * 5 = 15" { ... }`

### `lifter.zig` テスト（既存テスト末尾）

- `test "lifter: add i128 produces width=128" { ... }`
  - `"add i128 %a, %b"` を lift → `ibin.width == 128` を検証

### `codegen.zig` テスト（既存テスト末尾）

- `test "Codegen: load_wide emits rt_make_wide" { ... }`
- `test "Codegen: ibin add128 emits add i128 and rt_make_wide" { ... }`

### `benchmark.zig` テスト

- `test "Bench: VM bigint multiply i128" { ... }`
  - `ibin{mul,128}` 1万回、正確性: `3*5=15`

---

## 対象ファイル

| ファイル | 変更箇所 |
| :--- | :--- |
| `Zig/src/Runtime/vm.zig` | `bigintSub` 直後に `bigintMul`/`bigintBitwise` 追加、`ibin` w > 64 ブランチ拡張 |
| `Zig/src/Runtime/lifter.zig` | `parseIntWidth` 追加、`ibin`/`icmp` width 修正 |
| `Zig/src/Runtime/codegen.zig` | `runtime_implementation` に `memcpy` 宣言/`rt_make_wide`/`rt_get_wide_limb`/`rt_cleanup_wide` 追加、`load_wide`/`ibin`(w > 64) case 修正 |
| `Zig/src/Runtime/benchmark.zig` | `bigint` bench テスト追加 |

## 制約・スコープ外

- `ibin` w > 64 の `div`/`rem`/`shift` は `error.UnsupportedBigIntOp` のまま維持（別フェーズ）
- `icmp` w > 64 のコード生成は今回スコープ外（`vm.zig` 側は `bits` と同様に `u64` 比較を使う現状のまま）

---

## 検証

```bash
cd Zig

# Phase 1 (vm.zig)
zig test src/Runtime/vm.zig

# Phase 2 (lifter.zig)
zig test src/Runtime/lifter.zig

# Phase 3 (codegen.zig)
zig test src/Runtime/codegen.zig

# ベンチマーク
zig test src/Runtime/benchmark.zig

# リグレッション
zig test src/Runtime/decompiler.zig
zig test src/Runtime/speculative.zig
zig build
```
