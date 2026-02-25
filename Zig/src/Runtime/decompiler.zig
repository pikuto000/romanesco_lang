/// decompiler.zig
/// LLVM IR (codegen.zig が生成したもの) → Romanesco VM バイトコード へのデコンパイラ
///
/// codegen.zig が生成する構造化 IR のみを対象とし、各 rt_* ヘルパー呼び出しパターンを
/// 逆引きして Op 命令列を復元する。汎用 LLVM IR パーサーではない。
///
/// 復元戦略:
///   - entry 関数 (dllexport) → blocks[0]
///   - @__block_N → blocks[N]
///   - 各命令の「終端アンカー」行でオペコードを確定
///   - case_op / make_closure のブロック間参照は 2 パス目で解決

const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;
const loader = @import("loader.zig");

pub const DecompileError = error{
    InvalidIR,
    UnsupportedPattern,
    OutOfMemory,
    MissingEntryPoint,
};

// ============================================================
// 文字列ユーティリティ
// ============================================================

fn uintAfter(line: []const u8, prefix: []const u8) ?u32 {
    const pos = std.mem.indexOf(u8, line, prefix) orelse return null;
    return parseUintPrefix(line[pos + prefix.len ..]);
}

fn parseUintPrefix(s: []const u8) ?u32 {
    if (s.len == 0 or s[0] < '0' or s[0] > '9') return null;
    var end: usize = 0;
    while (end < s.len and s[end] >= '0' and s[end] <= '9') end += 1;
    return std.fmt.parseInt(u32, s[0..end], 10) catch null;
}

fn i64After(line: []const u8, prefix: []const u8) ?i64 {
    const pos = std.mem.indexOf(u8, line, prefix) orelse return null;
    const rest = line[pos + prefix.len ..];
    if (rest.len == 0) return null;
    var end: usize = if (rest[0] == '-') 1 else 0;
    while (end < rest.len and rest[end] >= '0' and rest[end] <= '9') end += 1;
    if (end == 0 or (end == 1 and rest[0] == '-')) return null;
    return std.fmt.parseInt(i64, rest[0..end], 10) catch null;
}

/// "getelementptr %Value, ptr %regs, i32 N" → N
fn gepReg(line: []const u8) ?u32 {
    return uintAfter(line, "getelementptr %Value, ptr %regs, i32 ");
}

fn has(line: []const u8, needle: []const u8) bool {
    return std.mem.containsAtLeast(u8, line, 1, needle);
}

// ============================================================
// フィックスアップ
// ============================================================

const CaseFixup = struct {
    block_i: usize,
    op_i: usize,
    inl_block_idx: u32,
    inr_block_idx: u32,
};

const ClosureFixup = struct {
    block_i: usize,
    op_i: usize,
    target_block_idx: u32,
};

// ============================================================
// 関数ボディパーサー
// ============================================================

const ArithKind = enum { add, sub, mul };

const FuncBodyParser = struct {
    allocator: Allocator,
    ops: std.ArrayList(Op),
    case_fixups: *std.ArrayList(CaseFixup),
    closure_fixups: *std.ArrayList(ClosureFixup),
    block_i: usize,

    arith_kind: ?ArithKind,
    arith_lhs: ?u32,
    arith_rhs: ?u32,

    move_src: ?u32,
    move_dst: ?u32,

    closure_caps: std.ArrayList(u32),

    call_func: ?u32,
    call_args: std.ArrayList(u32),
    pending_call_result: bool,

    pending_proj: ?enum { p1, p2 },
    pending_proj_src: u32,

    pending_src_ptr: ?u32,
    borrow_dst: ?u32,
    borrow_has_dst: bool,

    f_ptr_reg: ?u32,
    pair_fst: ?u32,
    pair_snd: ?u32,
    pair_pending_dst: ?u32,

    case_scr: ?u32,
    case_in_inl: bool,
    case_in_inr: bool,
    case_after_end: bool,
    case_inl_idx: ?u32,
    case_inr_idx: ?u32,

    ret_src: ?u32,

    fn init(
        allocator: Allocator,
        block_i: usize,
        case_fixups: *std.ArrayList(CaseFixup),
        closure_fixups: *std.ArrayList(ClosureFixup),
    ) FuncBodyParser {
        return .{
            .allocator = allocator,
            .ops = .{},
            .case_fixups = case_fixups,
            .closure_fixups = closure_fixups,
            .block_i = block_i,
            .arith_kind = null,
            .arith_lhs = null,
            .arith_rhs = null,
            .move_src = null,
            .move_dst = null,
            .closure_caps = .{},
            .call_func = null,
            .call_args = .{},
            .pending_call_result = false,
            .pending_proj = null,
            .pending_proj_src = 0,
            .pending_src_ptr = null,
            .borrow_dst = null,
            .borrow_has_dst = false,
            .f_ptr_reg = null,
            .pair_fst = null,
            .pair_snd = null,
            .pair_pending_dst = null,
            .case_scr = null,
            .case_in_inl = false,
            .case_in_inr = false,
            .case_after_end = false,
            .case_inl_idx = null,
            .case_inr_idx = null,
            .ret_src = null,
        };
    }

    fn deinit(self: *FuncBodyParser) void {
        self.closure_caps.deinit(self.allocator);
        self.call_args.deinit(self.allocator);
    }

    pub fn processLine(self: *FuncBodyParser, line: []const u8) !void {
        if (line.len == 0) return;
        const a = self.allocator;

        // ---- 1. case_op ラベル ----
        if (std.mem.startsWith(u8, line, "case_inl_") and std.mem.endsWith(u8, line, ":")) {
            self.case_in_inl = true;
            self.case_in_inr = false;
            return;
        }
        if (std.mem.startsWith(u8, line, "case_inr_") and std.mem.endsWith(u8, line, ":")) {
            self.case_in_inr = true;
            self.case_in_inl = false;
            return;
        }
        if (std.mem.startsWith(u8, line, "case_end_") and std.mem.endsWith(u8, line, ":")) {
            self.case_in_inl = false;
            self.case_in_inr = false;
            self.case_after_end = true;
            return;
        }

        // ---- 2. case_op 分岐内部 ----
        if (self.case_in_inl or self.case_in_inr) {
            if (has(line, "call %Value @__block_") and has(line, "(ptr null,")) {
                const blk = uintAfter(line, "call %Value @__block_") orelse return;
                if (self.case_in_inl) self.case_inl_idx = blk else self.case_inr_idx = blk;
            }
            return;
        }

        // ---- 3. case_end 後: %dst_ptr_ GEP で case_op emit ----
        if (self.case_after_end) {
            if (gepReg(line)) |dst| {
                if (has(line, "%dst_ptr_")) {
                    const scr = self.case_scr orelse return;
                    const inl = self.case_inl_idx orelse return;
                    const inr = self.case_inr_idx orelse return;
                    const op_i = self.ops.items.len;
                    try self.ops.append(a, .{ .case_op = .{
                        .dst = dst,
                        .scrutinee = scr,
                        .inl_branch = &[_]Op{},
                        .inr_branch = &[_]Op{},
                    } });
                    try self.case_fixups.append(a, .{
                        .block_i = self.block_i,
                        .op_i = op_i,
                        .inl_block_idx = inl,
                        .inr_block_idx = inr,
                    });
                    self.case_scr = null;
                    self.case_inl_idx = null;
                    self.case_inr_idx = null;
                    self.case_after_end = false;
                }
            }
            return;
        }

        // ---- 4. %regs からの GEP ----
        if (gepReg(line)) |reg| {
            if (has(line, "%lhs_ptr_")) {
                self.arith_lhs = reg;
            } else if (has(line, "%rhs_ptr_")) {
                self.arith_rhs = reg;
            } else if (has(line, "%src_ptr_")) {
                self.pending_src_ptr = reg;
                self.borrow_has_dst = false;
                self.borrow_dst = null;
            } else if (has(line, "%src_") and !has(line, "_ptr_")) {
                self.move_src = reg;
            } else if (has(line, "%dst_ptr_")) {
                if (self.pending_proj != null) {
                    const kind = self.pending_proj.?;
                    const src = self.pending_proj_src;
                    self.pending_proj = null;
                    self.pending_src_ptr = null;
                    if (kind == .p1) {
                        try self.ops.append(a, .{ .proj1 = .{ .dst = reg, .src = src } });
                    } else {
                        try self.ops.append(a, .{ .proj2 = .{ .dst = reg, .src = src } });
                    }
                } else if (self.pending_call_result) {
                    const func = self.call_func orelse return;
                    const args = try a.dupe(u32, self.call_args.items);
                    try self.ops.append(a, .{ .call = .{
                        .dst = reg,
                        .func = func,
                        .args = args,
                    } });
                    self.call_func = null;
                    self.f_ptr_reg = null;
                    self.call_args.clearRetainingCapacity();
                    self.pending_call_result = false;
                    self.borrow_has_dst = false;
                    self.pending_src_ptr = null;
                } else if (self.pair_fst != null and self.pair_snd != null) {
                    self.pair_pending_dst = reg;
                } else if (self.pending_src_ptr != null and !self.borrow_has_dst) {
                    self.borrow_dst = reg;
                    self.borrow_has_dst = true;
                }
            } else if (has(line, "%dst_") and !has(line, "_ptr_")) {
                self.move_dst = reg;
            } else if (has(line, "%f_ptr_inline_")) {
                self.call_func = reg;
                self.f_ptr_reg = reg;
            } else if (has(line, "%f_ptr_")) {
                self.f_ptr_reg = reg;
                self.call_func = reg;
                self.pair_fst = null;
                self.pair_snd = null;
                self.pair_pending_dst = null;
            } else if (has(line, "%s_ptr_")) {
                self.pair_fst = self.f_ptr_reg;
                self.pair_snd = reg;
            } else if (has(line, "%scr_ptr_")) {
                self.case_scr = reg;
            } else if (has(line, "%cap_src_")) {
                try self.closure_caps.append(a, reg);
            } else if (has(line, "%arg_src_")) {
                try self.call_args.append(a, reg);
            } else if (has(line, "%ret_ptr_")) {
                self.ret_src = reg;
            }
            return;
        }

        // ---- 5. alloca %Pair → make_pair (stack alloc) ----
        if (has(line, "= alloca %Pair")) {
            if (self.pair_fst != null and self.pair_snd != null) {
                const dst = self.pair_pending_dst orelse return;
                try self.ops.append(a, .{ .make_pair = .{
                    .dst = dst,
                    .fst = self.pair_fst.?,
                    .snd = self.pair_snd.?,
                } });
                self.resetPairState();
            }
            return;
        }

        // ---- 6. 算術演算種別 ----
        if (has(line, " = add i")) { self.arith_kind = .add; return; }
        if (has(line, " = sub i")) { self.arith_kind = .sub; return; }
        if (has(line, " = mul i")) { self.arith_kind = .mul; return; }

        // ---- 7. call アンカー群 ----

        // load_const int
        if (has(line, "call void @rt_make_int(ptr %r_")) {
            const dst = uintAfter(line, "ptr %r_") orelse return;
            const val = i64After(line, "i64 ") orelse return;
            const pos = std.mem.indexOf(u8, line, "i64 ") orelse return;
            const after = std.mem.trim(u8, line[pos + 4 ..], " ");
            if (after.len == 0) return;
            const fc = after[0];
            if (fc != '-' and (fc < '0' or fc > '9')) return;
            try self.ops.append(a, .{ .load_const = .{
                .dst = dst,
                .val = .{ .int = @bitCast(@as(i64, val)) },
            } });
            return;
        }

        // load_const unit
        if (has(line, "call void @rt_make_unit(ptr %r_")) {
            const dst = uintAfter(line, "ptr %r_") orelse return;
            try self.ops.append(a, .{ .load_const = .{ .dst = dst, .val = .unit } });
            return;
        }

        // free
        if (has(line, "call void @rt_make_unit(ptr %reg_ptr_")) {
            const reg = uintAfter(line, "ptr %reg_ptr_") orelse return;
            try self.ops.append(a, .{ .free = .{ .reg = reg } });
            return;
        }

        // 算術演算結果
        if (has(line, "call void @rt_make_int(ptr %dst_ptr_")) {
            if (self.arith_kind) |kind| {
                const dst = uintAfter(line, "ptr %dst_ptr_") orelse return;
                const lhs = self.arith_lhs orelse return;
                const rhs = self.arith_rhs orelse return;
                switch (kind) {
                    .add => try self.ops.append(a, .{ .add = .{ .dst = dst, .lhs = lhs, .rhs = rhs } }),
                    .sub => try self.ops.append(a, .{ .sub = .{ .dst = dst, .lhs = lhs, .rhs = rhs } }),
                    .mul => try self.ops.append(a, .{ .mul = .{ .dst = dst, .lhs = lhs, .rhs = rhs } }),
                }
                self.arith_kind = null;
                self.arith_lhs = null;
                self.arith_rhs = null;
            }
            return;
        }

        // make_closure
        if (has(line, "call void @rt_make_closure(")) {
            const dst = uintAfter(line, "ptr %dst_ptr_") orelse return;
            const blk = uintAfter(line, "ptr @__block_") orelse return;
            const arity = uintAfter(line, ", i32 ") orelse return;
            const caps = try a.dupe(u32, self.closure_caps.items);
            const op_i = self.ops.items.len;
            try self.ops.append(a, .{ .make_closure = .{
                .dst = dst,
                .body = &[_]Op{},
                .captures = caps,
                .arity = arity,
                .block_idx = blk,
            } });
            try self.closure_fixups.append(a, .{
                .block_i = self.block_i,
                .op_i = op_i,
                .target_block_idx = blk,
            });
            self.closure_caps.clearRetainingCapacity();
            return;
        }

        // call (通常)
        if (has(line, "call %Value @rt_call(")) {
            self.pending_call_result = true;
            return;
        }

        // call (インライン化、env != null)
        if (has(line, "call %Value @__block_") and !has(line, "(ptr null,")) {
            self.pending_call_result = true;
            return;
        }

        // proj1
        if (has(line, "call %Value @rt_proj1(ptr %src_ptr_")) {
            const src = uintAfter(line, "ptr %src_ptr_") orelse return;
            self.pending_proj = .p1;
            self.pending_proj_src = src;
            self.pending_src_ptr = null;
            self.borrow_has_dst = false;
            return;
        }

        // proj2
        if (has(line, "call %Value @rt_proj2(ptr %src_ptr_")) {
            const src = uintAfter(line, "ptr %src_ptr_") orelse return;
            self.pending_proj = .p2;
            self.pending_proj_src = src;
            self.pending_src_ptr = null;
            self.borrow_has_dst = false;
            return;
        }

        // make_inl
        if (has(line, "call void @rt_make_sum(") and has(line, "i64 3)")) {
            const dst = uintAfter(line, "ptr %dst_ptr_") orelse return;
            const src = self.pending_src_ptr orelse return;
            try self.ops.append(a, .{ .make_inl = .{ .dst = dst, .src = src } });
            self.pending_src_ptr = null;
            self.borrow_has_dst = false;
            return;
        }

        // make_inr
        if (has(line, "call void @rt_make_sum(") and has(line, "i64 4)")) {
            const dst = uintAfter(line, "ptr %dst_ptr_") orelse return;
            const src = self.pending_src_ptr orelse return;
            try self.ops.append(a, .{ .make_inr = .{ .dst = dst, .src = src } });
            self.pending_src_ptr = null;
            self.borrow_has_dst = false;
            return;
        }

        // make_pair (escape パス)
        if (has(line, "call void @rt_make_pair(ptr %dst_ptr_")) {
            const dst = uintAfter(line, "ptr %dst_ptr_") orelse return;
            const fst = self.pair_fst orelse self.f_ptr_reg orelse return;
            const snd = self.pair_snd orelse return;
            try self.ops.append(a, .{ .make_pair = .{ .dst = dst, .fst = fst, .snd = snd } });
            self.resetPairState();
            return;
        }

        // move 終端
        if (has(line, "call void @rt_init_unit(ptr %src_") and
            !has(line, "ptr %src_ptr_"))
        {
            if (self.move_dst) |dst| {
                if (self.move_src) |src| {
                    try self.ops.append(a, .{ .move = .{ .dst = dst, .src = src } });
                    self.move_src = null;
                    self.move_dst = null;
                }
            }
            return;
        }

        // ---- 8. borrow ----
        if (has(line, "store %Value ") and has(line, "ptr %dst_ptr_") and
            self.pending_src_ptr != null and self.borrow_has_dst and
            self.pending_proj == null and !self.pending_call_result)
        {
            const src = self.pending_src_ptr.?;
            const dst = self.borrow_dst orelse return;
            try self.ops.append(a, .{ .borrow = .{ .dst = dst, .src = src } });
            self.pending_src_ptr = null;
            self.borrow_dst = null;
            self.borrow_has_dst = false;
            return;
        }

        // ---- 9. ret ----
        if (std.mem.startsWith(u8, line, "ret i64 %v") or
            std.mem.startsWith(u8, line, "ret %Value %v"))
        {
            if (self.ret_src) |src| {
                try self.ops.append(a, .{ .ret = .{ .src = src } });
                self.ret_src = null;
            }
            return;
        }
    }

    fn resetPairState(self: *FuncBodyParser) void {
        self.pair_fst = null;
        self.pair_snd = null;
        self.pair_pending_dst = null;
        self.f_ptr_reg = null;
    }
};

// ============================================================
// モジュールパーサー
// ============================================================

fn findFuncEnd(lines: []const []const u8, define_line: usize) usize {
    var i = define_line + 1;
    while (i < lines.len) : (i += 1) {
        if (std.mem.eql(u8, lines[i], "}")) return i;
    }
    return lines.len;
}

const ModuleParser = struct {
    allocator: Allocator,
    lines: [][]const u8,

    fn init(allocator: Allocator, ir_text: []const u8) !ModuleParser {
        var list = std.ArrayList([]const u8){};
        errdefer list.deinit(allocator);
        var iter = std.mem.splitScalar(u8, ir_text, '\n');
        while (iter.next()) |raw| {
            try list.append(allocator, std.mem.trim(u8, raw, " \t\r"));
        }
        return .{
            .allocator = allocator,
            .lines = try list.toOwnedSlice(allocator),
        };
    }

    fn deinit(self: *ModuleParser) void {
        self.allocator.free(self.lines);
    }

    fn parse(self: *ModuleParser) !loader.LoadedProgram {
        const a = self.allocator;

        var case_fixups = std.ArrayList(CaseFixup){};
        defer case_fixups.deinit(a);
        var closure_fixups = std.ArrayList(ClosureFixup){};
        defer closure_fixups.deinit(a);

        const FuncInfo = struct {
            idx: u32,
            body_start: usize,
            body_end: usize,
        };
        var funcs = std.ArrayList(FuncInfo){};
        defer funcs.deinit(a);

        var max_idx: u32 = 0;
        var has_entry = false;

        {
            var li: usize = 0;
            while (li < self.lines.len) : (li += 1) {
                const line = self.lines[li];
                if (!std.mem.startsWith(u8, line, "define ")) continue;
                if (has(line, "@rt_") or has(line, "@printf") or
                    has(line, "@malloc") or has(line, "@free") or
                    has(line, "@exit"))
                {
                    li = findFuncEnd(self.lines, li);
                    continue;
                }
                const body_start = li + 1;
                const body_end = findFuncEnd(self.lines, li);

                if (has(line, "@__block_")) {
                    const blk = uintAfter(line, "@__block_") orelse {
                        li = body_end;
                        continue;
                    };
                    if (blk > max_idx) max_idx = blk;
                    try funcs.append(a, .{ .idx = blk, .body_start = body_start, .body_end = body_end });
                } else if (!has_entry) {
                    try funcs.append(a, .{ .idx = 0, .body_start = body_start, .body_end = body_end });
                    has_entry = true;
                }
                li = body_end;
            }
        }

        if (funcs.items.len == 0) return error.MissingEntryPoint;

        const num_blocks = max_idx + 1;
        const raw_blocks = try a.alloc(?[]Op, num_blocks);
        defer a.free(raw_blocks);
        for (raw_blocks) |*b| b.* = null;

        for (funcs.items) |fi| {
            if (fi.idx >= num_blocks) continue;
            var parser = FuncBodyParser.init(a, fi.idx, &case_fixups, &closure_fixups);
            defer parser.deinit();
            for (self.lines[fi.body_start..fi.body_end]) |line| {
                try parser.processLine(line);
            }
            raw_blocks[fi.idx] = try parser.ops.toOwnedSlice(a);
        }

        for (raw_blocks) |*rb| {
            if (rb.* == null) rb.* = try a.alloc(Op, 0);
        }

        for (closure_fixups.items) |fx| {
            const target: []const Op = if (fx.target_block_idx < num_blocks)
                raw_blocks[fx.target_block_idx].?
            else
                &[_]Op{};
            raw_blocks[fx.block_i].?[fx.op_i].make_closure.body = target;
        }

        for (case_fixups.items) |fx| {
            const inl: []const Op = if (fx.inl_block_idx < num_blocks) raw_blocks[fx.inl_block_idx].? else &[_]Op{};
            const inr: []const Op = if (fx.inr_block_idx < num_blocks) raw_blocks[fx.inr_block_idx].? else &[_]Op{};
            raw_blocks[fx.block_i].?[fx.op_i].case_op.inl_branch = inl;
            raw_blocks[fx.block_i].?[fx.op_i].case_op.inr_branch = inr;
        }

        const blocks = try a.alloc([]const Op, num_blocks);
        for (raw_blocks, 0..) |rb, i| blocks[i] = rb.?;

        return loader.LoadedProgram{
            .allocator = a,
            .constants = try a.alloc(Value, 0),
            .blocks = blocks,
        };
    }
};

// ============================================================
// 公開インターフェース
// ============================================================

/// codegen.generate() が生成した LLVM IR テキストを LoadedProgram へ変換する
pub const Decompiler = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Decompiler {
        return .{ .allocator = allocator };
    }

    pub fn decompile(self: *Decompiler, ir_text: []const u8) DecompileError!loader.LoadedProgram {
        var mod = ModuleParser.init(self.allocator, ir_text) catch return error.OutOfMemory;
        defer mod.deinit();
        return mod.parse() catch |err| switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.MissingEntryPoint => error.MissingEntryPoint,
        };
    }
};

// ============================================================
// テスト
// ============================================================

test "uintAfter / gepReg basic" {
    try std.testing.expectEqual(@as(?u32, 5), uintAfter("ptr %r_5, i64 42", "ptr %r_"));
    try std.testing.expectEqual(@as(?u32, 42), uintAfter("i64 42)", "i64 "));
    try std.testing.expectEqual(@as(?u32, null), uintAfter("foo", "bar"));
    try std.testing.expectEqual(
        @as(?u32, 3),
        gepReg("  %lhs_ptr_3 = getelementptr %Value, ptr %regs, i32 3"),
    );
}

test "Decompiler: load_const int + ret" {
    const allocator = std.testing.allocator;
    const ir =
        \\define dllexport i64 @main(ptr %external_regs) {
        \\entry:
        \\  %regs = alloca %Value, i32 1
        \\  %r_ptr_0 = getelementptr %Value, ptr %regs, i32 0
        \\  call void @rt_init_unit(ptr %r_ptr_0)
        \\  br label %body
        \\deopt_exit:
        \\  %deopt_val = or i64 0, 2147483648
        \\  ret i64 %deopt_val
        \\body:
        \\  %r_0 = getelementptr %Value, ptr %regs, i32 0
        \\  call void @rt_make_int(ptr %r_0, i64 99)
        \\  %ret_ptr_0 = getelementptr %Value, ptr %regs, i32 0
        \\  %v0 = call i64 @rt_get_int(ptr %ret_ptr_0)
        \\  ret i64 %v0
        \\  unreachable
        \\}
    ;
    var d = Decompiler.init(allocator);
    var prog = try d.decompile(ir);
    defer prog.deinit();
    const code = prog.mainCode();
    try std.testing.expectEqual(@as(usize, 2), code.len);
    try std.testing.expectEqual(@as(u32, 0), code[0].load_const.dst);
    try std.testing.expectEqual(@as(u64, 99), code[0].load_const.val.int);
    try std.testing.expectEqual(@as(u32, 0), code[1].ret.src);
}

test "Decompiler: add op" {
    const allocator = std.testing.allocator;
    const ir =
        \\define dllexport i64 @main(ptr %external_regs) {
        \\entry:
        \\  %regs = alloca %Value, i32 3
        \\  br label %body
        \\deopt_exit:
        \\  %deopt_val = or i64 0, 2147483648
        \\  ret i64 %deopt_val
        \\body:
        \\  %r_0 = getelementptr %Value, ptr %regs, i32 0
        \\  call void @rt_make_int(ptr %r_0, i64 10)
        \\  %r_1 = getelementptr %Value, ptr %regs, i32 1
        \\  call void @rt_make_int(ptr %r_1, i64 20)
        \\  %lhs_ptr_0 = getelementptr %Value, ptr %regs, i32 0
        \\  %rhs_ptr_1 = getelementptr %Value, ptr %regs, i32 1
        \\  %v10 = call i64 @rt_get_int(ptr %lhs_ptr_0)
        \\  %v11 = call i64 @rt_get_int(ptr %rhs_ptr_1)
        \\  %v12 = add i64 %v10, %v11
        \\  %dst_ptr_2 = getelementptr %Value, ptr %regs, i32 2
        \\  call void @rt_make_int(ptr %dst_ptr_2, i64 %v12)
        \\  %ret_ptr_2 = getelementptr %Value, ptr %regs, i32 2
        \\  %v20 = call i64 @rt_get_int(ptr %ret_ptr_2)
        \\  ret i64 %v20
        \\  unreachable
        \\}
    ;
    var d = Decompiler.init(allocator);
    var prog = try d.decompile(ir);
    defer prog.deinit();
    const code = prog.mainCode();
    try std.testing.expectEqual(@as(usize, 4), code.len);
    try std.testing.expectEqual(@as(u64, 10), code[0].load_const.val.int);
    try std.testing.expectEqual(@as(u64, 20), code[1].load_const.val.int);
    try std.testing.expectEqual(@as(u32, 2), code[2].add.dst);
    try std.testing.expectEqual(@as(u32, 0), code[2].add.lhs);
    try std.testing.expectEqual(@as(u32, 1), code[2].add.rhs);
    try std.testing.expectEqual(@as(u32, 2), code[3].ret.src);
}

test "Decompiler: make_pair + proj1" {
    const allocator = std.testing.allocator;
    const ir =
        \\define dllexport i64 @main(ptr %external_regs) {
        \\entry:
        \\  %regs = alloca %Value, i32 3
        \\  br label %body
        \\deopt_exit:
        \\  %deopt_val = or i64 0, 2147483648
        \\  ret i64 %deopt_val
        \\body:
        \\  %r_0 = getelementptr %Value, ptr %regs, i32 0
        \\  call void @rt_make_int(ptr %r_0, i64 7)
        \\  %r_1 = getelementptr %Value, ptr %regs, i32 1
        \\  call void @rt_make_unit(ptr %r_1)
        \\  %f_ptr_0 = getelementptr %Value, ptr %regs, i32 0
        \\  %s_ptr_1 = getelementptr %Value, ptr %regs, i32 1
        \\  %v0 = load %Value, ptr %f_ptr_0
        \\  %v1 = load %Value, ptr %s_ptr_1
        \\  %dst_ptr_2 = getelementptr %Value, ptr %regs, i32 2
        \\  call void @rt_make_pair(ptr %dst_ptr_2, %Value %v0, %Value %v1)
        \\  call void @rt_init_unit(ptr %f_ptr_0)
        \\  call void @rt_init_unit(ptr %s_ptr_1)
        \\  %src_ptr_2 = getelementptr %Value, ptr %regs, i32 2
        \\  %v2 = load %Value, ptr %src_ptr_2
        \\  %v3 = call %Value @rt_proj1(ptr %src_ptr_2, %Value %v2)
        \\  %dst_ptr_0 = getelementptr %Value, ptr %regs, i32 0
        \\  store %Value %v3, ptr %dst_ptr_0
        \\  %ret_ptr_0 = getelementptr %Value, ptr %regs, i32 0
        \\  %v4 = load %Value, ptr %ret_ptr_0
        \\  ret %Value %v4
        \\  unreachable
        \\}
    ;
    var d = Decompiler.init(allocator);
    var prog = try d.decompile(ir);
    defer prog.deinit();
    const code = prog.mainCode();
    // load_const 7, load_const unit, make_pair 2←0,1, proj1 0←2, ret 0
    try std.testing.expectEqual(@as(usize, 5), code.len);
    try std.testing.expectEqual(@as(u64, 7), code[0].load_const.val.int);
    try std.testing.expect(code[1].load_const.val == .unit);
    try std.testing.expectEqual(@as(u32, 2), code[2].make_pair.dst);
    try std.testing.expectEqual(@as(u32, 0), code[2].make_pair.fst);
    try std.testing.expectEqual(@as(u32, 1), code[2].make_pair.snd);
    try std.testing.expectEqual(@as(u32, 0), code[3].proj1.dst);
    try std.testing.expectEqual(@as(u32, 2), code[3].proj1.src);
    try std.testing.expectEqual(@as(u32, 0), code[4].ret.src);
}
