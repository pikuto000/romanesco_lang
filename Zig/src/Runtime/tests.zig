/// tests.zig — Runtime 全テスト統合ファイル
/// 実行: zig test src/Runtime/tests.zig
const std = @import("std");
const vm = @import("vm.zig");
const analyzer = @import("analyzer.zig");
const codegen = @import("codegen.zig");
const lifter = @import("lifter.zig");
const loader = @import("loader.zig");
const ir_parser = @import("ir_parser.zig");
const decompiler = @import("decompiler.zig");
const speculative = @import("speculative.zig");

// ---- 共通ヘルパー ----

fn makeEmptyAnalysis(allocator: std.mem.Allocator) analyzer.RangeAnalysisResult {
    return .{
        .bit_widths = std.AutoHashMap(u32, u8).init(allocator),
        .escapes = std.AutoHashMap(u32, void).init(allocator),
        .stable_values = std.AutoHashMap(u32, u64).init(allocator),
        .inlining_hints = std.AutoHashMap(usize, usize).init(allocator),
        .tag_types = std.AutoHashMap(u32, analyzer.TagType).init(allocator),
        .allocator = allocator,
    };
}

const default_cpu = vm.CpuFeatures{ .has_avx2 = false, .has_neon = false, .vector_width = 64 };

// ================================================================
// VM テスト
// ================================================================

test "VM basic operations" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);

    // LoadConst + Return
    {
        const code = &[_]vm.Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 42 } } },
            .{ .ret = .{ .src = 0 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), result.bits);
    }

    // MakePair + Proj1
    {
        const code = &[_]vm.Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
            .{ .load_const = .{ .dst = 1, .val = .{ .bits = 20 } } },
            .{ .make_pair = .{ .dst = 2, .fst = 0, .snd = 1 } },
            .{ .proj1 = .{ .dst = 3, .src = 2 } },
            .{ .ret = .{ .src = 3 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 10), result.bits);
    }

    // MakePair + Proj2
    {
        const code = &[_]vm.Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
            .{ .load_const = .{ .dst = 1, .val = .{ .bits = 20 } } },
            .{ .make_pair = .{ .dst = 2, .fst = 0, .snd = 1 } },
            .{ .proj2 = .{ .dst = 3, .src = 2 } },
            .{ .ret = .{ .src = 3 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 20), result.bits);
    }
}

test "VM closure" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);

    // Identity function: (lambda x. x)(42) = 42
    {
        const id_body = &[_]vm.Op{
            .{ .ret = .{ .src = 0 } },
        };
        const code = &[_]vm.Op{
            .{ .make_closure = .{ .dst = 0, .body = id_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 0 } },
            .{ .load_const = .{ .dst = 1, .val = .{ .bits = 42 } } },
            .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), result.bits);
    }
}

test "VM Sum and Case" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);

    // Case(inl(x), ...)
    {
        const inl_branch = &[_]vm.Op{
            .{ .ret = .{ .src = 2 } },
        };
        const inr_branch = &[_]vm.Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 999 } } },
            .{ .ret = .{ .src = 0 } },
        };
        const code = &[_]vm.Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 123 } } },
            .{ .make_inl = .{ .dst = 1, .src = 0 } },
            .{ .case_op = .{ .dst = 2, .scrutinee = 1, .inl_branch = inl_branch, .inr_branch = inr_branch } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 123), result.bits);
    }
}

test "ProfilingVM" {
    const allocator = std.testing.allocator;
    var pvm = vm.ProfilingVM.init(allocator);
    defer pvm.deinit();

    const code = &[_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 1 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 2 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };

    const result = try pvm.run(code);
    defer result.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 3), result.bits);
    try std.testing.expect(pvm.profile_data.counts.get(0).? >= 1);
    try std.testing.expect(pvm.profile_data.counts.get(1).? >= 1);
    try std.testing.expect(pvm.profile_data.counts.get(2).? >= 1);
}

test "VM nested closure" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);

    // λx. λy. x
    const inner_body = &[_]vm.Op{
        .{ .ret = .{ .src = 0 } },
    };
    const outer_body = &[_]vm.Op{
        .{ .make_closure = .{ .dst = 1, .body = inner_body, .captures = &[_]u32{0}, .arity = 1, .block_idx = 100 } },
        .{ .ret = .{ .src = 1 } },
    };

    const code = &[_]vm.Op{
        .{ .make_closure = .{ .dst = 0, .body = outer_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 101 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 100 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
        .{ .load_const = .{ .dst = 3, .val = .unit } },
        .{ .call = .{ .dst = 4, .func = 2, .args = &[_]u32{3} } },
        .{ .ret = .{ .src = 4 } },
    };

    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 100), result.bits);
}

test "bigintMul: 3 * 5 = 15" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const lhs = [_]u64{3};
    const rhs = [_]u64{5};
    const res = try vm_inst.bigintMul(&lhs, &rhs, 64);
    defer allocator.free(res);
    try std.testing.expectEqual(@as(usize, 1), res.len);
    try std.testing.expectEqual(@as(u64, 15), res[0]);
}

test "bigintMul: 2^64 * 1" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const lhs = [_]u64{ 0, 1 }; // 2^64
    const rhs = [_]u64{1};
    const res = try vm_inst.bigintMul(&lhs, &rhs, 128);
    defer allocator.free(res);
    try std.testing.expectEqual(@as(usize, 2), res.len);
    try std.testing.expectEqual(@as(u64, 0), res[0]);
    try std.testing.expectEqual(@as(u64, 1), res[1]);
}

test "bigintBitwise: and/or/xor 128bit" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const lhs = [_]u64{ 0xF0F0F0F0F0F0F0F0, 0xAAAAAAAAAAAAAAA };
    const rhs = [_]u64{ 0x0F0F0F0F0F0F0F0F, 0x555555555555555 };

    {
        const res = try vm_inst.bigintBitwise(&lhs, &rhs, .and_, 128);
        defer allocator.free(res);
        try std.testing.expectEqual(@as(u64, 0), res[0]);
        try std.testing.expectEqual(@as(u64, 0), res[1]);
    }
    {
        const res = try vm_inst.bigintBitwise(&lhs, &rhs, .or_, 128);
        defer allocator.free(res);
        try std.testing.expectEqual(@as(u64, 0xFFFFFFFFFFFFFFFF), res[0]);
        try std.testing.expectEqual(@as(u64, 0xFFFFFFFFFFFFFFF), res[1]);
    }
}

test "ibin wide add i128: (2^64-1) + 1" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const code = &[_]vm.Op{
        .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 0xFFFFFFFFFFFFFFFF, 0 }, .width = 128 } },
        .{ .load_wide = .{ .dst = 1, .limbs = &[_]u64{ 1, 0 }, .width = 128 } },
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .add, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 0), result.wide[0]);
    try std.testing.expectEqual(@as(u64, 1), result.wide[1]);
}

test "ibin wide mul i128: 3 * 5 = 15" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const code = &[_]vm.Op{
        .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 3, 0 }, .width = 128 } },
        .{ .load_wide = .{ .dst = 1, .limbs = &[_]u64{ 5, 0 }, .width = 128 } },
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .mul, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 15), result.wide[0]);
    try std.testing.expectEqual(@as(u64, 0), result.wide[1]);
}

test "bigintCmp: unsigned 128bit" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const lhs = [_]u64{ 0, 1 }; // 2^64
    const rhs = [_]u64{ 0xFFFFFFFFFFFFFFFF, 0 }; // 2^64 - 1

    try std.testing.expect(vm_inst.bigintCmp(&lhs, &rhs, 128, false) == .gt);
    try std.testing.expect(vm_inst.bigintCmp(&rhs, &lhs, 128, false) == .lt);
    try std.testing.expect(vm_inst.bigintCmp(&lhs, &lhs, 128, false) == .eq);
}

test "bigintCmp: signed 128bit" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const pos = [_]u64{ 1, 0 };
    const neg = [_]u64{ 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF }; // -1

    try std.testing.expect(vm_inst.bigintCmp(&pos, &neg, 128, true) == .gt);
    try std.testing.expect(vm_inst.bigintCmp(&neg, &pos, 128, true) == .lt);
}

test "ibin wide shl i128" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const code = &[_]vm.Op{
        .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 1, 0 }, .width = 128 } },
        .{ .load_bits = .{ .dst = 1, .val = 64, .width = 64 } },
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .shl, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 0), result.wide[0]);
    try std.testing.expectEqual(@as(u64, 1), result.wide[1]);
}

test "ibin wide lshr i128" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const code = &[_]vm.Op{
        .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 0, 1 }, .width = 128 } },
        .{ .load_bits = .{ .dst = 1, .val = 64, .width = 64 } },
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .lshr, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 1), result.wide[0]);
    try std.testing.expectEqual(@as(u64, 0), result.wide[1]);
}

test "ibin wide ashr i128" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    const code = &[_]vm.Op{
        // -2^127
        .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 0, 0x8000000000000000 }, .width = 128 } },
        .{ .load_bits = .{ .dst = 1, .val = 1, .width = 64 } },
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .ashr, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    // -2^126 = 上位リムに 0xC000000000000000
    try std.testing.expectEqual(@as(u64, 0), result.wide[0]);
    try std.testing.expectEqual(@as(u64, 0xC000000000000000), result.wide[1]);
}

test "icmp wide 128bit" {
    const allocator = std.testing.allocator;
    var vm_inst = vm.VM.init(allocator);
    {
        // 2^64 > 2^64 - 1
        const code = &[_]vm.Op{
            .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 0, 1 }, .width = 128 } },
            .{ .load_wide = .{ .dst = 1, .limbs = &[_]u64{ 0xFFFFFFFFFFFFFFFF, 0 }, .width = 128 } },
            .{ .icmp = .{ .dst = 2, .lhs = 0, .rhs = 1, .pred = .ugt, .width = 128 } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expect(std.meta.activeTag(result) == .inl); // true
    }
    {
        // 2^64 == 2^64
        const code = &[_]vm.Op{
            .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 0, 1 }, .width = 128 } },
            .{ .load_wide = .{ .dst = 1, .limbs = &[_]u64{ 0, 1 }, .width = 128 } },
            .{ .icmp = .{ .dst = 2, .lhs = 0, .rhs = 1, .pred = .eq, .width = 128 } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expect(std.meta.activeTag(result) == .inl); // true
    }
}

// ================================================================
// Analyzer テスト
// ================================================================

test "TagInference: load_const bits propagates through add" {
    const allocator = std.testing.allocator;
    var az = analyzer.RangeAnalyzer.init(allocator);
    const code = [_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 20 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    var result = try az.analyze(&code, null, 0);
    defer result.deinit();
    try std.testing.expect(result.isProvenBits(0));
    try std.testing.expect(result.isProvenBits(1));
    try std.testing.expect(result.isProvenBits(2));
}

test "TagInference: ibin w64 → bits, w128 → wide" {
    const allocator = std.testing.allocator;
    var az = analyzer.RangeAnalyzer.init(allocator);
    const code = [_]vm.Op{
        .{ .load_bits = .{ .dst = 0, .val = 5, .width = 64 } },
        .{ .load_bits = .{ .dst = 1, .val = 3, .width = 64 } },
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .add, .width = 64 } },
        .{ .load_wide = .{ .dst = 3, .limbs = &[_]u64{ 1, 0 }, .width = 128 } },
        .{ .load_wide = .{ .dst = 4, .limbs = &[_]u64{ 2, 0 }, .width = 128 } },
        .{ .ibin = .{ .dst = 5, .lhs = 3, .rhs = 4, .op = .add, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    var result = try az.analyze(&code, null, 0);
    defer result.deinit();
    try std.testing.expect(result.isProvenBits(0));
    try std.testing.expect(result.isProvenBits(2));
    try std.testing.expect(!result.isProvenBits(5));
    try std.testing.expectEqual(analyzer.TagType.wide, result.tag_types.get(5) orelse .unknown);
}

test "TagInference: call → unknown, prevents guard elision" {
    const allocator = std.testing.allocator;
    var az = analyzer.RangeAnalyzer.init(allocator);
    const args: []const u32 = &[_]u32{};
    const code = [_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 0 } } },
        .{ .call = .{ .dst = 1, .func = 0, .args = args } },
        .{ .add = .{ .dst = 2, .lhs = 1, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    var result = try az.analyze(&code, null, 0);
    defer result.deinit();
    // call結果はunknown
    try std.testing.expect(!result.isProvenBits(1));
    try std.testing.expectEqual(analyzer.TagType.unknown, result.tag_types.get(1) orelse .unknown);
    // addはbitsを返すので2はbits
    try std.testing.expect(result.isProvenBits(2));
}

test "TagInference: borrow inherits source tag" {
    const allocator = std.testing.allocator;
    var az = analyzer.RangeAnalyzer.init(allocator);
    const code = [_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 42 } } },
        .{ .borrow = .{ .dst = 1, .src = 0 } },
        .{ .ret = .{ .src = 1 } },
    };
    var result = try az.analyze(&code, null, 0);
    defer result.deinit();
    try std.testing.expect(result.isProvenBits(1));
}

// ================================================================
// Codegen テスト
// ================================================================

test "Codegen: load_bits emits rt_make_int" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .load_bits = .{ .dst = 0, .val = 42, .width = 64 } },
        .{ .ret = .{ .src = 0 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_load_bits", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_make_int") != null);
}

test "Codegen: ibin add64 emits add i64" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .add, .width = 64 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_ibin_add", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "add i64") != null);
}

test "Codegen: fadd emits bitcast and fadd double" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .fadd = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_fadd", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "bitcast i64") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "fadd double") != null);
}

test "Codegen: icmp eq32 emits br i1 and rt_make_sum" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .icmp = .{ .dst = 2, .lhs = 0, .rhs = 1, .pred = .eq, .width = 32 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_icmp_eq32", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "br i1") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_make_sum") != null);
}

test "Codegen: load_wide emits rt_make_wide" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .load_wide = .{ .dst = 0, .limbs = &[_]u64{ 1, 2 }, .width = 128 } },
        .{ .ret = .{ .src = 0 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_load_wide", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_make_wide") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "alloca [2 x i64]") != null);
}

test "Codegen: ibin add128 emits add i128 and rt_make_wide" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .add, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_ibin_add128", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "add i128") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_make_wide") != null);
}

test "Codegen: icmp eq128 emits icmp eq i128 and rt_get_wide_limb" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .icmp = .{ .dst = 2, .lhs = 0, .rhs = 1, .pred = .eq, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_icmp_eq128", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "icmp eq i128") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_get_wide_limb") != null);
}

test "Codegen: add with proven bits skips rt_guard_tag" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 20 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var az = analyzer.RangeAnalyzer.init(allocator);
    var analysis = try az.analyze(&code, null, 0);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_add_bits_proven", default_cpu);
    defer allocator.free(ir);
    // bits証明済みなのでcall命令としてのrt_guard_tagが含まれないことを確認
    try std.testing.expect(std.mem.indexOf(u8, ir, "call i1 @rt_guard_tag") == null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_get_int") != null);
}

test "Codegen: add with unknown type keeps rt_guard_tag" {
    const allocator = std.testing.allocator;
    const args: []const u32 = &[_]u32{};
    const code = [_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 0 } } },
        .{ .call = .{ .dst = 1, .func = 0, .args = args } },
        .{ .add = .{ .dst = 2, .lhs = 1, .rhs = 0 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&code};
    var az = analyzer.RangeAnalyzer.init(allocator);
    var analysis = try az.analyze(&code, null, 0);
    defer analysis.deinit();
    try std.testing.expect(!analysis.isProvenBits(1));
    var cg = codegen.CodeGen.init(allocator);
    const ir = try cg.generate(&program, analysis, "test_add_unknown", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "call i1 @rt_guard_tag") != null);
}

// ================================================================
// IR Parser テスト
// ================================================================

test "IRParser: basic function and instruction" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i64 @add(i64 %a, i64 %b) {
        \\entry:
        \\  %1 = add i64 %a, %b
        \\  ret i64 %1
        \\}
    ;
    var parser = ir_parser.IRParser.init(allocator);
    var mod = try parser.parse(ir_src);
    defer mod.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), mod.functions.len);
    const f = mod.functions[0];
    try std.testing.expectEqualStrings("add", f.name);
    try std.testing.expectEqualStrings("i64", f.ret_type);
    try std.testing.expectEqual(@as(usize, 2), f.params.len);
    try std.testing.expectEqualStrings("i64", f.params[0].type_name);
    try std.testing.expectEqualStrings("%a", f.params[0].name);

    try std.testing.expectEqual(@as(usize, 1), f.blocks.len);
    const b = f.blocks[0];
    try std.testing.expectEqualStrings("entry", b.label);
    try std.testing.expectEqual(@as(usize, 2), b.instructions.len);

    const insn0 = b.instructions[0];
    try std.testing.expectEqualStrings("%1", insn0.result.?);
    try std.testing.expectEqualStrings("add", insn0.opcode);
    try std.testing.expectEqual(@as(usize, 3), insn0.operands.len);
    try std.testing.expectEqualStrings("i64", insn0.operands[0]);
    try std.testing.expectEqualStrings("%a", insn0.operands[1]);
    try std.testing.expectEqualStrings("%b", insn0.operands[2]);

    const insn1 = b.instructions[1];
    try std.testing.expect(insn1.result == null);
    try std.testing.expectEqualStrings("ret", insn1.opcode);
}

test "IRParser: declare" {
    const allocator = std.testing.allocator;
    const ir_src = "declare void @rt_make_int(ptr, i64)";
    var parser = ir_parser.IRParser.init(allocator);
    var mod = try parser.parse(ir_src);
    defer mod.deinit(allocator);

    try std.testing.expectEqual(@as(usize, 1), mod.functions.len);
    const f = mod.functions[0];
    try std.testing.expectEqualStrings("rt_make_int", f.name);
    try std.testing.expect(f.is_declare);
    try std.testing.expectEqual(@as(usize, 0), f.blocks.len);
}

// ================================================================
// BytecodeLoader テスト
// ================================================================

test "BytecodeLoader: Roundtrip test" {
    const allocator = std.testing.allocator;
    const path = "test_roundtrip.rbc";

    // 1. RBCファイルを手動作成
    {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();

        try file.writeAll("RBC\x01");

        var buf4: [4]u8 = undefined;
        var buf8: [8]u8 = undefined;

        std.mem.writeInt(u32, &buf4, 1, .little);
        try file.writeAll(&buf4); // version

        // Constant Pool (1 value: 42)
        std.mem.writeInt(u32, &buf4, 1, .little);
        try file.writeAll(&buf4);
        try file.writeAll(&[_]u8{6}); // tag int
        std.mem.writeInt(u64, &buf8, 42, .little);
        try file.writeAll(&buf8);

        // Code Blocks (1 block)
        std.mem.writeInt(u32, &buf4, 1, .little);
        try file.writeAll(&buf4);
        std.mem.writeInt(u32, &buf4, 2, .little); // 2 ops
        try file.writeAll(&buf4);

        try file.writeAll(&[_]u8{0x02}); // LoadConst
        std.mem.writeInt(u32, &buf4, 0, .little); // dst
        try file.writeAll(&buf4);
        std.mem.writeInt(u32, &buf4, 0, .little); // const_idx
        try file.writeAll(&buf4);

        try file.writeAll(&[_]u8{0x05}); // Return
        std.mem.writeInt(u32, &buf4, 0, .little); // src
        try file.writeAll(&buf4);
    }
    defer std.fs.cwd().deleteFile(path) catch {};

    // 2. ロード
    var bl = loader.BytecodeLoader.init(allocator);
    var prog = try bl.loadFromFile(path);
    defer prog.deinit();

    try std.testing.expectEqual(@as(usize, 1), prog.constants.len);
    try std.testing.expectEqual(@as(u64, 42), prog.constants[0].bits);
    try std.testing.expectEqual(@as(usize, 1), prog.blocks.len);
    try std.testing.expectEqual(@as(usize, 2), prog.blocks[0].len);

    // 3. VMで実行
    var vm_inst = vm.VM.init(allocator);
    const result = try vm_inst.run(prog.mainCode());
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.bits);
}

// ================================================================
// Lifter テスト
// ================================================================

test "Lifter: basic arithmetic" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i64 @add(i64 %a, i64 %b) {
        \\entry:
        \\  %res = add i64 %a, %b
        \\  ret i64 %res
        \\}
    ;
    var l = lifter.Lifter.init(allocator);
    var prog = try l.lift(ir_src);
    defer prog.deinit();

    try std.testing.expect(prog.blocks.len >= 1);

    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 128);
    defer allocator.free(regs);
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 10 };
    regs[1] = .{ .bits = 20 };

    const result = try machine.exec(prog.blocks[0], regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 30), result.val.bits);
}

test "Lifter: loop and PHI (sum 0..n)" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i64 @sum(i64 %n) {
        \\entry:
        \\  br label %loop
        \\loop:
        \\  %i = phi i64 [ 0, %entry ], [ %i.next, %body ]
        \\  %acc = phi i64 [ 0, %entry ], [ %acc.next, %body ]
        \\  %cond = icmp slt i64 %i, %n
        \\  br i1 %cond, label %body, label %exit
        \\body:
        \\  %acc.next = add i64 %acc, %i
        \\  %i.next = add i64 %i, 1
        \\  br label %loop
        \\exit:
        \\  ret i64 %acc
        \\}
    ;
    var l = lifter.Lifter.init(allocator);
    var prog = try l.lift(ir_src);
    defer prog.deinit();

    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 128);
    defer allocator.free(regs);
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 11 }; // n = 11 (sum 0..10 = 55)

    const result = try machine.exec(prog.blocks[0], regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 55), result.val.bits);
}

test "Lifter: If-Else" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i64 @test_if(i64 %n) {
        \\entry:
        \\  %c = icmp slt i64 %n, 10
        \\  br i1 %c, label %t, label %f
        \\t:
        \\  ret i64 1
        \\f:
        \\  ret i64 0
        \\}
    ;
    var l = lifter.Lifter.init(allocator);
    var prog = try l.lift(ir_src);
    defer prog.deinit();

    var machine = vm.VM.init(allocator);
    {
        const regs = try allocator.alloc(vm.Value, 128);
        defer allocator.free(regs);
        for (regs) |*r| r.* = .unit;
        regs[0] = .{ .bits = 5 }; // 5 < 10 -> True
        const result = try machine.exec(prog.blocks[0], regs, 0, null);
        defer result.val.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 1), result.val.bits);
    }
    {
        const regs = try allocator.alloc(vm.Value, 128);
        defer allocator.free(regs);
        for (regs) |*r| r.* = .unit;
        regs[0] = .{ .bits = 15 }; // 15 < 10 -> False
        const result = try machine.exec(prog.blocks[0], regs, 0, null);
        defer result.val.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 0), result.val.bits);
    }
}

test "Lifter: i128 add" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i128 @test_i128(i128 %a, i128 %b) {
        \\entry:
        \\  %c = add i128 %a, %b
        \\  ret i128 %c
        \\}
    ;
    var l = lifter.Lifter.init(allocator);
    var prog = try l.lift(ir_src);
    defer prog.deinit();

    var ibin_op: ?vm.Op = null;
    for (prog.blocks[0]) |op| {
        if (op == .ibin) {
            ibin_op = op;
            break;
        }
    }

    try std.testing.expect(ibin_op != null);
    try std.testing.expectEqual(vm.IBinOp.add, ibin_op.?.ibin.op);
    try std.testing.expectEqual(@as(u16, 128), ibin_op.?.ibin.width);
}

// ================================================================
// Decompiler テスト
// ================================================================

test "uintAfter / gepReg basic" {
    try std.testing.expectEqual(@as(?u32, 5), decompiler.uintAfter("ptr %r_5, i64 42", "ptr %r_"));
    try std.testing.expectEqual(@as(?u32, 42), decompiler.uintAfter("i64 42)", "i64 "));
    try std.testing.expectEqual(@as(?u32, null), decompiler.uintAfter("foo", "bar"));
    try std.testing.expectEqual(
        @as(?u32, 3),
        decompiler.gepReg("  %lhs_ptr_3 = getelementptr %Value, ptr %regs, i32 3"),
    );
}

test "Decompiler: load_const int + ret" {
    const allocator = std.testing.allocator;
    const ir_src =
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
    var d = decompiler.Decompiler.init(allocator);
    var prog = try d.decompile(ir_src);
    defer prog.deinit();
    const code = prog.mainCode();
    try std.testing.expectEqual(@as(usize, 2), code.len);
    try std.testing.expectEqual(@as(u32, 0), code[0].load_const.dst);
    try std.testing.expectEqual(@as(u64, 99), code[0].load_const.val.bits);
    try std.testing.expectEqual(@as(u32, 0), code[1].ret.src);
}

test "Decompiler: add op" {
    const allocator = std.testing.allocator;
    const ir_src =
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
    var d = decompiler.Decompiler.init(allocator);
    var prog = try d.decompile(ir_src);
    defer prog.deinit();
    const code = prog.mainCode();
    try std.testing.expectEqual(@as(usize, 4), code.len);
    try std.testing.expectEqual(@as(u64, 10), code[0].load_const.val.bits);
    try std.testing.expectEqual(@as(u64, 20), code[1].load_const.val.bits);
    try std.testing.expectEqual(@as(u32, 2), code[2].add.dst);
    try std.testing.expectEqual(@as(u32, 0), code[2].add.lhs);
    try std.testing.expectEqual(@as(u32, 1), code[2].add.rhs);
    try std.testing.expectEqual(@as(u32, 2), code[3].ret.src);
}

test "Decompiler: make_pair + proj1" {
    const allocator = std.testing.allocator;
    const ir_src =
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
    var d = decompiler.Decompiler.init(allocator);
    var prog = try d.decompile(ir_src);
    defer prog.deinit();
    const code = prog.mainCode();
    // load_const 7, load_const unit, make_pair 2←0,1, proj1 0←2, ret 0
    try std.testing.expectEqual(@as(usize, 5), code.len);
    try std.testing.expectEqual(@as(u64, 7), code[0].load_const.val.bits);
    try std.testing.expect(code[1].load_const.val == .unit);
    try std.testing.expectEqual(@as(u32, 2), code[2].make_pair.dst);
    try std.testing.expectEqual(@as(u32, 0), code[2].make_pair.fst);
    try std.testing.expectEqual(@as(u32, 1), code[2].make_pair.snd);
    try std.testing.expectEqual(@as(u32, 0), code[3].proj1.dst);
    try std.testing.expectEqual(@as(u32, 2), code[3].proj1.src);
    try std.testing.expectEqual(@as(u32, 0), code[4].ret.src);
}

// ================================================================
// SpeculativeExecutor テスト
// ================================================================

test "SpeculativeExecutor Tiered demo" {
    const allocator = std.testing.allocator;
    var executor = speculative.SpeculativeExecutor.init(allocator);
    defer executor.deinit();

    const code = &[_]vm.Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 32 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };

    var i: usize = 0;
    while (i < 30) : (i += 1) {
        const res = try executor.execute(&[_][]const vm.Op{code}, code);
        defer res.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), res.bits);
    }
}

// ================================================================
// ベンチマーク テスト
// ================================================================

test "Bench: VM integer add loop" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .add, .width = 64 } },
        .{ .ret = .{ .src = 2 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer allocator.free(regs);

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 100_000) : (i += 1) {
        for (regs) |*r| r.* = .unit;
        regs[0] = .{ .bits = @as(u64, i) };
        regs[1] = .{ .bits = 1 };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM integer add 100k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 10 };
    regs[1] = .{ .bits = 32 };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.val.bits);
}

test "Bench: VM float ops" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .fadd = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .fmul = .{ .dst = 3, .lhs = 2, .rhs = 0 } },
        .{ .ret = .{ .src = 3 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer allocator.free(regs);

    const f2: f64 = 2.0;
    const f3: f64 = 3.0;
    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 1_000) : (i += 1) {
        for (regs) |*r| r.* = .unit;
        regs[0] = .{ .bits = @bitCast(f2) };
        regs[1] = .{ .bits = @bitCast(f3) };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM float ops 1k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    // 正確性確認: (2.0 + 3.0) * 2.0 = 10.0
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = @bitCast(f2) };
    regs[1] = .{ .bits = @bitCast(f3) };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    const got: f64 = @bitCast(result.val.bits);
    try std.testing.expectApproxEqAbs(@as(f64, 10.0), got, 1e-10);
}

test "Bench: VM integer comparison + branch" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .icmp = .{ .dst = 2, .lhs = 0, .rhs = 1, .pred = .slt, .width = 64 } },
        .{ .ret = .{ .src = 2 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer allocator.free(regs);

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 100) : (i += 1) {
        for (regs) |*r| r.* = .unit;
        regs[0] = .{ .bits = 5 };
        regs[1] = .{ .bits = 10 };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM icmp+branch 100] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 5 };
    regs[1] = .{ .bits = 10 };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expect(std.meta.activeTag(result.val) == .inl);
}

test "Bench: VM sext/zext/trunc" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .sext = .{ .dst = 1, .src = 0, .from = 8, .to = 64 } },
        .{ .ret = .{ .src = 1 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer allocator.free(regs);

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 10_000) : (i += 1) {
        for (regs) |*r| r.* = .unit;
        regs[0] = .{ .bits = 0xFF };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM sext 10k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 0xFF };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 0xFFFFFFFFFFFFFFFF), result.val.bits);
}

test "Bench: Lifter simple add function" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i64 @add(i64 %a, i64 %b) {
        \\entry:
        \\  %res = add i64 %a, %b
        \\  ret i64 %res
        \\}
    ;
    var l = lifter.Lifter.init(allocator);

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 1_000) : (i += 1) {
        var prog = try l.lift(ir_src);
        prog.deinit();
    }
    const ns = timer.read();
    std.debug.print("  [Bench: Lifter simple add 1k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    var prog = try l.lift(ir_src);
    defer prog.deinit();
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 128);
    defer allocator.free(regs);
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 10 };
    regs[1] = .{ .bits = 20 };
    const result = try machine.exec(prog.blocks[0], regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 30), result.val.bits);
}

test "Bench: Lifter loop with PHI" {
    const allocator = std.testing.allocator;
    const ir_src =
        \\define i64 @sum(i64 %n) {
        \\entry:
        \\  br label %loop
        \\loop:
        \\  %i = phi i64 [ 0, %entry ], [ %i.next, %body ]
        \\  %acc = phi i64 [ 0, %entry ], [ %acc.next, %body ]
        \\  %cond = icmp slt i64 %i, %n
        \\  br i1 %cond, label %body, label %exit
        \\body:
        \\  %acc.next = add i64 %acc, %i
        \\  %i.next = add i64 %i, 1
        \\  br label %loop
        \\exit:
        \\  ret i64 %acc
        \\}
    ;
    var l = lifter.Lifter.init(allocator);

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 500) : (i += 1) {
        var prog = try l.lift(ir_src);
        prog.deinit();
    }
    const ns = timer.read();
    std.debug.print("  [Bench: Lifter loop+PHI 500] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    var prog = try l.lift(ir_src);
    defer prog.deinit();
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 128);
    defer allocator.free(regs);
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 11 }; // n=11 -> sum 0..10 = 55
    const result = try machine.exec(prog.blocks[0], regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 55), result.val.bits);
}

test "Bench: Codegen ibin programs" {
    const allocator = std.testing.allocator;
    const prog_code = [_]vm.Op{
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .add, .width = 64 } },
        .{ .ret = .{ .src = 2 } },
    };
    const program = [_][]const vm.Op{&prog_code};
    var analysis = makeEmptyAnalysis(allocator);
    defer analysis.deinit();
    var cg = codegen.CodeGen.init(allocator);

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 500) : (i += 1) {
        const ir = try cg.generate(&program, analysis, "bench_ibin", default_cpu);
        allocator.free(ir);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: Codegen ibin 500] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    const ir = try cg.generate(&program, analysis, "bench_ibin_check", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "add i64") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_make_int") != null);
}

test "Bench: VM bigint multiply i128" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .mul, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer {
        for (regs) |r| r.deinit(allocator);
        allocator.free(regs);
    }
    for (regs) |*r| r.* = .unit;

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 10_000) : (i += 1) {
        for (regs) |*r| { r.deinit(allocator); r.* = .unit; }
        regs[0] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 3, 0 }) };
        regs[1] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 5, 0 }) };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM bigint mul 10k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    for (regs) |*r| { r.deinit(allocator); r.* = .unit; }
    regs[0] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 3, 0 }) };
    regs[1] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 5, 0 }) };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 15), result.val.wide[0]);
}

test "Bench: VM bigint shift i128" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .ibin = .{ .dst = 2, .lhs = 0, .rhs = 1, .op = .shl, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer {
        for (regs) |r| r.deinit(allocator);
        allocator.free(regs);
    }
    for (regs) |*r| r.* = .unit;

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 10_000) : (i += 1) {
        for (regs) |*r| { r.deinit(allocator); r.* = .unit; }
        regs[0] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 1, 0 }) };
        regs[1] = .{ .bits = 64 };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM bigint shl 10k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});
}

test "Bench: VM bigint compare i128" {
    const allocator = std.testing.allocator;
    const code = [_]vm.Op{
        .{ .icmp = .{ .dst = 2, .lhs = 0, .rhs = 1, .pred = .ugt, .width = 128 } },
        .{ .ret = .{ .src = 2 } },
    };
    var machine = vm.VM.init(allocator);
    const regs = try allocator.alloc(vm.Value, 16);
    defer {
        for (regs) |r| r.deinit(allocator);
        allocator.free(regs);
    }
    for (regs) |*r| r.* = .unit;

    var timer = try std.time.Timer.start();
    var i: u32 = 0;
    while (i < 10_000) : (i += 1) {
        for (regs) |*r| { r.deinit(allocator); r.* = .unit; }
        regs[0] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 0, 1 }) };
        regs[1] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 0xFFFFFFFFFFFFFFFF, 0 }) };
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM bigint icmp 10k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});
}
