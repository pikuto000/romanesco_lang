/// benchmark.zig — 新opcode AOT/JITパイプライン性能測定
const std = @import("std");
const vm = @import("vm.zig");
const lifter = @import("lifter.zig");
const analyzer = @import("analyzer.zig");
const codegen = @import("codegen.zig");

fn makeEmptyAnalysis(allocator: std.mem.Allocator) analyzer.RangeAnalysisResult {
    return .{
        .bit_widths = std.AutoHashMap(u32, u8).init(allocator),
        .escapes = std.AutoHashMap(u32, void).init(allocator),
        .stable_values = std.AutoHashMap(u32, u64).init(allocator),
        .inlining_hints = std.AutoHashMap(usize, usize).init(allocator),
        .allocator = allocator,
    };
}

const default_cpu = vm.CpuFeatures{ .has_avx2 = false, .has_neon = false, .vector_width = 64 };

// ---- VM integer add loop ----
test "Bench: VM integer add loop" {
    const allocator = std.testing.allocator;
    // ibin{add,64}: r2 = r0 + r1
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

    // 正確性確認
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 10 };
    regs[1] = .{ .bits = 32 };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.val.bits);
}

// ---- VM float ops ----
test "Bench: VM float ops" {
    const allocator = std.testing.allocator;
    // fadd r2, r0, r1 ; fmul r3, r2, r0 ; ret r3
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

// ---- VM integer comparison ----
test "Bench: VM integer comparison + branch" {
    const allocator = std.testing.allocator;
    // icmp slt r2, r0, r1 ; ret r2  → inl(unit)=true, inr(unit)=false
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

    // 正確性確認: 5 < 10 → inl(unit)
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 5 };
    regs[1] = .{ .bits = 10 };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expect(std.meta.activeTag(result.val) == .inl);
}

// ---- VM sext/zext/trunc ----
test "Bench: VM sext/zext/trunc" {
    const allocator = std.testing.allocator;
    // i8 値を i64 へ符号拡張: r1 = sext i8 r0 to i64
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
        regs[0] = .{ .bits = 0xFF }; // -1 as i8
        const result = try machine.exec(&code, regs, 0, null);
        result.val.deinit(allocator);
    }
    const ns = timer.read();
    std.debug.print("  [Bench: VM sext 10k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    // 正確性確認: sext i8 0xFF → i64 = 0xFFFFFFFFFFFFFFFF
    for (regs) |*r| r.* = .unit;
    regs[0] = .{ .bits = 0xFF };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 0xFFFFFFFFFFFFFFFF), result.val.bits);
}

// ---- Lifter: simple add function ----
test "Bench: Lifter simple add function" {
    const allocator = std.testing.allocator;
    const ir =
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
        var prog = try l.lift(ir);
        prog.deinit();
    }
    const ns = timer.read();
    std.debug.print("  [Bench: Lifter simple add 1k] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    // 正確性確認: 10 + 20 = 30
    var prog = try l.lift(ir);
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

// ---- Lifter: loop with PHI ----
test "Bench: Lifter loop with PHI" {
    const allocator = std.testing.allocator;
    const ir =
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
        var prog = try l.lift(ir);
        prog.deinit();
    }
    const ns = timer.read();
    std.debug.print("  [Bench: Lifter loop+PHI 500] {d:.2} ms\n", .{@as(f64, @floatFromInt(ns)) / 1e6});

    // 正確性確認: sum(0..10) = 55
    var prog = try l.lift(ir);
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

// ---- Codegen: ibin programs ----
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

    // 正確性確認: 生成 IR に add i64 が含まれる
    const ir = try cg.generate(&program, analysis, "bench_ibin_check", default_cpu);
    defer allocator.free(ir);
    try std.testing.expect(std.mem.indexOf(u8, ir, "add i64") != null);
    try std.testing.expect(std.mem.indexOf(u8, ir, "rt_make_int") != null);
}

// ---- VM bigint multiply i128 ----
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

    // 正確性確認: 3 * 5 = 15
    for (regs) |*r| { r.deinit(allocator); r.* = .unit; }
    regs[0] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 3, 0 }) };
    regs[1] = .{ .wide = try allocator.dupe(u64, &[_]u64{ 5, 0 }) };
    const result = try machine.exec(&code, regs, 0, null);
    defer result.val.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 15), result.val.wide[0]);
}

// ---- VM bigint shift i128 ----
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

// ---- VM bigint compare i128 ----
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
