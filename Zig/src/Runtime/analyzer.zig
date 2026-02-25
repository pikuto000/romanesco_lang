const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;
const ProfileData = vm.ProfileData;

pub const TagType = enum { bits, wide, unit, pair, sum, closure, unknown };

pub const RangeAnalysisResult = struct {
    bit_widths: std.AutoHashMap(u32, u8),
    escapes: std.AutoHashMap(u32, void),
    stable_values: std.AutoHashMap(u32, u64),
    inlining_hints: std.AutoHashMap(usize, usize), // pc -> block_idx
    tag_types: std.AutoHashMap(u32, TagType),
    allocator: Allocator,

    pub fn deinit(self: *RangeAnalysisResult) void {
        self.bit_widths.deinit();
        self.escapes.deinit();
        self.stable_values.deinit();
        self.inlining_hints.deinit();
        self.tag_types.deinit();
    }

    pub fn bitWidth(self: RangeAnalysisResult, reg: u32) u8 {
        return self.bit_widths.get(reg) orelse 64;
    }

    pub fn doesEscape(self: RangeAnalysisResult, reg: u32) bool {
        return self.escapes.contains(reg);
    }

    pub fn isProvenBits(self: RangeAnalysisResult, reg: u32) bool {
        return (self.tag_types.get(reg) orelse .unknown) == .bits;
    }
};

pub const RangeAnalyzer = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) RangeAnalyzer {
        return .{ .allocator = allocator };
    }

    fn requiredBits(val: u64) u8 {
        if (val == 0) return 1;
        return @as(u8, @intCast(64 - @clz(val)));
    }

    pub fn analyze(self: *RangeAnalyzer, code: []const Op, profile: ?*ProfileData, current_block_idx: usize) !RangeAnalysisResult {
        var widths = std.AutoHashMap(u32, u8).init(self.allocator);
        var escapes = std.AutoHashMap(u32, void).init(self.allocator);
        const stable_values = std.AutoHashMap(u32, u64).init(self.allocator);
        var inlining_hints = std.AutoHashMap(usize, usize).init(self.allocator);
        var tag_types = std.AutoHashMap(u32, TagType).init(self.allocator);
        errdefer tag_types.deinit();
        var worklist = try std.ArrayList(u32).initCapacity(self.allocator, 0);
        defer worklist.deinit(self.allocator);

        // Populate inlining hints from profile
        if (profile) |p| {
            for (code, 0..) |op, pc| {
                if (op == .call) {
                    if (p.getDominantCallTarget(pc, 5)) |target| {
                        try inlining_hints.put(pc, target);
                        
                        if (target == current_block_idx) {
                            // RECURSION DETECTED: We could use this to trigger unrolling
                            std.debug.print("  Recursion detected at PC {d} calling block {d}\n", .{pc, target});
                        }
                    }
                }
            }
        }

        // Forward pass: Bit-width inference + tag_types inference
        for (code) |op| {
            switch (op) {
                .load_const => |lc| {
                    if (lc.val == .bits) {
                        try widths.put(lc.dst, requiredBits(lc.val.bits));
                        try tag_types.put(lc.dst, .bits);
                    } else if (lc.val == .unit) {
                        try widths.put(lc.dst, 64);
                        try tag_types.put(lc.dst, .unit);
                    } else if (lc.val == .wide) {
                        try widths.put(lc.dst, 64);
                        try tag_types.put(lc.dst, .wide);
                    } else {
                        try widths.put(lc.dst, 64);
                        try tag_types.put(lc.dst, .unknown);
                    }
                },
                .move => |m| {
                    if (widths.get(m.src)) |w| try widths.put(m.dst, w);
                    try tag_types.put(m.dst, tag_types.get(m.src) orelse .unknown);
                },
                .borrow => |b| {
                    try tag_types.put(b.dst, tag_types.get(b.src) orelse .unknown);
                },
                .add => |a| {
                    const l = widths.get(a.lhs) orelse 64;
                    const r = widths.get(a.rhs) orelse 64;
                    const max_w = if (l > r) l else r;
                    try widths.put(a.dst, if (max_w >= 63) 64 else max_w + 1);
                    try tag_types.put(a.dst, .bits);
                },
                .sub => |s| {
                    const l = widths.get(s.lhs) orelse 64;
                    const r = widths.get(s.rhs) orelse 64;
                    try widths.put(s.dst, if (l > r) l else r);
                    try tag_types.put(s.dst, .bits);
                },
                .mul => |m| {
                    const l = widths.get(m.lhs) orelse 64;
                    const r = widths.get(m.rhs) orelse 64;
                    const sum = @as(u32, l) + r;
                    try widths.put(m.dst, if (sum >= 64) 64 else @as(u8, @intCast(sum)));
                    try tag_types.put(m.dst, .bits);
                },
                .ibin => |o| {
                    try tag_types.put(o.dst, if (o.width <= 64) .bits else .wide);
                },
                .icmp => |o| {
                    try tag_types.put(o.dst, .sum);
                },
                .fcmp => |o| {
                    try tag_types.put(o.dst, .sum);
                },
                .load_bits => |o| {
                    try tag_types.put(o.dst, .bits);
                },
                .load_wide => |o| {
                    try tag_types.put(o.dst, .wide);
                },
                .fadd => |o| { try tag_types.put(o.dst, .bits); },
                .fsub => |o| { try tag_types.put(o.dst, .bits); },
                .fmul => |o| { try tag_types.put(o.dst, .bits); },
                .fdiv => |o| { try tag_types.put(o.dst, .bits); },
                .frem => |o| { try tag_types.put(o.dst, .bits); },
                .sext => |o| { try tag_types.put(o.dst, .bits); },
                .zext => |o| { try tag_types.put(o.dst, .bits); },
                .trunc => |o| { try tag_types.put(o.dst, .bits); },
                .itof => |o| { try tag_types.put(o.dst, .bits); },
                .ftoi => |o| { try tag_types.put(o.dst, .bits); },
                .make_pair => |o| { try tag_types.put(o.dst, .pair); },
                .make_inl => |o| { try tag_types.put(o.dst, .sum); },
                .make_inr => |o| { try tag_types.put(o.dst, .sum); },
                .make_closure => |o| { try tag_types.put(o.dst, .closure); },
                .call => |o| { try tag_types.put(o.dst, .unknown); },
                .case_op => |o| { try tag_types.put(o.dst, .unknown); },
                .proj1 => |o| { try tag_types.put(o.dst, .unknown); },
                .proj2 => |o| { try tag_types.put(o.dst, .unknown); },
                else => {},
            }
        }

        // Backward pass: Escape analysis
        for (code) |op| {
            switch (op) {
                .ret => |r| try worklist.append(self.allocator, r.src),
                .make_closure => |mc| {
                    try worklist.append(self.allocator, mc.dst);
                    for (mc.captures) |c| try worklist.append(self.allocator, c);
                },
                .call => |c| {
                    try worklist.append(self.allocator, c.func);
                    for (c.args) |a| try worklist.append(self.allocator, a);
                },
                else => {},
            }
        }

        var processed_escapes = std.AutoHashMap(u32, void).init(self.allocator);
        defer processed_escapes.deinit();

        while (worklist.pop()) |reg| {
            if (!processed_escapes.contains(reg)) {
                try processed_escapes.put(reg, {});
                try escapes.put(reg, {});

                for (code) |op| {
                    switch (op) {
                        .make_pair => |mp| {
                            if (mp.dst == reg) {
                                try worklist.append(self.allocator, mp.fst);
                                try worklist.append(self.allocator, mp.snd);
                            }
                        },
                        .move => |m| {
                            if (m.dst == reg) try worklist.append(self.allocator, m.src);
                        },
                        .borrow => |b| {
                             if (b.dst == reg) try worklist.append(self.allocator, b.src);
                        },
                        else => {},
                    }
                }
            }
        }

        return RangeAnalysisResult{
            .bit_widths = widths,
            .escapes = escapes,
            .stable_values = stable_values,
            .inlining_hints = inlining_hints,
            .tag_types = tag_types,
            .allocator = self.allocator,
        };
    }
};

test "TagInference: load_const bits propagates through add" {
    const allocator = std.testing.allocator;
    var az = RangeAnalyzer.init(allocator);
    const code = [_]Op{
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
    var az = RangeAnalyzer.init(allocator);
    const code = [_]Op{
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
    try std.testing.expectEqual(TagType.wide, result.tag_types.get(5) orelse .unknown);
}

test "TagInference: call → unknown, prevents guard elision" {
    const allocator = std.testing.allocator;
    var az = RangeAnalyzer.init(allocator);
    const args: []const u32 = &[_]u32{};
    const code = [_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 0 } } },
        .{ .call = .{ .dst = 1, .func = 0, .args = args } },
        .{ .add = .{ .dst = 2, .lhs = 1, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };
    var result = try az.analyze(&code, null, 0);
    defer result.deinit();
    // call結果はunknown
    try std.testing.expect(!result.isProvenBits(1));
    try std.testing.expectEqual(TagType.unknown, result.tag_types.get(1) orelse .unknown);
    // addはbitsを返すので2はbits
    try std.testing.expect(result.isProvenBits(2));
}

test "TagInference: borrow inherits source tag" {
    const allocator = std.testing.allocator;
    var az = RangeAnalyzer.init(allocator);
    const code = [_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 42 } } },
        .{ .borrow = .{ .dst = 1, .src = 0 } },
        .{ .ret = .{ .src = 1 } },
    };
    var result = try az.analyze(&code, null, 0);
    defer result.deinit();
    try std.testing.expect(result.isProvenBits(1));
}
