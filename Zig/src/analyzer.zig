const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;
const ProfileData = vm.ProfileData;

pub const RangeAnalysisResult = struct {
    bit_widths: std.AutoHashMap(u32, u8),
    escapes: std.AutoHashMap(u32, void),
    stable_values: std.AutoHashMap(u32, u64),
    allocator: Allocator,

    pub fn deinit(self: *RangeAnalysisResult) void {
        self.bit_widths.deinit();
        self.escapes.deinit();
        self.stable_values.deinit();
    }

    pub fn bitWidth(self: RangeAnalysisResult, reg: u32) u8 {
        return self.bit_widths.get(reg) orelse 64;
    }

    pub fn doesEscape(self: RangeAnalysisResult, reg: u32) bool {
        return self.escapes.contains(reg);
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

    pub fn analyze(self: *RangeAnalyzer, code: []const Op, profile: ?*ProfileData) !RangeAnalysisResult {
        var widths = std.AutoHashMap(u32, u8).init(self.allocator);
        var escapes = std.AutoHashMap(u32, void).init(self.allocator);
        const stable_values = std.AutoHashMap(u32, u64).init(self.allocator);
        var worklist = try std.ArrayList(u32).initCapacity(self.allocator, 0);
        defer worklist.deinit(self.allocator);

        // Heuristic: If we have profile data, we could find constants.
        // For now, let's assume we don't have detailed value profiling, but the framework is ready.
        _ = profile;

        // Forward pass: Bit-width inference
        for (code) |op| {
            switch (op) {
                .load_const => |lc| {
                    if (lc.val == .int) {
                        try widths.put(lc.dst, requiredBits(lc.val.int));
                    } else {
                        try widths.put(lc.dst, 64);
                    }
                },
                .move => |m| {
                    if (widths.get(m.src)) |w| try widths.put(m.dst, w);
                },
                .add => |a| {
                    const l = widths.get(a.lhs) orelse 64;
                    const r = widths.get(a.rhs) orelse 64;
                    const max_w = if (l > r) l else r;
                    try widths.put(a.dst, if (max_w >= 63) 64 else max_w + 1);
                },
                .sub => |s| {
                    const l = widths.get(s.lhs) orelse 64;
                    const r = widths.get(s.rhs) orelse 64;
                    try widths.put(s.dst, if (l > r) l else r);
                },
                .mul => |m| {
                    const l = widths.get(m.lhs) orelse 64;
                    const r = widths.get(m.rhs) orelse 64;
                    const sum = @as(u32, l) + r;
                    try widths.put(m.dst, if (sum >= 64) 64 else @as(u8, @intCast(sum)));
                },
                else => {},
            }
        }

        // Backward pass: Escape analysis
        // Seed worklist with returning values and closures
        for (code) |op| {
            switch (op) {
                .ret => |r| try worklist.append(self.allocator, r.src),
                .make_closure => |mc| {
                    try worklist.append(self.allocator, mc.dst); // Closure itself escapes
                    for (mc.captures) |c| try worklist.append(self.allocator, c); // Captures escape
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

                // Naive backward scan: Find definitions of 'reg'
                // A better approach would be to build a Def-Use chain first.
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
                        // Projections don't propagate escape to the source pair structure itself in this model,
                        // but the *content* might if we tracked types deeper.
                        else => {},
                    }
                }
            }
        }

        return RangeAnalysisResult{
            .bit_widths = widths,
            .escapes = escapes,
            .stable_values = stable_values,
            .allocator = self.allocator,
        };
    }
};
