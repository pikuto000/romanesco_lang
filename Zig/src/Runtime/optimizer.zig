const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;

pub const Optimizer = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) Optimizer {
        return .{ .allocator = allocator };
    }

    pub fn optimize(self: *Optimizer, code: []const Op) ![]Op {
        var current_code = try self.allocator.dupe(Op, code);
        var changed = true;
        var fuel: usize = 10;

        while (changed and fuel > 0) {
            changed = false;
            
            // Constant Folding
            const folded = try self.foldConstants(current_code);
            if (folded.changed) {
                self.allocator.free(current_code);
                current_code = folded.code;
                changed = true;
            } else {
                self.allocator.free(folded.code);
            }

            // Dead Code Elimination
            const dce = try self.eliminateDeadCode(current_code);
            if (dce.changed) {
                self.allocator.free(current_code);
                current_code = dce.code;
                changed = true;
            } else {
                self.allocator.free(dce.code);
            }

            fuel -= 1;
        }

        // Register Allocation (Re-numbering)
        const allocated = try self.allocateRegisters(current_code);
        self.allocator.free(current_code);
        return allocated;
    }

    const FoldResult = struct { code: []Op, changed: bool };

    fn foldConstants(self: *Optimizer, code: []const Op) !FoldResult {
        var constants = std.AutoHashMap(u32, Value).init(self.allocator);
        defer constants.deinit();
        var new_ops = try std.ArrayList(Op).initCapacity(self.allocator, 0);
        errdefer new_ops.deinit(self.allocator);
        var changed = false;

        for (code) |op| {
            switch (op) {
                .load_const => |lc| {
                    try constants.put(lc.dst, lc.val);
                    try new_ops.append(self.allocator, op);
                },
                .add => |a| {
                    if (constants.get(a.lhs)) |l| {
                        if (constants.get(a.rhs)) |r| {
                            if (l == .bits and r == .bits) {
                                const res = l.bits +% r.bits;
                                const val = Value{ .bits = res };
                                try constants.put(a.dst, val);
                                try new_ops.append(self.allocator, Op{ .load_const = .{ .dst = a.dst, .val = val } });
                                changed = true;
                                continue;
                            }
                        }
                    }
                    try new_ops.append(self.allocator, op);
                },
                .sub => |s| {
                    if (constants.get(s.lhs)) |l| {
                        if (constants.get(s.rhs)) |r| {
                            if (l == .bits and r == .bits) {
                                const res = l.bits -% r.bits;
                                const val = Value{ .bits = res };
                                try constants.put(s.dst, val);
                                try new_ops.append(self.allocator, Op{ .load_const = .{ .dst = s.dst, .val = val } });
                                changed = true;
                                continue;
                            }
                        }
                    }
                    try new_ops.append(self.allocator, op);
                },
                .mul => |m| {
                    if (constants.get(m.lhs)) |l| {
                        if (constants.get(m.rhs)) |r| {
                            if (l == .bits and r == .bits) {
                                const res = l.bits *% r.bits;
                                const val = Value{ .bits = res };
                                try constants.put(m.dst, val);
                                try new_ops.append(self.allocator, Op{ .load_const = .{ .dst = m.dst, .val = val } });
                                changed = true;
                                continue;
                            }
                        }
                    }
                    try new_ops.append(self.allocator, op);
                },
                .ibin => |o| {
                    if (o.width <= 64) {
                        if (constants.get(o.lhs)) |l| {
                            if (constants.get(o.rhs)) |r| {
                                if (l == .bits and r == .bits) {
                                    // Simple constant folding for ibin
                                    const lv = l.bits;
                                    const rv = r.bits;
                                    const mask = if (o.width >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.width)) - 1;
                                    const res = switch (o.op) {
                                        .add => lv +% rv,
                                        .sub => lv -% rv,
                                        .mul => lv *% rv,
                                        .and_ => lv & rv,
                                        .or_ => lv | rv,
                                        .xor_ => lv ^ rv,
                                        else => {
                                            try new_ops.append(self.allocator, op);
                                            continue;
                                        },
                                    };
                                    const val = Value{ .bits = res & mask };
                                    try constants.put(o.dst, val);
                                    try new_ops.append(self.allocator, Op{ .load_const = .{ .dst = o.dst, .val = val } });
                                    changed = true;
                                    continue;
                                }
                            }
                        }
                    }
                    try new_ops.append(self.allocator, op);
                },
                else => try new_ops.append(self.allocator, op),
            }
        }
        return FoldResult{ .code = try new_ops.toOwnedSlice(self.allocator), .changed = changed };
    }

    fn eliminateDeadCode(self: *Optimizer, code: []const Op) !FoldResult {
        var used = std.AutoHashMap(u32, void).init(self.allocator);
        defer used.deinit();

        // Mark used registers
        for (code) |op| {
            switch (op) {
                .ret => |r| try used.put(r.src, {}),
                .move => |m| try used.put(m.src, {}),
                .add => |a| { try used.put(a.lhs, {}); try used.put(a.rhs, {}); },
                .sub => |s| { try used.put(s.lhs, {}); try used.put(s.rhs, {}); },
                .mul => |m| { try used.put(m.lhs, {}); try used.put(m.rhs, {}); },
                .call => |c| {
                    try used.put(c.func, {});
                    for (c.args) |a| try used.put(a, {});
                },
                .make_closure => |mc| {
                    for (mc.captures) |c| try used.put(c, {});
                },
                .make_pair => |mp| { try used.put(mp.fst, {}); try used.put(mp.snd, {}); },
                .proj1 => |p| try used.put(p.src, {}),
                .proj2 => |p| try used.put(p.src, {}),
                .make_inl => |m| try used.put(m.src, {}),
                .make_inr => |m| try used.put(m.src, {}),
                .case_op => |c| try used.put(c.scrutinee, {}),
                .borrow => |b| try used.put(b.src, {}),
                .free => |f| try used.put(f.reg, {}),
                .load_const => {}, 
                // Lifter ops
                .ibin => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .icmp => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .load_bits => {},
                .load_wide => {},
                .sext => |o| try used.put(o.src, {}),
                .zext => |o| try used.put(o.src, {}),
                .trunc => |o| try used.put(o.src, {}),
                .itof => |o| try used.put(o.src, {}),
                .ftoi => |o| try used.put(o.src, {}),
                .fadd => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .fsub => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .fmul => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .fdiv => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .frem => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
                .fcmp => |o| { try used.put(o.lhs, {}); try used.put(o.rhs, {}); },
            }
        }

        var new_ops = try std.ArrayList(Op).initCapacity(self.allocator, 0);
        errdefer new_ops.deinit(self.allocator);
        var changed = false;

        for (code) |op| {
            switch (op) {
                .load_const => |lc| {
                    if (!used.contains(lc.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .add => |a| {
                    if (!used.contains(a.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .sub => |s| {
                    if (!used.contains(s.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .mul => |m| {
                    if (!used.contains(m.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .ibin => |o| {
                    if (!used.contains(o.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .icmp => |o| {
                    if (!used.contains(o.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .load_bits => |o| {
                    if (!used.contains(o.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .load_wide => |o| {
                    if (!used.contains(o.dst)) {
                        changed = true;
                    } else {
                        try new_ops.append(self.allocator, op);
                    }
                },
                .sext => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .zext => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .trunc => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .itof => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .ftoi => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .fadd => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .fsub => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .fmul => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .fdiv => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .frem => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                .fcmp => |o| if (!used.contains(o.dst)) { changed = true; } else { try new_ops.append(self.allocator, op); },
                else => try new_ops.append(self.allocator, op),
            }
        }
        return FoldResult{ .code = try new_ops.toOwnedSlice(self.allocator), .changed = changed };
    }

    fn allocateRegisters(self: *Optimizer, code: []const Op) ![]Op {
        // 1. Compute last use of each register
        var last_use = std.AutoHashMap(u32, usize).init(self.allocator);
        defer last_use.deinit();

        for (code, 0..) |op, pc| {
            switch (op) {
                .move => |o| try last_use.put(o.src, pc),
                .add => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .sub => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .mul => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .make_pair => |o| { try last_use.put(o.fst, pc); try last_use.put(o.snd, pc); },
                .proj1 => |o| try last_use.put(o.src, pc),
                .proj2 => |o| try last_use.put(o.src, pc),
                .make_inl => |o| try last_use.put(o.src, pc),
                .make_inr => |o| try last_use.put(o.src, pc),
                .case_op => |o| try last_use.put(o.scrutinee, pc),
                .call => |o| { try last_use.put(o.func, pc); for (o.args) |a| try last_use.put(a, pc); },
                .make_closure => |o| { for (o.captures) |c| try last_use.put(c, pc); },
                .borrow => |o| try last_use.put(o.src, pc),
                .ret => |o| try last_use.put(o.src, pc),
                .free => |o| try last_use.put(o.reg, pc),
                .load_const => {},
                .ibin => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .icmp => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .load_bits => {},
                .load_wide => {},
                .sext => |o| try last_use.put(o.src, pc),
                .zext => |o| try last_use.put(o.src, pc),
                .trunc => |o| try last_use.put(o.src, pc),
                .itof => |o| try last_use.put(o.src, pc),
                .ftoi => |o| try last_use.put(o.src, pc),
                .fadd => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .fsub => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .fmul => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .fdiv => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .frem => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
                .fcmp => |o| { try last_use.put(o.lhs, pc); try last_use.put(o.rhs, pc); },
            }
        }

        // 2. Linear Scan Register Allocation
        var mapping = std.AutoHashMap(u32, u32).init(self.allocator);
        defer mapping.deinit();
        var free_pool = try std.ArrayList(u32).initCapacity(self.allocator, 0);
        defer free_pool.deinit(self.allocator);
        var next_available: u32 = 0;

        var result = try std.ArrayList(Op).initCapacity(self.allocator, 0);
        errdefer result.deinit(self.allocator);

        const map_reg = struct {
            fn call(m: *std.AutoHashMap(u32, u32), fp: *std.ArrayList(u32), na: *u32, old: u32, alloc: Allocator) !u32 {
                if (m.get(old)) |new| return new;
                const new = if (fp.pop()) |val| val else blk: {
                    const val = na.*;
                    na.* += 1;
                    break :blk val;
                };
                try m.put(old, new);
                _ = alloc; // Future use
                return new;
            }
        }.call;

        const release_if_last = struct {
            fn call(m: *std.AutoHashMap(u32, u32), fp: *std.ArrayList(u32), lu: *std.AutoHashMap(u32, usize), old: u32, pc: usize, alloc: Allocator) !void {
                if (lu.get(old)) |last_pc| {
                    if (last_pc == pc) {
                        if (m.get(old)) |new| {
                            try fp.append(alloc, new);
                            _ = m.remove(old);
                        }
                    }
                }
            }
        }.call;

        for (code, 0..) |op, pc| {
            switch (op) {
                .load_const => |o| {
                    const d = try map_reg(&mapping, &free_pool, &next_available, o.dst, self.allocator);
                    try result.append(self.allocator, .{ .load_const = .{ .dst = d, .val = try o.val.clone(self.allocator) } });
                    try release_if_last(&mapping, &free_pool, &last_use, o.dst, pc, self.allocator);
                },
                .move => |o| {
                    const s = try map_reg(&mapping, &free_pool, &next_available, o.src, self.allocator);
                    try release_if_last(&mapping, &free_pool, &last_use, o.src, pc, self.allocator);
                    const d = try map_reg(&mapping, &free_pool, &next_available, o.dst, self.allocator);
                    try result.append(self.allocator, .{ .move = .{ .dst = d, .src = s } });
                    try release_if_last(&mapping, &free_pool, &last_use, o.dst, pc, self.allocator);
                },
                .add => |o| {
                    const l = try map_reg(&mapping, &free_pool, &next_available, o.lhs, self.allocator);
                    const r = try map_reg(&mapping, &free_pool, &next_available, o.rhs, self.allocator);
                    try release_if_last(&mapping, &free_pool, &last_use, o.lhs, pc, self.allocator);
                    try release_if_last(&mapping, &free_pool, &last_use, o.rhs, pc, self.allocator);
                    const d = try map_reg(&mapping, &free_pool, &next_available, o.dst, self.allocator);
                    try result.append(self.allocator, .{ .add = .{ .dst = d, .lhs = l, .rhs = r } });
                    try release_if_last(&mapping, &free_pool, &last_use, o.dst, pc, self.allocator);
                },
                .ret => |o| {
                    const s = try map_reg(&mapping, &free_pool, &next_available, o.src, self.allocator);
                    try result.append(self.allocator, .{ .ret = .{ .src = s } });
                    try release_if_last(&mapping, &free_pool, &last_use, o.src, pc, self.allocator);
                },
                .call => |o| {
                    const f = try map_reg(&mapping, &free_pool, &next_available, o.func, self.allocator);
                    const args = try self.allocator.alloc(u32, o.args.len);
                    for (o.args, 0..) |a, i| args[i] = try map_reg(&mapping, &free_pool, &next_available, a, self.allocator);
                    
                    try release_if_last(&mapping, &free_pool, &last_use, o.func, pc, self.allocator);
                    for (o.args) |a| try release_if_last(&mapping, &free_pool, &last_use, a, pc, self.allocator);
                    
                    const d = try map_reg(&mapping, &free_pool, &next_available, o.dst, self.allocator);
                    try result.append(self.allocator, .{ .call = .{ .dst = d, .func = f, .args = args } });
                    try release_if_last(&mapping, &free_pool, &last_use, o.dst, pc, self.allocator);
                },
                // ... other ops should be mapped similarly
                else => try result.append(self.allocator, try o_clone(op, self.allocator)),
            }
        }

        return try result.toOwnedSlice(self.allocator);
    }

    fn o_clone(op: Op, allocator: Allocator) !Op {
        switch (op) {
            .make_closure => |o| {
                const caps = try allocator.dupe(u32, o.captures);
                return Op{ .make_closure = .{ .dst = o.dst, .body = o.body, .captures = caps, .arity = o.arity, .block_idx = o.block_idx } };
            },
            .call => |o| {
                const args = try allocator.dupe(u32, o.args);
                return Op{ .call = .{ .dst = o.dst, .func = o.func, .args = args } };
            },
            else => return op,
        }
    }
};
