const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ValueTag = enum(u64) {
    closure = 1,
    pair = 2,
    inl = 3,
    inr = 4,
    unit = 5,
    int = 6,
};

pub const Value = union(ValueTag) {
    closure: *Closure,
    pair: *[2]Value,
    inl: *Value,
    inr: *Value,
    unit: void,
    int: u64,

    pub fn deinit(self: Value, allocator: Allocator) void {
        switch (self) {
            .unit, .int => {},
            .closure => |c| {
                for (c.env) |v| v.deinit(allocator);
                allocator.free(c.env);
                // Note: Tiered JIT binaries are owned by SpeculativeExecutor
                allocator.destroy(c);
            },
            .pair => |p| {
                p[0].deinit(allocator);
                p[1].deinit(allocator);
                allocator.destroy(p);
            },
            .inl, .inr => |v| {
                v.deinit(allocator);
                allocator.destroy(v);
            },
        }
    }

    pub fn clone(self: Value, allocator: Allocator) !Value {
        switch (self) {
            .unit, .int => return self,
            .closure => |c| {
                const new_env = try allocator.alloc(Value, c.env.len);
                for (c.env, 0..) |v, i| {
                    new_env[i] = try v.clone(allocator);
                }
                const new_c = try allocator.create(Closure);
                new_c.* = .{
                    .body = c.body,
                    .env = new_env,
                    .arity = c.arity,
                };
                return .{ .closure = new_c };
            },
            .pair => |p| {
                const new_p = try allocator.create([2]Value);
                new_p[0] = try p[0].clone(allocator);
                new_p[1] = try p[1].clone(allocator);
                return .{ .pair = new_p };
            },
            .inl => |v| {
                const new_v = try allocator.create(Value);
                new_v.* = try v.clone(allocator);
                return .{ .inl = new_v };
            },
            .inr => |v| {
                const new_v = try allocator.create(Value);
                new_v.* = try v.clone(allocator);
                return .{ .inr = new_v };
            },
        }
    }
};

pub const CpuFeatures = struct {
    has_avx2: bool,
    has_neon: bool,
    vector_width: u16, // bits (e.g., 256 for AVX2)

    pub fn detect() CpuFeatures {
        const target = @import("builtin").target;
        var features = CpuFeatures{
            .has_avx2 = false,
            .has_neon = false,
            .vector_width = 64,
        };

        if (target.cpu.arch.isX86()) {
            if (std.Target.x86.featureSetHas(target.cpu.features, .avx2)) {
                features.has_avx2 = true;
                features.vector_width = 256;
            }
        } else if (target.cpu.arch.isARM() or target.cpu.arch.isAARCH64()) {
            // Neon detection logic... simplified for now
            features.has_neon = true;
            features.vector_width = 128;
        }
        return features;
    }
};

pub const Tier = u32;

pub const LLVMValue = extern struct {
    tag: u64,
    payload: ?*anyopaque,
};

pub const NativeEntry = *const fn (regs: [*]LLVMValue) callconv(.c) u64;

pub const Closure = struct {
    body: []const Op,
    env: []Value,
    arity: usize,
    tier: Tier = 0,
    native_ptr: ?NativeEntry = null,
    block_idx: usize = 0, // Reference to original block
};

pub const Op = union(enum) {
    move: struct { dst: u32, src: u32 },
    load_const: struct { dst: u32, val: Value },
    make_closure: struct { dst: u32, body: []const Op, captures: []const u32, arity: usize, block_idx: usize },
    call: struct { dst: u32, func: u32, args: []const u32 },
    ret: struct { src: u32 },
    make_pair: struct { dst: u32, fst: u32, snd: u32 },
    proj1: struct { dst: u32, src: u32 },
    proj2: struct { dst: u32, src: u32 },
    make_inl: struct { dst: u32, src: u32 },
    make_inr: struct { dst: u32, src: u32 },
    case_op: struct { dst: u32, scrutinee: u32, inl_branch: []const Op, inr_branch: []const Op },
    add: struct { dst: u32, lhs: u32, rhs: u32 },
    sub: struct { dst: u32, lhs: u32, rhs: u32 },
    mul: struct { dst: u32, lhs: u32, rhs: u32 },
    borrow: struct { dst: u32, src: u32 },
    free: struct { reg: u32 },
};

pub const ExecutionResult = struct {
    val: Value,
    deopt_pc: ?u32 = null,
};

pub const VM = struct {
    allocator: Allocator,
    max_depth: usize = 1000,

    pub fn init(allocator: Allocator) VM {
        return .{ .allocator = allocator };
    }

    pub fn run(self: *VM, code: []const Op) !Value {
        const regs = try self.allocator.alloc(Value, 32);
        defer self.allocator.free(regs);
        for (regs) |*r| r.* = .unit;
        
        const result = try self.exec(code, regs, 0, null);
        for (regs) |r| r.deinit(self.allocator);
        return result.val;
    }

    pub fn exec(self: *VM, code: []const Op, regs: []Value, depth: usize, profile: ?*ProfileData) !ExecutionResult {
        if (depth > self.max_depth) return error.StackOverflow;

        var pc: usize = 0;
        var ret_val: Value = .unit;

        while (pc < code.len) : (pc += 1) {
            if (profile) |p| {
                try p.record(pc);
                // Record values for potential specialization
                const o = code[pc];
                switch (o) {
                    .move => |m| try p.recordValue(pc, m.src, regs[m.src]),
                    .add => |a| { try p.recordValue(pc, a.lhs, regs[a.lhs]); try p.recordValue(pc, a.rhs, regs[a.rhs]); },
                    .sub => |s| { try p.recordValue(pc, s.lhs, regs[s.lhs]); try p.recordValue(pc, s.rhs, regs[s.rhs]); },
                    .mul => |m| { try p.recordValue(pc, m.lhs, regs[m.lhs]); try p.recordValue(pc, m.rhs, regs[m.rhs]); },
                    .call => |c| try p.recordValue(pc, c.func, regs[c.func]),
                    .case_op => |c| try p.recordValue(pc, c.scrutinee, regs[c.scrutinee]),
                    else => {},
                }
            }
            const op = code[pc];
            switch (op) {
                .move => |m| {
                    regs[m.dst].deinit(self.allocator);
                    regs[m.dst] = regs[m.src];
                    regs[m.src] = .unit;
                },
                .load_const => |lc| {
                    regs[lc.dst].deinit(self.allocator);
                    regs[lc.dst] = try lc.val.clone(self.allocator);
                },
                .make_closure => |mc| {
                    const env = try self.allocator.alloc(Value, mc.captures.len);
                    for (mc.captures, 0..) |src, i| {
                        env[i] = regs[src];
                        regs[src] = .unit;
                    }
                    const closure = try self.allocator.create(Closure);
                    closure.* = .{
                        .body = mc.body,
                        .env = env,
                        .arity = mc.arity,
                        .block_idx = mc.block_idx,
                    };
                    regs[mc.dst].deinit(self.allocator);
                    regs[mc.dst] = .{ .closure = closure };
                },
                .add => |a| {
                    const l = regs[a.lhs].int;
                    const r = regs[a.rhs].int;
                    regs[a.lhs].deinit(self.allocator); regs[a.lhs] = .unit;
                    regs[a.rhs].deinit(self.allocator); regs[a.rhs] = .unit;
                    regs[a.dst].deinit(self.allocator);
                    regs[a.dst] = .{ .int = l +% r };
                },
                .sub => |s| {
                    const l = regs[s.lhs].int;
                    const r = regs[s.rhs].int;
                    regs[s.lhs].deinit(self.allocator); regs[s.lhs] = .unit;
                    regs[s.rhs].deinit(self.allocator); regs[s.rhs] = .unit;
                    regs[s.dst].deinit(self.allocator);
                    regs[s.dst] = .{ .int = l -% r };
                },
                .mul => |m| {
                    const l = regs[m.lhs].int;
                    const r = regs[m.rhs].int;
                    regs[m.lhs].deinit(self.allocator); regs[m.lhs] = .unit;
                    regs[m.rhs].deinit(self.allocator); regs[m.rhs] = .unit;
                    regs[m.dst].deinit(self.allocator);
                    regs[m.dst] = .{ .int = l *% r };
                },
                .make_pair => |mp| {
                    const pair = try self.allocator.create([2]Value);
                    pair[0] = regs[mp.fst];
                    pair[1] = regs[mp.snd];
                    regs[mp.fst] = .unit;
                    regs[mp.snd] = .unit;
                    regs[mp.dst].deinit(self.allocator);
                    regs[mp.dst] = .{ .pair = pair };
                },
                .proj1 => |p| {
                    const pair_val = regs[p.src];
                    switch (pair_val) {
                        .pair => |pair| {
                            regs[p.dst].deinit(self.allocator);
                            regs[p.dst] = pair[0];
                            regs[p.src] = pair[1];
                            self.allocator.destroy(pair);
                        },
                        else => return error.NotAPair,
                    }
                },
                .proj2 => |p| {
                    const pair_val = regs[p.src];
                    switch (pair_val) {
                        .pair => |pair| {
                            regs[p.dst].deinit(self.allocator);
                            regs[p.dst] = pair[1];
                            regs[p.src] = pair[0];
                            self.allocator.destroy(pair);
                        },
                        else => return error.NotAPair,
                    }
                },
                .make_inl => |m| {
                    const val = try self.allocator.create(Value);
                    val.* = regs[m.src];
                    regs[m.src] = .unit;
                    regs[m.dst].deinit(self.allocator);
                    regs[m.dst] = .{ .inl = val };
                },
                .make_inr => |m| {
                    const val = try self.allocator.create(Value);
                    val.* = regs[m.src];
                    regs[m.src] = .unit;
                    regs[m.dst].deinit(self.allocator);
                    regs[m.dst] = .{ .inr = val };
                },
                .borrow => |b| {
                    regs[b.dst].deinit(self.allocator);
                    regs[b.dst] = try regs[b.src].clone(self.allocator);
                },
                .free => |f| {
                    regs[f.reg].deinit(self.allocator);
                    regs[f.reg] = .unit;
                },
                .call => |c| {
                    const func_val = regs[c.func];
                    // Record call target before consuming reg
                    if (profile) |p| {
                        switch (func_val) {
                            .closure => |cl| try p.recordCall(pc, cl.block_idx),
                            else => {},
                        }
                    }
                    regs[c.func] = .unit;
                    switch (func_val) {
                        .closure => |cl| {
                            // Record call target for inlining
                            if (profile) |p| {
                                try p.recordCall(pc, cl.block_idx);
                            }
                            // TIERED DISPATCH: Check if we should jump to Native code
                            if (cl.native_ptr) |native_func| {
                                var native_regs: [32]LLVMValue = undefined;
                                
                                // Sync VM -> Native
                                for (cl.env, 0..) |v, i| {
                                    native_regs[i] = switch (v) {
                                        .int => |n| .{ .tag = 6, .payload = @ptrFromInt(n) },
                                        .unit => .{ .tag = 5, .payload = null },
                                        else => .{ .tag = 0, .payload = null },
                                    };
                                }
                                for (c.args, 0..) |arg_src, i| {
                                    const v = regs[arg_src];
                                    native_regs[cl.env.len + i] = switch (v) {
                                        .int => |n| .{ .tag = 6, .payload = @ptrFromInt(n) },
                                        .unit => .{ .tag = 5, .payload = null },
                                        else => .{ .tag = 0, .payload = null },
                                    };
                                    regs[arg_src] = .unit;
                                }

                                const res_u64 = native_func(&native_regs);
                                
                                regs[c.dst].deinit(self.allocator);
                                regs[c.dst] = .{ .int = res_u64 };
                                ret_val.deinit(self.allocator);
                                ret_val = try regs[c.dst].clone(self.allocator);
                                func_val.deinit(self.allocator);
                                continue;
                            }

                            const new_regs = try self.allocator.alloc(Value, 32);
                            defer self.allocator.free(new_regs);
                            for (new_regs) |*r| r.* = .unit;
                            
                            for (cl.env, 0..) |v, i| { new_regs[i] = try v.clone(self.allocator); }
                            for (c.args, 0..) |arg_src, i| {
                                new_regs[cl.env.len + i] = regs[arg_src];
                                regs[arg_src] = .unit;
                            }
                            
                            const res = try self.exec(cl.body, new_regs, depth + 1, profile);
                            for (new_regs) |r| r.deinit(self.allocator);

                            regs[c.dst].deinit(self.allocator);
                            regs[c.dst] = res.val;
                            ret_val.deinit(self.allocator);
                            ret_val = try res.val.clone(self.allocator);
                            
                            func_val.deinit(self.allocator);
                        },
                        else => {
                            func_val.deinit(self.allocator);
                            return error.NotAFunction;
                        },
                    }
                },
                .ret => |r| {
                    const val = regs[r.src];
                    regs[r.src] = .unit;
                    return ExecutionResult{ .val = val };
                },
                .case_op => |c| {
                    const scrutinee = regs[c.scrutinee];
                    regs[c.scrutinee] = .unit;
                    switch (scrutinee) {
                        .inl => |v| {
                            regs[c.dst].deinit(self.allocator);
                            regs[c.dst] = v.*;
                            self.allocator.destroy(v);
                            const res = try self.exec(c.inl_branch, regs, depth + 1, profile);
                            regs[c.dst].deinit(self.allocator);
                            regs[c.dst] = res.val;
                            ret_val.deinit(self.allocator);
                            ret_val = try res.val.clone(self.allocator);
                        },
                        .inr => |v| {
                            regs[c.dst].deinit(self.allocator);
                            regs[c.dst] = v.*;
                            self.allocator.destroy(v);
                            const res = try self.exec(c.inr_branch, regs, depth + 1, profile);
                            regs[c.dst].deinit(self.allocator);
                            regs[c.dst] = res.val;
                            ret_val.deinit(self.allocator);
                            ret_val = try res.val.clone(self.allocator);
                        },
                        else => return error.InvalidSum,
                    }
                },
            }
        }

        return ExecutionResult{ .val = ret_val };
    }
};

test "VM basic operations" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);

    // LoadConst + Return
    {
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .int = 42 } } },
            .{ .ret = .{ .src = 0 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), result.int);
    }

    // MakePair + Proj1
    {
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .int = 10 } } },
            .{ .load_const = .{ .dst = 1, .val = .{ .int = 20 } } },
            .{ .make_pair = .{ .dst = 2, .fst = 0, .snd = 1 } },
            .{ .proj1 = .{ .dst = 3, .src = 2 } },
            .{ .ret = .{ .src = 3 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 10), result.int);
    }

    // MakePair + Proj2
    {
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .int = 10 } } },
            .{ .load_const = .{ .dst = 1, .val = .{ .int = 20 } } },
            .{ .make_pair = .{ .dst = 2, .fst = 0, .snd = 1 } },
            .{ .proj2 = .{ .dst = 3, .src = 2 } },
            .{ .ret = .{ .src = 3 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 20), result.int);
    }
}

test "VM closure" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);

    // Identity function: (lambda x. x)(42) = 42
    {
        const id_body = &[_]Op{
            .{ .ret = .{ .src = 0 } }, // In this VM, args start at 0 (after env)
        };
        const code = &[_]Op{
            .{ .make_closure = .{ .dst = 0, .body = id_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 0 } },
            .{ .load_const = .{ .dst = 1, .val = .{ .int = 42 } } },
            .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), result.int);
    }
}

test "VM Sum and Case" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);

    // Case(inl(x), ...)
    {
        const inl_branch = &[_]Op{
            .{ .ret = .{ .src = 2 } }, // In case_op, the inner value is put into dst (reg 2 here)
        };
        const inr_branch = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .int = 999 } } },
            .{ .ret = .{ .src = 0 } },
        };
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .int = 123 } } },
            .{ .make_inl = .{ .dst = 1, .src = 0 } },
            .{ .case_op = .{ .dst = 2, .scrutinee = 1, .inl_branch = inl_branch, .inr_branch = inr_branch } },
            .{ .ret = .{ .src = 2 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 123), result.int);
    }
}

test "ProfilingVM" {
    const allocator = std.testing.allocator;
    var pvm = ProfilingVM.init(allocator);
    defer pvm.deinit();

    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .int = 1 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .int = 2 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };

    const result = try pvm.run(code);
    defer result.deinit(allocator);

    try std.testing.expectEqual(@as(u64, 3), result.int);
    try std.testing.expect(pvm.profile_data.counts.get(0).? >= 1);
    try std.testing.expect(pvm.profile_data.counts.get(1).? >= 1);
    try std.testing.expect(pvm.profile_data.counts.get(2).? >= 1);
}

test "VM nested closure" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);

    // λx. λy. x
    const inner_body = &[_]Op{
        .{ .ret = .{ .src = 0 } }, // Capture is at 0, arg y would be at 1
    };
    const outer_body = &[_]Op{
        .{ .make_closure = .{ .dst = 1, .body = inner_body, .captures = &[_]u32{0}, .arity = 1, .block_idx = 100 } },
        .{ .ret = .{ .src = 1 } },
    };

    const code = &[_]Op{
        .{ .make_closure = .{ .dst = 0, .body = outer_body, .captures = &[_]u32{}, .arity = 1, .block_idx = 101 } },
        .{ .load_const = .{ .dst = 1, .val = .{ .int = 100 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } }, // returns inner closure capturing 100
        .{ .load_const = .{ .dst = 3, .val = .unit } }, // unit arg
        .{ .call = .{ .dst = 4, .func = 2, .args = &[_]u32{3} } },
        .{ .ret = .{ .src = 4 } },
    };

    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 100), result.int);
}

pub const ProfileData = struct {
    counts: std.AutoHashMap(usize, usize),
    // pc -> reg -> value frequency
    value_profiles: std.AutoHashMap(usize, std.AutoHashMap(u32, std.AutoHashMap(u64, usize))),
    // pc -> block_idx -> frequency
    call_profiles: std.AutoHashMap(usize, std.AutoHashMap(usize, usize)),
    allocator: Allocator,

    pub fn init(allocator: Allocator) ProfileData {
        return .{
            .counts = std.AutoHashMap(usize, usize).init(allocator),
            .value_profiles = std.AutoHashMap(usize, std.AutoHashMap(u32, std.AutoHashMap(u64, usize))).init(allocator),
            .call_profiles = std.AutoHashMap(usize, std.AutoHashMap(usize, usize)).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ProfileData) void {
        self.counts.deinit();
        var it = self.value_profiles.iterator();
        while (it.next()) |entry| {
            var reg_it = entry.value_ptr.iterator();
            while (reg_it.next()) |reg_entry| {
                reg_entry.value_ptr.deinit();
            }
            entry.value_ptr.deinit();
        }
        self.value_profiles.deinit();

        var call_it = self.call_profiles.iterator();
        while (call_it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.call_profiles.deinit();
    }

    pub fn record(self: *ProfileData, pc: usize) !void {
        const entry = try self.counts.getOrPut(pc);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }

    pub fn recordCall(self: *ProfileData, pc: usize, block_idx: usize) !void {
        var pc_entry = try self.call_profiles.getOrPut(pc);
        if (!pc_entry.found_existing) {
            pc_entry.value_ptr.* = std.AutoHashMap(usize, usize).init(self.allocator);
        }
        const freq_entry = try pc_entry.value_ptr.getOrPut(block_idx);
        if (freq_entry.found_existing) {
            freq_entry.value_ptr.* += 1;
        } else {
            freq_entry.value_ptr.* = 1;
        }
    }

    pub fn getDominantCallTarget(self: ProfileData, pc: usize, threshold: usize) ?usize {
        const pc_map = self.call_profiles.get(pc) orelse return null;
        var it = pc_map.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* >= threshold) return entry.key_ptr.*;
        }
        return null;
    }

    pub fn recordValue(self: *ProfileData, pc: usize, reg: u32, val: Value) !void {
        if (val != .int) return;
        const v = val.int;

        var pc_entry = try self.value_profiles.getOrPut(pc);
        if (!pc_entry.found_existing) {
            pc_entry.value_ptr.* = std.AutoHashMap(u32, std.AutoHashMap(u64, usize)).init(self.allocator);
        }
        
        var reg_entry = try pc_entry.value_ptr.getOrPut(reg);
        if (!reg_entry.found_existing) {
            reg_entry.value_ptr.* = std.AutoHashMap(u64, usize).init(self.allocator);
        }

        const freq_entry = try reg_entry.value_ptr.getOrPut(v);
        if (freq_entry.found_existing) {
            freq_entry.value_ptr.* += 1;
        } else {
            freq_entry.value_ptr.* = 1;
        }
    }

    pub fn getStableValue(self: ProfileData, pc: usize, reg: u32, threshold: usize) ?u64 {
        const pc_map = self.value_profiles.get(pc) orelse return null;
        const reg_map = pc_map.get(reg) orelse return null;
        var it = reg_map.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* >= threshold) return entry.key_ptr.*;
        }
        return null;
    }
};

pub const ProfilingVM = struct {
    vm: VM,
    profile_data: ProfileData,

    pub fn init(allocator: Allocator) ProfilingVM {
        return .{
            .vm = VM.init(allocator),
            .profile_data = ProfileData.init(allocator),
        };
    }

    pub fn deinit(self: *ProfilingVM) void {
        self.profile_data.deinit();
    }

    pub fn run(self: *ProfilingVM, code: []const Op) !Value {
        const regs = try self.vm.allocator.alloc(Value, 32);
        defer self.vm.allocator.free(regs);
        for (regs) |*r| r.* = .unit;
        
        const result = try self.vm.exec(code, regs, 0, &self.profile_data);
        for (regs) |r| r.deinit(self.vm.allocator);
        return result.val;
    }
};
