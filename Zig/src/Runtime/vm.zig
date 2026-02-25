const std = @import("std");
const Allocator = std.mem.Allocator;

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
            features.has_neon = true;
            features.vector_width = 128;
        }
        return features;
    }
};

pub const Tier = u32;

pub const IntWidth = u16;

pub const IBinOp = enum {
    add, sub, mul,
    sdiv, udiv,
    srem, urem,
    and_, or_, xor_,
    shl, lshr, ashr,
};

pub const ICmpPred = enum {
    eq, ne,
    slt, sle, sgt, sge,
    ult, ule, ugt, uge,
};

pub const FCmpPred = enum { oeq, one, olt, ole, ogt, oge, ord, uno };

pub const ValueTag = enum(u64) {
    closure = 1,
    pair = 2,
    inl = 3,
    inr = 4,
    unit = 5,
    bits = 6, // ≤64ビットのビット列（int/float/pointer等を区別しない）
    wide = 7, // >64ビットのビット列（ヒープ上のリム配列）
};

pub const Value = union(ValueTag) {
    closure: *Closure,
    pair: *[2]Value,
    inl: *Value,
    inr: *Value,
    unit: void,
    bits: u64,
    wide: []u64,

    pub fn deinit(self: Value, allocator: Allocator) void {
        switch (self) {
            .unit, .bits => {},
            .wide => |limbs| allocator.free(limbs),
            .closure => |c| {
                for (c.env) |v| v.deinit(allocator);
                allocator.free(c.env);
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
            .unit, .bits => return self,
            .wide => |limbs| {
                const new_limbs = try allocator.dupe(u64, limbs);
                return .{ .wide = new_limbs };
            },
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
                    .block_idx = c.block_idx,
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

pub const Closure = struct {
    body: []const Op,
    env: []Value,
    arity: usize,
    tier: u32 = 0,
    native_ptr: ?NativeEntry = null,
    block_idx: usize = 0,
};

pub const LLVMValue = extern struct {
    tag: u64,
    payload: ?*anyopaque,
};

pub const NativeEntry = *const fn (regs: [*]LLVMValue) callconv(.c) u64;

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
    ibin: struct { dst: u32, lhs: u32, rhs: u32, op: IBinOp, width: IntWidth },
    icmp: struct { dst: u32, lhs: u32, rhs: u32, pred: ICmpPred, width: IntWidth },
    load_bits: struct { dst: u32, val: u64, width: IntWidth },
    load_wide: struct { dst: u32, limbs: []const u64, width: IntWidth },
    sext: struct { dst: u32, src: u32, from: IntWidth, to: IntWidth },
    zext: struct { dst: u32, src: u32, from: IntWidth, to: IntWidth },
    trunc: struct { dst: u32, src: u32, from: IntWidth, to: IntWidth },
    itof: struct { dst: u32, src: u32, width: IntWidth, signed: bool },
    ftoi: struct { dst: u32, src: u32, width: IntWidth, signed: bool },
    fadd: struct { dst: u32, lhs: u32, rhs: u32 },
    fsub: struct { dst: u32, lhs: u32, rhs: u32 },
    fmul: struct { dst: u32, lhs: u32, rhs: u32 },
    fdiv: struct { dst: u32, lhs: u32, rhs: u32 },
    frem: struct { dst: u32, lhs: u32, rhs: u32 },
    fcmp: struct { dst: u32, lhs: u32, rhs: u32, pred: FCmpPred },
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

    fn signExtend64(self: VM, val: u64, width: IntWidth) i64 {
        _ = self;
        if (width >= 64) return @bitCast(val);
        const shift = @as(u6, @intCast(64 - width));
        return (@as(i64, @bitCast(val << shift))) >> shift;
    }

    fn limbCount(self: VM, width: IntWidth) usize {
        _ = self;
        return (@as(usize, width) + 63) / 64;
    }

    fn maskTopLimb(self: VM, limbs: []u64, width: IntWidth) void {
        _ = self;
        const top = (width - 1) / 64;
        const used_val = (width - 1) % 64 + 1;
        const mask = if (used_val == 64) ~@as(u64, 0) else (@as(u64, 1) << @as(u6, @intCast(used_val))) - 1;
        limbs[top] &= mask;
    }

    fn bigintAdd(self: *VM, lhs: []const u64, rhs: []const u64, width: IntWidth) ![]u64 {
        const n = self.limbCount(width);
        const dst = try self.allocator.alloc(u64, n);
        var carry: u1 = 0;
        for (0..n) |i| {
            const l = if (i < lhs.len) lhs[i] else 0;
            const r = if (i < rhs.len) rhs[i] else 0;
            const res1 = @addWithOverflow(l, r);
            const res2 = @addWithOverflow(res1[0], carry);
            dst[i] = res2[0];
            carry = res1[1] | res2[1];
        }
        self.maskTopLimb(dst, width);
        return dst;
    }

    fn bigintSub(self: *VM, lhs: []const u64, rhs: []const u64, width: IntWidth) ![]u64 {
        const n = self.limbCount(width);
        const dst = try self.allocator.alloc(u64, n);
        var borrow: u1 = 0;
        for (0..n) |i| {
            const l = if (i < lhs.len) lhs[i] else 0;
            const r = if (i < rhs.len) rhs[i] else 0;
            const res1 = @subWithOverflow(l, r);
            const res2 = @subWithOverflow(res1[0], borrow);
            dst[i] = res2[0];
            borrow = res1[1] | res2[1];
        }
        self.maskTopLimb(dst, width);
        return dst;
    }

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

    pub fn run(self: *VM, code: []const Op) !Value {
        const regs = try self.allocator.alloc(Value, 128);
        defer self.allocator.free(regs);
        for (regs) |*r| r.* = .unit;
        const result = try self.exec(code, regs, 0, null);
        for (regs) |r| r.deinit(self.allocator);
        return result.val;
    }

    pub fn exec(self: *VM, code_init: []const Op, regs_init: []Value, depth_init: usize, profile: ?*ProfileData) !ExecutionResult {
        var code = code_init;
        var regs = regs_init;
        const depth = depth_init;
        if (depth > self.max_depth) return error.StackOverflow;

        while (true) {
            var pc: usize = 0;
            var ret_val: Value = .unit;
            while (pc < code.len) : (pc +%= 1) {
                if (profile) |p| {
                    try p.record(pc);
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
                        closure.* = .{ .body = mc.body, .env = env, .arity = mc.arity, .block_idx = mc.block_idx };
                        regs[mc.dst].deinit(self.allocator);
                        regs[mc.dst] = .{ .closure = closure };
                    },
                                        .add => |a| {
                                            const l = regs[a.lhs].bits;
                                            const r = regs[a.rhs].bits;
                                            regs[a.dst].deinit(self.allocator);
                                            regs[a.dst] = .{ .bits = l +% r };
                                        },
                                        .sub => |s| {
                                            const l = regs[s.lhs].bits;
                                            const r = regs[s.rhs].bits;
                                            regs[s.dst].deinit(self.allocator);
                                            regs[s.dst] = .{ .bits = l -% r };
                                        },
                                        .mul => |m| {
                                            const l = regs[m.lhs].bits;
                                            const r = regs[m.rhs].bits;
                                            regs[m.dst].deinit(self.allocator);
                                            regs[m.dst] = .{ .bits = l *% r };
                                        },
                    
                    .make_pair => |mp| {
                        const pair = try self.allocator.create([2]Value);
                        pair[0] = regs[mp.fst]; pair[1] = regs[mp.snd];
                        regs[mp.fst] = .unit; regs[mp.snd] = .unit;
                        regs[mp.dst].deinit(self.allocator); regs[mp.dst] = .{ .pair = pair };
                    },
                    .proj1 => |p| {
                        const pair_val = regs[p.src];
                        switch (pair_val) {
                            .pair => |pair| {
                                regs[p.dst].deinit(self.allocator); regs[p.dst] = pair[0];
                                regs[p.src] = pair[1]; self.allocator.destroy(pair);
                            },
                            else => return error.NotAPair,
                        }
                    },
                    .proj2 => |p| {
                        const pair_val = regs[p.src];
                        switch (pair_val) {
                            .pair => |pair| {
                                regs[p.dst].deinit(self.allocator); regs[p.dst] = pair[1];
                                regs[p.src] = pair[0]; self.allocator.destroy(pair);
                            },
                            else => return error.NotAPair,
                        }
                    },
                    .make_inl => |m| {
                        const val = try self.allocator.create(Value);
                        val.* = regs[m.src]; regs[m.src] = .unit;
                        regs[m.dst].deinit(self.allocator); regs[m.dst] = .{ .inl = val };
                    },
                    .make_inr => |m| {
                        const val = try self.allocator.create(Value);
                        val.* = regs[m.src]; regs[m.src] = .unit;
                        regs[m.dst].deinit(self.allocator); regs[m.dst] = .{ .inr = val };
                    },
                    .borrow => |b| {
                        regs[b.dst].deinit(self.allocator);
                        regs[b.dst] = try regs[b.src].clone(self.allocator);
                    },
                    .free => |f| {
                        regs[f.reg].deinit(self.allocator); regs[f.reg] = .unit;
                    },
                    .call => |c| {
                        const func_val = regs[c.func];
                        regs[c.func] = .unit;
                        switch (func_val) {
                            .closure => |cl| {
                                if (profile) |p| try p.recordCall(pc, cl.block_idx);
                                if (cl.native_ptr) |native_func| {
                                    var native_regs: [32]LLVMValue = undefined;
                                    for (cl.env, 0..) |v, i| { native_regs[i] = switch (v) { .bits => |n| .{ .tag = 6, .payload = @ptrFromInt(n) }, .unit => .{ .tag = 5, .payload = null }, else => .{ .tag = 0, .payload = null } }; }
                                    for (c.args, 0..) |arg_src, i| { const v = regs[arg_src]; native_regs[cl.env.len + i] = switch (v) { .bits => |n| .{ .tag = 6, .payload = @ptrFromInt(n) }, .unit => .{ .tag = 5, .payload = null }, else => .{ .tag = 0, .payload = null } }; regs[arg_src] = .unit; }
                                    const res_u64 = native_func(&native_regs);
                                    regs[c.dst].deinit(self.allocator); regs[c.dst] = .{ .bits = res_u64 };
                                    ret_val.deinit(self.allocator); ret_val = try regs[c.dst].clone(self.allocator);
                                    func_val.deinit(self.allocator); continue;
                                }
                                                                                                                                if (pc + 1 < code.len and (code[pc + 1] == .ret)) {
                                                                                                                                    var temp_regs: [128]Value = undefined;
                                                                                                                                    for (&temp_regs) |*t| t.* = .unit;
                                                                                                                                    for (cl.env, 0..) |v, i| { temp_regs[i] = try v.clone(self.allocator); }
                                                                                                                                    for (c.args, 0..) |arg_src, i| {
                                                                                                                                        if (arg_src < regs.len) {
                                                                                                                                            temp_regs[cl.env.len + i] = regs[arg_src];
                                                                                                                                            regs[arg_src] = .unit;
                                                                                                                                        }
                                                                                                                                    }
                                                                                                                                    for (regs) |*r| { r.deinit(self.allocator); r.* = .unit; }
                                                                                                                                    for (0..@min(regs.len, 128)) |i| { regs[i] = temp_regs[i]; }
                                                                                                                                    const next_code = cl.body;
                                                                                                                                    func_val.deinit(self.allocator);
                                                                                                                                    code = next_code; pc = @as(usize, 0) -% 1;
                                                                                                                                    continue;
                                                                                                                                }
                                                                                                
                                                                                                                                const call_new_regs = try self.allocator.alloc(Value, 128);
                                                                                                                                defer self.allocator.free(call_new_regs);
                                                                                                                                for (call_new_regs) |*r| r.* = .unit;
                                                                                                                                
                                                                                                                                for (cl.env, 0..) |v, i| { call_new_regs[i] = try v.clone(self.allocator); }
                                                                                                                                for (c.args, 0..) |arg_src, i| {
                                                                                                                                    call_new_regs[cl.env.len + i] = regs[arg_src];
                                                                                                                                    regs[arg_src] = .unit;
                                                                                                                                }
                                                                                                                                
                                                                                                                                const res = try self.exec(cl.body, call_new_regs, depth + 1, profile);
                                                                                                                                regs[c.dst].deinit(self.allocator);
                                                                                                                                regs[c.dst] = res.val;
                                                                                                                                ret_val.deinit(self.allocator);
                                                                                                                                ret_val = try res.val.clone(self.allocator);
                                                                                                                                func_val.deinit(self.allocator);
                                                                                                                            },
                                                                                                                            else => { func_val.deinit(self.allocator); return error.NotAFunction; },
                                                                                                                        }
                                                                                                                    },
                                                                                                                    .ret => |r| {
                                                                                                                        const val = regs[r.src]; regs[r.src] = .unit;
                                                                                                                        ret_val.deinit(self.allocator); return ExecutionResult{ .val = val };
                                                                                                                    },
                                                                                                                    .case_op => |c| {
                                                                                                                        const scrutinee = regs[c.scrutinee]; regs[c.scrutinee] = .unit;
                                                                                                                        switch (scrutinee) {
                                                                                                                            .inl, .inr => |v| {
                                                                                                                                regs[c.dst].deinit(self.allocator); regs[c.dst] = v.*; self.allocator.destroy(v);
                                                                                                                                const res = try self.exec(if (scrutinee == .inl) c.inl_branch else c.inr_branch, regs, depth + 1, profile);
                                                                                                                                regs[c.dst].deinit(self.allocator); regs[c.dst] = res.val;
                                                                                                                                ret_val.deinit(self.allocator); ret_val = try res.val.clone(self.allocator);
                                                                                                                            },
                                                                                                                            else => return error.InvalidSum,
                                                                                                                        }
                                                                                                                    },
                                                                                                                    .load_bits => |o| {
                        regs[o.dst].deinit(self.allocator);
                        const mask = if (o.width >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.width)) - 1;
                        regs[o.dst] = .{ .bits = o.val & mask };
                    },
                    .load_wide => |o| {
                        regs[o.dst].deinit(self.allocator);
                        const limbs = try self.allocator.dupe(u64, o.limbs);
                        regs[o.dst] = .{ .wide = limbs };
                    },
                    .fadd => |o| {
                        const lf: f64 = @bitCast(regs[o.lhs].bits); const rf: f64 = @bitCast(regs[o.rhs].bits);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = @bitCast(lf + rf) };
                    },
                    .fsub => |o| {
                        const lf: f64 = @bitCast(regs[o.lhs].bits); const rf: f64 = @bitCast(regs[o.rhs].bits);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = @bitCast(lf - rf) };
                    },
                    .fmul => |o| {
                        const lf: f64 = @bitCast(regs[o.lhs].bits); const rf: f64 = @bitCast(regs[o.rhs].bits);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = @bitCast(lf * rf) };
                    },
                    .fdiv => |o| {
                        const lf: f64 = @bitCast(regs[o.lhs].bits); const rf: f64 = @bitCast(regs[o.rhs].bits);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = @bitCast(lf / rf) };
                    },
                    .frem => |o| {
                        const lf: f64 = @bitCast(regs[o.lhs].bits); const rf: f64 = @bitCast(regs[o.rhs].bits);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = @bitCast(@mod(lf, rf)) };
                    },
                    .ibin => |o| {
                        if (o.width <= 64) {
                            const l = regs[o.lhs].bits; const r = regs[o.rhs].bits;
                            const mask = if (o.width >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.width)) - 1;
                            const raw: u64 = switch (o.op) {
                                .add => l +% r, .sub => l -% r, .mul => l *% r, .and_ => l & r, .or_ => l | r, .xor_ => l ^ r,
                                .shl => l << @intCast(r & 63), .lshr => l >> @intCast(r & 63),
                                .ashr => @bitCast(self.signExtend64(l, o.width) >> @intCast(r & 63)),
                                .sdiv => blk: { const sl = self.signExtend64(l, o.width); const sr = self.signExtend64(r, o.width); if (sr == 0) return error.DivisionByZero; break :blk @bitCast(@divTrunc(sl, sr)); },
                                .udiv => blk: { if (r == 0) return error.DivisionByZero; break :blk l / r; },
                                .srem => blk: { const sl = self.signExtend64(l, o.width); const sr = self.signExtend64(r, o.width); if (sr == 0) return error.DivisionByZero; break :blk @bitCast(@rem(sl, sr)); },
                                .urem => blk: { if (r == 0) return error.DivisionByZero; break :blk l % r; },
                            };
                            regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = raw & mask };
                        } else {
                            const l = regs[o.lhs].wide; const r = regs[o.rhs].wide;
                            const res = switch (o.op) {
                                .add => try self.bigintAdd(l, r, o.width),
                                .sub => try self.bigintSub(l, r, o.width),
                                .mul => try self.bigintMul(l, r, o.width),
                                .and_ => try self.bigintBitwise(l, r, .and_, o.width),
                                .or_  => try self.bigintBitwise(l, r, .or_, o.width),
                                .xor_ => try self.bigintBitwise(l, r, .xor_, o.width),
                                else => return error.UnsupportedBigIntOp,
                            };
                            regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .wide = res };
                        }
                    },
                    .icmp => |o| {
                        if (o.width <= 64) {
                            const l = regs[o.lhs].bits; const r = regs[o.rhs].bits;
                            const cond: bool = switch (o.pred) {
                                .eq => l == r, .ne => l != r, .slt => self.signExtend64(l, o.width) < self.signExtend64(r, o.width), .sle => self.signExtend64(l, o.width) <= self.signExtend64(r, o.width), .sgt => self.signExtend64(l, o.width) > self.signExtend64(r, o.width), .sge => self.signExtend64(l, o.width) >= self.signExtend64(r, o.width), .ult => l < r, .ule => l <= r, .ugt => l > r, .uge => l >= r,
                            };
                            const v = try self.allocator.create(Value); v.* = .unit;
                            regs[o.dst].deinit(self.allocator); regs[o.dst] = if (cond) .{ .inl = v } else .{ .inr = v };
                        } else return error.UnsupportedBigInt;
                    },
                    .fcmp => |o| {
                        const l: f64 = @bitCast(regs[o.lhs].bits); const r: f64 = @bitCast(regs[o.rhs].bits);
                        const cond: bool = switch (o.pred) { .oeq => l == r, .one => l != r, .olt => l < r, .ole => l <= r, .ogt => l > r, .oge => l >= r, .ord => !std.math.isNan(l) and !std.math.isNan(r), .uno => std.math.isNan(l) or std.math.isNan(r) };
                        const v = try self.allocator.create(Value); v.* = .unit;
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = if (cond) .{ .inl = v } else .{ .inr = v };
                    },
                    .sext => |o| {
                        if (o.to > 64) return error.UnsupportedBigInt;
                        const val = regs[o.src].bits;
                        const raw: u64 = @bitCast(self.signExtend64(val, o.from));
                        const mask = if (o.to >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.to)) - 1;
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = raw & mask };
                    },
                    .zext => |o| {
                        if (o.to > 64) return error.UnsupportedBigInt;
                        const val = regs[o.src].bits;
                        const mask = if (o.to >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.to)) - 1;
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = val & mask };
                    },
                    .trunc => |o| {
                        if (o.to > 64) return error.UnsupportedBigInt;
                        const val = regs[o.src].bits;
                        const mask = if (o.to >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.to)) - 1;
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = val & mask };
                    },
                    .itof => |o| {
                        const val = regs[o.src].bits; const f: f64 = if (o.signed) @floatFromInt(self.signExtend64(val, o.width)) else @floatFromInt(val);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = @bitCast(f) };
                    },
                    .ftoi => |o| {
                        const f: f64 = @bitCast(regs[o.src].bits); const mask = if (o.width >= 64) ~@as(u64, 0) else (@as(u64, 1) << @intCast(o.width)) - 1;
                        const i: u64 = if (o.signed) @bitCast(@as(i64, @intFromFloat(f))) else @intFromFloat(f);
                        regs[o.dst].deinit(self.allocator); regs[o.dst] = .{ .bits = i & mask };
                    },
                }
            }
            return ExecutionResult{ .val = ret_val };
        }
    }
};

test "VM basic operations" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);

    // LoadConst + Return
    {
        const code = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 42 } } },
            .{ .ret = .{ .src = 0 } },
        };
        const result = try vm_inst.run(code);
        defer result.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), result.bits);
    }

    // MakePair + Proj1
    {
        const code = &[_]Op{
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
        const code = &[_]Op{
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
    var vm_inst = VM.init(allocator);

    // Identity function: (lambda x. x)(42) = 42
    {
        const id_body = &[_]Op{
            .{ .ret = .{ .src = 0 } }, // In this VM, args start at 0 (after env)
        };
        const code = &[_]Op{
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
    var vm_inst = VM.init(allocator);

    // Case(inl(x), ...)
    {
        const inl_branch = &[_]Op{
            .{ .ret = .{ .src = 2 } }, // In case_op, the inner value is put into dst (reg 2 here)
        };
        const inr_branch = &[_]Op{
            .{ .load_const = .{ .dst = 0, .val = .{ .bits = 999 } } },
            .{ .ret = .{ .src = 0 } },
        };
        const code = &[_]Op{
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
    var pvm = ProfilingVM.init(allocator);
    defer pvm.deinit();

    const code = &[_]Op{
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
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 100 } } },
        .{ .call = .{ .dst = 2, .func = 0, .args = &[_]u32{1} } }, // returns inner closure capturing 100
        .{ .load_const = .{ .dst = 3, .val = .unit } }, // unit arg
        .{ .call = .{ .dst = 4, .func = 2, .args = &[_]u32{3} } },
        .{ .ret = .{ .src = 4 } },
    };

    const result = try vm_inst.run(code);
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 100), result.bits);
}

pub const ProfileData = struct {
    counts: std.AutoHashMap(usize, usize),
    value_profiles: std.AutoHashMap(usize, std.AutoHashMap(u32, std.AutoHashMap(u64, usize))),
    call_profiles: std.AutoHashMap(usize, std.AutoHashMap(usize, usize)),
    allocator: Allocator,
    pub fn init(allocator: Allocator) ProfileData { return .{ .counts = std.AutoHashMap(usize, usize).init(allocator), .value_profiles = std.AutoHashMap(usize, std.AutoHashMap(u32, std.AutoHashMap(u64, usize))).init(allocator), .call_profiles = std.AutoHashMap(usize, std.AutoHashMap(usize, usize)).init(allocator), .allocator = allocator }; }
    pub fn deinit(self: *ProfileData) void { self.counts.deinit(); var it = self.value_profiles.iterator(); while (it.next()) |entry| { var reg_it = entry.value_ptr.iterator(); while (reg_it.next()) |reg_entry| { reg_entry.value_ptr.deinit(); } entry.value_ptr.deinit(); } self.value_profiles.deinit(); var call_it = self.call_profiles.iterator(); while (call_it.next()) |entry| { entry.value_ptr.deinit(); } self.call_profiles.deinit(); }
    pub fn record(self: *ProfileData, pc: usize) !void { const entry = try self.counts.getOrPut(pc); if (entry.found_existing) { entry.value_ptr.* += 1; } else { entry.value_ptr.* = 1; } }
    pub fn recordCall(self: *ProfileData, pc: usize, block_idx: usize) !void { var pc_entry = try self.call_profiles.getOrPut(pc); if (!pc_entry.found_existing) { pc_entry.value_ptr.* = std.AutoHashMap(usize, usize).init(self.allocator); } const freq_entry = try pc_entry.value_ptr.getOrPut(block_idx); if (freq_entry.found_existing) { freq_entry.value_ptr.* += 1; } else { freq_entry.value_ptr.* = 1; } }
    pub fn getDominantCallTarget(self: ProfileData, pc: usize, threshold: usize) ?usize { const pc_map = self.call_profiles.get(pc) orelse return null; var it = pc_map.iterator(); while (it.next()) |entry| { if (entry.value_ptr.* >= threshold) return entry.key_ptr.*; } return null; }
    pub fn recordValue(self: *ProfileData, pc: usize, reg: u32, val: Value) !void { if (val != .bits) return; const v = val.bits; var pc_entry = try self.value_profiles.getOrPut(pc); if (!pc_entry.found_existing) { pc_entry.value_ptr.* = std.AutoHashMap(u32, std.AutoHashMap(u64, usize)).init(self.allocator); } var reg_entry = try pc_entry.value_ptr.getOrPut(reg); if (!reg_entry.found_existing) { reg_entry.value_ptr.* = std.AutoHashMap(u64, usize).init(self.allocator); } const freq_entry = try reg_entry.value_ptr.getOrPut(v); if (freq_entry.found_existing) { freq_entry.value_ptr.* += 1; } else { freq_entry.value_ptr.* = 1; } }
    pub fn getStableValue(self: ProfileData, pc: usize, reg: u32, threshold: usize) ?u64 { const pc_map = self.value_profiles.get(pc) orelse return null; const reg_map = pc_map.get(reg) orelse return null; var it = reg_map.iterator(); while (it.next()) |entry| { if (entry.value_ptr.* >= threshold) return entry.key_ptr.*; } return null; }
};

pub const ProfilingVM = struct {
    vm: VM,
    profile_data: ProfileData,
    pub fn init(allocator: Allocator) ProfilingVM { return .{ .vm = VM.init(allocator), .profile_data = ProfileData.init(allocator) }; }
    pub fn deinit(self: *ProfilingVM) void { self.profile_data.deinit(); }
    pub fn run(self: *ProfilingVM, code: []const Op) !Value { const regs = try self.vm.allocator.alloc(Value, 128); defer self.vm.allocator.free(regs); for (regs) |*r| r.* = .unit; const result = try self.vm.exec(code, regs, 0, &self.profile_data); for (regs) |r| r.deinit(self.vm.allocator); return result.val; }
};

test "bigintMul: 3 * 5 = 15" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);
    const lhs = [_]u64{3};
    const rhs = [_]u64{5};
    const res = try vm_inst.bigintMul(&lhs, &rhs, 64);
    defer allocator.free(res);
    try std.testing.expectEqual(@as(usize, 1), res.len);
    try std.testing.expectEqual(@as(u64, 15), res[0]);
}

test "bigintMul: 2^64 * 1" {
    const allocator = std.testing.allocator;
    var vm_inst = VM.init(allocator);
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
    var vm_inst = VM.init(allocator);
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
    var vm_inst = VM.init(allocator);
    const code = &[_]Op{
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
    var vm_inst = VM.init(allocator);
    const code = &[_]Op{
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
