const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Value = vm.Value;
const Op = vm.Op;

pub const LoadedProgram = struct {
    allocator: Allocator,
    arena: *std.heap.ArenaAllocator,
    constants: []Value,
    blocks: []const []const Op,

    pub fn deinit(self: *LoadedProgram) void {
        for (self.constants) |v| v.deinit(self.allocator);
        self.allocator.free(self.constants);
        // Note: individual blocks and nested ops are allocated in arena
        self.arena.deinit();
        self.allocator.destroy(self.arena);
        self.allocator.free(self.blocks);
    }

    pub fn mainCode(self: LoadedProgram) []const Op {
        if (self.blocks.len == 0) return &[_]Op{};
        return self.blocks[0];
    }
};

pub const BytecodeLoader = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) BytecodeLoader {
        return .{ .allocator = allocator };
    }

    pub fn loadFromFile(self: *BytecodeLoader, path: []const u8) !LoadedProgram {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const file_size = (try file.stat()).size;
        const buffer = try self.allocator.alloc(u8, file_size);
        defer self.allocator.free(buffer);
        _ = try file.readAll(buffer);

        var fbs = std.io.fixedBufferStream(buffer);
        var reader = fbs.reader();
        
        // 1. Header
        var magic: [4]u8 = undefined;
        try reader.readNoEof(&magic);
        if (!std.mem.eql(u8, &magic, "RBC\x01")) return error.InvalidMagic;
        
        _ = try reader.readInt(u32, .little); // version

        // 2. Constant Pool
        const pool_size = try reader.readInt(u32, .little);
        const constants = try self.allocator.alloc(Value, pool_size);
        errdefer {
            for (constants) |v| v.deinit(self.allocator);
            self.allocator.free(constants);
        }

        for (0..pool_size) |i| {
            const tag = try reader.readByte();
            switch (tag) {
                5 => constants[i] = .unit,
                6 => {
                    const val = try reader.readInt(u64, .little);
                    constants[i] = .{ .bits = val };
                },
                else => return error.UnsupportedConstantType,
            }
        }

        // 3. Code Blocks
        const arena = try self.allocator.create(std.heap.ArenaAllocator);
        arena.* = std.heap.ArenaAllocator.init(self.allocator);
        errdefer { arena.deinit(); self.allocator.destroy(arena); }
        const aa = arena.allocator();

        const block_count = try reader.readInt(u32, .little);
        const blocks_mut = try self.allocator.alloc([]Op, block_count);
        defer self.allocator.free(blocks_mut);

        const RawOp = struct {
            opcode: u8,
            u1: u32 = 0,
            u2: u32 = 0,
            u3: u32 = 0,
            u4: u32 = 0,
            arr: []u32 = &[_]u32{},
        };

        const RawBlock = struct {
            ops: []RawOp,
        };
        
        const raw_blocks = try self.allocator.alloc(RawBlock, block_count);
        defer {
            for (raw_blocks) |rb| {
                for (rb.ops) |o| self.allocator.free(o.arr);
                self.allocator.free(rb.ops);
            }
            self.allocator.free(raw_blocks);
        }

        for (0..block_count) |i| {
            const op_count = try reader.readInt(u32, .little);
            const raw_ops = try self.allocator.alloc(RawOp, op_count);
            raw_blocks[i] = .{ .ops = raw_ops };

            for (0..op_count) |j| {
                const opcode = try reader.readByte();
                var ro = RawOp{ .opcode = opcode };
                switch (opcode) {
                    0x01, 0x07, 0x08, 0x09, 0x0A, 0x0F => { // dst, src
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                    },
                    0x02 => { // dst, const_idx
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                    },
                    0x03 => { // dst, block_idx, cap_count, captures, arity
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        const cap_count = try reader.readInt(u32, .little);
                        const caps = try self.allocator.alloc(u32, cap_count);
                        for (0..cap_count) |k| caps[k] = try reader.readInt(u32, .little);
                        ro.arr = caps;
                        ro.u3 = try reader.readInt(u32, .little);
                    },
                    0x04 => { // dst, func, arg_count, args
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        const arg_count = try reader.readInt(u32, .little);
                        const args = try self.allocator.alloc(u32, arg_count);
                        for (0..arg_count) |k| args[k] = try reader.readInt(u32, .little);
                        ro.arr = args;
                    },
                    0x05, 0x10 => { // src/reg
                        ro.u1 = try reader.readInt(u32, .little);
                    },
                    0x06 => { // dst, fst, snd
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        ro.u3 = try reader.readInt(u32, .little);
                    },
                    0x0B => { // dst, scrutinee, inl_idx, inr_idx
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        ro.u3 = try reader.readInt(u32, .little);
                        ro.u4 = try reader.readInt(u32, .little);
                    },
                    0x0C, 0x0D, 0x0E => { // dst, lhs, rhs
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        ro.u3 = try reader.readInt(u32, .little);
                    },
                    0x11, 0x12 => { // ibin/icmp: dst, lhs, rhs, op/pred(u8), width(u16)
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        ro.u3 = try reader.readInt(u32, .little);
                        const b1 = try reader.readByte();
                        const w = try reader.readInt(u16, .little);
                        ro.u4 = (@as(u32, b1) << 16) | w;
                    },
                    0x13 => { // load_bits: dst, val(u64), width(u16)
                        ro.u1 = try reader.readInt(u32, .little);
                        const val = try reader.readInt(u64, .little);
                        ro.u2 = @as(u32, @truncate(val));
                        ro.u3 = @as(u32, @truncate(val >> 32));
                        ro.u4 = try reader.readInt(u16, .little);
                    },
                    0x14 => { // load_wide: dst, width(u16), limb_count, limbs
                        ro.u1 = try reader.readInt(u32, .little);
                        const w = try reader.readInt(u16, .little);
                        const limb_count = try reader.readInt(u32, .little);
                        ro.u2 = (@as(u32, w) << 16) | (limb_count & 0xFFFF); // Simplified packing
                        const limbs = try self.allocator.alloc(u32, limb_count * 2); // Store u64 as 2x u32
                        for (0..limb_count) |k| {
                            const l = try reader.readInt(u64, .little);
                            limbs[k * 2] = @as(u32, @truncate(l));
                            limbs[k * 2 + 1] = @as(u32, @truncate(l >> 32));
                        }
                        ro.arr = limbs;
                    },
                    0x15, 0x16, 0x17 => { // sext/zext/trunc: dst, src, from(u16), to(u16)
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        const from = try reader.readInt(u16, .little);
                        const to = try reader.readInt(u16, .little);
                        ro.u3 = (@as(u32, from) << 16) | to;
                    },
                    0x18, 0x19 => { // itof/ftoi: dst, src, width(u16), signed(u8)
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        const w = try reader.readInt(u16, .little);
                        const s = try reader.readByte();
                        ro.u3 = (@as(u32, w) << 8) | s;
                    },
                    0x20, 0x21, 0x22, 0x23, 0x24 => { // fadd..frem: dst, lhs, rhs
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        ro.u3 = try reader.readInt(u32, .little);
                    },
                    0x25 => { // fcmp: dst, lhs, rhs, pred(u8)
                        ro.u1 = try reader.readInt(u32, .little);
                        ro.u2 = try reader.readInt(u32, .little);
                        ro.u3 = try reader.readInt(u32, .little);
                        ro.u4 = try reader.readByte();
                    },
                    else => return error.UnknownOpcode,
                }
                raw_ops[j] = ro;
            }
        }

        // Pass 2: Allocate Op arrays for each block
        for (0..block_count) |i| {
            blocks_mut[i] = try aa.alloc(Op, raw_blocks[i].ops.len);
        }

        // Pass 3: Finalize Op structures and patch pointers
        for (0..block_count) |i| {
            const op_count = raw_blocks[i].ops.len;
            for (0..op_count) |j| {
                const ro = raw_blocks[i].ops[j];
                switch (ro.opcode) {
                    0x01 => blocks_mut[i][j] = .{ .move = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x02 => {
                        const val = try constants[ro.u2].clone(aa);
                        blocks_mut[i][j] = .{ .load_const = .{ .dst = ro.u1, .val = val } };
                    },
                    0x03 => {
                        const caps = try aa.dupe(u32, ro.arr);
                        blocks_mut[i][j] = .{ .make_closure = .{
                            .dst = ro.u1,
                            .body = blocks_mut[ro.u2],
                            .captures = caps,
                            .arity = ro.u3,
                            .block_idx = ro.u2,
                        } };
                    },
                    0x04 => {
                        const call_args = try aa.dupe(u32, ro.arr);
                        blocks_mut[i][j] = .{ .call = .{
                            .dst = ro.u1,
                            .func = ro.u2,
                            .args = call_args,
                        } };
                    },
                    0x05 => blocks_mut[i][j] = .{ .ret = .{ .src = ro.u1 } },
                    0x06 => blocks_mut[i][j] = .{ .make_pair = .{ .dst = ro.u1, .fst = ro.u2, .snd = ro.u3 } },
                    0x07 => blocks_mut[i][j] = .{ .proj1 = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x08 => blocks_mut[i][j] = .{ .proj2 = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x09 => blocks_mut[i][j] = .{ .make_inl = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x0A => blocks_mut[i][j] = .{ .make_inr = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x0B => blocks_mut[i][j] = .{ .case_op = .{
                        .dst = ro.u1,
                        .scrutinee = ro.u2,
                        .inl_branch = blocks_mut[ro.u3],
                        .inr_branch = blocks_mut[ro.u4],
                    } },
                    0x11 => blocks_mut[i][j] = .{ .ibin = .{
                        .dst = ro.u1,
                        .lhs = ro.u2,
                        .rhs = ro.u3,
                        .op = @enumFromInt(@as(u8, @intCast(ro.u4 >> 16))),
                        .width = @as(u16, @intCast(ro.u4 & 0xFFFF)),
                    } },
                    0x12 => blocks_mut[i][j] = .{ .icmp = .{
                        .dst = ro.u1,
                        .lhs = ro.u2,
                        .rhs = ro.u3,
                        .pred = @enumFromInt(@as(u8, @intCast(ro.u4 >> 16))),
                        .width = @as(u16, @intCast(ro.u4 & 0xFFFF)),
                    } },
                    0x13 => blocks_mut[i][j] = .{ .load_bits = .{
                        .dst = ro.u1,
                        .val = (@as(u64, ro.u3) << 32) | ro.u2,
                        .width = @as(u16, @intCast(ro.u4)),
                    } },
                    0x14 => {
                        const width = @as(u16, @intCast(ro.u2 >> 16));
                        const limb_count = ro.arr.len / 2;
                        const limbs = try aa.alloc(u64, limb_count);
                        for (0..limb_count) |k| {
                            limbs[k] = (@as(u64, ro.arr[k * 2 + 1]) << 32) | ro.arr[k * 2];
                        }
                        blocks_mut[i][j] = .{ .load_wide = .{
                            .dst = ro.u1,
                            .width = width,
                            .limbs = limbs,
                        } };
                    },
                    0x15 => blocks_mut[i][j] = .{ .sext = .{
                        .dst = ro.u1,
                        .src = ro.u2,
                        .from = @as(u16, @intCast(ro.u3 >> 16)),
                        .to = @as(u16, @intCast(ro.u3 & 0xFFFF)),
                    } },
                    0x16 => blocks_mut[i][j] = .{ .zext = .{
                        .dst = ro.u1,
                        .src = ro.u2,
                        .from = @as(u16, @intCast(ro.u3 >> 16)),
                        .to = @as(u16, @intCast(ro.u3 & 0xFFFF)),
                    } },
                    0x17 => blocks_mut[i][j] = .{ .trunc = .{
                        .dst = ro.u1,
                        .src = ro.u2,
                        .from = @as(u16, @intCast(ro.u3 >> 16)),
                        .to = @as(u16, @intCast(ro.u3 & 0xFFFF)),
                    } },
                    0x18 => blocks_mut[i][j] = .{ .itof = .{
                        .dst = ro.u1,
                        .src = ro.u2,
                        .width = @as(u16, @intCast(ro.u3 >> 8)),
                        .signed = (ro.u3 & 0xFF) != 0,
                    } },
                    0x19 => blocks_mut[i][j] = .{ .ftoi = .{
                        .dst = ro.u1,
                        .src = ro.u2,
                        .width = @as(u16, @intCast(ro.u3 >> 8)),
                        .signed = (ro.u3 & 0xFF) != 0,
                    } },
                    0x20 => blocks_mut[i][j] = .{ .fadd = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x21 => blocks_mut[i][j] = .{ .fsub = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x22 => blocks_mut[i][j] = .{ .fmul = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x23 => blocks_mut[i][j] = .{ .fdiv = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x24 => blocks_mut[i][j] = .{ .frem = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x25 => blocks_mut[i][j] = .{ .fcmp = .{
                        .dst = ro.u1,
                        .lhs = ro.u2,
                        .rhs = ro.u3,
                        .pred = @enumFromInt(@as(u8, @intCast(ro.u4))),
                    } },
                    else => unreachable,
                }
            }
        }

        const blocks = try self.allocator.alloc([]const Op, block_count);
        for (0..block_count) |i| {
            blocks[i] = blocks_mut[i];
        }

        return LoadedProgram{
            .allocator = self.allocator,
            .arena = arena,
            .constants = constants,
            .blocks = blocks,
        };
    }
};

