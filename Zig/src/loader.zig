const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Value = vm.Value;
const Op = vm.Op;

pub const LoadedProgram = struct {
    allocator: Allocator,
    constants: []Value,
    blocks: []const []const Op,

    pub fn deinit(self: *LoadedProgram) void {
        for (self.constants) |v| v.deinit(self.allocator);
        self.allocator.free(self.constants);
        for (self.blocks) |block| {
            for (block) |op| {
                switch (op) {
                    .load_const => |lc| lc.val.deinit(self.allocator),
                    .make_closure => |mc| self.allocator.free(mc.captures),
                    .call => |c| self.allocator.free(c.args),
                    else => {},
                }
            }
            self.allocator.free(block);
        }
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
                    constants[i] = .{ .int = val };
                },
                else => return error.UnsupportedConstantType,
            }
        }

        // 3. Code Blocks
        const block_count = try reader.readInt(u32, .little);
        const blocks_mut = try self.allocator.alloc([]Op, block_count);
        errdefer self.allocator.free(blocks_mut);

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
                    else => return error.UnknownOpcode,
                }
                raw_ops[j] = ro;
            }
        }

        // Pass 2: Allocate Op arrays for each block
        for (0..block_count) |i| {
            blocks_mut[i] = try self.allocator.alloc(Op, raw_blocks[i].ops.len);
        }

        // Pass 3: Finalize Op structures and patch pointers
        for (0..block_count) |i| {
            const op_count = raw_blocks[i].ops.len;
            for (0..op_count) |j| {
                const ro = raw_blocks[i].ops[j];
                switch (ro.opcode) {
                    0x01 => blocks_mut[i][j] = .{ .move = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x02 => {
                        const val = try constants[ro.u2].clone(self.allocator);
                        blocks_mut[i][j] = .{ .load_const = .{ .dst = ro.u1, .val = val } };
                    },
                    0x03 => {
                        const caps = try self.allocator.dupe(u32, ro.arr);
                        blocks_mut[i][j] = .{ .make_closure = .{
                            .dst = ro.u1,
                            .body = blocks_mut[ro.u2],
                            .captures = caps,
                            .arity = ro.u3,
                        } };
                    },
                    0x04 => {
                        const args = try self.allocator.dupe(u32, ro.arr);
                        blocks_mut[i][j] = .{ .call = .{
                            .dst = ro.u1,
                            .func = ro.u2,
                            .args = args,
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
                    0x0C => blocks_mut[i][j] = .{ .add = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x0D => blocks_mut[i][j] = .{ .sub = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x0E => blocks_mut[i][j] = .{ .mul = .{ .dst = ro.u1, .lhs = ro.u2, .rhs = ro.u3 } },
                    0x0F => blocks_mut[i][j] = .{ .borrow = .{ .dst = ro.u1, .src = ro.u2 } },
                    0x10 => blocks_mut[i][j] = .{ .free = .{ .reg = ro.u1 } },
                    else => unreachable,
                }
            }
        }

        const blocks = try self.allocator.alloc([]const Op, block_count);
        for (0..block_count) |i| {
            blocks[i] = blocks_mut[i];
        }
        self.allocator.free(blocks_mut);

        return LoadedProgram{
            .allocator = self.allocator,
            .constants = constants,
            .blocks = blocks,
        };
    }
};

test "BytecodeLoader: Roundtrip test" {
    const allocator = std.testing.allocator;
    const path = "test_roundtrip.rbc";

    // 1. Manually create an RBC file
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

    // 2. Load it
    var bl = BytecodeLoader.init(allocator);
    var prog = try bl.loadFromFile(path);
    defer prog.deinit();

    try std.testing.expectEqual(@as(usize, 1), prog.constants.len);
    try std.testing.expectEqual(@as(u64, 42), prog.constants[0].int);
    try std.testing.expectEqual(@as(usize, 1), prog.blocks.len);
    try std.testing.expectEqual(@as(usize, 2), prog.blocks[0].len);

    // 3. Run it in VM
    var vm_inst = vm.VM.init(allocator);
    const result = try vm_inst.run(prog.mainCode());
    defer result.deinit(allocator);
    try std.testing.expectEqual(@as(u64, 42), result.int);
}
