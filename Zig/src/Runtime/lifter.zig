const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;
const loader = @import("loader.zig");
const ir_parser = @import("ir_parser.zig");

pub const LiftError = error{
    OutOfMemory,
    ParseError,
    UnsupportedInstruction,
    UnsupportedType,
    NoFunctions,
    InvalidCFG,
    VarNotLive,
};

pub const Lifter = struct {
    allocator: Allocator,

    const CFGNode = struct {
        block_idx: usize,
        successors: std.ArrayList(usize),
        predecessors: std.ArrayList(usize),
    };

    const FunctionCFG = struct {
        nodes: []CFGNode,
        allocator: Allocator,

        pub fn deinit(self: *FunctionCFG) void {
            for (self.nodes) |*node| {
                node.successors.deinit(self.allocator);
                node.predecessors.deinit(self.allocator);
            }
            self.allocator.free(self.nodes);
        }
    };

    pub fn init(allocator: Allocator) Lifter {
        return .{ .allocator = allocator };
    }

    fn buildCFG(self: *Lifter, func: ir_parser.ParsedFunction) !FunctionCFG {
        const a = self.allocator;
        var nodes = try a.alloc(CFGNode, func.blocks.len);
        for (nodes, 0..) |*node, i| {
            node.* = .{
                .block_idx = i,
                .successors = std.ArrayList(usize){},
                .predecessors = std.ArrayList(usize){},
            };
        }

        var label_to_idx = std.StringHashMap(usize).init(a);
        defer label_to_idx.deinit();
        for (func.blocks, 0..) |block, i| {
            try label_to_idx.put(block.label, i);
        }

        for (func.blocks, 0..) |block, i| {
            if (block.instructions.len == 0) continue;
            const last_insn = block.instructions[block.instructions.len - 1];
            
            if (std.mem.eql(u8, last_insn.opcode, "br")) {
                if (last_insn.operands.len == 2) {
                    const label = last_insn.operands[1];
                    const target_label = if (std.mem.startsWith(u8, label, "%")) label[1..] else label;
                    if (label_to_idx.get(target_label)) |target_idx| {
                        try nodes[i].successors.append(a, target_idx);
                        try nodes[target_idx].predecessors.append(a, i);
                    }
                } else if (last_insn.operands.len >= 6) {
                    const t_label = last_insn.operands[3];
                    const f_label = last_insn.operands[5];
                    const t_target = if (std.mem.startsWith(u8, t_label, "%")) t_label[1..] else t_label;
                    const f_target = if (std.mem.startsWith(u8, f_label, "%")) f_label[1..] else f_label;
                    if (label_to_idx.get(t_target)) |t_idx| {
                        try nodes[i].successors.append(a, t_idx);
                        try nodes[t_idx].predecessors.append(a, i);
                    }
                    if (label_to_idx.get(f_target)) |f_idx| {
                        try nodes[i].successors.append(a, f_idx);
                        try nodes[f_idx].predecessors.append(a, i);
                    }
                }
            } else if (std.mem.eql(u8, last_insn.opcode, "ret")) {
                // Terminator
            } else if (i + 1 < func.blocks.len) {
                try nodes[i].successors.append(a, i + 1);
                try nodes[i + 1].predecessors.append(a, i);
            }
        }

        return FunctionCFG{ .nodes = nodes, .allocator = a };
    }

    const LiveSet = std.AutoHashMap(u32, void);

    pub fn lift(self: *Lifter, ir_text: []const u8) LiftError!loader.LoadedProgram {
        var parser = ir_parser.IRParser.init(self.allocator);
        var mod = parser.parse(ir_text) catch return error.ParseError;
        defer mod.deinit(self.allocator);

        if (mod.functions.len == 0) return error.NoFunctions;

        const arena = try self.allocator.create(std.heap.ArenaAllocator);
        arena.* = std.heap.ArenaAllocator.init(self.allocator);
        errdefer { arena.deinit(); self.allocator.destroy(arena); }
        const aa = arena.allocator();

        var blocks = std.ArrayList([]const Op){};
        defer blocks.deinit(self.allocator);

        var current_block_idx: usize = 0;
        for (mod.functions) |func| {
            if (func.is_declare) continue;
            const func_blocks = try self.liftFunction(func, current_block_idx, aa);
            try blocks.appendSlice(self.allocator, func_blocks);
            current_block_idx += func_blocks.len;
        }

        const program = loader.LoadedProgram{
            .allocator = self.allocator,
            .arena = arena,
            .constants = try self.allocator.alloc(Value, 0),
            .blocks = try blocks.toOwnedSlice(self.allocator),
        };

        // Link placeholders
        for (program.blocks) |block| {
            for (@constCast(block)) |*op| {
                self.linkOp(op, @constCast(program.blocks));
            }
        }

        return program;
    }

    fn linkOp(self: *Lifter, op: *Op, all_blocks: [][]const Op) void {
        switch (op.*) {
            .make_closure => |*mc| {
                if (mc.block_idx < all_blocks.len) {
                    mc.body = all_blocks[mc.block_idx];
                }
            },
            .case_op => |*c| {
                for (@constCast(c.inl_branch)) |*inner| self.linkOp(inner, all_blocks);
                for (@constCast(c.inr_branch)) |*inner| self.linkOp(inner, all_blocks);
            },
            else => {},
        }
    }

    fn liftFunction(self: *Lifter, func: ir_parser.ParsedFunction, global_offset: usize, aa: Allocator) ![][]const Op {
        const a = self.allocator;
        var reg_map = std.StringHashMap(u32).init(a);
        defer reg_map.deinit();
        var next_reg: u32 = 0;

        for (func.params) |param| {
            if (reg_map.get(param.name)) |_| {} else {
                try reg_map.put(param.name, next_reg);
                next_reg += 1;
            }
        }
        for (func.blocks) |block| {
            for (block.instructions) |insn| {
                if (insn.result) |res| {
                    if (!reg_map.contains(res)) {
                        try reg_map.put(res, next_reg);
                        next_reg += 1;
                    }
                }
            }
        }

        const cfg = try self.buildCFG(func);
        var mut_cfg = cfg;
        defer mut_cfg.deinit();

        const live_ins = try self.computeLivenessCorrect(func, &reg_map, cfg);
        defer {
            for (live_ins) |*s| s.deinit();
            self.allocator.free(live_ins);
        }

        var blocks = try aa.alloc([]const Op, func.blocks.len);
        for (func.blocks, 0..) |block, i| {
            blocks[i] = try self.liftBlock(block, &reg_map, live_ins, global_offset, i, func, aa);
        }
        return blocks;
    }

    fn liftBlock(self: *Lifter, block: ir_parser.ParsedBlock, reg_map: *std.StringHashMap(u32), live_ins: []const LiveSet, global_offset: usize, local_idx: usize, func: ir_parser.ParsedFunction, aa: Allocator) ![]const Op {
        var ops = std.ArrayList(Op){};
        
        var captures = std.ArrayList(u32){};
        defer captures.deinit(self.allocator);
        var it = live_ins[local_idx].keyIterator();
        while (it.next()) |reg| try captures.append(self.allocator, reg.*);
        std.mem.sort(u32, captures.items, {}, std.sort.asc(u32));

        var local_map = std.AutoHashMap(u32, u32).init(self.allocator);
        defer local_map.deinit();
        var local_next: u32 = 0;

        if (local_idx == 0) {
            for (func.params, 0..) |param, i| {
                if (reg_map.get(param.name)) |global| {
                    try local_map.put(global, @intCast(i));
                    local_next = @max(local_next, @as(u32, @intCast(i)) + 1);
                }
            }
        }

        for (captures.items) |global_reg| {
            if (!local_map.contains(global_reg)) {
                try local_map.put(global_reg, local_next);
                local_next += 1;
            }
        }
        for (block.instructions) |insn| {
            if (std.mem.eql(u8, insn.opcode, "phi")) {
                const global_reg = reg_map.get(insn.result.?).?;
                try local_map.put(global_reg, local_next);
                local_next += 1;
            }
        }

        for (block.instructions) |insn| {
            try self.liftInstructionLocal(insn, reg_map, &local_map, &local_next, &ops, live_ins, global_offset, local_idx, func, aa);
        }
        return ops.toOwnedSlice(aa);
    }

    fn parseIntWidth(type_str: []const u8) u16 {
        if (type_str.len < 2 or type_str[0] != 'i') return 64;
        return std.fmt.parseInt(u16, type_str[1..], 10) catch 64;
    }

    fn liftInstructionLocal(self: *Lifter, insn: ir_parser.Instruction, global_reg_map: *std.StringHashMap(u32), local_map: *std.AutoHashMap(u32, u32), local_next: *u32, ops: *std.ArrayList(Op), live_ins: []const LiveSet, global_offset: usize, local_idx: usize, func: ir_parser.ParsedFunction, aa: Allocator) !void {
        const a = aa;
        if (std.mem.eql(u8, insn.opcode, "add") or std.mem.eql(u8, insn.opcode, "sub") or std.mem.eql(u8, insn.opcode, "mul") or
            std.mem.eql(u8, insn.opcode, "sdiv") or std.mem.eql(u8, insn.opcode, "udiv") or std.mem.eql(u8, insn.opcode, "srem") or std.mem.eql(u8, insn.opcode, "urem") or
            std.mem.eql(u8, insn.opcode, "and") or std.mem.eql(u8, insn.opcode, "or") or std.mem.eql(u8, insn.opcode, "xor") or
            std.mem.eql(u8, insn.opcode, "shl") or std.mem.eql(u8, insn.opcode, "lshr") or std.mem.eql(u8, insn.opcode, "ashr")) 
        {
            const op: vm.IBinOp = if (std.mem.eql(u8, insn.opcode, "add")) .add else if (std.mem.eql(u8, insn.opcode, "sub")) .sub else if (std.mem.eql(u8, insn.opcode, "mul")) .mul
                else if (std.mem.eql(u8, insn.opcode, "sdiv")) .sdiv else if (std.mem.eql(u8, insn.opcode, "udiv")) .udiv
                else if (std.mem.eql(u8, insn.opcode, "srem")) .srem else if (std.mem.eql(u8, insn.opcode, "urem")) .urem
                else if (std.mem.eql(u8, insn.opcode, "and")) .and_ else if (std.mem.eql(u8, insn.opcode, "or")) .or_ else if (std.mem.eql(u8, insn.opcode, "xor")) .xor_
                else if (std.mem.eql(u8, insn.opcode, "shl")) .shl else if (std.mem.eql(u8, insn.opcode, "lshr")) .lshr else .ashr;
            const dst = try self.allocLocal(insn.result.?, global_reg_map, local_map, local_next);
            const lhs = try self.useLocal(insn.operands[1], global_reg_map, local_map, local_next, ops, a);
            const rhs = try self.useLocal(insn.operands[2], global_reg_map, local_map, local_next, ops, a);
            const w = parseIntWidth(insn.operands[0]);
            try ops.append(a, .{ .ibin = .{ .dst = dst, .lhs = lhs, .rhs = rhs, .op = op, .width = w } });
        } else if (std.mem.eql(u8, insn.opcode, "ret")) {
            if (insn.operands.len >= 2) {
                const src = try self.useLocal(insn.operands[1], global_reg_map, local_map, local_next, ops, a);
                try ops.append(a, .{ .ret = .{ .src = src } });
            } else {
                const dst = local_next.*; local_next.* += 1;
                try ops.append(a, .{ .load_const = .{ .dst = dst, .val = .unit } });
                try ops.append(a, .{ .ret = .{ .src = dst } });
            }
        } else if (std.mem.eql(u8, insn.opcode, "br")) {
            if (insn.operands.len == 2) {
                const label = insn.operands[1];
                const dest_name = if (std.mem.startsWith(u8, label, "%")) label[1..] else label;
                for (func.blocks, 0..) |b, i| {
                    if (std.mem.eql(u8, b.label, dest_name)) {
                        try self.emitJumpLocal(i, global_offset, live_ins, global_reg_map, local_map, local_next, ops, func, local_idx, a);
                        break;
                    }
                }
            } else if (insn.operands.len >= 6) {
                const cond = try self.useLocal(insn.operands[1], global_reg_map, local_map, local_next, ops, a);
                const t_label = insn.operands[3];
                const f_label = insn.operands[5];
                var t_idx: usize = 0; var f_idx: usize = 0;
                for (func.blocks, 0..) |b, i| {
                    const l = b.label;
                    if (std.mem.eql(u8, l, if (std.mem.startsWith(u8, t_label, "%")) t_label[1..] else t_label)) t_idx = i;
                    if (std.mem.eql(u8, l, if (std.mem.startsWith(u8, f_label, "%")) f_label[1..] else f_label)) f_idx = i;
                }
                var t_ops = std.ArrayList(Op){};
                try self.emitJumpLocal(t_idx, global_offset, live_ins, global_reg_map, local_map, local_next, &t_ops, func, local_idx, a);
                var f_ops = std.ArrayList(Op){};
                try self.emitJumpLocal(f_idx, global_offset, live_ins, global_reg_map, local_map, local_next, &f_ops, func, local_idx, a);
                const dst = local_next.*; local_next.* += 1;
                try ops.append(a, .{ .case_op = .{ .dst = dst, .scrutinee = cond, .inl_branch = try t_ops.toOwnedSlice(a), .inr_branch = try f_ops.toOwnedSlice(a) } });
                try ops.append(a, .{ .ret = .{ .src = dst } });
            }
        } else if (std.mem.eql(u8, insn.opcode, "icmp")) {
            const pred_str = insn.operands[0];
            const lhs = try self.useLocal(insn.operands[2], global_reg_map, local_map, local_next, ops, a);
            const rhs = try self.useLocal(insn.operands[3], global_reg_map, local_map, local_next, ops, a);
            const dst = try self.allocLocal(insn.result.?, global_reg_map, local_map, local_next);
            const pred: vm.ICmpPred = if (std.mem.eql(u8, pred_str, "eq")) .eq else if (std.mem.eql(u8, pred_str, "ne")) .ne
                else if (std.mem.eql(u8, pred_str, "slt")) .slt else if (std.mem.eql(u8, pred_str, "sle")) .sle
                else if (std.mem.eql(u8, pred_str, "sgt")) .sgt else if (std.mem.eql(u8, pred_str, "sge")) .sge
                else if (std.mem.eql(u8, pred_str, "ult")) .ult else if (std.mem.eql(u8, pred_str, "ule")) .ule
                else if (std.mem.eql(u8, pred_str, "ugt")) .ugt else .uge;
            const w = parseIntWidth(insn.operands[1]);
            try ops.append(a, .{ .icmp = .{ .dst = dst, .lhs = lhs, .rhs = rhs, .pred = pred, .width = w } });
        }
    }

    fn allocLocal(self: *Lifter, name: []const u8, global_reg_map: *std.StringHashMap(u32), local_map: *std.AutoHashMap(u32, u32), local_next: *u32) !u32 {
        _ = self;
        const global = global_reg_map.get(name).?;
        const local = local_next.*;
        local_next.* += 1;
        try local_map.put(global, local);
        return local;
    }

    fn useLocal(self: *Lifter, op: []const u8, global_reg_map: *std.StringHashMap(u32), local_map: *std.AutoHashMap(u32, u32), local_next: *u32, ops: *std.ArrayList(Op), a: Allocator) !u32 {
        _ = self;
        if (std.mem.startsWith(u8, op, "%")) {
            const global = global_reg_map.get(op) orelse return error.UnsupportedInstruction;
            const local = local_map.get(global) orelse return error.VarNotLive;
            const r = local_next.*;
            local_next.* += 1;
            try ops.append(a, .{ .borrow = .{ .dst = r, .src = local } });
            return r;
        }
        const val = std.fmt.parseInt(u64, op, 10) catch 0;
        const r = local_next.*;
        local_next.* += 1;
        try ops.append(a, .{ .load_const = .{ .dst = r, .val = .{ .bits = val } } });
        return r;
    }

    fn emitJumpLocal(self: *Lifter, dest_idx: usize, global_offset: usize, live_ins: []const LiveSet, global_reg_map: *std.StringHashMap(u32), local_map: *std.AutoHashMap(u32, u32), local_next: *u32, ops: *std.ArrayList(Op), func: ir_parser.ParsedFunction, local_idx: usize, a: Allocator) !void {
        var captures = std.ArrayList(u32){};
        defer captures.deinit(self.allocator);
        var it = live_ins[dest_idx].keyIterator();
        while (it.next()) |reg| try captures.append(self.allocator, reg.*);
        std.mem.sort(u32, captures.items, {}, std.sort.asc(u32));

        var local_captures = std.ArrayList(u32){};
        // No defer deinit for local_captures since it will be owned by aa
        for (captures.items) |g| {
            if (local_map.get(g)) |l| {
                const b = local_next.*;
                local_next.* += 1;
                try ops.append(a, .{ .borrow = .{ .dst = b, .src = l } });
                try local_captures.append(a, b);
            } else return error.VarNotLive;
        }

        var args = std.ArrayList(u32){};
        // No defer deinit for args since it will be owned by aa
        for (func.blocks[dest_idx].instructions) |insn| {
            if (!std.mem.eql(u8, insn.opcode, "phi")) break;
            var k: usize = 1;
            while (k + 2 < insn.operands.len) {
                if (std.mem.eql(u8, insn.operands[k], "[")) {
                    const val = insn.operands[k + 1];
                    const label = insn.operands[k + 2];
                    const label_name = if (std.mem.startsWith(u8, label, "%")) label[1..] else label;
                    if (std.mem.eql(u8, label_name, func.blocks[local_idx].label)) {
                        const arg_reg = try self.useLocal(val, global_reg_map, local_map, local_next, ops, a);
                        try args.append(a, arg_reg);
                        break;
                    }
                    k += 4;
                } else k += 1;
            }
        }
        const closure_reg = local_next.*;
        local_next.* += 1;
        try ops.append(a, .{ .make_closure = .{ .dst = closure_reg, .body = &[_]Op{}, .block_idx = global_offset + dest_idx, .captures = try local_captures.toOwnedSlice(a), .arity = args.items.len } });
        const res_reg = local_next.*;
        local_next.* += 1;
        try ops.append(a, .{ .call = .{ .dst = res_reg, .func = closure_reg, .args = try args.toOwnedSlice(a) } });
        try ops.append(a, .{ .ret = .{ .src = res_reg } });
    }

    fn computeLivenessCorrect(self: *Lifter, func: ir_parser.ParsedFunction, reg_map: *std.StringHashMap(u32), cfg: FunctionCFG) ![]LiveSet {
        const a = self.allocator;
        const n = func.blocks.len;
        var live_in = try a.alloc(LiveSet, n);
        var defs = try a.alloc(LiveSet, n);
        var uses = try a.alloc(LiveSet, n);
        for (0..n) |i| {
            live_in[i] = LiveSet.init(a);
            defs[i] = LiveSet.init(a);
            uses[i] = LiveSet.init(a);
            for (func.blocks[i].instructions) |insn| {
                const is_phi = std.mem.eql(u8, insn.opcode, "phi");
                if (!is_phi) {
                    for (insn.operands) |op| {
                        if (std.mem.startsWith(u8, op, "%")) {
                            if (reg_map.get(op)) |r| if (!defs[i].contains(r)) try uses[i].put(r, {});
                        }
                    }
                }
                if (insn.result) |res| if (reg_map.get(res)) |r| try defs[i].put(r, {});
            }
        }
        defer {
            for (defs) |*s| s.deinit();
            for (uses) |*s| s.deinit();
            a.free(defs); a.free(uses);
        }
        var changed = true;
        while (changed) {
            changed = false;
            for (0..n) |i| {
                var new_live = try uses[i].clone();
                for (cfg.nodes[i].successors.items) |s| {
                    for (func.blocks[s].instructions) |insn| {
                        if (!std.mem.eql(u8, insn.opcode, "phi")) break;
                        var k: usize = 1;
                        while (k + 2 < insn.operands.len) {
                            if (std.mem.eql(u8, insn.operands[k], "[")) {
                                const val = insn.operands[k + 1];
                                const label = insn.operands[k + 2];
                                const label_name = if (std.mem.startsWith(u8, label, "%")) label[1..] else label;
                                if (std.mem.eql(u8, label_name, func.blocks[i].label)) {
                                    if (std.mem.startsWith(u8, val, "%")) {
                                        if (reg_map.get(val)) |r| if (!defs[i].contains(r)) try new_live.put(r, {});
                                    }
                                }
                                k += 4;
                            } else k += 1;
                        }
                    }
                    var it_live = live_in[s].keyIterator();
                    while (it_live.next()) |k| {
                        if (!defs[i].contains(k.*)) {
                            var is_phi_def = false;
                            for (func.blocks[s].instructions) |insn| {
                                if (!std.mem.eql(u8, insn.opcode, "phi")) break;
                                if (insn.result) |res| if (reg_map.get(res)) |r| if (r == k.*) { is_phi_def = true; break; };
                            }
                            if (!is_phi_def) try new_live.put(k.*, {});
                        }
                    }
                }
                if (new_live.count() != live_in[i].count()) {
                    changed = true; live_in[i].deinit(); live_in[i] = new_live;
                } else {
                    var it_eq = new_live.keyIterator(); var equal = true;
                    while (it_eq.next()) |k| if (!live_in[i].contains(k.*)) { equal = false; break; };
                    if (!equal) { changed = true; live_in[i].deinit(); live_in[i] = new_live; } else new_live.deinit();
                }
            }
        }
        return live_in;
    }

    fn isDominatedBy(self: *Lifter, doms: []usize, node: usize, potential_dom: usize) bool {
        _ = self;
        var current = node;
        while (current != std.math.maxInt(usize)) {
            if (current == potential_dom) return true;
            if (current == doms[current]) break;
            current = doms[current];
        }
        return false;
    }
};

