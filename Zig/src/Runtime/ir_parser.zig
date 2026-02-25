const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Param = struct {
    type_name: []const u8,
    name: []const u8,
};

pub const Instruction = struct {
    result: ?[]const u8,        // "%x" など（void 命令は null）
    opcode: []const u8,         // "add", "call", "br", "phi" 等
    operands: [][]const u8,     // トークン列（型・値・ラベル）
    raw: []const u8,            // 元のテキスト行（デバッグ用）

    pub fn deinit(self: Instruction, allocator: Allocator) void {
        if (self.result) |r| allocator.free(r);
        allocator.free(self.opcode);
        for (self.operands) |op| {
            allocator.free(op);
        }
        allocator.free(self.operands);
        allocator.free(self.raw);
    }
};

pub const ParsedBlock = struct {
    label: []const u8,          // "entry", "loop", "exit" 等
    instructions: []Instruction,

    pub fn deinit(self: ParsedBlock, allocator: Allocator) void {
        allocator.free(self.label);
        for (self.instructions) |insn| {
            insn.deinit(allocator);
        }
        allocator.free(self.instructions);
    }
};

pub const ParsedFunction = struct {
    name: []const u8,           // "factorial", "__block_1" 等
    params: []Param,            // 引数 (型・レジスタ名)
    ret_type: []const u8,       // "i64", "void", "double" 等
    blocks: []ParsedBlock,      // 基本ブロックの配列
    is_declare: bool,           // declare（外部宣言）か define か

    pub fn deinit(self: ParsedFunction, allocator: Allocator) void {
        allocator.free(self.name);
        for (self.params) |p| {
            allocator.free(p.type_name);
            allocator.free(p.name);
        }
        allocator.free(self.params);
        allocator.free(self.ret_type);
        for (self.blocks) |b| {
            b.deinit(allocator);
        }
        allocator.free(self.blocks);
    }
};

pub const ParsedModule = struct {
    functions: []ParsedFunction,

    pub fn deinit(self: *ParsedModule, allocator: Allocator) void {
        for (self.functions) |f| {
            f.deinit(allocator);
        }
        allocator.free(self.functions);
    }
};

pub const IRParser = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) IRParser {
        return .{ .allocator = allocator };
    }

    pub fn parse(self: *IRParser, ir_text: []const u8) !ParsedModule {
        const a = self.allocator;
        var functions = std.ArrayList(ParsedFunction){};
        errdefer {
            for (functions.items) |f| f.deinit(a);
            functions.deinit(a);
        }

        var line_it = std.mem.splitScalar(u8, ir_text, '\n');
        while (line_it.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r\n");
            if (trimmed.len == 0 or trimmed[0] == ';') continue;

            if (std.mem.startsWith(u8, trimmed, "define ") or std.mem.startsWith(u8, trimmed, "declare ")) {
                try functions.append(a, try self.parseFunction(&line_it, trimmed));
            }
        }

        return ParsedModule{ .functions = try functions.toOwnedSlice(a) };
    }

    fn parseFunction(self: *IRParser, line_it: *std.mem.SplitIterator(u8, .scalar), header: []const u8) !ParsedFunction {
        const a = self.allocator;
        const is_declare = std.mem.startsWith(u8, header, "declare ");
        
        // 簡単なトークナイズで header を分解
        // "define [attributes] <ret_type> @<name>(<params>) [attributes] {"
        var tokens = std.ArrayList([]const u8){};
        defer {
            for (tokens.items) |t| a.free(t);
            tokens.deinit(a);
        }

        var it = std.mem.tokenizeAny(u8, header, " \t,(){}");
        while (it.next()) |t| {
            try tokens.append(a, try a.dupe(u8, t));
        }

        if (tokens.items.len < 3) return error.InvalidIR;

        // name は @ で始まるトークンを探す
        var name: []const u8 = "";
        var ret_type: []const u8 = "";
        var name_idx: usize = 0;

        for (tokens.items, 0..) |t, i| {
            if (std.mem.startsWith(u8, t, "@")) {
                name = try a.dupe(u8, t[1..]);
                name_idx = i;
                // その前が ret_type (attributes があるかもしれないが一旦簡易的に)
                if (i > 0) {
                    ret_type = try a.dupe(u8, tokens.items[i-1]);
                }
                break;
            }
        }

        var params = std.ArrayList(Param){};
        errdefer {
            for (params.items) |p| {
                a.free(p.type_name);
                a.free(p.name);
            }
            params.deinit(a);
        }

        // 引数パース (name_idx 以降)
        var i = name_idx + 1;
        while (i + 1 < tokens.items.len) : (i += 2) {
            const t_name = tokens.items[i];
            const p_name = tokens.items[i+1];
            if (std.mem.startsWith(u8, p_name, "%")) {
                try params.append(a, .{
                    .type_name = try a.dupe(u8, t_name),
                    .name = try a.dupe(u8, p_name),
                });
            } else {
                // 引数名がない場合（declare 等）
                break;
            }
        }

        var blocks = std.ArrayList(ParsedBlock){};
        errdefer {
            for (blocks.items) |b| b.deinit(a);
            blocks.deinit(a);
        }

        if (!is_declare) {
            var current_block_label: ?[]const u8 = null;
            var current_instructions = std.ArrayList(Instruction){};
            errdefer {
                for (current_instructions.items) |insn| insn.deinit(a);
                current_instructions.deinit(a);
            }

            while (line_it.next()) |line| {
                const trimmed = std.mem.trim(u8, line, " \t\r\n");
                if (trimmed.len == 0) continue;
                if (trimmed[0] == ';') continue;
                if (std.mem.eql(u8, trimmed, "}")) break;

                if (std.mem.endsWith(u8, trimmed, ":")) {
                    // ラベル
                    if (current_block_label != null or current_instructions.items.len > 0) {
                        try blocks.append(a, .{
                            .label = current_block_label orelse try a.dupe(u8, "entry"),
                            .instructions = try current_instructions.toOwnedSlice(a),
                        });
                        current_block_label = null;
                        current_instructions = std.ArrayList(Instruction){};
                    }
                    current_block_label = try a.dupe(u8, trimmed[0 .. trimmed.len - 1]);
                } else {
                    try current_instructions.append(a, try self.parseInstruction(trimmed));
                }
            }

            if (current_block_label != null or current_instructions.items.len > 0) {
                try blocks.append(a, .{
                    .label = current_block_label orelse try a.dupe(u8, "entry"),
                    .instructions = try current_instructions.toOwnedSlice(a),
                });
            }
        }

        return ParsedFunction{
            .name = name,
            .params = try params.toOwnedSlice(a),
            .ret_type = ret_type,
            .blocks = try blocks.toOwnedSlice(a),
            .is_declare = is_declare,
        };
    }

    fn parseInstruction(self: *IRParser, line: []const u8) !Instruction {
        const a = self.allocator;
        var result: ?[]const u8 = null;
        var opcode: []const u8 = "";
        var operands = std.ArrayList([]const u8){};
        errdefer {
            if (result) |r| a.free(r);
            a.free(opcode);
            for (operands.items) |o| a.free(o);
            operands.deinit(a);
        }

        var it = std.mem.tokenizeAny(u8, line, " \t,");
        const first = it.next() orelse return error.InvalidIR;

        if (std.mem.startsWith(u8, first, "%")) {
            // "%x = opcode ..."
            result = try a.dupe(u8, first);
            const eq = it.next() orelse return error.InvalidIR;
            if (!std.mem.eql(u8, eq, "=")) return error.InvalidIR;
            opcode = try a.dupe(u8, it.next() orelse return error.InvalidIR);
        } else {
            // "opcode ..."
            opcode = try a.dupe(u8, first);
        }

        while (it.next()) |t| {
            // ( ) [ ] { } 等をオペランドに含めるか、トークンとして分けるか
            // 一旦単純に空白とコンマで分ける
            try operands.append(a, try a.dupe(u8, t));
        }

        return Instruction{
            .result = result,
            .opcode = opcode,
            .operands = try operands.toOwnedSlice(a),
            .raw = try a.dupe(u8, line),
        };
    }
};

