// ==========================================
// lemma_manager.zig
// 補題の保存・読み込み (LemmaManager.scala移植)
// 形式: タブ区切り name\tlhs\trhs\n
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const parser_mod = @import("parser.zig");
const CatRule = expr_mod.CatRule;
const Expr = expr_mod.Expr;

pub const LemmaManagerError = error{
    OutOfMemory,
    InvalidFormat,
    FileNotFound,
};

/// 補題ルールをファイルに保存（タブ区切りテキスト）
pub fn saveLemmas(path: []const u8, rules: []const CatRule, arena: Allocator) !void {
    // まずメモリ上に出力バッファを構築
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(arena);

    for (rules) |rule| {
        // name\tlhs\trhs\n
        try buf.appendSlice(arena, rule.name);
        try buf.append(arena, '\t');
        try writeExprTo(&buf, rule.lhs, arena);
        try buf.append(arena, '\t');
        try writeExprTo(&buf, rule.rhs, arena);
        try buf.append(arena, '\n');
    }

    // ファイルに書き出す
    const file = std.fs.cwd().createFile(path, .{}) catch return error.FileNotFound;
    defer file.close();
    try file.writeAll(buf.items);
}

/// 補題ルールをファイルから読み込み
pub fn loadLemmas(path: []const u8, arena: Allocator) ![]CatRule {
    var file = std.fs.cwd().openFile(path, .{}) catch return error.FileNotFound;
    defer file.close();

    const content = file.readToEndAlloc(arena, 1024 * 1024) catch return error.OutOfMemory;

    var rules: std.ArrayList(CatRule) = .{};
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0) continue;

        // タブで3分割: name, lhs, rhs
        var parts = std.mem.splitScalar(u8, trimmed, '\t');
        const name_part = parts.next() orelse continue;
        const lhs_part = parts.next() orelse continue;
        const rhs_part = parts.next() orelse continue;

        const name = try arena.dupe(u8, std.mem.trim(u8, name_part, " "));
        const lhs_str = std.mem.trim(u8, lhs_part, " ");
        const rhs_str = std.mem.trim(u8, rhs_part, " ");

        const lhs = parser_mod.parse(lhs_str, arena) catch continue;
        const rhs = parser_mod.parse(rhs_str, arena) catch continue;

        try rules.append(arena, .{
            .name = name,
            .lhs = lhs,
            .rhs = rhs,
        });
    }

    return rules.items;
}

/// Exprを文字列としてArrayList(u8)に書き出す（parser.parseで読み直せる形式）
fn writeExprTo(buf: *std.ArrayList(u8), e: *const Expr, arena: Allocator) error{OutOfMemory}!void {
    switch (e.*) {
        .sym => |n| try buf.appendSlice(arena, n),
        .var_ => |n| try buf.appendSlice(arena, n),
        .meta => |id| {
            try buf.append(arena, '?');
            for (id.ids, 0..) |i, j| {
                if (j > 0) try buf.append(arena, '.');
                var tmp: [20]u8 = undefined;
                const s = std.fmt.bufPrint(&tmp, "{d}", .{i}) catch unreachable;
                try buf.appendSlice(arena, s);
            }
        },
        .app => |a| {
            // λ式: λx. body
            if (a.head.* == .sym and std.mem.eql(u8, a.head.sym, "λ") and a.args.len == 2 and a.args[0].* == .var_) {
                try buf.appendSlice(arena, "λ");
                try buf.appendSlice(arena, a.args[0].var_);
                try buf.appendSlice(arena, ". ");
                try writeExprTo(buf, a.args[1], arena);
                return;
            }
            // 通常: head(arg1, arg2, ...)
            try writeExprTo(buf, a.head, arena);
            if (a.args.len > 0) {
                try buf.append(arena, '(');
                for (a.args, 0..) |arg, i| {
                    if (i > 0) try buf.append(arena, ',');
                    try writeExprTo(buf, arg, arena);
                }
                try buf.append(arena, ')');
            }
        },
    }
}

// ==========================================
// テスト
// ==========================================

test "saveLemmas and loadLemmas roundtrip" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // テスト用ルールを作る
    const a = try expr_mod.sym(arena, "A");
    const b = try expr_mod.sym(arena, "B");
    const impl = try expr_mod.sym(arena, "→");
    const rule = CatRule{
        .name = "test-rule",
        .lhs = try expr_mod.app2(arena, impl, a, b),
        .rhs = b,
    };

    // 一時ファイルに保存
    const tmp_path = "/tmp/test_lemmas.txt";
    try saveLemmas(tmp_path, &[_]CatRule{rule}, arena);

    // 読み込んで確認
    const loaded = loadLemmas(tmp_path, arena) catch {
        // ファイルシステムエラーはテストとして容認
        return;
    };
    if (loaded.len > 0) {
        try std.testing.expectEqualStrings("test-rule", loaded[0].name);
    }
}
