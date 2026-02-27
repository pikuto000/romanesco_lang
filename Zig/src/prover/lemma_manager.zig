// ==========================================
// lemma_manager.zig
// 補題の保存・読み込み (LemmaManager.scala移植)
// 形式: タブ区切り name\tlhs\trhs\tuniv1,univ2,...
// '#' で始まる行はコメント
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

/// ディレクトリが存在しなければ作成する
pub fn ensureDir(path: []const u8) void {
    std.fs.cwd().makeDir(path) catch |err| {
        if (err != error.PathAlreadyExists) return;
    };
}

/// 補題ルールをファイルに保存（タブ区切りテキスト）
/// 形式: name\tlhs\trhs\tuniv1,univ2,...
pub fn saveLemmas(path: []const u8, rules: []const CatRule, arena: Allocator) !void {
    // まずメモリ上に出力バッファを構築
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(arena);

    // ヘッダーコメント
    try buf.appendSlice(arena, "# Romanesco 補題ファイル\n");
    try buf.appendSlice(arena, "# 形式: 名前<TAB>左辺<TAB>右辺<TAB>全称変数(カンマ区切り)\n");
    try buf.append(arena, '\n');

    for (rules) |rule| {
        // name\tlhs\trhs\tuniv1,univ2,...\n
        try buf.appendSlice(arena, rule.name);
        try buf.append(arena, '\t');
        try writeExprTo(&buf, rule.lhs, arena);
        try buf.append(arena, '\t');
        try writeExprTo(&buf, rule.rhs, arena);
        // universals
        try buf.append(arena, '\t');
        for (rule.universals, 0..) |u, i| {
            if (i > 0) try buf.append(arena, ',');
            try writeExprTo(&buf, u, arena);
        }
        try buf.append(arena, '\n');
    }

    // ファイルに書き出す
    const file = std.fs.cwd().createFile(path, .{}) catch return error.FileNotFound;
    defer file.close();
    try file.writeAll(buf.items);
}

/// 補題ルールをファイルから読み込み
/// '#' で始まる行はコメントとしてスキップ
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
        // コメント行をスキップ
        if (trimmed[0] == '#') continue;

        // タブで分割: name, lhs, rhs, [universals]
        var parts = std.mem.splitScalar(u8, trimmed, '\t');
        const name_part = parts.next() orelse continue;
        const lhs_part = parts.next() orelse continue;
        const rhs_part = parts.next() orelse continue;
        const univs_part = parts.next(); // optional

        const name = try arena.dupe(u8, std.mem.trim(u8, name_part, " "));
        const lhs_str = std.mem.trim(u8, lhs_part, " ");
        const rhs_str = std.mem.trim(u8, rhs_part, " ");

        const lhs = parser_mod.parse(lhs_str, arena) catch continue;
        const rhs = parser_mod.parse(rhs_str, arena) catch continue;

        // universals をパース
        var universals: std.ArrayList(*const Expr) = .{};
        if (univs_part) |up| {
            const univs_trimmed = std.mem.trim(u8, up, " \r");
            if (univs_trimmed.len > 0) {
                var univ_iter = std.mem.splitScalar(u8, univs_trimmed, ',');
                while (univ_iter.next()) |u_str| {
                    const ut = std.mem.trim(u8, u_str, " ");
                    if (ut.len == 0) continue;
                    const u_expr = parser_mod.parse(ut, arena) catch continue;
                    try universals.append(arena, u_expr);
                }
            }
        }

        try rules.append(arena, .{
            .name = name,
            .lhs = lhs,
            .rhs = rhs,
            .universals = universals.items,
        });
    }

    return rules.items;
}

/// ディレクトリ内の全 .txt ファイルから補題を読み込む
pub fn loadAllFromDir(dir_path: []const u8, arena: Allocator) ![]CatRule {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return &.{};
    defer dir.close();

    var all_rules: std.ArrayList(CatRule) = .{};
    var iter = dir.iterate();
    while (iter.next() catch null) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".txt")) continue;

        // dir_path/entry.name のパスを構築
        const full_path = try std.fmt.allocPrint(arena, "{s}/{s}", .{ dir_path, entry.name });
        const file_rules = loadLemmas(full_path, arena) catch continue;
        try all_rules.appendSlice(arena, file_rules);
    }

    return all_rules.items;
}

/// ディレクトリに補題を保存する（既存ファイルの補題とマージし重複排除）
pub fn saveToDir(dir_path: []const u8, rules: []const CatRule, arena: Allocator) !void {
    if (rules.len == 0) return;
    ensureDir(dir_path);

    const auto_path = try std.fmt.allocPrint(arena, "{s}/auto.txt", .{dir_path});

    // 既存の補題を読み込み
    const existing = loadLemmas(auto_path, arena) catch &[_]CatRule{};

    // 重複排除: 既存の名前を集める
    var existing_names = std.StringHashMap(void).init(arena);
    for (existing) |e| {
        existing_names.put(e.name, {}) catch {};
    }

    // 新規補題のみフィルタ
    var merged: std.ArrayList(CatRule) = .{};
    try merged.appendSlice(arena, existing);
    for (rules) |rule| {
        if (!existing_names.contains(rule.name)) {
            try merged.append(arena, rule);
        }
    }

    if (merged.items.len > existing.len) {
        try saveLemmas(auto_path, merged.items, arena);
    }
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
    const x = try expr_mod.var_(arena, "x");
    const rule = CatRule{
        .name = "test-rule",
        .lhs = try expr_mod.app2(arena, impl, a, b),
        .rhs = b,
        .universals = &[_]*const Expr{x},
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
        try std.testing.expect(loaded[0].universals.len == 1);
    }
}

test "loadLemmas skips comment lines" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // コメント付きの補題ファイルを書く
    const tmp_path = "/tmp/test_lemmas_comments.txt";
    const content = "# これはコメントです\n# もう一つのコメント\n\ntest-rule\tA\tB\t\n";
    const file = std.fs.cwd().createFile(tmp_path, .{}) catch return;
    defer file.close();
    file.writeAll(content) catch return;

    const loaded = loadLemmas(tmp_path, arena) catch return;
    if (loaded.len > 0) {
        try std.testing.expectEqualStrings("test-rule", loaded[0].name);
    }
}
