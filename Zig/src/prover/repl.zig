// ==========================================
// repl.zig
// 対話的証明シェル (Repl.scala移植)
// undo/abort + タクティクス + 補題管理
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const tactics_mod = @import("tactics.zig");
const search_mod = @import("search.zig");
const plugin_mod = @import("plugin.zig");
const lemma_manager = @import("lemma_manager.zig");
const Goal = expr_mod.Goal;
const CatRule = expr_mod.CatRule;
const ProofState = expr_mod.ProofState;
const ProofTree = expr_mod.ProofTree;

pub const Repl = struct {
    arena: Allocator,
    history: std.ArrayList(ProofState),
    state: ?ProofState,
    plugins: []const search_mod.Plugin,
    /// ファイルから読み込んだ補題
    loaded_lemmas: std.ArrayList(CatRule),
    /// セッション中に生成された補題
    session_lemmas: std.ArrayList(CatRule),

    pub fn init(arena: Allocator) Repl {
        return .{
            .arena = arena,
            .history = .{},
            .state = null,
            .plugins = &plugin_mod.all_plugins,
            .loaded_lemmas = .{},
            .session_lemmas = .{},
        };
    }

    /// 読み込み済み + セッション補題を結合した規則リストを返す
    fn allLemmaRules(self: *Repl) []const CatRule {
        var combined: std.ArrayList(CatRule) = .{};
        combined.appendSlice(self.arena, self.loaded_lemmas.items) catch {};
        combined.appendSlice(self.arena, self.session_lemmas.items) catch {};
        return combined.items;
    }

    /// REPLのメインループ
    pub fn run(self: *Repl) !void {
        const stdin = std.io.getStdIn().reader();
        const stdout = std.io.getStdOut().writer();

        try stdout.writeAll("=== Romanesco Interactive Tactic Shell ===\n");
        try stdout.writeAll("Type 'help' for commands, 'exit' to quit.\n\n");

        var buf: [1024]u8 = undefined;

        while (true) {
            if (self.state) |s| {
                if (s.isSolved()) {
                    try stdout.writeAll("\nGoal solved successfully!\n");
                    self.state = null;
                    continue;
                }
                try stdout.print("Tactic [{d} subgoals] > ", .{s.goals.len});
            } else {
                try stdout.writeAll("Start Goal > ");
            }

            const line = stdin.readUntilDelimiterOrEof(&buf, '\n') catch return;
            if (line == null) return;
            const input = std.mem.trim(u8, line.?, &[_]u8{ '\r', '\n', ' ', '\t' });
            if (input.len == 0) continue;

            if (std.mem.eql(u8, input, "exit")) return;

            if (self.state == null) {
                if (std.mem.eql(u8, input, "help")) {
                    try self.showHelp(stdout);
                    continue;
                }
                // 補題管理コマンドはゴール外でも使える
                if (try self.handleLemmaCommand(input, stdout)) continue;

                // TODO: パーサー実装後に式のパースを追加
                try stdout.writeAll("Parser not yet implemented. Use programmatic API.\n");
                continue;
            }

            // タクティクスの実行
            try self.executeTactic(input, stdout);
        }
    }

    /// 補題管理コマンドの処理。処理した場合 true を返す。
    fn handleLemmaCommand(self: *Repl, input: []const u8, writer: anytype) !bool {
        // "save <filename>"
        if (std.mem.startsWith(u8, input, "save ")) {
            const filename = std.mem.trim(u8, input[5..], " \t");
            if (filename.len == 0) {
                try writer.writeAll("Usage: save <filename>\n");
                return true;
            }
            lemma_manager.saveLemmas(filename, self.session_lemmas.items, self.arena) catch |err| {
                try writer.print("保存失敗: {}\n", .{err});
                return true;
            };
            try writer.print("{d} 個のセッション補題を {s} に保存しました\n", .{ self.session_lemmas.items.len, filename });
            return true;
        }

        // "load <filename>"
        if (std.mem.startsWith(u8, input, "load ")) {
            const filename = std.mem.trim(u8, input[5..], " \t");
            if (filename.len == 0) {
                try writer.writeAll("Usage: load <filename>\n");
                return true;
            }
            const new_lemmas = lemma_manager.loadLemmas(filename, self.arena) catch |err| {
                try writer.print("読み込み失敗: {}\n", .{err});
                return true;
            };
            self.loaded_lemmas.appendSlice(self.arena, new_lemmas) catch {};
            try writer.print("{d} 個の補題を {s} から読み込みました\n", .{ new_lemmas.len, filename });
            return true;
        }

        // "lemmas"
        if (std.mem.eql(u8, input, "lemmas")) {
            try writer.writeAll("\n--- 読み込み済み補題 ---\n");
            if (self.loaded_lemmas.items.len == 0) {
                try writer.writeAll("  (なし)\n");
            } else {
                for (self.loaded_lemmas.items) |lemma| {
                    try writer.print("  {s}: ", .{lemma.name});
                    try writeLemmaExpr(writer, lemma);
                    try writer.writeAll("\n");
                }
            }
            try writer.writeAll("\n--- セッション補題 ---\n");
            if (self.session_lemmas.items.len == 0) {
                try writer.writeAll("  (なし)\n");
            } else {
                for (self.session_lemmas.items) |lemma| {
                    try writer.print("  {s}: ", .{lemma.name});
                    try writeLemmaExpr(writer, lemma);
                    try writer.writeAll("\n");
                }
            }
            try writer.writeAll("\n");
            return true;
        }

        return false;
    }

    fn executeTactic(self: *Repl, input: []const u8, writer: anytype) !void {
        // 補題管理コマンドをタクティクモードでも受け付ける
        if (try self.handleLemmaCommand(input, writer)) return;

        if (std.mem.eql(u8, input, "undo")) {
            if (self.history.items.len > 0) {
                self.state = self.history.pop();
                try writer.writeAll("Undone.\n");
            } else {
                try writer.writeAll("Nothing to undo.\n");
            }
            return;
        }

        if (std.mem.eql(u8, input, "abort")) {
            self.state = null;
            self.history.clearRetainingCapacity();
            try writer.writeAll("Aborted.\n");
            return;
        }

        const s = self.state orelse return;

        // 履歴に保存
        try self.history.append(self.arena, s);

        const result = if (std.mem.eql(u8, input, "intro"))
            tactics_mod.intro(s, null, self.arena)
        else if (std.mem.eql(u8, input, "split"))
            tactics_mod.split(s, self.arena)
        else if (std.mem.eql(u8, input, "auto"))
            tactics_mod.auto(s, self.plugins, self.arena)
        else if (std.mem.eql(u8, input, "assumption"))
            tactics_mod.assumption(s, self.arena)
        else if (std.mem.eql(u8, input, "reflexivity") or std.mem.eql(u8, input, "refl"))
            tactics_mod.reflexivity(s, self.arena)
        else if (std.mem.eql(u8, input, "help")) {
            try self.showTacticHelp(writer);
            return;
        } else {
            try writer.print("Unknown tactic: {s}\n", .{input});
            _ = self.history.pop();
            return;
        };

        switch (result) {
            .ok => |new_state| {
                self.state = new_state;
                try self.showCurrentState(writer);
            },
            .err => |msg| {
                try writer.print("Error: {s}\n", .{msg});
                _ = self.history.pop();
            },
        }
    }

    fn showCurrentState(self: *Repl, writer: anytype) !void {
        _ = self;
        try writer.writeAll("\n");
        // TODO: ゴールの詳細表示
        try writer.writeAll("(Goal display not yet implemented)\n");
    }

    fn showHelp(_: *Repl, writer: anytype) !void {
        try writer.writeAll(
            \\Commands:
            \\  help          - Show this help
            \\  exit          - Exit the shell
            \\  save <file>   - Save session lemmas to file
            \\  load <file>   - Load lemmas from file
            \\  lemmas        - List loaded and session lemmas
            \\
        );
    }

    fn showTacticHelp(_: *Repl, writer: anytype) !void {
        try writer.writeAll(
            \\Tactics:
            \\  intro      - Introduce implication/universal
            \\  split      - Split conjunction
            \\  auto       - Automatic proof search
            \\  assumption - Solve from context
            \\  reflexivity/refl - Solve reflexive equation
            \\  undo       - Undo last tactic
            \\  abort      - Abandon current proof
            \\  save <file>   - Save session lemmas to file
            \\  load <file>   - Load lemmas from file
            \\  lemmas        - List all lemmas
            \\
        );
    }
};

/// 補題の式を読みやすい形式で出力
fn writeLemmaExpr(writer: anytype, lemma: CatRule) !void {
    // lhs → rhs 形式で出力
    try writeExprForDisplay(writer, lemma.lhs);
    try writer.writeAll(" → ");
    try writeExprForDisplay(writer, lemma.rhs);
    if (lemma.universals.len > 0) {
        try writer.writeAll(" [∀ ");
        for (lemma.universals, 0..) |u, i| {
            if (i > 0) try writer.writeAll(", ");
            try writeExprForDisplay(writer, u);
        }
        try writer.writeAll("]");
    }
}

/// Exprを表示用に出力
fn writeExprForDisplay(writer: anytype, e: *const expr_mod.Expr) !void {
    switch (e.*) {
        .sym => |n| try writer.writeAll(n),
        .var_ => |n| try writer.writeAll(n),
        .meta => |id| {
            try writer.writeAll("?");
            for (id.ids, 0..) |i, j| {
                if (j > 0) try writer.writeAll(".");
                try writer.print("{d}", .{i});
            }
        },
        .app => |a| {
            if (a.head.* == .sym and std.mem.eql(u8, a.head.sym, "λ") and a.args.len == 2 and a.args[0].* == .var_) {
                try writer.print("λ{s}. ", .{a.args[0].var_});
                try writeExprForDisplay(writer, a.args[1]);
                return;
            }
            try writeExprForDisplay(writer, a.head);
            if (a.args.len > 0) {
                try writer.writeAll("(");
                for (a.args, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writeExprForDisplay(writer, arg);
                }
                try writer.writeAll(")");
            }
        },
    }
}

test "Repl initialization" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const repl = Repl.init(arena);
    try std.testing.expect(repl.state == null);
    try std.testing.expect(repl.history.items.len == 0);
    try std.testing.expect(repl.loaded_lemmas.items.len == 0);
    try std.testing.expect(repl.session_lemmas.items.len == 0);
}
