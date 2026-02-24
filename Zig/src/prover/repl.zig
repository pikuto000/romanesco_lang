// ==========================================
// repl.zig
// 対話的証明シェル (Repl.scala移植)
// undo/abort + タクティクス
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const tactics_mod = @import("tactics.zig");
const search_mod = @import("search.zig");
const plugin_mod = @import("plugin.zig");
const Goal = expr_mod.Goal;
const ProofState = expr_mod.ProofState;
const ProofTree = expr_mod.ProofTree;

pub const Repl = struct {
    arena: Allocator,
    history: std.ArrayList(ProofState),
    state: ?ProofState,
    plugins: []const search_mod.Plugin,

    pub fn init(arena: Allocator) Repl {
        return .{
            .arena = arena,
            .history = std.ArrayList(ProofState).init(arena),
            .state = null,
            .plugins = &plugin_mod.all_plugins,
        };
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
                // TODO: パーサー実装後に式のパースを追加
                try stdout.writeAll("Parser not yet implemented. Use programmatic API.\n");
                continue;
            }

            // タクティクスの実行
            try self.executeTactic(input, stdout);
        }
    }

    fn executeTactic(self: *Repl, input: []const u8, writer: anytype) !void {
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
        try self.history.append(s);

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
            \\  help     - Show this help
            \\  exit     - Exit the shell
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
            \\
        );
    }
};

test "Repl initialization" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const repl = Repl.init(arena);
    try std.testing.expect(repl.state == null);
    try std.testing.expect(repl.history.items.len == 0);
}
