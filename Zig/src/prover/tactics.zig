// ==========================================
// tactics.zig
// タクティクスの実装 (Tactics.scala移植)
// intro, split, destruct, auto等
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const unifier_mod = @import("unifier.zig");
const search_mod = @import("search.zig");
const syms = @import("symbols.zig");
const Expr = expr_mod.Expr;
const Goal = expr_mod.Goal;
const ContextEntry = expr_mod.ContextEntry;
const ProofTree = expr_mod.ProofTree;
const ProofState = expr_mod.ProofState;
const LogicState = expr_mod.LogicState;

pub const TacticError = error{ InvalidGoal, HypothesisNotFound, NoCurrentGoal, OutOfMemory, Timeout };
pub const TacticResult = union(enum) {
    ok: ProofState,
    err: []const u8,
};

/// intro: →, ⊸, ∀ の導入
pub fn intro(state: ProofState, name: ?[]const u8, arena: Allocator) TacticResult {
    const goal = state.currentGoal() orelse return .{ .err = "No current goal" };
    const target = goal.target;

    if (target.* != .app or target.app.head.* != .sym) return .{ .err = "intro: Goal is not an implication or universal quantification" };
    const hn = target.app.head.sym;
    const args = target.app.args;

    // →導入
    if ((std.mem.eql(u8, hn, syms.Implies) or std.mem.eql(u8, hn, "⊃") or std.mem.eql(u8, hn, "⇒")) and args.len == 2) {
        const hyp_name = name orelse blk: {
            var buf: [32]u8 = undefined;
            const n = std.fmt.bufPrint(&buf, "h{d}", .{goal.context.len}) catch return .{ .err = "intro: name gen failed" };
            break :blk arena.dupe(u8, n) catch return .{ .err = "OOM" };
        };
        var new_ctx: std.ArrayList(ContextEntry) = .{};
        new_ctx.append(arena, .{ .name = hyp_name, .expr = args[0] }) catch return .{ .err = "OOM" };
        for (goal.context) |e| new_ctx.append(arena, e) catch return .{ .err = "OOM" };

        const new_goal = Goal{ .context = new_ctx.items, .linear_context = goal.linear_context, .target = args[1] };
        var new_goals: std.ArrayList(Goal) = .{};
        new_goals.append(arena, new_goal) catch return .{ .err = "OOM" };
        for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
        return .{ .ok = .{
            .goals = new_goals.items,
            .completed_proofs = state.completed_proofs,
            .original_goal = state.original_goal,
            .rules = state.rules,
        } };
    }

    // ⊸導入
    if (std.mem.eql(u8, hn, syms.LImplies) and args.len == 2) {
        const hyp_name = name orelse blk: {
            var buf: [32]u8 = undefined;
            const n = std.fmt.bufPrint(&buf, "lin{d}", .{goal.linear_context.len}) catch return .{ .err = "intro: name gen failed" };
            break :blk arena.dupe(u8, n) catch return .{ .err = "OOM" };
        };
        var new_lin: std.ArrayList(ContextEntry) = .{};
        new_lin.append(arena, .{ .name = hyp_name, .expr = args[0] }) catch return .{ .err = "OOM" };
        for (goal.linear_context) |e| new_lin.append(arena, e) catch return .{ .err = "OOM" };

        const new_goal = Goal{ .context = goal.context, .linear_context = new_lin.items, .target = args[1] };
        var new_goals: std.ArrayList(Goal) = .{};
        new_goals.append(arena, new_goal) catch return .{ .err = "OOM" };
        for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
        return .{ .ok = .{
            .goals = new_goals.items,
            .completed_proofs = state.completed_proofs,
            .original_goal = state.original_goal,
            .rules = state.rules,
        } };
    }

    // ∀導入
    if (std.mem.eql(u8, hn, syms.Forall) and args.len >= 2 and args[0].* == .var_) {
        const v_name = args[0].var_;
        const body = if (args.len == 3) args[2] else args[1];
        const hyp_name = name orelse blk: {
            var buf: [32]u8 = undefined;
            const n = std.fmt.bufPrint(&buf, "{s}_t{d}", .{ v_name, state.goals.len }) catch return .{ .err = "intro: name gen failed" };
            break :blk arena.dupe(u8, n) catch return .{ .err = "OOM" };
        };
        const fresh_var = expr_mod.var_(arena, hyp_name) catch return .{ .err = "OOM" };
        const instantiated = unifier_mod.substVar(body, v_name, fresh_var, arena) catch return .{ .err = "OOM" };
        const new_goal = Goal{ .context = goal.context, .linear_context = goal.linear_context, .target = instantiated };
        var new_goals: std.ArrayList(Goal) = .{};
        new_goals.append(arena, new_goal) catch return .{ .err = "OOM" };
        for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
        return .{ .ok = .{
            .goals = new_goals.items,
            .completed_proofs = state.completed_proofs,
            .original_goal = state.original_goal,
            .rules = state.rules,
        } };
    }

    return .{ .err = "intro: Goal is not an implication or universal quantification" };
}

/// split: ∧の分解
pub fn split(state: ProofState, arena: Allocator) TacticResult {
    const goal = state.currentGoal() orelse return .{ .err = "No current goal" };
    const target = goal.target;

    if (target.* != .app or target.app.head.* != .sym) return .{ .err = "split: Goal is not a conjunction" };
    const hn = target.app.head.sym;
    const args = target.app.args;

    if ((std.mem.eql(u8, hn, syms.And) or std.mem.eql(u8, hn, syms.Product)) and args.len == 2) {
        const g1 = Goal{ .context = goal.context, .linear_context = goal.linear_context, .target = args[0] };
        const g2 = Goal{ .context = goal.context, .linear_context = goal.linear_context, .target = args[1] };
        var new_goals: std.ArrayList(Goal) = .{};
        new_goals.append(arena, g1) catch return .{ .err = "OOM" };
        new_goals.append(arena, g2) catch return .{ .err = "OOM" };
        for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
        return .{ .ok = .{
            .goals = new_goals.items,
            .completed_proofs = state.completed_proofs,
            .original_goal = state.original_goal,
            .rules = state.rules,
        } };
    }

    return .{ .err = "split: Goal is not a conjunction" };
}

/// auto: 自動証明を試行
pub fn auto(state: ProofState, plugins: []const search_mod.Plugin, arena: Allocator) TacticResult {
    const goal = state.currentGoal() orelse return .{ .err = "No current goal" };

    var engine = search_mod.ProverEngine.init(
        expr_mod.ProverConfig{ .rules = state.rules },
        plugins,
        arena,
        arena,
    );
    defer engine.deinit();

    const result = engine.prove(goal.target, 10, 5000) catch return .{ .err = "auto: timeout or error" };

    return switch (result) {
        .success => |s| {
            var new_proofs: std.ArrayList(ProofTree) = .{};
            for (state.completed_proofs) |p| new_proofs.append(arena, p) catch return .{ .err = "OOM" };
            new_proofs.append(arena, s.tree) catch return .{ .err = "OOM" };
            var new_goals: std.ArrayList(Goal) = .{};
            for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
            return .{ .ok = .{
                .goals = new_goals.items,
                .completed_proofs = new_proofs.items,
                .original_goal = state.original_goal,
                .rules = state.rules,
            } };
        },
        .fail => .{ .err = "auto: no proof found" },
    };
}

/// assumption: コンテキストからゴールを直接解決
pub fn assumption(state: ProofState, arena: Allocator) TacticResult {
    const goal = state.currentGoal() orelse return .{ .err = "No current goal" };

    for (goal.context) |entry| {
        if (entry.expr.eql(goal.target)) {
            var new_goals: std.ArrayList(Goal) = .{};
            for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
            return .{ .ok = .{
                .goals = new_goals.items,
                .completed_proofs = state.completed_proofs,
                .original_goal = state.original_goal,
                .rules = state.rules,
            } };
        }
    }

    return .{ .err = "assumption: No matching hypothesis found" };
}

/// reflexivity: 等式 a = a を解決
pub fn reflexivity(state: ProofState, arena: Allocator) TacticResult {
    const goal = state.currentGoal() orelse return .{ .err = "No current goal" };
    const target = goal.target;

    if (target.* == .app and target.app.head.* == .sym and target.app.args.len == 2) {
        const hn = target.app.head.sym;
        if (std.mem.eql(u8, hn, "=") or std.mem.eql(u8, hn, syms.Path)) {
            const l_idx: usize = if (std.mem.eql(u8, hn, syms.Path) and target.app.args.len == 3) 1 else 0;
            const r_idx: usize = if (std.mem.eql(u8, hn, syms.Path) and target.app.args.len == 3) 2 else 1;
            if (target.app.args.len > r_idx and target.app.args[l_idx].eql(target.app.args[r_idx])) {
                var new_goals: std.ArrayList(Goal) = .{};
                for (state.goals[1..]) |g| new_goals.append(arena, g) catch return .{ .err = "OOM" };
                return .{ .ok = .{
                    .goals = new_goals.items,
                    .completed_proofs = state.completed_proofs,
                    .original_goal = state.original_goal,
                    .rules = state.rules,
                } };
            }
        }
    }

    return .{ .err = "reflexivity: Goal is not a reflexive equation" };
}

// テスト
test "intro tactic on implication" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const implies_sym = try expr_mod.sym(arena, "→");
    const a = try expr_mod.sym(arena, "A");
    const b = try expr_mod.sym(arena, "B");
    const target = try expr_mod.app2(arena, implies_sym, a, b);

    const state = ProofState{
        .goals = &[_]Goal{.{ .target = target }},
        .completed_proofs = &.{},
        .original_goal = target,
    };

    const result = intro(state, null, arena);
    switch (result) {
        .ok => |s| {
            try std.testing.expect(s.goals.len == 1);
            try std.testing.expect(s.goals[0].target.eql(b));
            try std.testing.expect(s.goals[0].context.len == 1);
        },
        .err => |msg| {
            std.debug.print("Unexpected error: {s}\n", .{msg});
            try std.testing.expect(false);
        },
    }
}
