// ==========================================
// rewrite_plugin.zig
// 等式書換えプラグイン (RewritePlugin.scala移植)
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const unifier_mod = @import("../unifier.zig");
const rewriter_mod = @import("../rewriter.zig");
const Expr = expr_mod.Expr;
const ContextEntry = expr_mod.ContextEntry;
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;

/// ∀x.∀y. a=b の形から等式部分と変数リストを抽出
fn extractEq(e: *const Expr, vars_out: *std.ArrayList([]const u8), arena: std.mem.Allocator) ?struct { lhs: *const Expr, rhs: *const Expr } {
    if (e.* == .app and e.app.head.* == .sym and std.mem.eql(u8, e.app.head.sym, "∀") and e.app.args.len >= 2) {
        if (e.app.args[0].* == .var_) {
            vars_out.append(arena, e.app.args[0].var_) catch return null; // allocator as first param
        }
        const inner = e.app.args[e.app.args.len - 1];
        return extractEq(inner, vars_out, arena);
    }
    if (e.* == .app and e.app.head.* == .sym and std.mem.eql(u8, e.app.head.sym, "=") and e.app.args.len == 2) {
        return .{ .lhs = e.app.args[0], .rhs = e.app.args[1] };
    }
    // Path型もサポート: path(A, x, y)
    if (e.* == .app and e.app.head.* == .sym and std.mem.eql(u8, e.app.head.sym, "path") and e.app.args.len == 3) {
        return .{ .lhs = e.app.args[1], .rhs = e.app.args[2] };
    }
    return null;
}

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    // ゴールが等式 a = b の場合、両辺を正規化して比較
    if (goal.* == .app and goal.app.head.* == .sym and
        (std.mem.eql(u8, goal.app.head.sym, "=") or std.mem.eql(u8, goal.app.head.sym, "path")) and
        goal.app.args.len >= 2)
    {
        const l = goal.app.args[if (goal.app.args.len == 3) 1 else 0];
        const r = goal.app.args[if (goal.app.args.len == 3) 2 else 1];
        const l_norm = rewriter_mod.normalize(l, args.prover.config.rules, args.arena) catch return results.items;
        const r_norm = rewriter_mod.normalize(r, args.prover.config.rules, args.arena) catch return results.items;

        if (l_norm.eql(r_norm)) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "rewrite-normalize",
                .rule_name = "rewrite-normalize",
                .status = .success,
                .depth = args.depth,
            }));
            return results.items;
        }
    }

    // コンテキスト内の等式を使ってゴールを書き換え（量化子剥ぎ取り対応）
    for (args.context) |entry| {
        // 量化子を剥ぎ取って等式を取り出す
        var eq_vars: std.ArrayList([]const u8) = .{};
        const eq_pair = extractEq(entry.expr, &eq_vars, args.arena) orelse continue;

        // 変数→メタ変数に置換（eq_varsの変数名を全てメタに）
        var eq_subst = std.StringHashMap(*const Expr).init(args.arena);
        for (eq_vars.items) |vn| {
            const m = args.prover.freshMeta() catch continue;
            eq_subst.put(vn, m) catch continue;
        }

        // LHS/RHSのメタ変数置換
        const eq_l = applyVarToMeta(eq_pair.lhs, &eq_subst, args.arena) catch continue;
        const eq_r = applyVarToMeta(eq_pair.rhs, &eq_subst, args.arena) catch continue;

        // ゴール内でeq_lをeq_rに再帰的置換（最大10候補）
        var rewrite_count: usize = 0;
        if (tryRewrite(goal, eq_l, eq_r, args, &results, &rewrite_count, 10)) |_| {} else |_| {}

        // 逆方向: eq_rをeq_lに置換
        if (tryRewrite(goal, eq_r, eq_l, args, &results, &rewrite_count, 10)) |_| {} else |_| {}

        if (results.items.len > 0) return results.items;
    }

    return results.items;
}

fn applyVarToMeta(e: *const Expr, var_subst: *const std.StringHashMap(*const Expr), arena: std.mem.Allocator) error{OutOfMemory}!*const Expr {
    return switch (e.*) {
        .var_ => |n| var_subst.get(n) orelse e,
        .app => |a| {
            const new_head = try applyVarToMeta(a.head, var_subst, arena);
            var changed = new_head != a.head;
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try applyVarToMeta(arg, var_subst, arena);
                if (new_args[i] != arg) changed = true;
            }
            if (!changed) return e;
            const result = try arena.create(Expr);
            result.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return result;
        },
        else => e,
    };
}

fn tryRewrite(goal: *const Expr, eq_l: *const Expr, eq_r: *const Expr, args: HookArgs, results: *std.ArrayList(Tree(SearchNode)), count: *usize, max: usize) HookError!void {
    if (count.* >= max) return;
    const rewritten = unifier_mod.replaceExpr(goal, eq_l, eq_r, args.arena) catch return;
    if (!rewritten.eql(goal)) {
        count.* += 1;
        const sub_tree = args.prover.search(rewritten, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "rewrite",
                .rule_name = "rewrite",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }
}

pub const plugin = Plugin{
    .name = "Rewrite",
    .priority = 160,
    .goal_hooks = &goalHooks,
};
