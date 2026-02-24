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
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    // ゴールが等式 a = b の場合、両辺を正規化して比較
    if (goal.* == .app and goal.app.head.* == .sym and std.mem.eql(u8, goal.app.head.sym, "=") and goal.app.args.len == 2) {
        const l = goal.app.args[0];
        const r = goal.app.args[1];
        const l_norm = rewriter_mod.normalize(l, args.prover.config.rules, args.arena) catch return results.items;
        const r_norm = rewriter_mod.normalize(r, args.prover.config.rules, args.arena) catch return results.items;

        if (l_norm.eql(r_norm)) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "rewrite-normalize",
                .rule_name = "rewrite-normalize",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // コンテキスト内の等式を使ってゴールを書き換え
    for (args.context) |entry| {
        if (entry.expr.* != .app or entry.expr.app.head.* != .sym) continue;
        if (!std.mem.eql(u8, entry.expr.app.head.sym, "=") or entry.expr.app.args.len != 2) continue;

        const eq_l = entry.expr.app.args[0];
        const eq_r = entry.expr.app.args[1];

        // ゴール内のeq_lをeq_rに置換して再証明
        const rewritten = unifier_mod.replaceExpr(goal, eq_l, eq_r, args.arena) catch continue;
        if (!rewritten.eql(goal)) {
            const sub_tree = args.prover.search(rewritten, args.context, args.state, args.subst, args.depth + 1, args.limit) catch continue;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "rewrite",
                    .rule_name = "rewrite",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "Rewrite",
    .priority = 160,
    .goal_hooks = &goalHooks,
};
