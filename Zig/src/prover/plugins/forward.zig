// ==========================================
// forward.zig
// 前方推論プラグイン (ForwardReasoningSearch.scala移植)
// Modus ponens, ∀除去, 飽和ループ
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const unifier_mod = @import("../unifier.zig");
const syms = @import("../symbols.zig");
const Expr = expr_mod.Expr;
const ContextEntry = expr_mod.ContextEntry;
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;

fn contextHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};

    // Modus Ponens: A → B と A がコンテキストにあればBを追加
    var new_facts: std.ArrayList(ContextEntry) = .{};

    for (args.context) |entry| {
        if (entry.expr.* != .app or entry.expr.app.head.* != .sym) continue;
        const hn = entry.expr.app.head.sym;

        if (std.mem.eql(u8, hn, syms.Implies) and entry.expr.app.args.len == 2) {
            const a = entry.expr.app.args[0];
            const b = entry.expr.app.args[1];

            // Aがコンテキストに存在するか確認
            for (args.context) |other| {
                const unify_result = unifier_mod.unify(other.expr, a, args.subst, args.arena) catch continue;
                if (unify_result.first()) |s| {
                    const derived = unifier_mod.applySubst(b, &s, args.arena) catch continue;
                    var buf: [32]u8 = undefined;
                    const n = std.fmt.bufPrint(&buf, "mp_{s}", .{entry.name}) catch unreachable;
                    try new_facts.append(args.arena, .{ .name = try args.arena.dupe(u8, n), .expr = derived });
                }
            }
        }
    }

    if (new_facts.items.len > 0) {
        // 導出された事実をコンテキストに追加して再探索
        var new_ctx: std.ArrayList(ContextEntry) = .{};
        for (new_facts.items) |fact| try new_ctx.append(args.arena, fact);
        for (args.context) |entry| try new_ctx.append(args.arena, entry);

        const sub_tree = args.prover.search(args.goal, new_ctx.items, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "forward-reasoning",
                .rule_name = "forward-reasoning",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // 前方ルール適用
    for (args.context) |entry| {
        const forward_results = args.prover.forwardApplyRules(entry.expr, args.subst) catch continue;
        for (forward_results) |fr| {
            var new_ctx2: std.ArrayList(ContextEntry) = .{};
            try new_ctx2.append(args.arena, .{ .name = fr.rule_name, .expr = fr.result });
            for (args.context) |e2| try new_ctx2.append(args.arena, e2);

            const sub_tree2 = args.prover.search(args.goal, new_ctx2.items, args.state, fr.subst, args.depth + 1, args.limit) catch continue;
            if (search_mod.findSuccess(sub_tree2) != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "forward-rule",
                    .rule_name = "forward-rule",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "ForwardReasoning",
    .priority = 190,
    .context_hooks = &contextHooks,
};
