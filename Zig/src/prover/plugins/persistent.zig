// ==========================================
// persistent.zig
// 直観主義論理プラグイン (PersistentLogicSearch.scala移植)
// バックチェイン, ∧分解, ∨場合分け
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
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
const sym = expr_mod.sym;

fn contextHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    for (args.context) |entry| {
        const hyp = entry.expr;
        if (hyp.* != .app or hyp.app.head.* != .sym) continue;
        const hn = hyp.app.head.sym;
        const h_args = hyp.app.args;

        // バックチェイン: A → B がコンテキストにあり、ゴールがBとユニファイ可能ならAを証明
        if (std.mem.eql(u8, hn, syms.Implies) and h_args.len == 2) {
            const a = h_args[0];
            const b = h_args[1];
            const unify_result = unifier_mod.unify(b, goal, args.subst, args.arena) catch continue;
            if (unify_result.first()) |s| {
                const sub_tree = args.prover.search(a, args.context, args.state, try s.clone(), args.depth + 1, args.limit) catch continue;
                if (search_mod.findSuccess(sub_tree) != null) {
                    try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                        .goal = "backchain",
                        .rule_name = "backchain",
                        .status = .success,
                        .depth = args.depth,
                    }));
                }
            }
        }

        // ∧分解: A ∧ B がコンテキストにある場合、AとBを個別に追加
        if ((std.mem.eql(u8, hn, syms.And) or std.mem.eql(u8, hn, syms.Product)) and h_args.len == 2) {
            const a = h_args[0];
            const b = h_args[1];
            var new_ctx: std.ArrayList(ContextEntry) = .{};
            var buf1: [32]u8 = undefined;
            var buf2: [32]u8 = undefined;
            const n1 = std.fmt.bufPrint(&buf1, "{s}_l", .{entry.name}) catch unreachable;
            const n2 = std.fmt.bufPrint(&buf2, "{s}_r", .{entry.name}) catch unreachable;
            try new_ctx.append(args.arena, .{ .name = try args.arena.dupe(u8, n1), .expr = a });
            try new_ctx.append(args.arena, .{ .name = try args.arena.dupe(u8, n2), .expr = b });
            for (args.context) |e2| {
                if (!std.mem.eql(u8, e2.name, entry.name)) try new_ctx.append(args.arena, e2);
            }
            const sub_tree = args.prover.search(goal, new_ctx.items, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch continue;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "and-elim",
                    .rule_name = "and-elim",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }

        // ∨場合分け: A ∨ B がコンテキストにある場合、AでのケースとBでのケースを両方証明
        if ((std.mem.eql(u8, hn, syms.Or) or std.mem.eql(u8, hn, syms.Coproduct)) and h_args.len == 2) {
            const a = h_args[0];
            const b = h_args[1];

            // Aのケース
            var ctx_a: std.ArrayList(ContextEntry) = .{};
            var buf3: [32]u8 = undefined;
            const n3 = std.fmt.bufPrint(&buf3, "{s}_case_l", .{entry.name}) catch unreachable;
            try ctx_a.append(args.arena, .{ .name = try args.arena.dupe(u8, n3), .expr = a });
            for (args.context) |e2| {
                if (!std.mem.eql(u8, e2.name, entry.name)) try ctx_a.append(args.arena, e2);
            }
            const tree_a = args.prover.search(goal, ctx_a.items, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch continue;

            if (search_mod.findSuccess(tree_a) != null) {
                // Bのケース
                var ctx_b: std.ArrayList(ContextEntry) = .{};
                var buf4: [32]u8 = undefined;
                const n4 = std.fmt.bufPrint(&buf4, "{s}_case_r", .{entry.name}) catch unreachable;
                try ctx_b.append(args.arena, .{ .name = try args.arena.dupe(u8, n4), .expr = b });
                for (args.context) |e2| {
                    if (!std.mem.eql(u8, e2.name, entry.name)) try ctx_b.append(args.arena, e2);
                }
                const tree_b = args.prover.search(goal, ctx_b.items, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch continue;
                if (search_mod.findSuccess(tree_b) != null) {
                    try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                        .goal = "or-elim",
                        .rule_name = "or-elim",
                        .status = .success,
                        .depth = args.depth,
                    }));
                }
            }
        }

        // ∀除去: ∀x. B がコンテキストにある場合、メタ変数でインスタンス化
        if (std.mem.eql(u8, hn, syms.Forall) and h_args.len >= 2 and h_args[0].* == .var_) {
            const v_name = h_args[0].var_;
            const body = if (h_args.len == 3) h_args[2] else h_args[1];
            const m = args.prover.freshMeta() catch continue;
            const instantiated = unifier_mod.substVar(body, v_name, m, args.arena) catch continue;

            const unify_result = unifier_mod.unify(instantiated, goal, args.subst, args.arena) catch continue;
            if (unify_result.first() != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "forall-elim",
                    .rule_name = "forall-elim",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "Persistent",
    .priority = 100,
    .context_hooks = &contextHooks,
};
