// ==========================================
// introduction.zig
// 導入規則プラグイン (IntroductionPlugin移植)
// ∀,→,∧,∨,∃の導入規則
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
const var_ = expr_mod.var_;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;

    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // → 導入: A → B のゴールに対し、Aをコンテキストに追加してBを証明
    if (std.mem.eql(u8, hn, syms.Implies) and goal_args.len == 2) {
        const a = goal_args[0];
        const b = goal_args[1];

        // 新しいコンテキストにAを追加
        var new_ctx = try std.ArrayList(ContextEntry).initCapacity(args.arena, args.context.len + 1);
        var buf: [32]u8 = undefined;
        const hyp_name = std.fmt.bufPrint(&buf, "h{d}", .{args.depth}) catch unreachable;
        const hyp_name_owned = try args.arena.dupe(u8, hyp_name);
        try new_ctx.append(args.arena, .{ .name = hyp_name_owned, .expr = a });
        for (args.context) |entry| try new_ctx.append(args.arena, entry);

        // Bを証明
        const sub_tree = try args.prover.search(
            b,
            new_ctx.items,
            args.state,
            try args.subst.clone(),
            args.depth + 1,
            args.limit,
        );

        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "implies-intro",
                .rule_name = "implies-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // ∧ 導入: A ∧ B のゴールに対し、AとBを両方証明
    if ((std.mem.eql(u8, hn, syms.And) or std.mem.eql(u8, hn, syms.Product)) and goal_args.len == 2) {
        const a = goal_args[0];
        const b = goal_args[1];

        const tree_a = try args.prover.search(a, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit);
        if (search_mod.findSuccess(tree_a)) |_| {
            const tree_b = try args.prover.search(b, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit);
            if (search_mod.findSuccess(tree_b) != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "product-intro",
                    .rule_name = "product-intro",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    // ∨ 導入: A ∨ B のゴールに対し、AかBのどちらかを証明
    if ((std.mem.eql(u8, hn, syms.Or) or std.mem.eql(u8, hn, syms.Coproduct)) and goal_args.len == 2) {
        const a = goal_args[0];
        const b = goal_args[1];

        const tree_a = try args.prover.search(a, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit);
        if (search_mod.findSuccess(tree_a) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "coproduct-intro-l",
                .rule_name = "coproduct-intro-l",
                .status = .success,
                .depth = args.depth,
            }));
        }

        const tree_b = try args.prover.search(b, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit);
        if (search_mod.findSuccess(tree_b) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "coproduct-intro-r",
                .rule_name = "coproduct-intro-r",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // ∀ 導入: ∀x. B のゴールに対し、フレッシュ変数でBを証明
    if (std.mem.eql(u8, hn, syms.Forall) and goal_args.len >= 2 and goal_args[0].* == .var_) {
        const v_name = goal_args[0].var_;
        const body = if (goal_args.len == 3) goal_args[2] else goal_args[1];

        var buf2: [32]u8 = undefined;
        const fresh_name = std.fmt.bufPrint(&buf2, "{s}_{d}", .{ v_name, args.depth }) catch unreachable;
        const fresh_owned = try args.arena.dupe(u8, fresh_name);
        const fresh_var = try var_(args.arena, fresh_owned);

        const instantiated = try unifier_mod.substVar(body, v_name, fresh_var, args.arena);

        // フレッシュ変数をコンテキストに追加
        var new_ctx = try std.ArrayList(ContextEntry).initCapacity(args.arena, args.context.len + 1);
        const type_expr = if (goal_args.len == 3) goal_args[1] else try sym(args.arena, "Type");
        try new_ctx.append(args.arena, .{ .name = fresh_owned, .expr = type_expr });
        for (args.context) |entry| try new_ctx.append(args.arena, entry);

        const sub_tree = try args.prover.search(instantiated, new_ctx.items, args.state, try args.subst.clone(), args.depth + 1, args.limit);
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "forall-intro",
                .rule_name = "forall-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // ∃ 導入: ∃x. B のゴールに対し、メタ変数で具体化してBを証明
    if (std.mem.eql(u8, hn, syms.Exists) and goal_args.len >= 2 and goal_args[0].* == .var_) {
        const v_name = goal_args[0].var_;
        const body = if (goal_args.len == 3) goal_args[2] else goal_args[1];

        const m = try args.prover.freshMeta();
        const instantiated = try unifier_mod.substVar(body, v_name, m, args.arena);

        const sub_tree = try args.prover.search(instantiated, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit);
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "exists-intro",
                .rule_name = "exists-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "Introduction",
    .priority = 50,
    .goal_hooks = &goalHooks,
};

fn findSuccess(tree: Tree(SearchNode)) ?SearchNode {
    return search_mod.findSuccess(tree);
}
