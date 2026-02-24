// ==========================================
// modal.zig
// モーダル論理プラグイン (ModalLogicSearch.scala移植)
// □導入/除去, ◇, アクセス可能性
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

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // □導入: □A → 全てのモーダル仮定を□仮定のみに制限してAを証明
    if (std.mem.eql(u8, hn, syms.Box) and goal_args.len == 1) {
        const a = goal_args[0];
        // □仮定のみのコンテキストを構築
        var modal_ctx = std.ArrayList(ContextEntry).init(args.arena);
        for (args.context) |entry| {
            if (entry.expr.* == .app and entry.expr.app.head.* == .sym and
                std.mem.eql(u8, entry.expr.app.head.sym, syms.Box))
            {
                try modal_ctx.append(entry);
            }
        }

        const sub_tree = args.prover.search(a, modal_ctx.items, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "box-intro",
                .rule_name = "box-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // ◇導入: ◇A → Aを証明
    if (std.mem.eql(u8, hn, syms.Diamond) and goal_args.len == 1) {
        const a = goal_args[0];
        const sub_tree = args.prover.search(a, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "diamond-intro",
                .rule_name = "diamond-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

fn contextHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);

    // □除去: □Aがコンテキストにある場合、Aを追加
    for (args.context) |entry| {
        if (entry.expr.* == .app and entry.expr.app.head.* == .sym and
            std.mem.eql(u8, entry.expr.app.head.sym, syms.Box) and entry.expr.app.args.len == 1)
        {
            const a = entry.expr.app.args[0];
            var new_ctx = std.ArrayList(ContextEntry).init(args.arena);
            var buf: [32]u8 = undefined;
            const n = try std.fmt.bufPrint(&buf, "{s}_unbox", .{entry.name});
            try new_ctx.append(.{ .name = try args.arena.dupe(u8, n), .expr = a });
            for (args.context) |e2| try new_ctx.append(e2);

            const sub_tree = args.prover.search(args.goal, new_ctx.items, args.state, args.subst, args.depth + 1, args.limit) catch continue;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "box-elim",
                    .rule_name = "box-elim",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "ModalLogic",
    .priority = 130,
    .goal_hooks = &goalHooks,
    .context_hooks = &contextHooks,
};
