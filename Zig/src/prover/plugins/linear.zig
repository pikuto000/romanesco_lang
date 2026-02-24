// ==========================================
// linear.zig
// 線形論理プラグイン (LinearLogicSearch.scala移植)
// ⊸,⊗,!,フレーム推論,ドミノ推論
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const unifier_mod = @import("../unifier.zig");
const syms = @import("../symbols.zig");
const Expr = expr_mod.Expr;
const ContextEntry = expr_mod.ContextEntry;
const LogicState = expr_mod.LogicState;
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;
const sym = expr_mod.sym;
const app2 = expr_mod.app2;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // ⊸導入: A ⊸ B → 線形コンテキストにAを追加してBを証明
    if (std.mem.eql(u8, hn, syms.LImplies) and goal_args.len == 2) {
        const a = goal_args[0];
        const b = goal_args[1];
        var new_linear = std.ArrayList(ContextEntry).init(args.arena);
        for (args.state.linear_context) |e| try new_linear.append(e);
        var buf: [32]u8 = undefined;
        const n = try std.fmt.bufPrint(&buf, "lh{d}", .{args.depth});
        try new_linear.append(.{ .name = try args.arena.dupe(u8, n), .expr = a });

        const new_state = LogicState{
            .linear_context = new_linear.items,
            .meta_counter = args.state.meta_counter,
            .induction_depth = args.state.induction_depth,
            .raa_depth = args.state.raa_depth,
        };
        const sub_tree = args.prover.search(b, args.context, new_state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "lollipop-intro",
                .rule_name = "lollipop-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // ⊗導入: A ⊗ B → AとBを両方証明 (線形リソースを分割)
    if (std.mem.eql(u8, hn, syms.Tensor) and goal_args.len == 2) {
        const a = goal_args[0];
        const b = goal_args[1];

        // 簡略化: 全リソースをAに渡し、残りをBに渡す
        const tree_a = args.prover.search(a, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(tree_a) != null) {
            // Bは残りの線形リソースで証明 (簡略化版: 空の線形コンテキスト)
            const empty_state = LogicState{};
            const tree_b = args.prover.search(b, args.context, empty_state, args.subst, args.depth + 1, args.limit) catch return results.items;
            if (search_mod.findSuccess(tree_b) != null) {
                try results.append(try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "tensor-intro",
                    .rule_name = "tensor-intro",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    // !A導入: !A → 持続的にAが使用可能
    if (std.mem.eql(u8, hn, syms.Bang) and goal_args.len == 1) {
        const a = goal_args[0];
        const sub_tree = args.prover.search(a, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "bang-intro",
                .rule_name = "bang-intro",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

fn contextHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    // 線形コンテキストからの⊸除去
    for (args.state.linear_context, 0..) |entry, idx| {
        if (entry.expr.* != .app or entry.expr.app.head.* != .sym) continue;
        const hn = entry.expr.app.head.sym;

        if (std.mem.eql(u8, hn, syms.LImplies) and entry.expr.app.args.len == 2) {
            const a = entry.expr.app.args[0];
            const b = entry.expr.app.args[1];

            // ゴールとBをユニファイ
            const unify_result = unifier_mod.unify(b, goal, args.subst, args.arena) catch continue;
            if (unify_result.first()) |s| {
                // この⊸を消費して、Aを証明
                var new_linear = std.ArrayList(ContextEntry).init(args.arena);
                for (args.state.linear_context, 0..) |e, j| {
                    if (j != idx) try new_linear.append(e);
                }
                const new_state = LogicState{
                    .linear_context = new_linear.items,
                    .meta_counter = args.state.meta_counter,
                };
                const sub_tree = args.prover.search(a, args.context, new_state, s, args.depth + 1, args.limit) catch continue;
                if (search_mod.findSuccess(sub_tree) != null) {
                    try results.append(try Tree(SearchNode).leaf(args.arena, .{
                        .goal = "lollipop-elim",
                        .rule_name = "lollipop-elim",
                        .status = .success,
                        .depth = args.depth,
                    }));
                }
            }
        }

        // ⊗分解: A ⊗ B を A と B に分解
        if (std.mem.eql(u8, hn, syms.Tensor) and entry.expr.app.args.len == 2) {
            const a = entry.expr.app.args[0];
            const b = entry.expr.app.args[1];

            var new_linear = std.ArrayList(ContextEntry).init(args.arena);
            for (args.state.linear_context, 0..) |e, j| {
                if (j != idx) try new_linear.append(e);
            }
            var buf1: [32]u8 = undefined;
            var buf2: [32]u8 = undefined;
            const n1 = try std.fmt.bufPrint(&buf1, "{s}_l", .{entry.name});
            const n2 = try std.fmt.bufPrint(&buf2, "{s}_r", .{entry.name});
            try new_linear.append(.{ .name = try args.arena.dupe(u8, n1), .expr = a });
            try new_linear.append(.{ .name = try args.arena.dupe(u8, n2), .expr = b });
            const new_state = LogicState{
                .linear_context = new_linear.items,
                .meta_counter = args.state.meta_counter,
            };
            const sub_tree = args.prover.search(goal, args.context, new_state, args.subst, args.depth + 1, args.limit) catch continue;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "tensor-elim",
                    .rule_name = "tensor-elim",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "LinearLogic",
    .priority = 80,
    .goal_hooks = &goalHooks,
    .context_hooks = &contextHooks,
};
