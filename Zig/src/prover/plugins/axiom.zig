// ==========================================
// axiom.zig
// 公理プラグイン (AxiomPlugin移植)
// True導入, 爆発律, 反射律, 公理探索
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const unifier_mod = @import("../unifier.zig");
const rewriter_mod = @import("../rewriter.zig");
const syms = @import("../symbols.zig");
const Expr = expr_mod.Expr;
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;
const sym = expr_mod.sym;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    // 正規化
    const goal_norm = args.prover.normalize(
        unifier_mod.applySubst(goal, &args.subst, args.arena) catch return results.items,
    ) catch return results.items;

    // ⊤の導入
    if (goal_norm.* == .sym and (std.mem.eql(u8, goal_norm.sym, syms.True) or std.mem.eql(u8, goal_norm.sym, "⊤"))) {
        try results.append(try Tree(SearchNode).leaf(args.arena, .{
            .goal = "⊤",
            .rule_name = "true-intro",
            .status = .success,
            .depth = args.depth,
        }));
    }

    // 爆発律 (⊥がコンテキストにある場合)
    for (args.context) |entry| {
        if (entry.expr.* == .sym and (std.mem.eql(u8, entry.expr.sym, syms.False) or std.mem.eql(u8, entry.expr.sym, "⊥"))) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "explosion",
                .rule_name = "explosion",
                .status = .success,
                .depth = args.depth,
            }));
            break;
        }
    }
    // 線形コンテキストも確認
    for (args.state.linear_context) |entry| {
        if (entry.expr.* == .sym and (std.mem.eql(u8, entry.expr.sym, syms.False) or std.mem.eql(u8, entry.expr.sym, "⊥"))) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "explosion",
                .rule_name = "explosion",
                .status = .success,
                .depth = args.depth,
            }));
            break;
        }
    }

    // 反射律 (= or path)
    if (goal_norm.* == .app and goal_norm.app.head.* == .sym) {
        const hn = goal_norm.app.head.sym;
        if ((std.mem.eql(u8, hn, "=") and goal_norm.app.args.len == 2) or
            (std.mem.eql(u8, hn, syms.Path) and goal_norm.app.args.len == 3))
        {
            const l_idx: usize = if (std.mem.eql(u8, hn, syms.Path)) 1 else 0;
            const r_idx: usize = if (std.mem.eql(u8, hn, syms.Path)) 2 else 1;
            if (goal_norm.app.args.len > r_idx) {
                const l = goal_norm.app.args[l_idx];
                const r = goal_norm.app.args[r_idx];
                const l_norm = args.prover.normalize(l) catch l;
                const r_norm = args.prover.normalize(r) catch r;
                const unify_result = unifier_mod.unify(l_norm, r_norm, args.subst, args.arena) catch return results.items;
                if (unify_result.first() != null) {
                    try results.append(try Tree(SearchNode).leaf(args.arena, .{
                        .goal = "reflexivity",
                        .rule_name = "reflexivity",
                        .status = .success,
                        .depth = args.depth,
                    }));
                }
            }
        }
    }

    // 公理 (コンテキストからゴールとユニファイ)
    for (args.context) |entry| {
        const h_norm = args.prover.normalize(
            unifier_mod.applySubst(entry.expr, &args.subst, args.arena) catch continue,
        ) catch continue;
        const unify_result = unifier_mod.unify(h_norm, goal_norm, args.subst, args.arena) catch continue;
        if (unify_result.first() != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = entry.name,
                .rule_name = entry.name,
                .status = .success,
                .depth = args.depth,
            }));
        }
    }
    // 線形コンテキストも確認
    for (args.state.linear_context) |entry| {
        const h_norm = args.prover.normalize(
            unifier_mod.applySubst(entry.expr, &args.subst, args.arena) catch continue,
        ) catch continue;
        const unify_result = unifier_mod.unify(h_norm, goal_norm, args.subst, args.arena) catch continue;
        if (unify_result.first() != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = entry.name,
                .rule_name = entry.name,
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "Axiom",
    .priority = 10,
    .goal_hooks = &goalHooks,
};

test "axiom plugin true-intro" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var engine = search_mod.ProverEngine.init(expr_mod.ProverConfig{}, &.{}, arena);
    const true_sym = try sym(arena, "⊤");
    const results = try goalHooks(.{
        .goal = true_sym,
        .context = &.{},
        .state = expr_mod.LogicState{},
        .subst = expr_mod.Subst.init(arena),
        .depth = 0,
        .limit = 5,
        .arena = arena,
        .prover = &engine,
    });
    try std.testing.expect(results.len > 0);
    try std.testing.expect(results[0] == .node);
    try std.testing.expectEqualStrings("true-intro", results[0].node.value.rule_name);
}
