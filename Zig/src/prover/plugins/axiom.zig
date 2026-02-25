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
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    // 正規化
    const goal_norm = args.prover.normalize(
        unifier_mod.applySubst(goal, &args.subst, args.arena) catch return results.items,
    ) catch return results.items;

    // ⊤の導入
    if (goal_norm.* == .sym and (std.mem.eql(u8, goal_norm.sym, syms.True) or 
                                 std.mem.eql(u8, goal_norm.sym, "⊤") or
                                 std.mem.eql(u8, goal_norm.sym, syms.Terminal))) {
        try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
            .goal = "⊤",
            .rule_name = "true-intro",
            .status = .success,
            .depth = args.depth,
        }));
    }

    // 爆発律 (⊥がコンテキストにある場合)
    for (args.context) |entry| {
        if (entry.expr.* == .sym and (std.mem.eql(u8, entry.expr.sym, syms.False) or 
                                      std.mem.eql(u8, entry.expr.sym, "⊥") or
                                      std.mem.eql(u8, entry.expr.sym, syms.Initial))) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
        if (entry.expr.* == .sym and (std.mem.eql(u8, entry.expr.sym, syms.False) or 
                                      std.mem.eql(u8, entry.expr.sym, "⊥") or
                                      std.mem.eql(u8, entry.expr.sym, syms.Initial))) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
                if (unify_result.first()) |s| {
                    _ = s;
                    try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
        if (unify_result.first()) |s| {
            _ = s;
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
        if (unify_result.first()) |s| {
            _ = s;
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = entry.name,
                .rule_name = entry.name,
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub fn buildRules(arena: Allocator) anyerror![]const expr_mod.CatRule {
    var rules: std.ArrayList(expr_mod.CatRule) = .{};
    const b = expr_mod.RuleBuilder.init(arena);

    // ==========================================
    // 論理射 (Logic-Category Curry-Howard-Lambek)
    // ==========================================
    try rules.append(arena, .{ .name = "and-is-×", .lhs = try b.a2(try b.s(syms.And), try b.v("A"), try b.v("B")), .rhs = try b.a2(try b.s(syms.Product), try b.v("A"), try b.v("B")) });
    try rules.append(arena, .{ .name = "or-is-+", .lhs = try b.a2(try b.s(syms.Or), try b.v("A"), try b.v("B")), .rhs = try b.a2(try b.s(syms.Coproduct), try b.v("A"), try b.v("B")) });
    try rules.append(arena, .{ .name = "→-is-^", .lhs = try b.a2(try b.s(syms.Implies), try b.v("A"), try b.v("B")), .rhs = try b.a2(try b.s(syms.Exp), try b.v("B"), try b.v("A")) });
    try rules.append(arena, .{ .name = "⊤-is-1", .lhs = try b.s(syms.True), .rhs = try b.s(syms.Terminal) });
    try rules.append(arena, .{ .name = "⊥-is-0", .lhs = try b.s(syms.False), .rhs = try b.s(syms.Initial) });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "Axiom",
    .priority = 10,
    .build_rules = &buildRules,
    .goal_hooks = &goalHooks,
};
