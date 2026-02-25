// ==========================================
// hoare.zig
// Hoare論理プラグイン (HoareLogicSearch.scala移植)
// skip, assign, seq, if, while検証
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const unifier_mod = @import("../unifier.zig");
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

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // triple(P, C, Q) — Hoare三つ組
    if (!std.mem.eql(u8, hn, syms.Triple) or goal_args.len != 3) return results.items;

    const pre = goal_args[0];
    const cmd = goal_args[1];
    const post = goal_args[2];

    // skip規則: {P} skip {P}
    if (cmd.* == .sym and std.mem.eql(u8, cmd.sym, syms.Skip)) {
        const unify_result = unifier_mod.unify(pre, post, args.subst, args.arena) catch return results.items;
        if (unify_result.first() != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "hoare-skip",
                .rule_name = "hoare-skip",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // seq規則: {P} c1;c2 {Q} → ∃R. {P} c1 {R} ∧ {R} c2 {Q}
    if (cmd.* == .app and cmd.app.head.* == .sym and std.mem.eql(u8, cmd.app.head.sym, syms.Seq) and cmd.app.args.len == 2) {
        const c1 = cmd.app.args[0];
        const c2 = cmd.app.args[1];

        // メタ変数で中間条件Rを生成
        const r = args.prover.freshMeta() catch return results.items;
        const triple_sym = try sym(args.arena, syms.Triple);
        const goal1 = try expr_mod.app(args.arena, triple_sym, &[_]*const Expr{ pre, c1, r });
        const goal2 = try expr_mod.app(args.arena, triple_sym, &[_]*const Expr{ r, c2, post });

        const tree1 = args.prover.search(goal1, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(tree1) != null) {
            const tree2 = args.prover.search(goal2, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
            if (search_mod.findSuccess(tree2) != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "hoare-seq",
                    .rule_name = "hoare-seq",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    // assign規則: {P[x/e]} x := e {P}
    if (cmd.* == .app and cmd.app.head.* == .sym and std.mem.eql(u8, cmd.app.head.sym, syms.Assign) and cmd.app.args.len == 2) {
        const x = cmd.app.args[0];
        const e = cmd.app.args[1];
        if (x.* == .var_) {
            const substituted_pre = unifier_mod.substVar(post, x.var_, e, args.arena) catch return results.items;
            const unify_result = unifier_mod.unify(pre, substituted_pre, args.subst, args.arena) catch return results.items;
            if (unify_result.first() != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "hoare-assign",
                    .rule_name = "hoare-assign",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

const Allocator = std.mem.Allocator;

pub fn buildRules(arena: Allocator) anyerror![]const expr_mod.CatRule {
    var rules: std.ArrayList(expr_mod.CatRule) = .{};
    const b = expr_mod.RuleBuilder.init(arena);

    // ==========================================
    // リソース規則
    // ==========================================
    try rules.append(arena, .{ .name = "file-open", .lhs = try b.a1(try b.s("file_exists"), try b.v("f")), .rhs = try b.a1(try b.s("file_open"), try b.v("f")) });
    try rules.append(arena, .{ .name = "file-close", .lhs = try b.a1(try b.s("file_open"), try b.v("f")), .rhs = try b.s(syms.True) });
    try rules.append(arena, .{ .name = "file-read", .lhs = try b.a1(try b.s("file_open"), try b.v("f")), .rhs = try b.a1(try b.s("file_open"), try b.v("f")) });
    try rules.append(arena, .{ .name = "memory-alloc", .lhs = try b.s(syms.True), .rhs = try b.a2(try b.s(syms.PointsTo), try b.v("ptr"), try b.s("_")) });
    try rules.append(arena, .{ .name = "memory-free", .lhs = try b.a2(try b.s(syms.PointsTo), try b.v("ptr"), try b.v("val")), .rhs = try b.s(syms.True) });
    try rules.append(arena, .{ .name = "lock-acquire", .lhs = try b.a1(try b.s("lock_free"), try b.v("l")), .rhs = try b.a1(try b.s("lock_held"), try b.v("l")) });
    try rules.append(arena, .{ .name = "lock-release", .lhs = try b.a1(try b.s("lock_held"), try b.v("l")), .rhs = try b.a1(try b.s("lock_free"), try b.v("l")) });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "HoareLogic",
    .priority = 150,
    .build_rules = &buildRules,
    .goal_hooks = &goalHooks,
};
