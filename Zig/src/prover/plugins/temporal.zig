// ==========================================
// temporal.zig
// 時相論理プラグイン (TemporalLogicSearch.scala移植)
// G展開, F到達, X-step, Until
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
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;
const Allocator = std.mem.Allocator;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // G(A) → A ∧ X(G(A)) に展開
    if (std.mem.eql(u8, hn, syms.Globally) and goal_args.len == 1) {
        const a = goal_args[0];
        // まずAを証明
        const tree_a = args.prover.search(a, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(tree_a) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "globally-unfold",
                .rule_name = "globally-unfold",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // F(A) → A ∨ X(F(A)) に展開
    if (std.mem.eql(u8, hn, syms.Finally) and goal_args.len == 1) {
        const a = goal_args[0];
        // Aを直接証明試行
        const tree_a = args.prover.search(a, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(tree_a) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "finally-now",
                .rule_name = "finally-now",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // X(A) → 次のステップでAを証明 (guardedフラグ付き)
    if (std.mem.eql(u8, hn, syms.Next) and goal_args.len == 1) {
        const a = goal_args[0];
        const sub_tree = args.prover.search(a, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "next-step",
                .rule_name = "next-step",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    // A U B → B ∨ (A ∧ X(A U B))
    if (std.mem.eql(u8, hn, syms.Until) and goal_args.len == 2) {
        const b = goal_args[1];
        // まずBを直接証明
        const tree_b = args.prover.search(b, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(tree_b) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "until-base",
                .rule_name = "until-base",
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

    // G-dist-tensor: G(A⊗B) → G(A) ⊗ G(B)
    try rules.append(arena, .{ .name = "G-dist-tensor", .lhs = try b.a1(try b.s(syms.Globally), try b.a2(try b.s(syms.Tensor), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.Tensor), try b.a1(try b.s(syms.Globally), try b.v("A")), try b.a1(try b.s(syms.Globally), try b.v("B"))) });
    // G-dist-sepand: G(A*B) → G(A) * G(B)
    try rules.append(arena, .{ .name = "G-dist-sepand", .lhs = try b.a1(try b.s(syms.Globally), try b.a2(try b.s(syms.SepAnd), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.SepAnd), try b.a1(try b.s(syms.Globally), try b.v("A")), try b.a1(try b.s(syms.Globally), try b.v("B"))) });
    // G-dist-limplies: G(A⊸B) → G(A) ⊸ G(B)
    try rules.append(arena, .{ .name = "G-dist-limplies", .lhs = try b.a1(try b.s(syms.Globally), try b.a2(try b.s(syms.LImplies), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.LImplies), try b.a1(try b.s(syms.Globally), try b.v("A")), try b.a1(try b.s(syms.Globally), try b.v("B"))) });
    // F-expansion: F(A) → A ∨ X(F(A))
    try rules.append(arena, .{ .name = "F-expansion", .lhs = try b.a1(try b.s(syms.Finally), try b.v("A")), .rhs = try b.a2(try b.s(syms.Or), try b.v("A"), try b.a1(try b.s(syms.Next), try b.a1(try b.s(syms.Finally), try b.v("A")))) });
    // U-expansion: U(A,B) → B ∨ (A ∧ X(U(A,B)))
    try rules.append(arena, .{ .name = "U-expansion", .lhs = try b.a2(try b.s(syms.Until), try b.v("A"), try b.v("B")), .rhs = try b.a2(try b.s(syms.Or), try b.v("B"), try b.a2(try b.s(syms.And), try b.v("A"), try b.a1(try b.s(syms.Next), try b.a2(try b.s(syms.Until), try b.v("A"), try b.v("B"))))) });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "TemporalLogic",
    .priority = 140,
    .build_rules = &buildRules,
    .goal_hooks = &goalHooks,
};
