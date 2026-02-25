// ==========================================
// cubical.zig
// Cubicalプラグイン (CubicalPlugin.scala移植)
// Kan操作, 面制約
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
const Allocator = std.mem.Allocator;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;

    // hcomp(A, I1, u, u0) → u(I1) は rewriter で処理
    // hcomp(A, I0, u, u0) → u0 は rewriter で処理

    // Kan fill: fill(A, p, u0) のゴールをcompに帰着
    if (std.mem.eql(u8, hn, syms.Fill) and goal.app.args.len == 3) {
        // fill は comp の一般化。refl の場合は rewriter で処理済み
        // 非自明な場合はcompに帰着
        const a = goal.app.args[0];
        const p = goal.app.args[1];
        const base_val = goal.app.args[2];
        const comp_sym = try sym(args.arena, syms.Comp);
        const comp_goal = try expr_mod.app(args.arena, comp_sym, &[_]*const Expr{ a, p, base_val });
        const sub_tree = args.prover.search(comp_goal, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
                .goal = "fill-to-comp",
                .rule_name = "fill-to-comp",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub fn rewriteHook(e: *const Expr, arena: Allocator) error{OutOfMemory}!?*const Expr {
    if (e.* != .app) return null;
    const a = e.app;
    if (a.head.* != .sym) return null;
    const hn = a.head.sym;
    const args = a.args;

    // 面制約の正規化
    if (std.mem.eql(u8, hn, syms.FaceAnd) and args.len == 2) {
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I1)) return args[1];
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I1)) return args[0];
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I0)) return expr_mod.sym(arena, syms.I0);
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I0)) return expr_mod.sym(arena, syms.I0);
    }
    if (std.mem.eql(u8, hn, syms.FaceOr) and args.len == 2) {
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I0)) return args[1];
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I0)) return args[0];
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I1)) return expr_mod.sym(arena, syms.I1);
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I1)) return expr_mod.sym(arena, syms.I1);
    }
    if (std.mem.eql(u8, hn, syms.FaceNeg) and args.len == 1) {
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I0)) return expr_mod.sym(arena, syms.I1);
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I1)) return expr_mod.sym(arena, syms.I0);
        // ¬ᶠ(¬ᶠ(φ)) → φ
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.FaceNeg) and args[0].app.args.len == 1) {
            return args[0].app.args[0];
        }
    }

    // hcomp(A, I1, u, u0) → u(I1)
    if (std.mem.eql(u8, hn, syms.HComp) and args.len == 4) {
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I1)) {
            return expr_mod.app1(arena, args[2], try expr_mod.sym(arena, syms.I1));
        }
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I0)) return args[3];
    }

    // comp(A, refl, u0) → u0
    if (std.mem.eql(u8, hn, syms.Comp) and args.len == 3) {
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl)) {
            return args[2];
        }
    }

    // fill(A, refl(x), u0) → refl(x)
    if (std.mem.eql(u8, hn, syms.Fill) and args.len == 3) {
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl) and args[1].app.args.len == 1) {
            return args[1];
        }
    }

    return null;
}

pub fn buildRules(arena: Allocator) anyerror![]const expr_mod.CatRule {
    var rules: std.ArrayList(expr_mod.CatRule) = .{};
    const b = expr_mod.RuleBuilder.init(arena);

    // hcomp-trivial: hcomp(A, I1, u, u0) → u(I1)
    try rules.append(arena, .{ .name = "hcomp-trivial", .lhs = try b.a4(try b.s(syms.HComp), try b.v("A"), try b.s(syms.I1), try b.v("u"), try b.v("u0")), .rhs = try b.a1(try b.v("u"), try b.s(syms.I1)) });
    // hcomp-base: hcomp(A, I0, u, u0) → u0
    try rules.append(arena, .{ .name = "hcomp-base", .lhs = try b.a4(try b.s(syms.HComp), try b.v("A"), try b.s(syms.I0), try b.v("u"), try b.v("u0")), .rhs = try b.v("u0") });
    // fill-to-hcomp: fill(A, p, u0) → hcomp(A, φ, u, u0)
    try rules.append(arena, .{ .name = "fill-to-hcomp", .lhs = try b.a3(try b.s(syms.Fill), try b.v("A"), try b.v("p"), try b.v("u0")), .rhs = try b.a4(try b.s(syms.HComp), try b.v("A"), try b.v("φ"), try b.v("u"), try b.v("u0")) });

    // 区間代数 (Face Algebra)
    try rules.append(arena, .{ .name = "face-and-i1", .lhs = try b.a2(try b.s(syms.FaceAnd), try b.s(syms.I1), try b.v("i")), .rhs = try b.v("i") });
    try rules.append(arena, .{ .name = "face-and-i0", .lhs = try b.a2(try b.s(syms.FaceAnd), try b.s(syms.I0), try b.v("i")), .rhs = try b.s(syms.I0) });
    try rules.append(arena, .{ .name = "face-or-i0", .lhs = try b.a2(try b.s(syms.FaceOr), try b.s(syms.I0), try b.v("i")), .rhs = try b.v("i") });
    try rules.append(arena, .{ .name = "face-or-i1", .lhs = try b.a2(try b.s(syms.FaceOr), try b.s(syms.I1), try b.v("i")), .rhs = try b.s(syms.I1) });
    try rules.append(arena, .{ .name = "face-neg-i0", .lhs = try b.a1(try b.s(syms.FaceNeg), try b.s(syms.I0)), .rhs = try b.s(syms.I1) });
    try rules.append(arena, .{ .name = "face-neg-i1", .lhs = try b.a1(try b.s(syms.FaceNeg), try b.s(syms.I1)), .rhs = try b.s(syms.I0) });
    try rules.append(arena, .{ .name = "face-neg-neg", .lhs = try b.a1(try b.s(syms.FaceNeg), try b.a1(try b.s(syms.FaceNeg), try b.v("i"))), .rhs = try b.v("i") });
    // ド・モルガン
    try rules.append(arena, .{ .name = "face-dm-and", .lhs = try b.a1(try b.s(syms.FaceNeg), try b.a2(try b.s(syms.FaceAnd), try b.v("i"), try b.v("j"))), .rhs = try b.a2(try b.s(syms.FaceOr), try b.a1(try b.s(syms.FaceNeg), try b.v("i")), try b.a1(try b.s(syms.FaceNeg), try b.v("j"))) });
    try rules.append(arena, .{ .name = "face-dm-or", .lhs = try b.a1(try b.s(syms.FaceNeg), try b.a2(try b.s(syms.FaceOr), try b.v("i"), try b.v("j"))), .rhs = try b.a2(try b.s(syms.FaceAnd), try b.a1(try b.s(syms.FaceNeg), try b.v("i")), try b.a1(try b.s(syms.FaceNeg), try b.v("j"))) });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "Cubical",
    .priority = 125,
    .build_rules = &buildRules,
    .goal_hooks = &goalHooks,
    .rewrite_hook = &rewriteHook,
};
