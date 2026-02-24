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

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
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
        const sub_tree = args.prover.search(comp_goal, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "fill-to-comp",
                .rule_name = "fill-to-comp",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "Cubical",
    .priority = 125,
    .goal_hooks = &goalHooks,
};
