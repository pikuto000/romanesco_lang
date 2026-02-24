// ==========================================
// hott.zig
// HoTTプラグイン (HoTTSearch.scala移植)
// パス帰納法, 高階帰納型, isSet/isProp加速
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

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // path導入: path(A, a, b) → refl(a) (a = bの場合) はAxiomPluginで処理済み

    // transport: transport(P, p, x) のゴールを簡約
    if (std.mem.eql(u8, hn, syms.Transport) and goal_args.len == 3) {
        // transportのゴールをrefl経由で簡約試行
        const refl_sym = try sym(args.arena, syms.Refl);
        const p = goal_args[1];
        if (p.* == .app and p.app.head.* == .sym and std.mem.eql(u8, p.app.head.sym, syms.Refl)) {
            // transport(P, refl(a), x) → x
            const x = goal_args[2];
            const sub_tree = args.prover.search(x, args.context, args.state, args.subst, args.depth + 1, args.limit) catch return results.items;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "transport-refl",
                    .rule_name = "transport-refl",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
        _ = refl_sym;
    }

    // パス帰納法 (J-eliminator): path(A, a, b)がゴールでa≠bの場合
    if (std.mem.eql(u8, hn, syms.Path) and goal_args.len == 3) {
        const a_term = goal_args[1];
        const b_term = goal_args[2];
        if (!a_term.eql(b_term)) {
            // コンテキストからa=bのパスを探す
            for (args.context) |entry| {
                if (entry.expr.* == .app and entry.expr.app.head.* == .sym and
                    std.mem.eql(u8, entry.expr.app.head.sym, syms.Path) and entry.expr.app.args.len == 3)
                {
                    const unify1 = unifier_mod.unify(entry.expr.app.args[1], a_term, args.subst, args.arena) catch continue;
                    if (unify1.first()) |s1| {
                        const unify2 = unifier_mod.unify(entry.expr.app.args[2], b_term, s1, args.arena) catch continue;
                        if (unify2.first() != null) {
                            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                                .goal = "path-induction",
                                .rule_name = "path-induction",
                                .status = .success,
                                .depth = args.depth,
                            }));
                        }
                    }
                }
            }
        }
    }

    // isSet/isProp加速
    if (std.mem.eql(u8, hn, "isSet") or std.mem.eql(u8, hn, "isProp")) {
        // 基本型は自動的にisSet/isProp
        if (goal_args.len >= 1 and goal_args[0].* == .sym) {
            const type_name = goal_args[0].sym;
            if (std.mem.eql(u8, type_name, "Nat") or std.mem.eql(u8, type_name, "Bool") or
                std.mem.eql(u8, type_name, "Unit") or std.mem.eql(u8, type_name, syms.True))
            {
                try results.append(try Tree(SearchNode).leaf(args.arena, .{
                    .goal = "isSet-basic",
                    .rule_name = "isSet-basic",
                    .status = .success,
                    .depth = args.depth,
                }));
            }
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "HoTT",
    .priority = 120,
    .goal_hooks = &goalHooks,
};
