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
const Allocator = std.mem.Allocator;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
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
            const sub_tree = args.prover.search(x, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
                            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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

pub fn rewriteHook(e: *const Expr, arena: Allocator) error{OutOfMemory}!?*const Expr {
    if (e.* != .app) return null;
    const a = e.app;
    if (a.head.* != .sym) return null;
    const hn = a.head.sym;
    const args = a.args;

    // inv(refl(_)) → refl(_), inv(inv(p)) → p
    if (std.mem.eql(u8, hn, "inv") and args.len == 1) {
        if (args[0].* == .app and args[0].app.head.* == .sym) {
            if (std.mem.eql(u8, args[0].app.head.sym, syms.Refl)) {
                return args[0];
            }
        }
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "inv") and args[0].app.args.len == 1) {
            return args[0].app.args[0];
        }
    }

    // compose/concat(refl, p) → p, compose/concat(p, refl) → p
    if ((std.mem.eql(u8, hn, syms.Compose) or std.mem.eql(u8, hn, syms.Concat)) and args.len == 2) {
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.Refl)) return args[1];
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl)) return args[0];
    }

    // transport の完全実装
    if (std.mem.eql(u8, hn, syms.Transport) and args.len == 3) {
        const type_fam = args[0];
        const path_arg = args[1];
        const val = args[2];

        // transport(_, refl(_), x) → x
        if (path_arg.* == .app and path_arg.app.head.* == .sym and std.mem.eql(u8, path_arg.app.head.sym, syms.Refl)) {
            return val;
        }

        // transport(λz. body, p, v) → 各種簡約
        if (type_fam.* == .app and type_fam.app.head.* == .sym and std.mem.eql(u8, type_fam.app.head.sym, "λ") and type_fam.app.args.len == 2 and type_fam.app.args[0].* == .var_) {
            const z = type_fam.app.args[0];
            const body = type_fam.app.args[1];

            // 定数型: body に z が自由出現しない → v
            if (!body.contains(z)) {
                return val;
            }

            // 積型: λz. A(z) × B(z)
            if (body.* == .app and body.app.head.* == .sym and
                (std.mem.eql(u8, body.app.head.sym, syms.Product) or std.mem.eql(u8, body.app.head.sym, syms.And)) and
                body.app.args.len == 2)
            {
                const a_fam = try arena.create(Expr);
                a_fam.* = .{ .app = .{ .head = try expr_mod.sym(arena, "λ"), .args = try arena.dupe(*const Expr, &[_]*const Expr{ z, body.app.args[0] }) } };
                const b_fam = try arena.create(Expr);
                b_fam.* = .{ .app = .{ .head = try expr_mod.sym(arena, "λ"), .args = try arena.dupe(*const Expr, &[_]*const Expr{ z, body.app.args[1] }) } };
                const pi1_sym = try expr_mod.sym(arena, syms.Proj1);
                const pi2_sym = try expr_mod.sym(arena, syms.Proj2);
                const transport_sym = try expr_mod.sym(arena, syms.Transport);
                const pair_sym = try expr_mod.sym(arena, syms.Pair);
                const t1 = try expr_mod.app(arena, transport_sym, &[_]*const Expr{ a_fam, path_arg, try expr_mod.app1(arena, pi1_sym, val) });
                const t2 = try expr_mod.app(arena, transport_sym, &[_]*const Expr{ b_fam, path_arg, try expr_mod.app1(arena, pi2_sym, val) });
                return expr_mod.app2(arena, pair_sym, t1, t2);
            }

            // 余積型 (coproduct / +): λz. A(z) + B(z)
            if (body.* == .app and body.app.head.* == .sym and
                (std.mem.eql(u8, body.app.head.sym, syms.Coproduct) or std.mem.eql(u8, body.app.head.sym, syms.Or)) and
                body.app.args.len == 2)
            {
                // inl(x) → inl(transport(λz.A(z), p, x))
                if (val.* == .app and val.app.head.* == .sym and std.mem.eql(u8, val.app.head.sym, syms.Inl) and val.app.args.len == 1) {
                    const a_fam = try arena.create(Expr);
                    a_fam.* = .{ .app = .{ .head = try expr_mod.sym(arena, "λ"), .args = try arena.dupe(*const Expr, &[_]*const Expr{ z, body.app.args[0] }) } };
                    const transport_sym = try sym(arena, syms.Transport);
                    const new_val = try expr_mod.app(arena, transport_sym, &[_]*const Expr{ a_fam, path_arg, val.app.args[0] });
                    return expr_mod.app1(arena, try expr_mod.sym(arena, syms.Inl), new_val);
                }
                if (val.* == .app and val.app.head.* == .sym and std.mem.eql(u8, val.app.head.sym, syms.Inr) and val.app.args.len == 1) {
                    const b_fam = try arena.create(Expr);
                    b_fam.* = .{ .app = .{ .head = try expr_mod.sym(arena, "λ"), .args = try arena.dupe(*const Expr, &[_]*const Expr{ z, body.app.args[1] }) } };
                    const transport_sym = try sym(arena, syms.Transport);
                    const new_val = try expr_mod.app(arena, transport_sym, &[_]*const Expr{ b_fam, path_arg, val.app.args[0] });
                    return expr_mod.app1(arena, try expr_mod.sym(arena, syms.Inr), new_val);
                }
            }
        }
    }

    return null;
}

pub fn buildRules(arena: Allocator) anyerror![]const expr_mod.CatRule {
    var rules: std.ArrayList(expr_mod.CatRule) = .{};
    const b = expr_mod.RuleBuilder.init(arena);

    // path-refl: refl(a) → path(A, a, a)
    try rules.append(arena, .{ .name = "path-refl", .lhs = try b.a1(try b.s(syms.Refl), try b.v("a")), .rhs = try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("a"), try b.v("a")) });
    // path-inv: inv(path(A,a,b)) → path(A,b,a)
    try rules.append(arena, .{ .name = "path-inv", .lhs = try b.a1(try b.s("inv"), try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("a"), try b.v("b"))), .rhs = try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("b"), try b.v("a")) });
    // univalence: equiv(A,B) → path(Type,A,B)
    try rules.append(arena, .{ .name = "univalence", .lhs = try b.a2(try b.s("equiv"), try b.v("A"), try b.v("B")), .rhs = try b.a3(try b.s(syms.Path), try b.s(syms.Type), try b.v("A"), try b.v("B")) });
    // path-to-equiv: path(Type,A,B) → equiv(A,B)
    try rules.append(arena, .{ .name = "path-to-equiv", .lhs = try b.a3(try b.s(syms.Path), try b.s(syms.Type), try b.v("A"), try b.v("B")), .rhs = try b.a2(try b.s("equiv"), try b.v("A"), try b.v("B")) });
    // path-concat: concat(path(A,a,b), path(A,b,c)) → path(A,a,c)
    try rules.append(arena, .{ .name = "path-concat", .lhs = try b.a2(try b.s(syms.Concat), try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("a"), try b.v("b")), try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("b"), try b.v("c"))), .rhs = try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("a"), try b.v("c")) });
    // concat-to-comp: concat(p,q) → comp(A, p, q)
    try rules.append(arena, .{ .name = "concat-to-comp", .lhs = try b.a2(try b.s(syms.Concat), try b.v("p"), try b.v("q")), .rhs = try b.a3(try b.s(syms.Comp), try b.v("A"), try b.v("p"), try b.v("q")) });

    // HoTT特性 (prop/set)
    // prop-path: isProp(A) → path(A,x,y)
    try rules.append(arena, .{ .name = "prop-path", .lhs = try b.a1(try b.s("isProp"), try b.v("A")), .rhs = try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("x"), try b.v("y")) });
    // set-path: isSet(A) → path(path(A,x,y),p,q)
    try rules.append(arena, .{ .name = "set-path", .lhs = try b.a1(try b.s("isSet"), try b.v("A")), .rhs = try b.a3(try b.s(syms.Path), try b.a3(try b.s(syms.Path), try b.v("A"), try b.v("x"), try b.v("y")), try b.v("p"), try b.v("q")) });
    // prop-to-set: isProp(A) → isSet(A)
    try rules.append(arena, .{ .name = "prop-to-set", .lhs = try b.a1(try b.s("isProp"), try b.v("A")), .rhs = try b.a1(try b.s("isSet"), try b.v("A")) });
    // prop-prod: isProp(A) ∧ isProp(B) → isProp(A×B)
    try rules.append(arena, .{ .name = "prop-prod", .lhs = try b.a2(try b.s(syms.And), try b.a1(try b.s("isProp"), try b.v("A")), try b.a1(try b.s("isProp"), try b.v("B"))), .rhs = try b.a1(try b.s("isProp"), try b.a2(try b.s(syms.Product), try b.v("A"), try b.v("B"))) });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "HoTT",
    .priority = 120,
    .build_rules = &buildRules,
    .goal_hooks = &goalHooks,
    .rewrite_hook = rewriteHook,
};
