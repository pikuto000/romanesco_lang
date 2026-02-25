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
const Allocator = std.mem.Allocator;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results: std.ArrayList(Tree(SearchNode)) = .{};
    const goal = args.goal;

    if (goal.* != .app or goal.app.head.* != .sym) return results.items;
    const hn = goal.app.head.sym;
    const goal_args = goal.app.args;

    // □導入: □A → 全てのモーダル仮定を□仮定のみに制限してAを証明
    if (std.mem.eql(u8, hn, syms.Box) and goal_args.len == 1) {
        const a = goal_args[0];
        // □仮定のみのコンテキストを構築
        var modal_ctx: std.ArrayList(ContextEntry) = .{};
        for (args.context) |entry| {
            if (entry.expr.* == .app and entry.expr.app.head.* == .sym and
                std.mem.eql(u8, entry.expr.app.head.sym, syms.Box))
            {
                try modal_ctx.append(args.arena, entry);
            }
        }

        const sub_tree = args.prover.search(a, modal_ctx.items, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
        const sub_tree = args.prover.search(a, args.context, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch return results.items;
        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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
    var results: std.ArrayList(Tree(SearchNode)) = .{};

    // □除去: □Aがコンテキストにある場合、Aを追加
    for (args.context) |entry| {
        if (entry.expr.* == .app and entry.expr.app.head.* == .sym and
            std.mem.eql(u8, entry.expr.app.head.sym, syms.Box) and entry.expr.app.args.len == 1)
        {
            const a = entry.expr.app.args[0];
            var new_ctx: std.ArrayList(ContextEntry) = .{};
            var buf: [32]u8 = undefined;
            const n = std.fmt.bufPrint(&buf, "{s}_unbox", .{entry.name}) catch unreachable;
            try new_ctx.append(args.arena, .{ .name = try args.arena.dupe(u8, n), .expr = a });
            for (args.context) |e2| try new_ctx.append(args.arena, e2);

            const sub_tree = args.prover.search(args.goal, new_ctx.items, args.state, try args.subst.clone(), args.depth + 1, args.limit) catch continue;
            if (search_mod.findSuccess(sub_tree) != null) {
                try results.append(args.arena, try Tree(SearchNode).leaf(args.arena, .{
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

pub fn buildRules(arena: Allocator) anyerror![]const expr_mod.CatRule {
    var rules: std.ArrayList(expr_mod.CatRule) = .{};
    const b = expr_mod.RuleBuilder.init(arena);

    // modal-K: □(A→B) → □A → □B
    try rules.append(arena, .{ .name = "modal-K", .lhs = try b.a1(try b.s(syms.Box), try b.a2(try b.s(syms.Implies), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.Implies), try b.a1(try b.s(syms.Box), try b.v("A")), try b.a1(try b.s(syms.Box), try b.v("B"))) });
    // modal-K-linear: □(A⊸B) → □A ⊸ □B
    try rules.append(arena, .{ .name = "modal-K-linear", .lhs = try b.a1(try b.s(syms.Box), try b.a2(try b.s(syms.LImplies), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.LImplies), try b.a1(try b.s(syms.Box), try b.v("A")), try b.a1(try b.s(syms.Box), try b.v("B"))) });
    // modal-T: □A → A
    try rules.append(arena, .{ .name = "modal-T", .lhs = try b.a1(try b.s(syms.Box), try b.v("A")), .rhs = try b.v("A") });
    // modal-4: □A → □□A
    try rules.append(arena, .{ .name = "modal-4", .lhs = try b.a1(try b.s(syms.Box), try b.v("A")), .rhs = try b.a1(try b.s(syms.Box), try b.a1(try b.s(syms.Box), try b.v("A"))) });
    // modal-5: ◇A → □◇A
    try rules.append(arena, .{ .name = "modal-5", .lhs = try b.a1(try b.s(syms.Diamond), try b.v("A")), .rhs = try b.a1(try b.s(syms.Box), try b.a1(try b.s(syms.Diamond), try b.v("A"))) });
    // modal-duality: ◇A ↔ ¬□¬A
    try rules.append(arena, .{ .name = "modal-duality", .lhs = try b.a1(try b.s(syms.Diamond), try b.v("A")), .rhs = try b.a2(try b.s(syms.Implies), try b.a1(try b.s(syms.Box), try b.a2(try b.s(syms.Implies), try b.v("A"), try b.s(syms.False))), try b.s(syms.False)) });
    // modal-dist-tensor: □(A⊗B) → □A ⊗ □B
    try rules.append(arena, .{ .name = "modal-dist-tensor", .lhs = try b.a1(try b.s(syms.Box), try b.a2(try b.s(syms.Tensor), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.Tensor), try b.a1(try b.s(syms.Box), try b.v("A")), try b.a1(try b.s(syms.Box), try b.v("B"))) });
    // modal-dist-forall: □(∀x.P(x)) → ∀x.□P(x)
    try rules.append(arena, .{ .name = "modal-dist-forall", .lhs = try b.a1(try b.s(syms.Box), try b.a2(try b.s(syms.Forall), try b.v("x"), try b.a1(try b.v("P"), try b.v("x")))), .rhs = try b.a2(try b.s(syms.Forall), try b.v("x"), try b.a1(try b.s(syms.Box), try b.a1(try b.v("P"), try b.v("x")))) });
    // modal-dist-sepand: □(A*B) → □A * □B
    try rules.append(arena, .{ .name = "modal-dist-sepand", .lhs = try b.a1(try b.s(syms.Box), try b.a2(try b.s(syms.SepAnd), try b.v("A"), try b.v("B"))), .rhs = try b.a2(try b.s(syms.SepAnd), try b.a1(try b.s(syms.Box), try b.v("A")), try b.a1(try b.s(syms.Box), try b.v("B"))) });
    // modal-box-G: □G(A) → G(□A)
    try rules.append(arena, .{ .name = "modal-box-G", .lhs = try b.a1(try b.s(syms.Box), try b.a1(try b.s(syms.Globally), try b.v("A"))), .rhs = try b.a1(try b.s(syms.Globally), try b.a1(try b.s(syms.Box), try b.v("A"))) });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "ModalLogic",
    .priority = 130,
    .build_rules = &buildRules,
    .goal_hooks = &goalHooks,
    .context_hooks = &contextHooks,
};
