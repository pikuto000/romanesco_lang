// ==========================================
// induction.zig
// 帰納法プラグイン (InductionPlugin.scala移植)
// InitialAlgebra帰納, base/step
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const unifier_mod = @import("../unifier.zig");
const Expr = expr_mod.Expr;
const InitialAlgebra = expr_mod.InitialAlgebra;
const ArgType = expr_mod.ArgType;
const ContextEntry = expr_mod.ContextEntry;
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const app1 = expr_mod.app1;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);
    const goal = args.goal;

    // 帰納法の深さ制限
    if (args.state.induction_depth >= args.prover.config.max_induction) return results.items;

    // ∀x. P(x) の形のゴールに対して帰納法を適用
    if (goal.* != .app or goal.app.head.* != .sym or !std.mem.eql(u8, goal.app.head.sym, "∀")) return results.items;
    if (goal.app.args.len < 2 or goal.app.args[0].* != .var_) return results.items;

    const v_name = goal.app.args[0].var_;
    const body = if (goal.app.args.len == 3) goal.app.args[2] else goal.app.args[1];
    const type_expr = if (goal.app.args.len == 3) goal.app.args[1] else null;

    // 型に一致する代数を探す
    for (args.prover.config.algebras) |algebra| {
        const type_matches = if (type_expr) |te| (te.* == .sym and std.mem.eql(u8, te.sym, algebra.name)) else false;
        if (!type_matches) continue;

        // 各コンストラクタについてケースを証明
        var all_cases_ok = true;
        for (algebra.constructors) |ctor| {
            // コンストラクタの引数を生成
            const ctor_sym = try sym(args.arena, ctor.symbol);
            var ctor_args = std.ArrayList(*const Expr).init(args.arena);
            var new_ctx = std.ArrayList(ContextEntry).init(args.arena);
            for (args.context) |entry| try new_ctx.append(entry);

            for (ctor.arg_types, 0..) |arg_type, i| {
                var buf: [32]u8 = undefined;
                const arg_name = try std.fmt.bufPrint(&buf, "{s}{d}", .{ algebra.var_prefix, i });
                const arg_name_owned = try args.arena.dupe(u8, arg_name);
                const arg_var = try var_(args.arena, arg_name_owned);
                try ctor_args.append(arg_var);

                if (arg_type == .recursive) {
                    // 帰納法の仮定を追加
                    const ih = try unifier_mod.substVar(body, v_name, arg_var, args.arena);
                    var buf2: [32]u8 = undefined;
                    const ih_name = try std.fmt.bufPrint(&buf2, "ih_{s}{d}", .{ algebra.var_prefix, i });
                    try new_ctx.append(.{ .name = try args.arena.dupe(u8, ih_name), .expr = ih });
                }
            }

            // コンストラクタ適用
            const ctor_app = if (ctor_args.items.len == 0)
                ctor_sym
            else
                try expr_mod.app(args.arena, ctor_sym, ctor_args.items);

            // body[v/ctor(args)] を証明
            const instantiated = try unifier_mod.substVar(body, v_name, ctor_app, args.arena);

            const new_state = expr_mod.LogicState{
                .linear_context = args.state.linear_context,
                .meta_counter = args.state.meta_counter,
                .induction_depth = args.state.induction_depth + 1,
                .raa_depth = args.state.raa_depth,
            };

            const sub_tree = args.prover.search(instantiated, new_ctx.items, new_state, args.subst, args.depth + 1, args.limit) catch {
                all_cases_ok = false;
                break;
            };
            if (search_mod.findSuccess(sub_tree) == null) {
                all_cases_ok = false;
                break;
            }
        }

        if (all_cases_ok) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = "induction",
                .rule_name = "induction",
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "Induction",
    .priority = 180,
    .goal_hooks = &goalHooks,
};
