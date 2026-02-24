// ==========================================
// user_rule.zig
// ユーザ定義規則プラグイン (UserRulePlugin移植)
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const SearchNode = expr_mod.SearchNode;
const Tree = expr_mod.Tree;
const Plugin = search_mod.Plugin;
const HookArgs = search_mod.HookArgs;
const HookError = search_mod.HookError;

fn goalHooks(args: HookArgs) HookError![]const Tree(SearchNode) {
    var results = std.ArrayList(Tree(SearchNode)).init(args.arena);

    // ルール適用 (バックワード推論)
    const applications = args.prover.applyRules(args.goal, args.subst) catch return results.items;

    for (applications) |ra| {
        const sub_tree = args.prover.search(
            ra.new_goal,
            args.context,
            args.state,
            ra.subst,
            args.depth + 1,
            args.limit,
        ) catch continue;

        if (search_mod.findSuccess(sub_tree) != null) {
            try results.append(try Tree(SearchNode).leaf(args.arena, .{
                .goal = ra.rule_name,
                .rule_name = ra.rule_name,
                .status = .success,
                .depth = args.depth,
            }));
        }
    }

    return results.items;
}

pub const plugin = Plugin{
    .name = "UserRule",
    .priority = 200,
    .goal_hooks = &goalHooks,
};
