const std = @import("std");
const prover = @import("prover.zig");
const expr_mod = prover.expr;
const search_mod = prover.search;
const plugin_mod = prover.plugin;
const rules_mod = prover.rules;
const parser_mod = prover.parser;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const builder = rules_mod.RuleBuilder.init(arena);
    const raw_rules = try builder.buildAll();
    
    // Filter out expansion rules
    var rules_list: std.ArrayList(expr_mod.CatRule) = .{};
    for (raw_rules) |rule| {
        if (std.mem.eql(u8, rule.name, "eq-refl") or std.mem.eql(u8, rule.name, "path-refl")) continue;
        try rules_list.append(arena, rule);
    }
    const config = expr_mod.ProverConfig{ .rules = rules_list.items };

    // inv(refl(a))
    const goal_str = "inv(refl(a))";
    const goal_expr = try parser_mod.parse(goal_str, arena);
    
    var engine = search_mod.ProverEngine.init(config, &plugin_mod.all_plugins, arena, allocator);
    defer engine.deinit();
    
    const normalized = try engine.normalize(goal_expr);
    
    std.debug.print("Goal: {s}\n", .{goal_str});
    if (normalized.* == .app and normalized.app.head.* == .sym) {
        std.debug.print("Normalized head: {s}\n", .{normalized.app.head.sym});
    } else if (normalized.* == .sym) {
        std.debug.print("Normalized sym: {s}\n", .{normalized.sym});
    } else {
        std.debug.print("Normalized type: {any}\n", .{@as(expr_mod.Expr, normalized.*)});
    }
    
    const expected_str = "refl(a)";
    const expected_expr = try parser_mod.parse(expected_str, arena);
    
    if (normalized.eql(expected_expr)) {
        std.debug.print("SUCCESS: inv(refl(a)) -> refl(a)\n", .{});
    } else {
        std.debug.print("FAILURE: inv(refl(a)) did not reduce to refl(a)\n", .{});
    }

    // Test the whole equality proof
    const eq_goal_str = "inv(refl(a)) = refl(a)";
    const eq_goal_expr = try parser_mod.parse(eq_goal_str, arena);
    const result = try engine.prove(eq_goal_expr, 5, 5000);
    
    if (result == .success) {
        std.debug.print("Equality Proof SUCCESS\n", .{});
    } else {
        std.debug.print("Equality Proof FAILURE: {s}\n", .{result.fail.reason});
    }
}
