// ==========================================
// benchmark.zig
// 証明器パフォーマンス計測
// ==========================================

const std = @import("std");
const prover = @import("prover.zig");
const expr_mod = prover.expr;
const syms = prover.symbols;
const search_mod = prover.search;
const plugin_mod = prover.plugin;
const rules_mod = prover.rules;
const parser_mod = prover.parser;

const ProverEngine = search_mod.ProverEngine;
const ProverConfig = expr_mod.ProverConfig;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var arena_state = std.heap.ArenaAllocator.init(allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    std.debug.print("=== Romanesco Prover Benchmark (Zig) ===\n\n", .{});

    // 標準ルールの準備
    const raw_rules = try rules_mod.buildAll(arena);
    
    // rewriter用のルール（簡約ではないルールを除去）
    var rules_list: std.ArrayList(expr_mod.CatRule) = .{};
    for (raw_rules) |rule| {
        // これらのルールは正規化で勝手に適用されると困る（検索用）
        const is_expansion = std.mem.eql(u8, rule.name, "eq-refl") or
            std.mem.eql(u8, rule.name, "path-refl") or
            std.mem.eql(u8, rule.name, "and-is-×") or
            std.mem.eql(u8, rule.name, "or-is-+") or
            std.mem.eql(u8, rule.name, "→-is-^") or
            std.mem.eql(u8, rule.name, "⊤-is-1") or
            std.mem.eql(u8, rule.name, "⊥-is-0") or
            std.mem.eql(u8, rule.name, "tensor-is-×") or
            std.mem.eql(u8, rule.name, "⊸-is-^");
            
        if (is_expansion) continue;
        try rules_list.append(arena, rule);
    }
    const all_rules = rules_list.items;
    const config = ProverConfig{ .rules = all_rules, .max_complexity = 500 };

    const bench_cases = [_]struct {
        name: []const u8,
        goal: []const u8,
        depth: u32,
    }{
        .{ .name = "Identity", .goal = "A → A", .depth = 5 },
        .{ .name = "Conjunction Commutativity", .goal = "(A ∧ B) → (B ∧ A)", .depth = 10 },
        .{ .name = "Syllogism", .goal = "(A → B) → (B → C) → (A → C)", .depth = 10 },
        .{ .name = "Modus Ponens", .goal = "(A ∧ (A → B)) → B", .depth = 10 },
        .{ .name = "HoTT inv(refl) reduction", .goal = "inv(refl(a)) = refl(a)", .depth = 5 },
        .{ .name = "List fmap(f, nil) reduction", .goal = "fmap(f, nil) = nil", .depth = 5 },
        .{ .name = "Distributivity", .goal = "(A ∧ (B ∨ C)) → ((A ∧ B) ∨ (A ∧ C))", .depth = 15 },
    };

    for (bench_cases) |case| {
        std.debug.print("Benchmarking: {s} (goal: {s})\n", .{ case.name, case.goal });
        
        const goal_expr = try parser_mod.parse(case.goal, arena);
        
        var timer = try std.time.Timer.start();
        
        var engine = ProverEngine.init(config, &plugin_mod.all_plugins, arena, allocator);
        defer engine.deinit();
        
        const result = try engine.prove(goal_expr, case.depth, 10000);
        const elapsed = timer.read();

        if (result == .success) {
            std.debug.print("  Result: Success\n", .{});
        } else {
            std.debug.print("  Result: Failure ({s})\n", .{result.fail.reason});
        }
        std.debug.print("  Time: {d}.{d:0>3} ms\n\n", .{ elapsed / 1_000_000, (elapsed % 1_000_000) / 1_000 });
    }
}
