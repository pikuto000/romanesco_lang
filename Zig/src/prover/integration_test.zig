// ==========================================
// integration_test.zig
// End-to-end証明テスト (SolverTests移植)
// 全プラグインを使った統合テスト
// ==========================================

const std = @import("std");
const expr_mod = @import("expr.zig");
const search_mod = @import("search.zig");
const plugin_mod = @import("plugin.zig");
const rewriter_mod = @import("rewriter.zig");
const unifier_mod = @import("unifier.zig");
const parser_mod = @import("parser.zig");
const rules_mod = @import("rules.zig");
const syms = @import("symbols.zig");

const Expr = expr_mod.Expr;
const ProverConfig = expr_mod.ProverConfig;
const ProverEngine = search_mod.ProverEngine;
const Plugin = search_mod.Plugin;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const meta = expr_mod.meta;
const app = expr_mod.app;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

/// ヘルパー: 証明が成功することを検証
fn expectProved(goal: *const Expr, max_depth: u32, plugins: []const Plugin, config: ProverConfig, arena: std.mem.Allocator) !void {
    var engine = ProverEngine.init(config, plugins, arena, std.testing.allocator);
    defer engine.deinit();
    const result = engine.prove(goal, max_depth, 5000) catch |err| {
        std.debug.print("Prove error: {}\n", .{err});
        return error.TestUnexpectedResult;
    };
    if (result != .success) {
        std.debug.print("Expected proof to succeed for goal, but got failure: {s}\n", .{result.fail.reason});
        return error.TestUnexpectedResult;
    }
}

/// ヘルパー: 証明が失敗することを検証
fn expectNotProved(goal: *const Expr, max_depth: u32, plugins: []const Plugin, config: ProverConfig, arena: std.mem.Allocator) !void {
    var engine = ProverEngine.init(config, plugins, arena, std.testing.allocator);
    defer engine.deinit();
    const result = engine.prove(goal, max_depth, 5000) catch return; // timeout = not proved
    if (result == .success) {
        return error.TestUnexpectedResult;
    }
}

// ==========================================
// 基本論理テスト
// ==========================================

test "⊤ (True introduction)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const goal = try sym(arena, "⊤");
    try expectProved(goal, 3, &plugin_mod.all_plugins, .{}, arena);
}

test "A → A (implication reflexivity)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const impl = try sym(arena, syms.Implies);
    const goal = try app2(arena, impl, a, a);
    try expectProved(goal, 5, &plugin_mod.all_plugins, .{}, arena);
}

test "A → (B → A) (weakening)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const impl = try sym(arena, syms.Implies);
    const b_impl_a = try app2(arena, impl, b, a);
    const goal = try app2(arena, impl, a, b_impl_a);
    try expectProved(goal, 5, &plugin_mod.all_plugins, .{}, arena);
}

test "(A ∧ B) → A (conjunction elimination left)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const and_sym = try sym(arena, syms.And);
    const impl = try sym(arena, syms.Implies);
    const a_and_b = try app2(arena, and_sym, a, b);
    const goal = try app2(arena, impl, a_and_b, a);
    try expectProved(goal, 10, &plugin_mod.all_plugins, .{}, arena);
}

test "(A ∧ B) → (B ∧ A) (conjunction commutativity)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const and_sym = try sym(arena, syms.And);
    const impl = try sym(arena, syms.Implies);
    const a_and_b = try app2(arena, and_sym, a, b);
    const b_and_a = try app2(arena, and_sym, b, a);
    const goal = try app2(arena, impl, a_and_b, b_and_a);
    try expectProved(goal, 10, &plugin_mod.all_plugins, .{}, arena);
}

test "A → (A ∨ B) (disjunction introduction)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const or_sym = try sym(arena, syms.Or);
    const impl = try sym(arena, syms.Implies);
    const a_or_b = try app2(arena, or_sym, a, b);
    const goal = try app2(arena, impl, a, a_or_b);
    try expectProved(goal, 10, &plugin_mod.all_plugins, .{}, arena);
}

test "a = a (reflexivity)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "a");
    const eq = try sym(arena, "=");
    const goal = try app2(arena, eq, a, a);
    try expectProved(goal, 3, &plugin_mod.all_plugins, .{}, arena);
}

test "⊥ → A (explosion / ex falso)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const bot = try sym(arena, "⊥");
    const impl = try sym(arena, syms.Implies);
    const goal = try app2(arena, impl, bot, a);
    try expectProved(goal, 5, &plugin_mod.all_plugins, .{}, arena);
}

// ==========================================
// 高階論理テスト
// ==========================================

test "(A → B) → (B → C) → (A → C) (syllogism)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const c = try sym(arena, "C");
    const impl = try sym(arena, syms.Implies);
    const a_impl_b = try app2(arena, impl, a, b);
    const b_impl_c = try app2(arena, impl, b, c);
    const a_impl_c = try app2(arena, impl, a, c);
    const inner = try app2(arena, impl, b_impl_c, a_impl_c);
    const goal = try app2(arena, impl, a_impl_b, inner);
    try expectProved(goal, 10, &plugin_mod.all_plugins, .{}, arena);
}

test "(A ∧ (A → B)) → B (modus ponens)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const impl = try sym(arena, syms.Implies);
    const and_sym = try sym(arena, syms.And);
    const a_impl_b = try app2(arena, impl, a, b);
    const premise = try app2(arena, and_sym, a, a_impl_b);
    const goal = try app2(arena, impl, premise, b);
    try expectProved(goal, 10, &plugin_mod.all_plugins, .{}, arena);
}

// ==========================================
// パーサー統合テスト
// ==========================================

test "parser + prover: parse and prove ⊤" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const goal = try parser_mod.parse("⊤", arena);
    try expectProved(goal, 3, &plugin_mod.all_plugins, .{}, arena);
}

test "parser + prover: parse and prove a = a" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const goal = try parser_mod.parse("a = a", arena);
    try expectProved(goal, 3, &plugin_mod.all_plugins, .{}, arena);
}

// ==========================================
// 正規化テスト
// ==========================================

test "rewriter: A ∧ ⊤ → A" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const top = try sym(arena, "⊤");
    const and_sym = try sym(arena, syms.And);
    const a_and_top = try app2(arena, and_sym, a, top);
    const result = try rewriter_mod.normalize(a_and_top, &.{}, arena);
    try std.testing.expect(result.eql(a));
}

test "rewriter: A ∨ ⊥ → A" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const bot = try sym(arena, "⊥");
    const or_sym = try sym(arena, syms.Or);
    const a_or_bot = try app2(arena, or_sym, a, bot);
    const result = try rewriter_mod.normalize(a_or_bot, &.{}, arena);
    try std.testing.expect(result.eql(a));
}

test "rewriter: A ∧ ⊥ → ⊥" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const bot = try sym(arena, "⊥");
    const and_sym = try sym(arena, syms.And);
    const a_and_bot = try app2(arena, and_sym, a, bot);
    const result = try rewriter_mod.normalize(a_and_bot, &.{}, arena);
    try std.testing.expect(result.* == .sym and std.mem.eql(u8, result.sym, "⊥"));
}

test "plugin rewrite hook: HoTT inv(refl) → refl" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var engine = ProverEngine.init(.{}, &plugin_mod.all_plugins, arena, std.testing.allocator);
    defer engine.deinit();

    // inv(refl(a))
    const a = try sym(arena, "a");
    const refl_a = try app1(arena, try sym(arena, syms.Refl), a);
    const inv_refl_a = try app1(arena, try sym(arena, "inv"), refl_a);

    const result = try engine.normalize(inv_refl_a);

    // refl(a) に簡約されるはず
    try std.testing.expect(result.eql(refl_a));
}

test "plugin rewrite hook: List fmap(f, nil) → nil" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var engine = ProverEngine.init(.{}, &plugin_mod.all_plugins, arena, std.testing.allocator);
    defer engine.deinit();

    // fmap(f, nil)
    const f = try sym(arena, "f");
    const nil = try sym(arena, "nil");
    const fmap_f_nil = try app2(arena, try sym(arena, "fmap"), f, nil);

    const result = try engine.normalize(fmap_f_nil);

    // nil に簡約されるはず
    try std.testing.expect(result.eql(nil));
}

// ==========================================
// ユニフィケーションテスト
// ==========================================

test "unification: meta unifies with any term" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const m = try meta(arena, 1);
    const a = try sym(arena, "A");
    const subst = expr_mod.Subst.init(arena);
    const result = try unifier_mod.unify(m, a, subst, arena);
    try std.testing.expect(result.first() != null);
}

test "unification: f(X) unifies with f(a)" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const f = try sym(arena, "f");
    const m = try meta(arena, 1);
    const a = try sym(arena, "a");
    const fx = try app1(arena, f, m);
    const fa = try app1(arena, f, a);
    const subst = expr_mod.Subst.init(arena);
    const result = try unifier_mod.unify(fx, fa, subst, arena);
    const s = result.first();
    try std.testing.expect(s != null);
    // メタ変数1がaに束縛されているはず
    const bound = s.?.get(expr_mod.MetaId{ .ids = &[_]i32{1} });
    try std.testing.expect(bound != null);
    try std.testing.expect(bound.?.eql(a));
}

// ==========================================
// ルールビルダーテスト
// ==========================================

test "rules: build standard rules" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const all_rules = try rules_mod.buildAll(arena);
    try std.testing.expect(all_rules.len >= 10);
}

// ==========================================
// ルール付き証明テスト
// ==========================================

test "prove with rules: ⊤" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const all_rules = try rules_mod.buildAll(arena);
    const config = ProverConfig{ .rules = all_rules };
    const goal = try sym(arena, "⊤");
    try expectProved(goal, 3, &plugin_mod.all_plugins, config, arena);
}

test "prove with rules: A → A" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const all_rules = try rules_mod.buildAll(arena);
    const config = ProverConfig{ .rules = all_rules };

    const a = try sym(arena, "A");
    const impl = try sym(arena, syms.Implies);
    const goal = try app2(arena, impl, a, a);
    try expectProved(goal, 5, &plugin_mod.all_plugins, config, arena);
}
