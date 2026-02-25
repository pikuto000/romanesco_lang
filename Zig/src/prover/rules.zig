// ==========================================
// rules.zig
// StandardRules (StandardRules.scala移植)
// 圏論的核心規則 + 古典論理
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const syms = @import("symbols.zig");
const CatRule = expr_mod.CatRule;
const Expr = expr_mod.Expr;
const RuleBuilder = expr_mod.RuleBuilder;

/// アリーナを使って全ルールを構築する
pub fn buildAll(arena: Allocator) ![]const CatRule {
    var rules: std.ArrayList(CatRule) = .{};
    const b = RuleBuilder.init(arena);

    // ==========================================
    // 圏論基本
    // ==========================================
    try rules.append(arena, .{ .name = "id-left", .lhs = try b.a2(try b.s(syms.Compose), try b.s(syms.Id), try b.v("f")), .rhs = try b.v("f") });
    try rules.append(arena, .{ .name = "id-right", .lhs = try b.a2(try b.s(syms.Compose), try b.v("f"), try b.s(syms.Id)), .rhs = try b.v("f") });
    try rules.append(arena, .{ .name = "comp-assoc", .lhs = try b.a2(try b.s(syms.Compose), try b.a2(try b.s(syms.Compose), try b.v("f"), try b.v("g")), try b.v("h")), .rhs = try b.a2(try b.s(syms.Compose), try b.v("f"), try b.a2(try b.s(syms.Compose), try b.v("g"), try b.v("h"))) });

    // ==========================================
    // 積 (Products)
    // ==========================================
    try rules.append(arena, .{ .name = "fst-β", .lhs = try b.a1(try b.s(syms.Proj1), try b.a2(try b.s(syms.Pair), try b.v("a"), try b.v("b"))), .rhs = try b.v("a") });
    try rules.append(arena, .{ .name = "snd-β", .lhs = try b.a1(try b.s(syms.Proj2), try b.a2(try b.s(syms.Pair), try b.v("a"), try b.v("b"))), .rhs = try b.v("b") });
    try rules.append(arena, .{ .name = "product-η", .lhs = try b.a2(try b.s(syms.Pair), try b.a1(try b.s(syms.Proj1), try b.v("p")), try b.a1(try b.s(syms.Proj2), try b.v("p"))), .rhs = try b.v("p") });

    // ==========================================
    // 余積 (Coproducts)
    // ==========================================
    try rules.append(arena, .{ .name = "case-inl-β", .lhs = try b.a3(try b.s(syms.Case), try b.a1(try b.s(syms.Inl), try b.v("x")), try b.v("f"), try b.v("g")), .rhs = try b.a1(try b.v("f"), try b.v("x")) });
    try rules.append(arena, .{ .name = "case-inr-β", .lhs = try b.a3(try b.s(syms.Case), try b.a1(try b.s(syms.Inr), try b.v("y")), try b.v("f"), try b.v("g")), .rhs = try b.a1(try b.v("g"), try b.v("y")) });

    // ==========================================
    // 指数対象 (Exponentials)
    // ==========================================
    // λ-η: λx.f(x) → f
    try rules.append(arena, .{ .name = "lambda-η", .lhs = try b.a2(try b.s(syms.Lambda), try b.v("x"), try b.a1(try b.v("f"), try b.v("x"))), .rhs = try b.v("f") });

    // ==========================================
    // 等式
    // ==========================================
    try rules.append(arena, .{ .name = "eq-refl", .lhs = try b.a1(try b.s(syms.Refl), try b.v("a")), .rhs = try b.a2(try b.s(syms.Eq), try b.v("a"), try b.v("a")) });

    return rules.toOwnedSlice(arena);
}

/// 古典論理ルール（EM, DNE）
pub fn buildClassical(arena: Allocator) ![]const CatRule {
    var rules: std.ArrayList(CatRule) = .{};
    const b = RuleBuilder.init(arena);
    // EM: ⊤ → A ∨ (A → ⊥)
    try rules.append(arena, .{ .name = "EM", .lhs = try b.s(syms.True), .rhs = try b.a2(try b.s(syms.Or), try b.v("A"), try b.a2(try b.s(syms.Implies), try b.v("A"), try b.s(syms.False))) });
    // DNE: (A → ⊥) → ⊥  →  A
    try rules.append(arena, .{ .name = "DNE", .lhs = try b.a2(try b.s(syms.Implies), try b.a2(try b.s(syms.Implies), try b.v("A"), try b.s(syms.False)), try b.s(syms.False)), .rhs = try b.v("A") });
    return rules.toOwnedSlice(arena);
}

// テスト
test "buildAll returns core rules" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const rules = try buildAll(arena);
    try std.testing.expect(rules.len >= 10);
}
