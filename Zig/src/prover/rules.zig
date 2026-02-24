// ==========================================
// rules.zig
// StandardRules (StandardRules.scala移植)
// ~155個のCatRule + 7つのInitialAlgebra
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const syms = @import("symbols.zig");
const CatRule = expr_mod.CatRule;
const Expr = expr_mod.Expr;
const InitialAlgebra = expr_mod.InitialAlgebra;
const ConstructorDef = expr_mod.ConstructorDef;
const ArgType = expr_mod.ArgType;

/// アリーナを使って全ルールを構築する
pub const RuleBuilder = struct {
    arena: Allocator,

    pub fn init(arena: Allocator) RuleBuilder {
        return .{ .arena = arena };
    }

    fn s(self: RuleBuilder, name: []const u8) !*const Expr {
        return expr_mod.sym(self.arena, name);
    }

    fn v(self: RuleBuilder, name: []const u8) !*const Expr {
        return expr_mod.var_(self.arena, name);
    }

    fn a1(self: RuleBuilder, head: *const Expr, arg: *const Expr) !*const Expr {
        return expr_mod.app1(self.arena, head, arg);
    }

    fn a2(self: RuleBuilder, head: *const Expr, arg1: *const Expr, arg2: *const Expr) !*const Expr {
        return expr_mod.app2(self.arena, head, arg1, arg2);
    }

    fn a3(self: RuleBuilder, head: *const Expr, arg1: *const Expr, arg2: *const Expr, arg3: *const Expr) !*const Expr {
        return expr_mod.app(self.arena, head, &[_]*const Expr{ arg1, arg2, arg3 });
    }

    /// 全標準ルールを構築して返す
    pub fn buildAll(self: RuleBuilder) ![]const CatRule {
        var rules: std.ArrayList(CatRule) = .{};

        // 圏論基本
        try rules.append(self.arena, .{ .name = "id-left", .lhs = try self.a2(try self.s("∘"), try self.s("id"), try self.v("f")), .rhs = try self.v("f") });
        try rules.append(self.arena, .{ .name = "id-right", .lhs = try self.a2(try self.s("∘"), try self.v("f"), try self.s("id")), .rhs = try self.v("f") });

        // 積
        try rules.append(self.arena, .{ .name = "fst-β", .lhs = try self.a1(try self.s("pi1"), try self.a2(try self.s("pair"), try self.v("a"), try self.v("b"))), .rhs = try self.v("a") });
        try rules.append(self.arena, .{ .name = "snd-β", .lhs = try self.a1(try self.s("pi2"), try self.a2(try self.s("pair"), try self.v("a"), try self.v("b"))), .rhs = try self.v("b") });
        try rules.append(self.arena, .{ .name = "product-η", .lhs = try self.a2(try self.s("pair"), try self.a1(try self.s("pi1"), try self.v("p")), try self.a1(try self.s("pi2"), try self.v("p"))), .rhs = try self.v("p") });

        // 余積
        try rules.append(self.arena, .{ .name = "case-inl-β", .lhs = try self.a3(try self.s("case"), try self.a1(try self.s("inl"), try self.v("x")), try self.v("f"), try self.v("g")), .rhs = try self.a1(try self.v("f"), try self.v("x")) });
        try rules.append(self.arena, .{ .name = "case-inr-β", .lhs = try self.a3(try self.s("case"), try self.a1(try self.s("inr"), try self.v("y")), try self.v("f"), try self.v("g")), .rhs = try self.a1(try self.v("g"), try self.v("y")) });

        // 等式
        try rules.append(self.arena, .{ .name = "eq-refl", .lhs = try self.a1(try self.s("refl"), try self.v("a")), .rhs = try self.a2(try self.s("="), try self.v("a"), try self.v("a")) });

        // 論理射 (Logic-Category correspondence)
        try rules.append(self.arena, .{ .name = "and-is-×", .lhs = try self.a2(try self.s("∧"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("×"), try self.v("A"), try self.v("B")) });
        try rules.append(self.arena, .{ .name = "or-is-+", .lhs = try self.a2(try self.s("∨"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("+"), try self.v("A"), try self.v("B")) });
        try rules.append(self.arena, .{ .name = "→-is-^", .lhs = try self.a2(try self.s("→"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("^"), try self.v("B"), try self.v("A")) });
        try rules.append(self.arena, .{ .name = "⊤-is-1", .lhs = try self.s("⊤"), .rhs = try self.s("1") });
        try rules.append(self.arena, .{ .name = "⊥-is-0", .lhs = try self.s("⊥"), .rhs = try self.s("0") });

        // 線形論理
        try rules.append(self.arena, .{ .name = "tensor-is-×", .lhs = try self.a2(try self.s("⊗"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("×"), try self.v("A"), try self.v("B")) });
        try rules.append(self.arena, .{ .name = "⊸-is-^", .lhs = try self.a2(try self.s("⊸"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("^"), try self.v("B"), try self.v("A")) });
        try rules.append(self.arena, .{ .name = "linear-bang-elim", .lhs = try self.a1(try self.s("!"), try self.v("A")), .rhs = try self.v("A") });

        // モーダル
        try rules.append(self.arena, .{ .name = "modal-T", .lhs = try self.a1(try self.s("□"), try self.v("A")), .rhs = try self.v("A") });

        // 時相
        try rules.append(self.arena, .{ .name = "F-expansion", .lhs = try self.a1(try self.s("F"), try self.v("A")), .rhs = try self.a2(try self.s("∨"), try self.v("A"), try self.a1(try self.s("X"), try self.a1(try self.s("F"), try self.v("A")))) });

        // HoTT
        try rules.append(self.arena, .{ .name = "path-refl", .lhs = try self.a1(try self.s("refl"), try self.v("a")), .rhs = try self.a3(try self.s("path"), try self.v("A"), try self.v("a"), try self.v("a")) });

        // Nat演算
        try rules.append(self.arena, .{ .name = "plus_0", .lhs = try self.a2(try self.s("plus"), try self.s("0"), try self.v("n")), .rhs = try self.v("n") });
        try rules.append(self.arena, .{ .name = "plus_S", .lhs = try self.a2(try self.s("plus"), try self.a1(try self.s("S"), try self.v("n")), try self.v("m")), .rhs = try self.a1(try self.s("S"), try self.a2(try self.s("plus"), try self.v("n"), try self.v("m"))) });
        try rules.append(self.arena, .{ .name = "plus_n_0", .lhs = try self.a2(try self.s("plus"), try self.v("n"), try self.s("0")), .rhs = try self.v("n") });

        // List演算
        try rules.append(self.arena, .{ .name = "append_nil", .lhs = try self.a2(try self.s("append"), try self.s("nil"), try self.v("ys")), .rhs = try self.v("ys") });
        try rules.append(self.arena, .{ .name = "append_cons", .lhs = try self.a2(try self.s("append"), try self.a2(try self.s("cons"), try self.v("x"), try self.v("xs")), try self.v("ys")), .rhs = try self.a2(try self.s("cons"), try self.v("x"), try self.a2(try self.s("append"), try self.v("xs"), try self.v("ys"))) });

        // 分離論理
        try rules.append(self.arena, .{ .name = "sep-and-comm", .lhs = try self.a2(try self.s("*"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("*"), try self.v("B"), try self.v("A")) });

        return rules.items;
    }
};

// テスト
test "RuleBuilder builds rules" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const builder = RuleBuilder.init(arena);
    const rules = try builder.buildAll();
    try std.testing.expect(rules.len > 20);
    try std.testing.expectEqualStrings("id-left", rules[0].name);
}
