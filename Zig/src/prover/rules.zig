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

    fn a4(self: RuleBuilder, head: *const Expr, a1_: *const Expr, a2_: *const Expr, a3_: *const Expr, a4_: *const Expr) !*const Expr {
        return expr_mod.app(self.arena, head, &[_]*const Expr{ a1_, a2_, a3_, a4_ });
    }

    /// 全標準ルール（非古典論理）を構築して返す
    pub fn buildAll(self: RuleBuilder) ![]const CatRule {
        var rules: std.ArrayList(CatRule) = .{};

        // ==========================================
        // 圏論基本
        // ==========================================
        try rules.append(self.arena, .{ .name = "id-left", .lhs = try self.a2(try self.s("∘"), try self.s("id"), try self.v("f")), .rhs = try self.v("f") });
        try rules.append(self.arena, .{ .name = "id-right", .lhs = try self.a2(try self.s("∘"), try self.v("f"), try self.s("id")), .rhs = try self.v("f") });
        try rules.append(self.arena, .{ .name = "comp-assoc", .lhs = try self.a2(try self.s("∘"), try self.a2(try self.s("∘"), try self.v("f"), try self.v("g")), try self.v("h")), .rhs = try self.a2(try self.s("∘"), try self.v("f"), try self.a2(try self.s("∘"), try self.v("g"), try self.v("h"))) });

        // ==========================================
        // 積 (Products)
        // ==========================================
        try rules.append(self.arena, .{ .name = "fst-β", .lhs = try self.a1(try self.s("pi1"), try self.a2(try self.s("pair"), try self.v("a"), try self.v("b"))), .rhs = try self.v("a") });
        try rules.append(self.arena, .{ .name = "snd-β", .lhs = try self.a1(try self.s("pi2"), try self.a2(try self.s("pair"), try self.v("a"), try self.v("b"))), .rhs = try self.v("b") });
        try rules.append(self.arena, .{ .name = "product-η", .lhs = try self.a2(try self.s("pair"), try self.a1(try self.s("pi1"), try self.v("p")), try self.a1(try self.s("pi2"), try self.v("p"))), .rhs = try self.v("p") });

        // ==========================================
        // 余積 (Coproducts)
        // ==========================================
        try rules.append(self.arena, .{ .name = "case-inl-β", .lhs = try self.a3(try self.s("case"), try self.a1(try self.s("inl"), try self.v("x")), try self.v("f"), try self.v("g")), .rhs = try self.a1(try self.v("f"), try self.v("x")) });
        try rules.append(self.arena, .{ .name = "case-inr-β", .lhs = try self.a3(try self.s("case"), try self.a1(try self.s("inr"), try self.v("y")), try self.v("f"), try self.v("g")), .rhs = try self.a1(try self.v("g"), try self.v("y")) });

        // ==========================================
        // 指数対象 (Exponentials)
        // ==========================================
        // λ-η: λx.f(x) → f
        try rules.append(self.arena, .{ .name = "lambda-η", .lhs = try self.a2(try self.s("λ"), try self.v("x"), try self.a1(try self.v("f"), try self.v("x"))), .rhs = try self.v("f") });

        // ==========================================
        // 等式
        // ==========================================
        try rules.append(self.arena, .{ .name = "eq-refl", .lhs = try self.a1(try self.s("refl"), try self.v("a")), .rhs = try self.a2(try self.s("="), try self.v("a"), try self.v("a")) });

        // ==========================================
        // 論理射 (Logic-Category Curry-Howard-Lambek)
        // ==========================================
        try rules.append(self.arena, .{ .name = "and-is-×", .lhs = try self.a2(try self.s("∧"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("×"), try self.v("A"), try self.v("B")) });
        try rules.append(self.arena, .{ .name = "or-is-+", .lhs = try self.a2(try self.s("∨"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("+"), try self.v("A"), try self.v("B")) });
        try rules.append(self.arena, .{ .name = "→-is-^", .lhs = try self.a2(try self.s("→"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("^"), try self.v("B"), try self.v("A")) });
        try rules.append(self.arena, .{ .name = "⊤-is-1", .lhs = try self.s("⊤"), .rhs = try self.s("1") });
        try rules.append(self.arena, .{ .name = "⊥-is-0", .lhs = try self.s("⊥"), .rhs = try self.s("0") });

        // ==========================================
        // 線形論理
        // ==========================================
        try rules.append(self.arena, .{ .name = "tensor-is-×", .lhs = try self.a2(try self.s("⊗"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("×"), try self.v("A"), try self.v("B")) });
        try rules.append(self.arena, .{ .name = "⊸-is-^", .lhs = try self.a2(try self.s("⊸"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("^"), try self.v("B"), try self.v("A")) });
        try rules.append(self.arena, .{ .name = "linear-bang-elim", .lhs = try self.a1(try self.s("!"), try self.v("A")), .rhs = try self.v("A") });

        // 分離論理
        try rules.append(self.arena, .{ .name = "sep-and-comm", .lhs = try self.a2(try self.s("*"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("*"), try self.v("B"), try self.v("A")) });

        // ==========================================
        // モーダル論理
        // ==========================================
        // modal-K: □(A→B) → □A → □B
        try rules.append(self.arena, .{ .name = "modal-K", .lhs = try self.a1(try self.s("□"), try self.a2(try self.s("→"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("→"), try self.a1(try self.s("□"), try self.v("A")), try self.a1(try self.s("□"), try self.v("B"))) });
        // modal-K-linear: □(A⊸B) → □A ⊸ □B
        try rules.append(self.arena, .{ .name = "modal-K-linear", .lhs = try self.a1(try self.s("□"), try self.a2(try self.s("⊸"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("⊸"), try self.a1(try self.s("□"), try self.v("A")), try self.a1(try self.s("□"), try self.v("B"))) });
        // modal-T: □A → A
        try rules.append(self.arena, .{ .name = "modal-T", .lhs = try self.a1(try self.s("□"), try self.v("A")), .rhs = try self.v("A") });
        // modal-4: □A → □□A
        try rules.append(self.arena, .{ .name = "modal-4", .lhs = try self.a1(try self.s("□"), try self.v("A")), .rhs = try self.a1(try self.s("□"), try self.a1(try self.s("□"), try self.v("A"))) });
        // modal-5: ◇A → □◇A
        try rules.append(self.arena, .{ .name = "modal-5", .lhs = try self.a1(try self.s("◇"), try self.v("A")), .rhs = try self.a1(try self.s("□"), try self.a1(try self.s("◇"), try self.v("A"))) });
        // modal-duality: ◇A ↔ ¬□¬A
        try rules.append(self.arena, .{ .name = "modal-duality", .lhs = try self.a1(try self.s("◇"), try self.v("A")), .rhs = try self.a2(try self.s("→"), try self.a1(try self.s("□"), try self.a2(try self.s("→"), try self.v("A"), try self.s("⊥"))), try self.s("⊥")) });
        // modal-dist-tensor: □(A⊗B) → □A ⊗ □B
        try rules.append(self.arena, .{ .name = "modal-dist-tensor", .lhs = try self.a1(try self.s("□"), try self.a2(try self.s("⊗"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("⊗"), try self.a1(try self.s("□"), try self.v("A")), try self.a1(try self.s("□"), try self.v("B"))) });
        // modal-dist-forall: □(∀x.P(x)) → ∀x.□P(x)
        try rules.append(self.arena, .{ .name = "modal-dist-forall", .lhs = try self.a1(try self.s("□"), try self.a2(try self.s("∀"), try self.v("x"), try self.a1(try self.v("P"), try self.v("x")))), .rhs = try self.a2(try self.s("∀"), try self.v("x"), try self.a1(try self.s("□"), try self.a1(try self.v("P"), try self.v("x")))) });
        // modal-dist-sepand: □(A*B) → □A * □B
        try rules.append(self.arena, .{ .name = "modal-dist-sepand", .lhs = try self.a1(try self.s("□"), try self.a2(try self.s("*"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("*"), try self.a1(try self.s("□"), try self.v("A")), try self.a1(try self.s("□"), try self.v("B"))) });
        // modal-box-G: □G(A) → G(□A)
        try rules.append(self.arena, .{ .name = "modal-box-G", .lhs = try self.a1(try self.s("□"), try self.a1(try self.s("G"), try self.v("A"))), .rhs = try self.a1(try self.s("G"), try self.a1(try self.s("□"), try self.v("A"))) });
        // G-modal-K-linear: G(□(A⊸B)) → G(□A ⊸ □B)
        try rules.append(self.arena, .{ .name = "G-modal-K-linear", .lhs = try self.a1(try self.s("G"), try self.a1(try self.s("□"), try self.a2(try self.s("⊸"), try self.v("A"), try self.v("B")))), .rhs = try self.a1(try self.s("G"), try self.a2(try self.s("⊸"), try self.a1(try self.s("□"), try self.v("A")), try self.a1(try self.s("□"), try self.v("B")))) });

        // ==========================================
        // 時相論理
        // ==========================================
        // G-dist-tensor: G(A⊗B) → G(A) ⊗ G(B)
        try rules.append(self.arena, .{ .name = "G-dist-tensor", .lhs = try self.a1(try self.s("G"), try self.a2(try self.s("⊗"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("⊗"), try self.a1(try self.s("G"), try self.v("A")), try self.a1(try self.s("G"), try self.v("B"))) });
        // G-dist-sepand: G(A*B) → G(A) * G(B)
        try rules.append(self.arena, .{ .name = "G-dist-sepand", .lhs = try self.a1(try self.s("G"), try self.a2(try self.s("*"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("*"), try self.a1(try self.s("G"), try self.v("A")), try self.a1(try self.s("G"), try self.v("B"))) });
        // G-dist-limplies: G(A⊸B) → G(A) ⊸ G(B)
        try rules.append(self.arena, .{ .name = "G-dist-limplies", .lhs = try self.a1(try self.s("G"), try self.a2(try self.s("⊸"), try self.v("A"), try self.v("B"))), .rhs = try self.a2(try self.s("⊸"), try self.a1(try self.s("G"), try self.v("A")), try self.a1(try self.s("G"), try self.v("B"))) });
        // F-expansion: F(A) → A ∨ X(F(A))
        try rules.append(self.arena, .{ .name = "F-expansion", .lhs = try self.a1(try self.s("F"), try self.v("A")), .rhs = try self.a2(try self.s("∨"), try self.v("A"), try self.a1(try self.s("X"), try self.a1(try self.s("F"), try self.v("A")))) });
        // U-expansion: U(A,B) → B ∨ (A ∧ X(U(A,B)))
        try rules.append(self.arena, .{ .name = "U-expansion", .lhs = try self.a2(try self.s("U"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("∨"), try self.v("B"), try self.a2(try self.s("∧"), try self.v("A"), try self.a1(try self.s("X"), try self.a2(try self.s("U"), try self.v("A"), try self.v("B"))))) });

        // ==========================================
        // HoTT (Homotopy Type Theory)
        // ==========================================
        // path-refl: refl(a) → path(A, a, a)
        try rules.append(self.arena, .{ .name = "path-refl", .lhs = try self.a1(try self.s("refl"), try self.v("a")), .rhs = try self.a3(try self.s("path"), try self.v("A"), try self.v("a"), try self.v("a")) });
        // path-inv: inv(path(A,a,b)) → path(A,b,a)
        try rules.append(self.arena, .{ .name = "path-inv", .lhs = try self.a1(try self.s("inv"), try self.a3(try self.s("path"), try self.v("A"), try self.v("a"), try self.v("b"))), .rhs = try self.a3(try self.s("path"), try self.v("A"), try self.v("b"), try self.v("a")) });
        // univalence: equiv(A,B) → path(Type,A,B)
        try rules.append(self.arena, .{ .name = "univalence", .lhs = try self.a2(try self.s("equiv"), try self.v("A"), try self.v("B")), .rhs = try self.a3(try self.s("path"), try self.s("Type"), try self.v("A"), try self.v("B")) });
        // path-to-equiv: path(Type,A,B) → equiv(A,B)
        try rules.append(self.arena, .{ .name = "path-to-equiv", .lhs = try self.a3(try self.s("path"), try self.s("Type"), try self.v("A"), try self.v("B")), .rhs = try self.a2(try self.s("equiv"), try self.v("A"), try self.v("B")) });
        // path-concat: concat(path(A,a,b), path(A,b,c)) → path(A,a,c)
        try rules.append(self.arena, .{ .name = "path-concat", .lhs = try self.a2(try self.s("concat"), try self.a3(try self.s("path"), try self.v("A"), try self.v("a"), try self.v("b")), try self.a3(try self.s("path"), try self.v("A"), try self.v("b"), try self.v("c"))), .rhs = try self.a3(try self.s("path"), try self.v("A"), try self.v("a"), try self.v("c")) });
        // concat-to-comp: concat(p,q) → comp(A, p, q)
        try rules.append(self.arena, .{ .name = "concat-to-comp", .lhs = try self.a2(try self.s("concat"), try self.v("p"), try self.v("q")), .rhs = try self.a3(try self.s("comp"), try self.v("A"), try self.v("p"), try self.v("q")), .universals = &.{} });

        // ==========================================
        // HoTT特性 (prop/set)
        // ==========================================
        // prop-path: isProp(A) → path(A,x,y)
        try rules.append(self.arena, .{ .name = "prop-path", .lhs = try self.a1(try self.s("isProp"), try self.v("A")), .rhs = try self.a3(try self.s("path"), try self.v("A"), try self.v("x"), try self.v("y")) });
        // set-path: isSet(A) → path(path(A,x,y),p,q)
        try rules.append(self.arena, .{ .name = "set-path", .lhs = try self.a1(try self.s("isSet"), try self.v("A")), .rhs = try self.a3(try self.s("path"), try self.a3(try self.s("path"), try self.v("A"), try self.v("x"), try self.v("y")), try self.v("p"), try self.v("q")) });
        // prop-to-set: isProp(A) → isSet(A)
        try rules.append(self.arena, .{ .name = "prop-to-set", .lhs = try self.a1(try self.s("isProp"), try self.v("A")), .rhs = try self.a1(try self.s("isSet"), try self.v("A")) });
        // prop-prod: isProp(A) ∧ isProp(B) → isProp(A×B)
        try rules.append(self.arena, .{ .name = "prop-prod", .lhs = try self.a2(try self.s("∧"), try self.a1(try self.s("isProp"), try self.v("A")), try self.a1(try self.s("isProp"), try self.v("B"))), .rhs = try self.a1(try self.s("isProp"), try self.a2(try self.s("×"), try self.v("A"), try self.v("B"))) });

        // ==========================================
        // Cubical Type Theory
        // ==========================================
        // hcomp-trivial: hcomp(A, I1, u, u0) → u(I1)
        try rules.append(self.arena, .{ .name = "hcomp-trivial", .lhs = try self.a4(try self.s("hcomp"), try self.v("A"), try self.s("I1"), try self.v("u"), try self.v("u0")), .rhs = try self.a1(try self.v("u"), try self.s("I1")) });
        // hcomp-base: hcomp(A, I0, u, u0) → u0
        try rules.append(self.arena, .{ .name = "hcomp-base", .lhs = try self.a4(try self.s("hcomp"), try self.v("A"), try self.s("I0"), try self.v("u"), try self.v("u0")), .rhs = try self.v("u0") });
        // fill-to-hcomp: fill(A, p, u0) → hcomp(A, φ, u, u0)
        try rules.append(self.arena, .{ .name = "fill-to-hcomp", .lhs = try self.a3(try self.s("fill"), try self.v("A"), try self.v("p"), try self.v("u0")), .rhs = try self.a4(try self.s("hcomp"), try self.v("A"), try self.v("φ"), try self.v("u"), try self.v("u0")) });

        // ==========================================
        // 区間代数 (Face Algebra)
        // ==========================================
        try rules.append(self.arena, .{ .name = "face-and-i1", .lhs = try self.a2(try self.s("∧ᶠ"), try self.s("I1"), try self.v("i")), .rhs = try self.v("i") });
        try rules.append(self.arena, .{ .name = "face-and-i0", .lhs = try self.a2(try self.s("∧ᶠ"), try self.s("I0"), try self.v("i")), .rhs = try self.s("I0") });
        try rules.append(self.arena, .{ .name = "face-or-i0", .lhs = try self.a2(try self.s("∨ᶠ"), try self.s("I0"), try self.v("i")), .rhs = try self.v("i") });
        try rules.append(self.arena, .{ .name = "face-or-i1", .lhs = try self.a2(try self.s("∨ᶠ"), try self.s("I1"), try self.v("i")), .rhs = try self.s("I1") });
        try rules.append(self.arena, .{ .name = "face-neg-i0", .lhs = try self.a1(try self.s("¬ᶠ"), try self.s("I0")), .rhs = try self.s("I1") });
        try rules.append(self.arena, .{ .name = "face-neg-i1", .lhs = try self.a1(try self.s("¬ᶠ"), try self.s("I1")), .rhs = try self.s("I0") });
        try rules.append(self.arena, .{ .name = "face-neg-neg", .lhs = try self.a1(try self.s("¬ᶠ"), try self.a1(try self.s("¬ᶠ"), try self.v("i"))), .rhs = try self.v("i") });
        // ド・モルガン
        try rules.append(self.arena, .{ .name = "face-dm-and", .lhs = try self.a1(try self.s("¬ᶠ"), try self.a2(try self.s("∧ᶠ"), try self.v("i"), try self.v("j"))), .rhs = try self.a2(try self.s("∨ᶠ"), try self.a1(try self.s("¬ᶠ"), try self.v("i")), try self.a1(try self.s("¬ᶠ"), try self.v("j"))) });
        try rules.append(self.arena, .{ .name = "face-dm-or", .lhs = try self.a1(try self.s("¬ᶠ"), try self.a2(try self.s("∨ᶠ"), try self.v("i"), try self.v("j"))), .rhs = try self.a2(try self.s("∧ᶠ"), try self.a1(try self.s("¬ᶠ"), try self.v("i")), try self.a1(try self.s("¬ᶠ"), try self.v("j"))) });

        // ==========================================
        // 自然数演算
        // ==========================================
        try rules.append(self.arena, .{ .name = "plus_0", .lhs = try self.a2(try self.s("plus"), try self.s("0"), try self.v("n")), .rhs = try self.v("n") });
        try rules.append(self.arena, .{ .name = "plus_S", .lhs = try self.a2(try self.s("plus"), try self.a1(try self.s("S"), try self.v("n")), try self.v("m")), .rhs = try self.a1(try self.s("S"), try self.a2(try self.s("plus"), try self.v("n"), try self.v("m"))) });
        try rules.append(self.arena, .{ .name = "plus_n_0", .lhs = try self.a2(try self.s("plus"), try self.v("n"), try self.s("0")), .rhs = try self.v("n") });
        try rules.append(self.arena, .{ .name = "plus_n_S", .lhs = try self.a2(try self.s("plus"), try self.v("n"), try self.a1(try self.s("S"), try self.v("m"))), .rhs = try self.a1(try self.s("S"), try self.a2(try self.s("plus"), try self.v("n"), try self.v("m"))) });

        // ==========================================
        // リスト・ツリー演算
        // ==========================================
        try rules.append(self.arena, .{ .name = "append_nil", .lhs = try self.a2(try self.s("append"), try self.s("nil"), try self.v("ys")), .rhs = try self.v("ys") });
        try rules.append(self.arena, .{ .name = "append_cons", .lhs = try self.a2(try self.s("append"), try self.a2(try self.s("cons"), try self.v("x"), try self.v("xs")), try self.v("ys")), .rhs = try self.a2(try self.s("cons"), try self.v("x"), try self.a2(try self.s("append"), try self.v("xs"), try self.v("ys"))) });
        try rules.append(self.arena, .{ .name = "append_nil_r", .lhs = try self.a2(try self.s("append"), try self.v("xs"), try self.s("nil")), .rhs = try self.v("xs") });
        try rules.append(self.arena, .{ .name = "reverse_nil", .lhs = try self.a1(try self.s("reverse"), try self.s("nil")), .rhs = try self.s("nil") });
        try rules.append(self.arena, .{ .name = "reverse_cons", .lhs = try self.a1(try self.s("reverse"), try self.a2(try self.s("cons"), try self.v("x"), try self.v("xs"))), .rhs = try self.a2(try self.s("append"), try self.a1(try self.s("reverse"), try self.v("xs")), try self.a2(try self.s("cons"), try self.v("x"), try self.s("nil"))) });
        try rules.append(self.arena, .{ .name = "reverse_append", .lhs = try self.a1(try self.s("reverse"), try self.a2(try self.s("append"), try self.v("xs"), try self.v("ys"))), .rhs = try self.a2(try self.s("append"), try self.a1(try self.s("reverse"), try self.v("ys")), try self.a1(try self.s("reverse"), try self.v("xs"))) });
        try rules.append(self.arena, .{ .name = "mirror_leaf", .lhs = try self.a1(try self.s("mirror"), try self.s("leaf")), .rhs = try self.s("leaf") });
        try rules.append(self.arena, .{ .name = "mirror_node", .lhs = try self.a1(try self.s("mirror"), try self.a3(try self.s("node"), try self.v("l"), try self.v("val"), try self.v("r"))), .rhs = try self.a3(try self.s("node"), try self.a1(try self.s("mirror"), try self.v("r")), try self.v("val"), try self.a1(try self.s("mirror"), try self.v("l"))) });
        try rules.append(self.arena, .{ .name = "list_prop_nil", .lhs = try self.a1(try self.s("list_prop"), try self.s("nil")), .rhs = try self.s("⊤") });
        try rules.append(self.arena, .{ .name = "list_prop_cons", .lhs = try self.a1(try self.s("list_prop"), try self.a2(try self.s("cons"), try self.v("x"), try self.v("xs"))), .rhs = try self.a1(try self.s("list_prop"), try self.v("xs")) });
        try rules.append(self.arena, .{ .name = "list_prop_append", .lhs = try self.a1(try self.s("list_prop"), try self.a2(try self.s("append"), try self.v("xs"), try self.v("ys"))), .rhs = try self.a2(try self.s("∧"), try self.a1(try self.s("list_prop"), try self.v("xs")), try self.a1(try self.s("list_prop"), try self.v("ys"))) });

        // ==========================================
        // リソース規則
        // ==========================================
        try rules.append(self.arena, .{ .name = "file-open", .lhs = try self.a1(try self.s("file_exists"), try self.v("f")), .rhs = try self.a1(try self.s("file_open"), try self.v("f")) });
        try rules.append(self.arena, .{ .name = "file-close", .lhs = try self.a1(try self.s("file_open"), try self.v("f")), .rhs = try self.s("⊤") });
        try rules.append(self.arena, .{ .name = "file-read", .lhs = try self.a1(try self.s("file_open"), try self.v("f")), .rhs = try self.a1(try self.s("file_open"), try self.v("f")) });
        try rules.append(self.arena, .{ .name = "memory-alloc", .lhs = try self.s("⊤"), .rhs = try self.a2(try self.s("↦"), try self.v("ptr"), try self.s("_")) });
        try rules.append(self.arena, .{ .name = "memory-free", .lhs = try self.a2(try self.s("↦"), try self.v("ptr"), try self.v("val")), .rhs = try self.s("⊤") });
        try rules.append(self.arena, .{ .name = "lock-acquire", .lhs = try self.a1(try self.s("lock_free"), try self.v("l")), .rhs = try self.a1(try self.s("lock_held"), try self.v("l")) });
        try rules.append(self.arena, .{ .name = "lock-release", .lhs = try self.a1(try self.s("lock_held"), try self.v("l")), .rhs = try self.a1(try self.s("lock_free"), try self.v("l")) });

        return rules.items;
    }

    /// 古典論理ルール（EM, DNE）
    pub fn buildClassical(self: RuleBuilder) ![]const CatRule {
        var rules: std.ArrayList(CatRule) = .{};
        // EM: ⊤ → A ∨ (A → ⊥)
        try rules.append(self.arena, .{ .name = "EM", .lhs = try self.s("⊤"), .rhs = try self.a2(try self.s("∨"), try self.v("A"), try self.a2(try self.s("→"), try self.v("A"), try self.s("⊥"))) });
        // DNE: (A → ⊥) → ⊥  →  A
        try rules.append(self.arena, .{ .name = "DNE", .lhs = try self.a2(try self.s("→"), try self.a2(try self.s("→"), try self.v("A"), try self.s("⊥")), try self.s("⊥")), .rhs = try self.v("A") });
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
    try std.testing.expect(rules.len >= 60);
    try std.testing.expectEqualStrings("id-left", rules[0].name);
}

test "RuleBuilder builds classical rules" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const builder = RuleBuilder.init(arena);
    const cls_rules = try builder.buildClassical();
    try std.testing.expect(cls_rules.len == 2);
    try std.testing.expectEqualStrings("EM", cls_rules[0].name);
    try std.testing.expectEqualStrings("DNE", cls_rules[1].name);
}
