// ==========================================
// rewriter.zig
// 圏論的項書き換えエンジン (Rewriter.scala移植)
// AC正規化 + 組込み簡約規則
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const unifier = @import("unifier.zig");
const syms = @import("symbols.zig");
const Expr = expr_mod.Expr;
const CatRule = expr_mod.CatRule;
const Subst = expr_mod.Subst;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const app = expr_mod.app;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

/// 正規化: 不動点まで反復簡約
pub fn normalize(e: *const Expr, rules: []const CatRule, arena: Allocator) error{OutOfMemory}!*const Expr {
    return normalizeN(e, rules, 100, arena);
}

pub fn normalizeN(e: *const Expr, rules: []const CatRule, max_iter: u32, arena: Allocator) error{OutOfMemory}!*const Expr {
    var current = e;
    var iter: u32 = 0;
    while (iter < max_iter) : (iter += 1) {
        const reduced = try step(current, rules, arena);
        const ac_normalized = try acNormalize(reduced, arena);
        if (ac_normalized.eql(current)) return current;
        current = ac_normalized;
    }
    return current;
}

/// 1ステップの簡約
fn step(e: *const Expr, rules: []const CatRule, arena: Allocator) error{OutOfMemory}!*const Expr {
    return switch (e.*) {
        .app => |a| {
            const new_head = try step(a.head, rules, arena);
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try step(arg, rules, arena);
            }
            const new_expr = try arena.create(Expr);
            new_expr.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return rewriteRule(new_expr, rules, arena);
        },
        else => e,
    };
}

/// AC (Associative-Commutative) 正規化
/// ⊗,∧,∨,*等の可換・結合的演算子を正規化
fn acNormalize(e: *const Expr, arena: Allocator) error{OutOfMemory}!*const Expr {
    if (e.* != .app) return e;
    const a = e.app;
    if (a.head.* != .sym) {
        // 子要素を再帰的に正規化
        const new_head = try acNormalize(a.head, arena);
        const new_args = try arena.alloc(*const Expr, a.args.len);
        for (a.args, 0..) |arg, i| {
            new_args[i] = try acNormalize(arg, arena);
        }
        const result = try arena.create(Expr);
        result.* = .{ .app = .{ .head = new_head, .args = new_args } };
        return result;
    }

    const op = a.head.sym;
    const is_ac = std.mem.eql(u8, op, syms.SepAnd) or
        std.mem.eql(u8, op, syms.Tensor) or
        std.mem.eql(u8, op, syms.And) or
        std.mem.eql(u8, op, syms.Or) or
        std.mem.eql(u8, op, "plus") or
        std.mem.eql(u8, op, syms.FaceAnd) or
        std.mem.eql(u8, op, syms.FaceOr);

    if (!is_ac) {
        // 子要素を再帰的に正規化
        const new_args = try arena.alloc(*const Expr, a.args.len);
        for (a.args, 0..) |arg, i| {
            new_args[i] = try acNormalize(arg, arena);
        }
        const result = try arena.create(Expr);
        result.* = .{ .app = .{ .head = a.head, .args = new_args } };
        return result;
    }

    // フラット化: 同じ演算子の入れ子を展開
    var flattened: std.ArrayList(*const Expr) = .{};
    for (a.args) |arg| {
        try collectAC(arg, op, &flattened, arena);
    }

    const is_and = std.mem.eql(u8, op, syms.And);
    const is_or = std.mem.eql(u8, op, syms.Or);

    // 短絡評価
    if (is_and) {
        for (flattened.items) |item| {
            if (item.* == .sym and (std.mem.eql(u8, item.sym, syms.False) or std.mem.eql(u8, item.sym, "⊥"))) {
                return sym(arena, syms.False);
            }
        }
        // ⊤を除去
        var filtered: std.ArrayList(*const Expr) = .{};
        for (flattened.items) |item| {
            if (!(item.* == .sym and (std.mem.eql(u8, item.sym, syms.True) or std.mem.eql(u8, item.sym, "⊤")))) {
                try filtered.append(arena, item);
            }
        }
        flattened = filtered;
    } else if (is_or) {
        for (flattened.items) |item| {
            if (item.* == .sym and (std.mem.eql(u8, item.sym, syms.True) or std.mem.eql(u8, item.sym, "⊤"))) {
                return sym(arena, syms.True);
            }
        }
        var filtered: std.ArrayList(*const Expr) = .{};
        for (flattened.items) |item| {
            if (!(item.* == .sym and (std.mem.eql(u8, item.sym, syms.False) or std.mem.eql(u8, item.sym, "⊥")))) {
                try filtered.append(arena, item);
            }
        }
        flattened = filtered;
    }

    // 空の場合の単位元
    if (flattened.items.len == 0) {
        if (std.mem.eql(u8, op, syms.Tensor)) return sym(arena, syms.LPlus);
        if (std.mem.eql(u8, op, syms.SepAnd)) return sym(arena, syms.LOne);
        if (is_or) return sym(arena, syms.False);
        return sym(arena, syms.True);
    }
    if (flattened.items.len == 1) return flattened.items[0];

    // 左畳み込みで二項演算に戻す
    const op_sym = try sym(arena, op);
    var result = flattened.items[0];
    for (flattened.items[1..]) |item| {
        result = try app2(arena, op_sym, result, item);
    }
    return result;
}

/// AC演算子のフラット化
fn collectAC(e: *const Expr, op: []const u8, list: *std.ArrayList(*const Expr), arena: Allocator) error{OutOfMemory}!void {
    if (e.* == .app and e.app.head.* == .sym and std.mem.eql(u8, e.app.head.sym, op) and e.app.args.len == 2) {
        try collectAC(e.app.args[0], op, list, arena);
        try collectAC(e.app.args[1], op, list, arena);
    } else {
        try list.append(arena, try acNormalize(e, arena));
    }
}

/// 規則適用 (ユーザ定義 + 組込み)
fn rewriteRule(e: *const Expr, rules: []const CatRule, arena: Allocator) error{OutOfMemory}!*const Expr {
    // 優先度の高い等値チェック
    if (e.* == .app and e.app.args.len == 2) {
        const head = e.app.head;
        if (head.* == .sym) {
            const hn = head.sym;
            if ((std.mem.eql(u8, hn, "=") or std.mem.eql(u8, hn, "bisim")) and e.app.args[0].eql(e.app.args[1])) {
                return sym(arena, syms.True);
            }
            if (std.mem.eql(u8, hn, syms.Path) and e.app.args.len >= 2) {
                // path(_, l, r) where l == r
                if (e.app.args.len == 3 and e.app.args[1].eql(e.app.args[2])) {
                    return sym(arena, syms.True);
                }
            }
        }
    }

    if (rules.len == 0) return builtinRules(e, arena);

    // ユーザ定義ルールの適用 (先頭記号でフィルタ)
    const head_sym = e.headSymbol();
    for (rules) |rule| {
        if (!std.mem.eql(u8, rule.lhs.headSymbol(), head_sym)) continue;
        const subst = Subst.init(arena);
        const result = try unifier.unify(e, rule.lhs, subst, arena);
        if (result.first()) |s| {
            return unifier.applySubst(rule.rhs, &s, arena);
        }
    }

    return builtinRules(e, arena);
}

/// 組込み簡約規則
fn builtinRules(e: *const Expr, arena: Allocator) error{OutOfMemory}!*const Expr {
    if (e.* != .app) return e;
    const a = e.app;
    if (a.head.* != .sym) {
        // β簡約: ((λv. body) arg) → body[v/arg]
        if (a.head.* == .app) {
            const inner = a.head.app;
            if (inner.head.* == .sym and std.mem.eql(u8, inner.head.sym, "λ") and inner.args.len == 2 and inner.args[0].* == .var_ and a.args.len > 0) {
                const substituted = try unifier.substVar(inner.args[1], inner.args[0].var_, a.args[0], arena);
                if (a.args.len == 1) return substituted;
                return app(arena, substituted, a.args[1..]);
            }
        }
        return e;
    }

    const hn = a.head.sym;
    const args = a.args;

    // inv(refl(_)) → refl(_) 原式
    if (std.mem.eql(u8, hn, "inv") and args.len == 1) {
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.Refl)) {
            return args[0];
        }
        // inv(inv(p)) → p
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "inv") and args[0].app.args.len == 1) {
            return args[0].app.args[0];
        }
    }

    // compose/concat(refl, p) → p, compose/concat(p, refl) → p
    if ((std.mem.eql(u8, hn, syms.Compose) or std.mem.eql(u8, hn, syms.Concat)) and args.len == 2) {
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.Refl)) return args[1];
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl)) return args[0];
    }

    // transport(_, refl(_), x) → x
    if (std.mem.eql(u8, hn, syms.Transport) and args.len == 3) {
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl)) {
            return args[2];
        }
        // transport(λz. body, p, v) where body doesn't depend on z → v
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "λ") and args[0].app.args.len == 2 and args[0].app.args[0].* == .var_) {
            if (!args[0].app.args[1].contains(args[0].app.args[0])) {
                return args[2];
            }
        }
    }

    // 圏論的公理
    if (std.mem.eql(u8, hn, syms.Compose) and args.len == 2) {
        // f ∘ id → f, id ∘ f → f
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.Id)) return args[0];
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.Id)) return args[1];
        // (f ∘ g) ∘ h → f ∘ (g ∘ h) (右結合)
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.Compose) and args[0].app.args.len == 2) {
            const inner_args = args[0].app.args;
            const g_h = try app2(arena, a.head, inner_args[1], args[1]);
            return app2(arena, a.head, inner_args[0], g_h);
        }
        // pi1 ∘ pair(f, g) → f
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.Proj1) and args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Pair) and args[1].app.args.len == 2) {
            return args[1].app.args[0];
        }
        // pi2 ∘ pair(f, g) → g
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.Proj2) and args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Pair) and args[1].app.args.len == 2) {
            return args[1].app.args[1];
        }
    }

    // 面制約の正規化
    if (std.mem.eql(u8, hn, syms.FaceAnd) and args.len == 2) {
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I1)) return args[1];
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I1)) return args[0];
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I0)) return sym(arena, syms.I0);
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I0)) return sym(arena, syms.I0);
    }
    if (std.mem.eql(u8, hn, syms.FaceOr) and args.len == 2) {
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I0)) return args[1];
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I0)) return args[0];
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I1)) return sym(arena, syms.I1);
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I1)) return sym(arena, syms.I1);
    }
    if (std.mem.eql(u8, hn, syms.FaceNeg) and args.len == 1) {
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I0)) return sym(arena, syms.I1);
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.I1)) return sym(arena, syms.I0);
        // ¬ᶠ(¬ᶠ(φ)) → φ
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.FaceNeg) and args[0].app.args.len == 1) {
            return args[0].app.args[0];
        }
    }

    // hcomp(A, I1, u, u0) → u(I1)
    if (std.mem.eql(u8, hn, syms.HComp) and args.len == 4) {
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I1)) {
            return app1(arena, args[2], try sym(arena, syms.I1));
        }
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.I0)) return args[3];
    }

    // comp(A, refl, u0) → u0
    if (std.mem.eql(u8, hn, syms.Comp) and args.len == 3) {
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl)) {
            return args[2];
        }
    }

    // fill(A, refl(x), u0) → refl(x)
    if (std.mem.eql(u8, hn, syms.Fill) and args.len == 3) {
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Refl) and args[1].app.args.len == 1) {
            return args[1];
        }
    }

    // ¬A → A → ⊥
    if (std.mem.eql(u8, hn, syms.Not) and args.len == 1) {
        const false_sym = try sym(arena, syms.False);
        const implies_sym = try sym(arena, syms.Implies);
        return app2(arena, implies_sym, args[0], false_sym);
    }

    // 論理的簡約
    if (std.mem.eql(u8, hn, "=") and args.len == 2 and args[0].eql(args[1])) return sym(arena, syms.True);
    if (std.mem.eql(u8, hn, syms.Globally) and args.len == 1 and args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.True)) return sym(arena, syms.True);

    // A ∧ ⊤ → A, A ∧ ⊥ → ⊥, etc.
    if ((std.mem.eql(u8, hn, syms.And) or std.mem.eql(u8, hn, syms.Product)) and args.len == 2) {
        if (isSym(args[0], syms.True)) return args[1];
        if (isSym(args[1], syms.True)) return args[0];
        if (isSym(args[0], syms.False)) return sym(arena, syms.False);
        if (isSym(args[1], syms.False)) return sym(arena, syms.False);
    }
    if ((std.mem.eql(u8, hn, syms.Or) or std.mem.eql(u8, hn, syms.Coproduct)) and args.len == 2) {
        if (isSym(args[0], syms.True)) return sym(arena, syms.True);
        if (isSym(args[1], syms.True)) return sym(arena, syms.True);
        if (isSym(args[0], syms.False)) return args[1];
        if (isSym(args[1], syms.False)) return args[0];
    }

    // id(x) → x
    if (std.mem.eql(u8, hn, "id") and args.len == 1) return args[0];

    // head(cons_stream(x, _)) → x, tail(cons_stream(_, s)) → s
    if (std.mem.eql(u8, hn, "head") and args.len == 1 and args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "cons_stream") and args[0].app.args.len == 2) {
        return args[0].app.args[0];
    }
    if (std.mem.eql(u8, hn, "tail") and args.len == 1 and args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "cons_stream") and args[0].app.args.len == 2) {
        return args[0].app.args[1];
    }

    return e;
}

fn isSym(e: *const Expr, name: []const u8) bool {
    return e.* == .sym and std.mem.eql(u8, e.sym, name);
}

// ==========================================
// テスト
// ==========================================

test "normalize identity" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const result = try normalize(a, &.{}, arena);
    try std.testing.expect(result.eql(a));
}

test "normalize compose with id" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const compose = try sym(arena, "∘");
    const f = try sym(arena, "f");
    const id = try sym(arena, "id");
    const f_id = try app2(arena, compose, f, id);

    const result = try normalize(f_id, &.{}, arena);
    try std.testing.expect(result.eql(f));
}

test "normalize And with True" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const and_sym = try sym(arena, "∧");
    const a = try sym(arena, "A");
    const true_sym = try sym(arena, "⊤");
    const a_and_true = try app2(arena, and_sym, a, true_sym);

    const result = try normalize(a_and_true, &.{}, arena);
    try std.testing.expect(result.eql(a));
}

test "normalize And with False short-circuit" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const and_sym = try sym(arena, "∧");
    const a = try sym(arena, "A");
    const false_sym = try sym(arena, "⊥");
    const a_and_false = try app2(arena, and_sym, a, false_sym);

    const result = try normalize(a_and_false, &.{}, arena);
    try std.testing.expect(result.* == .sym);
    try std.testing.expectEqualStrings("⊥", result.sym);
}

test "normalize pi1 ∘ pair(f, g) = f" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const compose_s = try sym(arena, "∘");
    const pi1 = try sym(arena, "pi1");
    const pair_s = try sym(arena, "pair");
    const f = try sym(arena, "f");
    const g = try sym(arena, "g");
    const pair_fg = try app2(arena, pair_s, f, g);
    const pi1_pair = try app2(arena, compose_s, pi1, pair_fg);

    const result = try normalize(pi1_pair, &.{}, arena);
    try std.testing.expect(result.eql(f));
}
