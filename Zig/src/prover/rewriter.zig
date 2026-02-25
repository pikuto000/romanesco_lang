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
const app = expr_mod.app;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

/// 簡約フック: 特定ドメインの簡約規則を外部から注入
pub const RewriteHook = *const fn (
    e: *const Expr,
    arena: Allocator,
) error{OutOfMemory}!?*const Expr;

/// 正規化: 不動点まで反復簡約
pub fn normalize(e: *const Expr, rules: []const CatRule, arena: Allocator) error{OutOfMemory}!*const Expr {
    return normalizeWithHooks(e, rules, &.{}, arena);
}

pub fn normalizeWithHooks(e: *const Expr, rules: []const CatRule, hooks: []const RewriteHook, arena: Allocator) error{OutOfMemory}!*const Expr {
    return normalizeNWithHooks(e, rules, hooks, 100, arena);
}

pub fn normalizeN(e: *const Expr, rules: []const CatRule, max_iter: u32, arena: Allocator) error{OutOfMemory}!*const Expr {
    return normalizeNWithHooks(e, rules, &.{}, max_iter, arena);
}

pub fn normalizeNWithHooks(e: *const Expr, rules: []const CatRule, hooks: []const RewriteHook, max_iter: u32, arena: Allocator) error{OutOfMemory}!*const Expr {
    var current = e;
    var iter: u32 = 0;
    while (iter < max_iter) : (iter += 1) {
        const reduced = try step(current, rules, hooks, arena);
        const ac_normalized = try acNormalize(reduced, arena);
        if (ac_normalized.eql(current)) return current;
        current = ac_normalized;
    }
    return current;
}

/// 1ステップの簡約
fn step(e: *const Expr, rules: []const CatRule, hooks: []const RewriteHook, arena: Allocator) error{OutOfMemory}!*const Expr {
    return switch (e.*) {
        .app => |a| {
            const new_head = try step(a.head, rules, hooks, arena);
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try step(arg, rules, hooks, arena);
            }
            const new_expr = try arena.create(Expr);
            new_expr.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return rewriteRule(new_expr, rules, hooks, arena);
        },
        else => rewriteRule(e, rules, hooks, arena),
    };
}

/// AC (Associative-Commutative) 正規化
fn acNormalize(e: *const Expr, arena: Allocator) error{OutOfMemory}!*const Expr {
    if (e.* != .app) return e;
    const a = e.app;
    if (a.head.* != .sym) {
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
        const new_args = try arena.alloc(*const Expr, a.args.len);
        for (a.args, 0..) |arg, i| {
            new_args[i] = try acNormalize(arg, arena);
        }
        const result = try arena.create(Expr);
        result.* = .{ .app = .{ .head = a.head, .args = new_args } };
        return result;
    }

    var flattened: std.ArrayList(*const Expr) = .{};
    for (a.args) |arg| {
        try collectAC(arg, op, &flattened, arena);
    }

    const is_and = std.mem.eql(u8, op, syms.And);
    const is_or = std.mem.eql(u8, op, syms.Or);

    if (is_and) {
        for (flattened.items) |item| {
            if (item.* == .sym and (std.mem.eql(u8, item.sym, syms.False) or std.mem.eql(u8, item.sym, "⊥"))) {
                return sym(arena, syms.False);
            }
        }
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

    if (flattened.items.len == 0) {
        if (std.mem.eql(u8, op, syms.Tensor)) return sym(arena, syms.LPlus);
        if (std.mem.eql(u8, op, syms.SepAnd)) return sym(arena, syms.LOne);
        if (is_or) return sym(arena, syms.False);
        return sym(arena, syms.True);
    }
    if (flattened.items.len == 1) return flattened.items[0];

    const op_sym = try sym(arena, op);
    var result = flattened.items[0];
    for (flattened.items[1..]) |item| {
        result = try app2(arena, op_sym, result, item);
    }
    return result;
}

fn collectAC(e: *const Expr, op: []const u8, list: *std.ArrayList(*const Expr), arena: Allocator) error{OutOfMemory}!void {
    if (e.* == .app and e.app.head.* == .sym and std.mem.eql(u8, e.app.head.sym, op) and e.app.args.len == 2) {
        try collectAC(e.app.args[0], op, list, arena);
        try collectAC(e.app.args[1], op, list, arena);
    } else {
        try list.append(arena, try acNormalize(e, arena));
    }
}

/// 規則適用 (ユーザ定義 + フック + 組込みコア)
fn rewriteRule(e: *const Expr, rules: []const CatRule, hooks: []const RewriteHook, arena: Allocator) error{OutOfMemory}!*const Expr {
    // 優先度の高い等値チェック
    if (e.* == .app and e.app.args.len == 2) {
        const head = e.app.head;
        if (head.* == .sym) {
            const hn = head.sym;
            if ((std.mem.eql(u8, hn, "=") or std.mem.eql(u8, hn, "bisim")) and e.app.args[0].eql(e.app.args[1])) {
                return sym(arena, syms.True);
            }
            if (std.mem.eql(u8, hn, syms.Path) and e.app.args.len >= 2) {
                if (e.app.args.len == 3 and e.app.args[1].eql(e.app.args[2])) {
                    return sym(arena, syms.True);
                }
            }
        }
    }

    if (rules.len > 0) {
        const head_sym = e.headSymbol();
        for (rules) |rule| {
            if (!std.mem.eql(u8, rule.lhs.headSymbol(), head_sym)) continue;
            const subst = Subst.init(arena);
            const result = try unifier.unify(e, rule.lhs, subst, arena);
            if (result.first()) |s| {
                return unifier.applySubst(rule.rhs, &s, arena);
            }
        }
    }

    for (hooks) |hook| {
        if (try hook(e, arena)) |res| return res;
    }

    return builtinRules(e, arena);
}

/// 組込み簡約規則 (普遍的なコアのみ)
fn builtinRules(e: *const Expr, arena: Allocator) error{OutOfMemory}!*const Expr {
    if (e.* != .app) return e;
    const a = e.app;
    if (a.head.* != .sym) {
        // β簡約
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

    // 圏論的公理
    if (std.mem.eql(u8, hn, syms.Compose) and args.len == 2) {
        if (args[1].* == .sym and std.mem.eql(u8, args[1].sym, syms.Id)) return args[0];
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.Id)) return args[1];
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, syms.Compose) and args[0].app.args.len == 2) {
            const inner_args = args[0].app.args;
            const g_h = try app2(arena, a.head, inner_args[1], args[1]);
            return app2(arena, a.head, inner_args[0], g_h);
        }
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.Proj1) and args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Pair) and args[1].app.args.len == 2) {
            return args[1].app.args[0];
        }
        if (args[0].* == .sym and std.mem.eql(u8, args[0].sym, syms.Proj2) and args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, syms.Pair) and args[1].app.args.len == 2) {
            return args[1].app.args[1];
        }
    }

    // ¬A → A → ⊥
    if (std.mem.eql(u8, hn, syms.Not) and args.len == 1) {
        const false_sym = try sym(arena, syms.False);
        const implies_sym = try sym(arena, syms.Implies);
        return app2(arena, implies_sym, args[0], false_sym);
    }

    if (std.mem.eql(u8, hn, syms.Eq) and args.len == 2 and args[0].eql(args[1])) return sym(arena, syms.True);

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

    if (std.mem.eql(u8, hn, "id") and args.len == 1) return args[0];

    return e;
}

fn isSym(e: *const Expr, name: []const u8) bool {
    return e.* == .sym and std.mem.eql(u8, e.sym, name);
}
