// ==========================================
// unifier.zig
// 単一化アルゴリズム (Unifier.scala移植)
// 高階パターンユニフィケーション + 宇宙レベルチェック
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const Expr = expr_mod.Expr;
const MetaId = expr_mod.MetaId;
const Subst = expr_mod.Subst;
const MetaIdContext = expr_mod.MetaIdContext;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const app = expr_mod.app;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

/// 置換を式に適用する
pub fn applySubst(e: *const Expr, s: *const Subst, arena: Allocator) !*const Expr {
    if (s.count() == 0) return e;
    return switch (e.*) {
        .meta => |id| {
            if (s.get(id)) |val| {
                return applySubst(val, s, arena);
            }
            return e;
        },
        .app => |a| {
            const new_head = try applySubst(a.head, s, arena);
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try applySubst(arg, s, arena);
            }
            const result = try arena.create(Expr);
            result.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return result;
        },
        else => e,
    };
}

/// CatRuleに置換を適用
pub fn applySubstToRule(rule: expr_mod.CatRule, s: *const Subst, arena: Allocator) !expr_mod.CatRule {
    return .{
        .name = rule.name,
        .lhs = try applySubst(rule.lhs, s, arena),
        .rhs = try applySubst(rule.rhs, s, arena),
        .universals = blk: {
            if (rule.universals.len == 0) break :blk &.{};
            const new_u = try arena.alloc(*const Expr, rule.universals.len);
            for (rule.universals, 0..) |u, i| {
                new_u[i] = try applySubst(u, s, arena);
            }
            break :blk new_u;
        },
        .domain = rule.domain,
    };
}

/// ユニフィケーション結果のイテレータ
/// ScalaのLazyList[Subst]に対応
pub const UnifyResult = struct {
    substs: []const Subst,

    pub fn empty() UnifyResult {
        return .{ .substs = &.{} };
    }

    pub fn single(arena: Allocator, s: Subst) !UnifyResult {
        const arr = try arena.alloc(Subst, 1);
        arr[0] = s;
        return .{ .substs = arr };
    }

    pub fn first(self: UnifyResult) ?Subst {
        return if (self.substs.len > 0) self.substs[0] else null;
    }
};

/// メタ変数の出現チェック (occurs check)
pub fn occursCheck(meta_id: MetaId, e: *const Expr) bool {
    return switch (e.*) {
        .meta => |id| MetaId.eql(id, meta_id),
        .app => |a| {
            if (occursCheck(meta_id, a.head)) return true;
            for (a.args) |arg| {
                if (occursCheck(meta_id, arg)) return true;
            }
            return false;
        },
        else => false,
    };
}

/// 自由変数の収集
pub fn collectFreeVars(e: *const Expr, arena: Allocator) !std.StringHashMap(void) {
    var result = std.StringHashMap(void).init(arena);
    try collectFreeVarsInto(e, &result, null);
    return result;
}

fn collectFreeVarsInto(e: *const Expr, result: *std.StringHashMap(void), bound: ?*const std.StringHashMap(void)) !void {
    switch (e.*) {
        .var_ => |n| {
            if (bound) |b| {
                if (b.contains(n)) return;
            }
            try result.put(n, {});
        },
        .app => |a| {
            // λ式の場合、束縛変数を追加
            if (a.head.* == .sym and std.mem.eql(u8, a.head.sym, "λ") and a.args.len == 2 and a.args[0].* == .var_) {
                // 新しい束縛セットを作る（簡略化: 再帰のみ）
                var new_bound = if (bound) |b| b.* else std.StringHashMap(void).init(result.allocator);
                _ = &new_bound;
                try new_bound.put(a.args[0].var_, {});
                try collectFreeVarsInto(a.args[1], result, &new_bound);
            } else {
                try collectFreeVarsInto(a.head, result, bound);
                for (a.args) |arg| {
                    try collectFreeVarsInto(arg, result, bound);
                }
            }
        },
        else => {},
    }
}

/// 変数の置換 (Prover.substVar相当)
pub fn substVar(e: *const Expr, var_name: []const u8, replacement: *const Expr, arena: Allocator) !*const Expr {
    return switch (e.*) {
        .var_ => |n| if (std.mem.eql(u8, n, var_name)) replacement else e,
        .sym => e,
        .meta => e,
        .app => |a| {
            // λ束縛のシャドウイング
            if (a.head.* == .sym and std.mem.eql(u8, a.head.sym, "λ") and a.args.len == 2 and a.args[0].* == .var_) {
                if (std.mem.eql(u8, a.args[0].var_, var_name)) return e;
                const new_body = try substVar(a.args[1], var_name, replacement, arena);
                if (new_body == a.args[1]) return e;
                return app2(arena, a.head, a.args[0], new_body);
            }
            const new_head = try substVar(a.head, var_name, replacement, arena);
            var changed = new_head != a.head;
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try substVar(arg, var_name, replacement, arena);
                if (new_args[i] != arg) changed = true;
            }
            if (!changed) return e;
            const result = try arena.create(Expr);
            result.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return result;
        },
    };
}

/// メインのユニフィケーション関数
/// e1とe2を統一し、成功すれば置換を返す
pub fn unify(e1: *const Expr, e2: *const Expr, subst: Subst, arena: Allocator) !UnifyResult {
    const r1 = try applySubst(e1, &subst, arena);
    const r2 = try applySubst(e2, &subst, arena);

    if (r1.eql(r2)) return UnifyResult.single(arena, subst);

    // λ抽象の単一化
    if (r1.* == .app and r2.* == .app) {
        const a1 = r1.app;
        const a2 = r2.app;
        if (isLambda(a1) and isLambda(a2)) {
            const v1 = a1.args[0].var_;
            const b1 = a1.args[1];
            const b2 = a2.args[1];
            const v2 = a2.args[0].var_;
            const new_b2 = if (std.mem.eql(u8, v1, v2)) b2 else try substVar(b2, v2, a1.args[0], arena);
            return unify(b1, new_b2, subst, arena);
        }
    }

    // メタ変数同士のアプリケーション
    if (r1.* == .app and r2.* == .app and r1.app.head.* == .meta and r2.app.head.* == .meta) {
        if (MetaId.eql(r1.app.head.meta, r2.app.head.meta) and r1.app.args.len == r2.app.args.len) {
            var s = subst;
            for (r1.app.args, r2.app.args) |a1_arg, a2_arg| {
                const res = try unify(a1_arg, a2_arg, s, arena);
                if (res.first()) |ns| {
                    s = ns;
                } else return UnifyResult.empty();
            }
            return UnifyResult.single(arena, s);
        }
    }

    // 宇宙レベルの単一化 (Russell Paradox回避)
    if (r1.* == .app and r2.* == .app) {
        if (r1.app.head.* == .sym and std.mem.eql(u8, r1.app.head.sym, "Type") and
            r2.app.head.* == .sym and std.mem.eql(u8, r2.app.head.sym, "Type"))
        {
            const l1 = if (r1.app.args.len > 0 and r1.app.args[0].* == .meta) @as(i32, @intCast(r1.app.args[0].meta.ids.len)) else @as(i32, -1);
            const l2 = if (r2.app.args.len > 0 and r2.app.args[0].* == .meta) @as(i32, @intCast(r2.app.args[0].meta.ids.len)) else @as(i32, -1);
            if (l1 != -1 and l2 != -1 and l1 != l2) return UnifyResult.empty();
            // 引数を順に統一
            if (r1.app.args.len == r2.app.args.len) {
                var s = subst;
                for (r1.app.args, r2.app.args) |a1_arg, a2_arg| {
                    const res = try unify(a1_arg, a2_arg, s, arena);
                    if (res.first()) |ns| {
                        s = ns;
                    } else return UnifyResult.empty();
                }
                return UnifyResult.single(arena, s);
            }
        }
    }

    // 高階パターン単一化 (App(Meta(id), args) = t)
    if (r1.* == .app and r1.app.head.* == .meta) {
        return solveHigherOrderOrMeta(r1, r2, subst, arena);
    }
    if (r2.* == .app and r2.app.head.* == .meta) {
        return solveHigherOrderOrMeta(r2, r1, subst, arena);
    }

    // 基本的なメタ変数の単一化
    if (r1.* == .meta) {
        if (occursCheck(r1.meta, r2)) return UnifyResult.empty();
        var new_subst = subst;
        try new_subst.put(r1.meta, r2);
        return UnifyResult.single(arena, new_subst);
    }
    if (r2.* == .meta) {
        if (occursCheck(r2.meta, r1)) return UnifyResult.empty();
        var new_subst = subst;
        try new_subst.put(r2.meta, r1);
        return UnifyResult.single(arena, new_subst);
    }

    // 変数の単一化
    if (r1.* == .var_ and r2.* == .var_) {
        if (std.mem.eql(u8, r1.var_, r2.var_)) return UnifyResult.single(arena, subst);
        return UnifyResult.empty();
    }

    // シンボルの不一致
    if (r1.* == .sym and r2.* == .sym) {
        if (!std.mem.eql(u8, r1.sym, r2.sym)) return UnifyResult.empty();
        return UnifyResult.single(arena, subst);
    }

    // アプリケーションの分解
    if (r1.* == .app and r2.* == .app) {
        if (r1.app.args.len != r2.app.args.len) return UnifyResult.empty();
        const head_res = try unify(r1.app.head, r2.app.head, subst, arena);
        if (head_res.first()) |s| {
            var current = s;
            for (r1.app.args, r2.app.args) |a1_arg, a2_arg| {
                const res = try unify(a1_arg, a2_arg, current, arena);
                if (res.first()) |ns| {
                    current = ns;
                } else return UnifyResult.empty();
            }
            return UnifyResult.single(arena, current);
        }
    }

    return UnifyResult.empty();
}

fn isLambda(a: Expr.App) bool {
    return a.head.* == .sym and std.mem.eql(u8, a.head.sym, "λ") and a.args.len == 2 and a.args[0].* == .var_;
}

fn solveHigherOrderOrMeta(meta_app: *const Expr, target: *const Expr, subst: Subst, arena: Allocator) !UnifyResult {
    const id = meta_app.app.head.meta;
    const args = meta_app.app.args;

    if (occursCheck(id, target)) return UnifyResult.empty();

    // 引数を変数名のリストとして取得し、ターゲットを抽象化
    const vars = try arena.alloc([]const u8, args.len);
    for (0..args.len) |i| {
        var buf: [16]u8 = undefined;
        const name = try std.fmt.bufPrint(&buf, "bv{d}", .{i});
        vars[i] = try arena.dupe(u8, name);
    }

    // ターゲットから引数と一致する部分式を対応する変数に置換
    var body = target;
    for (args, vars) |arg, vn| {
        body = try replaceExpr(body, arg, try var_(arena, vn), arena);
    }

    // λ抽象で包む
    var lambda = body;
    var i: usize = vars.len;
    while (i > 0) {
        i -= 1;
        const lam_sym = try sym(arena, "λ");
        const v = try var_(arena, vars[i]);
        lambda = try app2(arena, lam_sym, v, lambda);
    }

    var new_subst = subst;
    try new_subst.put(id, lambda);
    return UnifyResult.single(arena, new_subst);
}

/// 式中のパターンを置換する
pub fn replaceExpr(e: *const Expr, pattern: *const Expr, replacement: *const Expr, arena: Allocator) !*const Expr {
    if (e.eql(pattern)) return replacement;
    return switch (e.*) {
        .app => |a| {
            const new_head = try replaceExpr(a.head, pattern, replacement, arena);
            var changed = new_head != a.head;
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try replaceExpr(arg, pattern, replacement, arena);
                if (new_args[i] != arg) changed = true;
            }
            if (!changed) return e;
            const result = try arena.create(Expr);
            result.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return result;
        },
        else => e,
    };
}

// ==========================================
// テスト
// ==========================================

test "unify identical symbols" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const subst = Subst.init(arena);
    const result = try unify(a, a, subst, arena);
    try std.testing.expect(result.first() != null);
}

test "unify meta with symbol" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const m = try expr_mod.meta(arena, 0);
    const a = try sym(arena, "A");
    const subst = Subst.init(arena);
    const result = try unify(m, a, subst, arena);
    const s = result.first();
    try std.testing.expect(s != null);
    const bound = s.?.get(m.meta);
    try std.testing.expect(bound != null);
    try std.testing.expect(bound.?.eql(a));
}

test "unify App decomposition" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const f = try sym(arena, "f");
    const m = try expr_mod.meta(arena, 0);
    const a = try sym(arena, "A");
    const fa = try app1(arena, f, a);
    const fm = try app1(arena, f, m);

    const subst = Subst.init(arena);
    const result = try unify(fa, fm, subst, arena);
    const s = result.first();
    try std.testing.expect(s != null);
}

test "unify occurs check" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const m = try expr_mod.meta(arena, 0);
    const f = try sym(arena, "f");
    const fm = try app1(arena, f, m); // f(?0) — ?0を含む

    const subst = Subst.init(arena);
    const result = try unify(m, fm, subst, arena);
    try std.testing.expect(result.first() == null);
}

test "substVar" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const x = try var_(arena, "x");
    const a = try sym(arena, "A");
    const f = try sym(arena, "f");
    const fx = try app1(arena, f, x);

    const result = try substVar(fx, "x", a, arena);
    // f(A) になるはず
    try std.testing.expect(result.* == .app);
    try std.testing.expect(result.app.args[0].eql(a));
}
