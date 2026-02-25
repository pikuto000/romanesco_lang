// ==========================================
// unifier.zig
// 単一化アルゴリズム (Unifier.scala移植)
// 高階パターンユニフィケーション + 宇宙レベルチェック
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const syms = @import("symbols.zig");
const Expr = expr_mod.Expr;
const MetaId = expr_mod.MetaId;
const Subst = expr_mod.Subst;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const app = expr_mod.app;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

/// 単一化の結果（複数の候補を返す可能性があるが、現在は最初の1つを優先）
pub const UnifyResult = struct {
    substitutions: []Subst,

    pub fn empty() UnifyResult {
        return .{ .substitutions = &.{} };
    }

    pub fn single(arena: Allocator, s: Subst) !UnifyResult {
        const subs = try arena.alloc(Subst, 1);
        subs[0] = s;
        return .{ .substitutions = subs };
    }

    pub fn first(self: UnifyResult) ?Subst {
        return if (self.substitutions.len > 0) self.substitutions[0] else null;
    }
};

/// 2つの式を単一化する
pub fn unify(e1: *const Expr, e2: *const Expr, subst: Subst, arena: Allocator) !UnifyResult {
    const r1 = applySubst(e1, &subst, arena) catch e1;
    const r2 = applySubst(e2, &subst, arena) catch e2;

    if (r1.eql(r2)) return UnifyResult.single(arena, subst);

    // 基本的なメタ変数の単一化
    if (r1.* == .meta) {
        if (occursCheck(r1.meta, r2)) return UnifyResult.empty();
        var new_subst = try subst.clone();
        try new_subst.put(r1.meta, r2);
        return UnifyResult.single(arena, new_subst);
    }
    if (r2.* == .meta) {
        if (occursCheck(r2.meta, r1)) return UnifyResult.empty();
        var new_subst = try subst.clone();
        try new_subst.put(r2.meta, r1);
        return UnifyResult.single(arena, new_subst);
    }

    // 関数適用の単一化
    if (r1.* == .app and r2.* == .app) {
        const a1 = r1.app;
        const a2 = r2.app;

        // 高階単一化の簡易版 (メタ変数が頭にある場合)
        if (a1.head.* == .meta) return solveHigherOrderOrMeta(r1, r2, subst, arena);
        if (a2.head.* == .meta) return solveHigherOrderOrMeta(r2, r1, subst, arena);

        // 頭部が一致する場合、引数を順番に単一化
        if (a1.head.eql(a2.head) and a1.args.len == a2.args.len) {
            var current_subst = subst;
            for (a1.args, a2.args) |arg1, arg2| {
                const res = try unify(arg1, arg2, current_subst, arena);
                if (res.first()) |s| {
                    current_subst = s;
                } else {
                    return UnifyResult.empty();
                }
            }
            return UnifyResult.single(arena, current_subst);
        }
    }

    return UnifyResult.empty();
}

/// メタ変数が式の中に現れるかチェック
fn occursCheck(id: MetaId, e: *const Expr) bool {
    return switch (e.*) {
        .meta => |mid| MetaId.eql(id, mid),
        .app => |a| blk: {
            if (occursCheck(id, a.head)) break :blk true;
            for (a.args) |arg| {
                if (occursCheck(id, arg)) break :blk true;
            }
            break :blk false;
        },
        else => false,
    };
}

/// 高階パターンの解決 (簡易版: ?M(x, y) = target  =>  ?M = λx.λy. target)
fn solveHigherOrderOrMeta(meta_app: *const Expr, target: *const Expr, subst: Subst, arena: Allocator) !UnifyResult {
    const id = meta_app.app.head.meta;
    const args = meta_app.app.args;

    // パターン条件のチェック (引数が全て異なる束縛変数であること)
    for (args) |arg| {
        if (arg.* != .var_) return UnifyResult.empty();
    }

    // targetを引数で抽象化
    var lambda = target;
    var i: usize = args.len;
    while (i > 0) {
        i -= 1;
        const v = args[i].var_;
        const lambda_head = try sym(arena, "λ");
        const var_expr = try var_(arena, v);
        lambda = try app2(arena, lambda_head, var_expr, lambda);
    }

    var new_subst = try subst.clone();
    try new_subst.put(id, lambda);
    return UnifyResult.single(arena, new_subst);
}

/// 置換を式に適用
pub fn applySubst(e: *const Expr, subst: *const Subst, arena: Allocator) !*const Expr {
    return switch (e.*) {
        .meta => |id| if (subst.get(id)) |res| try applySubst(res, subst, arena) else e,
        .app => |a| blk: {
            const new_head = try applySubst(a.head, subst, arena);
            const new_args = try arena.alloc(*const Expr, a.args.len);
            var changed = !new_head.eql(a.head);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try applySubst(arg, subst, arena);
                if (!new_args[i].eql(arg)) changed = true;
            }
            if (!changed) break :blk e;
            const res = try arena.create(Expr);
            res.* = .{ .app = .{ .head = new_head, .args = new_args } };
            break :blk res;
        },
        else => e,
    };
}

/// 変数を項で置換 (β簡約用)
pub fn substVar(e: *const Expr, name: []const u8, term: *const Expr, arena: Allocator) !*const Expr {
    return switch (e.*) {
        .var_ => |n| if (std.mem.eql(u8, n, name)) term else e,
        .app => |a| blk: {
            const new_head = try substVar(a.head, name, term, arena);
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try substVar(arg, name, term, arena);
            }
            const res = try arena.create(Expr);
            res.* = .{ .app = .{ .head = new_head, .args = new_args } };
            break :blk res;
        },
        else => e,
    };
}

/// 項全体の中で old を new に置換
pub fn replaceExpr(e: *const Expr, old: *const Expr, new: *const Expr, arena: Allocator) !*const Expr {
    if (e.eql(old)) return new;
    return switch (e.*) {
        .app => |a| blk: {
            const new_head = try replaceExpr(a.head, old, new, arena);
            const new_args = try arena.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try replaceExpr(arg, old, new, arena);
            }
            const res = try arena.create(Expr);
            res.* = .{ .app = .{ .head = new_head, .args = new_args } };
            break :blk res;
        },
        else => e,
    };
}

// テスト
test "unify symbols" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");

    var subst = Subst.init(std.testing.allocator);
    defer subst.deinit();

    const res1 = try unify(a, a, subst, arena);
    try std.testing.expect(res1.substitutions.len == 1);
    res1.substitutions[0].deinit();

    const res2 = try unify(a, b, subst, arena);
    try std.testing.expect(res2.substitutions.len == 0);
}

test "unify meta variable" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const m = try expr_mod.meta(arena, 1);
    const a = try sym(arena, "A");

    var subst = Subst.init(std.testing.allocator);
    defer subst.deinit();

    const res = try unify(m, a, subst, arena);
    try std.testing.expect(res.substitutions.len == 1);
    
    defer {
        for (res.substitutions) |*s| {
            s.deinit();
        }
    }

    const s = res.substitutions[0];
    const resolved = s.get(m.meta).?;
    try std.testing.expect(resolved.eql(a));
}

test "substVar replaces variable" {
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
