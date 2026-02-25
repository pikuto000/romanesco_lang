// ==========================================
// algebra.zig
// 代数プラグイン (AlgebraPlugin.scala移植)
// Nat, List, Tree, S1, Maybe, Interval, Susp の帰納型定義
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const syms = @import("../symbols.zig");
const InitialAlgebra = expr_mod.InitialAlgebra;
const ConstructorDef = expr_mod.ConstructorDef;
const ArgType = expr_mod.ArgType;
const ConstructorType = expr_mod.ConstructorType;
const Plugin = search_mod.Plugin;
const Allocator = std.mem.Allocator;

/// 標準的な帰納型の定義
pub const nat_algebra = InitialAlgebra{
    .name = "Nat",
    .constructors = &[_]ConstructorDef{
        .{ .symbol = "0", .arg_types = &.{} },
        .{ .symbol = "S", .arg_types = &[_]ArgType{.recursive} },
    },
    .var_prefix = "n",
};

pub const list_algebra = InitialAlgebra{
    .name = "List",
    .constructors = &[_]ConstructorDef{
        .{ .symbol = "nil", .arg_types = &.{} },
        .{ .symbol = "cons", .arg_types = &[_]ArgType{ .constant, .recursive } },
    },
    .var_prefix = "l",
};

pub const tree_algebra = InitialAlgebra{
    .name = "Tree",
    .constructors = &[_]ConstructorDef{
        .{ .symbol = "leaf", .arg_types = &[_]ArgType{.constant} },
        .{ .symbol = "node", .arg_types = &[_]ArgType{ .recursive, .constant, .recursive } },
    },
    .var_prefix = "t",
};

pub const maybe_algebra = InitialAlgebra{
    .name = "Maybe",
    .constructors = &[_]ConstructorDef{
        .{ .symbol = "nothing", .arg_types = &.{} },
        .{ .symbol = "just", .arg_types = &[_]ArgType{.constant} },
    },
    .var_prefix = "m",
};

// HoTT高次帰納型
pub const s1_algebra = InitialAlgebra{
    .name = "S1",
    .constructors = &[_]ConstructorDef{
        .{ .symbol = "base", .arg_types = &.{} },
        .{ .symbol = "loop", .arg_types = &.{}, .ctor_type = .{ .path = .{ .from = undefined, .to = undefined } } },
    },
    .var_prefix = "s",
};

pub const interval_algebra = InitialAlgebra{
    .name = "Interval",
    .constructors = &[_]ConstructorDef{
        .{ .symbol = "left", .arg_types = &.{} },
        .{ .symbol = "right", .arg_types = &.{} },
        .{ .symbol = "seg", .arg_types = &.{}, .ctor_type = .{ .path = .{ .from = undefined, .to = undefined } } },
    },
    .var_prefix = "i",
};

pub const standard_algebras = [_]InitialAlgebra{
    nat_algebra,
    list_algebra,
    tree_algebra,
    maybe_algebra,
};

pub fn rewriteHook(e: *const expr_mod.Expr, arena: Allocator) error{OutOfMemory}!?*const expr_mod.Expr {
    if (e.* != .app) return null;
    const a = e.app;
    if (a.head.* != .sym) return null;
    const hn = a.head.sym;
    const args = a.args;

    // head(cons_stream(x, _)) → x, tail(cons_stream(_, s)) → s
    if (std.mem.eql(u8, hn, "head") and args.len == 1 and args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "cons_stream") and args[0].app.args.len == 2) {
        return args[0].app.args[0];
    }
    if (std.mem.eql(u8, hn, "tail") and args.len == 1 and args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "cons_stream") and args[0].app.args.len == 2) {
        return args[0].app.args[1];
    }

    // List モナド/ファンクタ
    if (std.mem.eql(u8, hn, "fmap") and args.len == 2) {
        if (isSym(args[1], "nil")) return expr_mod.sym(arena, "nil");
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, "cons") and args[1].app.args.len == 2) {
            const cons_sym = try expr_mod.sym(arena, "cons");
            const fmap_sym = try expr_mod.sym(arena, "fmap");
            const fx = try expr_mod.app1(arena, args[0], args[1].app.args[0]);
            const fxs = try expr_mod.app2(arena, fmap_sym, args[0], args[1].app.args[1]);
            return expr_mod.app2(arena, cons_sym, fx, fxs);
        }
    }

    if (std.mem.eql(u8, hn, "bind") and args.len == 2) {
        if (isSym(args[0], "nil")) return expr_mod.sym(arena, "nil");
        if (isSym(args[1], "return")) return args[0];
        if (args[1].* == .app and args[1].app.head.* == .sym and std.mem.eql(u8, args[1].app.head.sym, "return") and args[1].app.args.len == 0) return args[0];

        // Maybe モナド
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "just") and args[0].app.args.len == 1) {
            return expr_mod.app1(arena, args[1], args[0].app.args[0]);
        }
        if (isSym(args[0], "nothing")) return expr_mod.sym(arena, "nothing");
    }

    // ベクタ演算
    if (std.mem.eql(u8, hn, "vlength") and args.len == 1) {
        if (isSym(args[0], "vnil")) return expr_mod.sym(arena, "0");
        if (args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "vcons") and args[0].app.args.len == 2) {
            const vlength_sym = try expr_mod.sym(arena, "vlength");
            const succ_sym = try expr_mod.sym(arena, "S");
            return expr_mod.app1(arena, succ_sym, try expr_mod.app1(arena, vlength_sym, args[0].app.args[1]));
        }
    }
    if (std.mem.eql(u8, hn, "vhead") and args.len == 1 and args[0].* == .app and args[0].app.head.* == .sym and std.mem.eql(u8, args[0].app.head.sym, "vcons") and args[0].app.args.len == 2) {
        return args[0].app.args[0];
    }

    return null;
}

fn isSym(e: *const expr_mod.Expr, name: []const u8) bool {
    return e.* == .sym and std.mem.eql(u8, e.sym, name);
}

pub fn buildRules(arena: Allocator) anyerror![]const expr_mod.CatRule {
    var rules: std.ArrayList(expr_mod.CatRule) = .{};
    const b = expr_mod.RuleBuilder.init(arena);

    // ==========================================
    // 自然数演算
    // ==========================================
    try rules.append(arena, .{ .name = "plus_0", .lhs = try b.a2(try b.s("plus"), try b.s(syms.Zero), try b.v("n")), .rhs = try b.v("n") });
    try rules.append(arena, .{ .name = "plus_S", .lhs = try b.a2(try b.s("plus"), try b.a1(try b.s(syms.Succ), try b.v("n")), try b.v("m")), .rhs = try b.a1(try b.s(syms.Succ), try b.a2(try b.s("plus"), try b.v("n"), try b.v("m"))) });
    try rules.append(arena, .{ .name = "plus_n_0", .lhs = try b.a2(try b.s("plus"), try b.v("n"), try b.s(syms.Zero)), .rhs = try b.v("n") });
    try rules.append(arena, .{ .name = "plus_n_S", .lhs = try b.a2(try b.s("plus"), try b.v("n"), try b.a1(try b.s(syms.Succ), try b.v("m"))), .rhs = try b.a1(try b.s(syms.Succ), try b.a2(try b.s("plus"), try b.v("n"), try b.v("m"))) });

    // ==========================================
    // リスト・ツリー演算
    // ==========================================
    try rules.append(arena, .{ .name = "append_nil", .lhs = try b.a2(try b.s("append"), try b.s("nil"), try b.v("ys")), .rhs = try b.v("ys") });
    try rules.append(arena, .{ .name = "append_cons", .lhs = try b.a2(try b.s("append"), try b.a2(try b.s("cons"), try b.v("x"), try b.v("xs")), try b.v("ys")), .rhs = try b.a2(try b.s("cons"), try b.v("x"), try b.a2(try b.s("append"), try b.v("xs"), try b.v("ys"))) });
    try rules.append(arena, .{ .name = "append_nil_r", .lhs = try b.a2(try b.s("append"), try b.v("xs"), try b.s("nil")), .rhs = try b.v("xs") });
    try rules.append(arena, .{ .name = "reverse_nil", .lhs = try b.a1(try b.s("reverse"), try b.s("nil")), .rhs = try b.s("nil") });
    try rules.append(arena, .{ .name = "reverse_cons", .lhs = try b.a1(try b.s("reverse"), try b.a2(try b.s("cons"), try b.v("x"), try b.v("xs"))), .rhs = try b.a2(try b.s("append"), try b.a1(try b.s("reverse"), try b.v("xs")), try b.a2(try b.s("cons"), try b.v("x"), try b.s("nil"))) });
    try rules.append(arena, .{ .name = "reverse_append", .lhs = try b.a1(try b.s("reverse"), try b.a2(try b.s("append"), try b.v("xs"), try b.v("ys"))), .rhs = try b.a2(try b.s("append"), try b.a1(try b.s("reverse"), try b.v("ys")), try b.a1(try b.s("reverse"), try b.v("xs"))) });
    try rules.append(arena, .{ .name = "mirror_leaf", .lhs = try b.a1(try b.s("mirror"), try b.s("leaf")), .rhs = try b.s("leaf") });
    try rules.append(arena, .{ .name = "mirror_node", .lhs = try b.a1(try b.s("mirror"), try b.a3(try b.s("node"), try b.v("l"), try b.v("val"), try b.v("r"))), .rhs = try b.a3(try b.s("node"), try b.a1(try b.s("mirror"), try b.v("r")), try b.v("val"), try b.a1(try b.s("mirror"), try b.v("l"))) });
    try rules.append(arena, .{ .name = "list_prop_nil", .lhs = try b.a1(try b.s("list_prop"), try b.s("nil")), .rhs = try b.s(syms.True) });
    try rules.append(arena, .{ .name = "list_prop_cons", .lhs = try b.a1(try b.s("list_prop"), try b.a2(try b.s("cons"), try b.v("x"), try b.v("xs"))), .rhs = try b.a1(try b.s("list_prop"), try b.v("xs")) });
    try rules.append(arena, .{ .name = "list_prop_append", .lhs = try b.a1(try b.s("list_prop"), try b.a2(try b.s("append"), try b.v("xs"), try b.v("ys"))), .rhs = try b.a2(try b.s(syms.And), try b.a1(try b.s("list_prop"), try b.v("xs")), try b.a1(try b.s("list_prop"), try b.v("ys"))) });

    // ==========================================
    // リスト・ファンクタ/モナド
    // ==========================================
    try rules.append(arena, .{ .name = "fmap_nil", .lhs = try b.a2(try b.s("fmap"), try b.v("f"), try b.s("nil")), .rhs = try b.s("nil") });
    try rules.append(arena, .{ .name = "fmap_cons", .lhs = try b.a2(try b.s("fmap"), try b.v("f"), try b.a2(try b.s("cons"), try b.v("x"), try b.v("xs"))), .rhs = try b.a2(try b.s("cons"), try b.a1(try b.v("f"), try b.v("x")), try b.a2(try b.s("fmap"), try b.v("f"), try b.v("xs"))) });
    try rules.append(arena, .{ .name = "bind_nil", .lhs = try b.a2(try b.s("bind"), try b.s("nil"), try b.v("f")), .rhs = try b.s("nil") });
    try rules.append(arena, .{ .name = "bind_return", .lhs = try b.a2(try b.s("bind"), try b.v("m"), try b.s("return")), .rhs = try b.v("m") });

    return rules.toOwnedSlice(arena);
}

pub const plugin = Plugin{
    .name = "Algebra",
    .priority = 170,
    .build_rules = &buildRules,
    .provided_algebras = &standard_algebras,
    .rewrite_hook = &rewriteHook,
};
