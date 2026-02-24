// ==========================================
// algebra.zig
// 代数プラグイン (AlgebraPlugin.scala移植)
// Nat, List, Tree, S1, Maybe, Interval, Susp の帰納型定義
// ==========================================

const std = @import("std");
const expr_mod = @import("../expr.zig");
const search_mod = @import("../search.zig");
const InitialAlgebra = expr_mod.InitialAlgebra;
const ConstructorDef = expr_mod.ConstructorDef;
const ArgType = expr_mod.ArgType;
const ConstructorType = expr_mod.ConstructorType;
const Plugin = search_mod.Plugin;

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

pub const plugin = Plugin{
    .name = "Algebra",
    .priority = 170,
    .provided_algebras = &standard_algebras,
};
