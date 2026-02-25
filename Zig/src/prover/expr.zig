// ==========================================
// expr.zig
// 基本的なデータ構造の定義 (Core.scala + Types.scala移植)
// Expr AST, MetaId, Goal, CatRule, Tree, ProofTree等
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const symbols = @import("symbols.zig");
pub const InternedString = symbols.InternedString;
pub const StringPool = symbols.StringPool;

// ==========================================
// MetaId — メタ変数のユニークな識別子
// ==========================================

pub const MetaId = struct {
    ids: []const i32,

    pub fn single(id: i32) MetaId {
        return .{ .ids = &[_]i32{id} };
    }

    pub fn eql(a: MetaId, b: MetaId) bool {
        return std.mem.eql(i32, a.ids, b.ids);
    }

    pub fn hash(self: MetaId) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(std.mem.sliceAsBytes(self.ids));
        return h.final();
    }

    pub fn dupe(self: MetaId, allocator: Allocator) !MetaId {
        return .{ .ids = try allocator.dupe(i32, self.ids) };
    }

    pub fn format(self: MetaId, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (self.ids, 0..) |id, i| {
            if (i > 0) try writer.writeByte('.');
            try std.fmt.formatInt(id, 10, .lower, .{}, writer);
        }
    }
};

// ==========================================
// Expr — 論理式を表現する抽象構文木
// ==========================================

pub const Expr = union(enum) {
    var_: []const u8, // 束縛変数 / 個体変数
    meta: MetaId, // メタ変数
    sym: []const u8, // 定数シンボル
    app: App, // 関数適用

    pub const App = struct {
        head: *const Expr,
        args: []const *const Expr,
    };

    /// 目標の先頭記号（インデックス用）
    pub fn headSymbol(self: *const Expr) []const u8 {
        return switch (self.*) {
            .app => |a| if (a.head.* == .sym) a.head.sym else a.head.headSymbol(),
            .sym => |n| n,
            .var_ => "_VAR_",
            .meta => "_META_",
        };
    }

    /// 式の複雑さ（ヒューリスティック用スコア）
    pub fn complexity(self: *const Expr) u32 {
        return switch (self.*) {
            .var_, .meta, .sym => 1,
            .app => |a| blk: {
                var max_child: u32 = 0;
                for (a.args) |arg| {
                    const c = arg.complexity();
                    if (c > max_child) max_child = c;
                }
                break :blk a.head.complexity() + max_child + 1;
            },
        };
    }

    /// 構造的等値比較
    pub fn eql(a: *const Expr, b: *const Expr) bool {
        if (a == b) return true;
        return switch (a.*) {
            .var_ => |n| b.* == .var_ and std.mem.eql(u8, n, b.var_),
            .meta => |id| b.* == .meta and MetaId.eql(id, b.meta),
            .sym => |n| b.* == .sym and std.mem.eql(u8, n, b.sym),
            .app => |aa| blk: {
                if (b.* != .app) break :blk false;
                const ba = b.app;
                if (!aa.head.eql(ba.head)) break :blk false;
                if (aa.args.len != ba.args.len) break :blk false;
                for (aa.args, ba.args) |x, y| {
                    if (!x.eql(y)) break :blk false;
                }
                break :blk true;
            },
        };
    }

    /// 他の式を含むか
    pub fn contains(self: *const Expr, other: *const Expr) bool {
        if (self.eql(other)) return true;
        return switch (self.*) {
            .app => |a| {
                if (a.head.contains(other)) return true;
                for (a.args) |arg| {
                    if (arg.contains(other)) return true;
                }
                return false;
            },
            else => false,
        };
    }

    /// アリーナに式を複製
    pub fn dupeArena(self: *const Expr, arena: Allocator) !*const Expr {
        const new_expr = try arena.create(Expr);
        new_expr.* = switch (self.*) {
            .var_ => |n| .{ .var_ = try arena.dupe(u8, n) },
            .meta => |id| .{ .meta = try id.dupe(arena) },
            .sym => |n| .{ .sym = try arena.dupe(u8, n) },
            .app => |a| blk: {
                const new_head = try a.head.dupeArena(arena);
                const new_args = try arena.alloc(*const Expr, a.args.len);
                for (a.args, 0..) |arg, i| {
                    new_args[i] = try arg.dupeArena(arena);
                }
                break :blk .{ .app = .{ .head = new_head, .args = new_args } };
            },
        };
        return new_expr;
    }

    /// Expr のハッシュ値 (発散検出・キャッシュキー用)
    pub fn hash(self: *const Expr) u64 {
        var h = std.hash.Wyhash.init(0x52_6f_6d_61); // 'Roma'
        hashInto(self, &h);
        return h.final();
    }

    fn hashInto(self: *const Expr, h: *std.hash.Wyhash) void {
        switch (self.*) {
            .sym => |n| {
                h.update("s");
                h.update(n);
            },
            .var_ => |n| {
                h.update("v");
                h.update(n);
            },
            .meta => |id| {
                h.update("m");
                h.update(std.mem.sliceAsBytes(id.ids));
            },
            .app => |a| {
                h.update("a");
                hashInto(a.head, h);
                var len_bytes: [4]u8 = undefined;
                std.mem.writeInt(u32, &len_bytes, @intCast(a.args.len), .little);
                h.update(&len_bytes);
                for (a.args) |arg| {
                    hashInto(arg, h);
                }
            },
        }
    }

    /// フォーマット出力
    pub fn format(self: *const Expr, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.*) {
            .var_ => |n| try writer.writeAll(n),
            .meta => |id| {
                try writer.writeByte('?');
                try id.format("", .{}, writer);
            },
            .sym => |n| try writer.writeAll(n),
            .app => |a| {
                // λ式の特別表示
                if (a.head.* == .sym and std.mem.eql(u8, a.head.sym, "λ") and a.args.len == 2 and a.args[0].* == .var_) {
                    try writer.writeAll("λ");
                    try writer.writeAll(a.args[0].var_);
                    try writer.writeAll(". ");
                    try a.args[1].format("", .{}, writer);
                    return;
                }
                // =の特別表示
                if (a.head.* == .sym and std.mem.eql(u8, a.head.sym, "=") and a.args.len == 2) {
                    try a.args[0].format("", .{}, writer);
                    try writer.writeAll(" = ");
                    try a.args[1].format("", .{}, writer);
                    return;
                }
                if (a.args.len == 0) {
                    try a.head.format("", .{}, writer);
                } else {
                    try a.head.format("", .{}, writer);
                    try writer.writeByte('(');
                    for (a.args, 0..) |arg, i| {
                        if (i > 0) try writer.writeByte(',');
                        try arg.format("", .{}, writer);
                    }
                    try writer.writeByte(')');
                }
            },
        }
    }
};

// ==========================================
// Expr構築ヘルパー (アリーナ割当)
// ==========================================

pub fn sym(arena: Allocator, name: []const u8) !*const Expr {
    const e = try arena.create(Expr);
    e.* = .{ .sym = name };
    return e;
}

pub fn var_(arena: Allocator, name: []const u8) !*const Expr {
    const e = try arena.create(Expr);
    e.* = .{ .var_ = name };
    return e;
}

pub fn meta(arena: Allocator, id: i32) !*const Expr {
    const e = try arena.create(Expr);
    const ids = try arena.alloc(i32, 1);
    ids[0] = id;
    e.* = .{ .meta = .{ .ids = ids } };
    return e;
}

pub fn app(arena: Allocator, head: *const Expr, args: []const *const Expr) !*const Expr {
    const e = try arena.create(Expr);
    const args_copy = try arena.alloc(*const Expr, args.len);
    @memcpy(args_copy, args);
    e.* = .{ .app = .{ .head = head, .args = args_copy } };
    return e;
}

pub fn app1(arena: Allocator, head: *const Expr, arg: *const Expr) !*const Expr {
    return app(arena, head, &[_]*const Expr{arg});
}

pub fn app2(arena: Allocator, head: *const Expr, a: *const Expr, b: *const Expr) !*const Expr {
    return app(arena, head, &[_]*const Expr{ a, b });
}

// ==========================================
// RuleBuilder — 規則構築ヘルパー
// ==========================================

pub const RuleBuilder = struct {
    arena: Allocator,

    pub fn init(arena: Allocator) RuleBuilder {
        return .{ .arena = arena };
    }

    pub fn s(self: RuleBuilder, name: []const u8) !*const Expr {
        return sym(self.arena, name);
    }

    pub fn v(self: RuleBuilder, name: []const u8) !*const Expr {
        return var_(self.arena, name);
    }

    pub fn a1(self: RuleBuilder, head: *const Expr, arg: *const Expr) !*const Expr {
        return app1(self.arena, head, arg);
    }

    pub fn a2(self: RuleBuilder, head: *const Expr, arg1: *const Expr, arg2: *const Expr) !*const Expr {
        return app2(self.arena, head, arg1, arg2);
    }

    pub fn a3(self: RuleBuilder, head: *const Expr, arg1: *const Expr, arg2: *const Expr, arg3: *const Expr) !*const Expr {
        return app(self.arena, head, &[_]*const Expr{ arg1, arg2, arg3 });
    }

    pub fn a4(self: RuleBuilder, head: *const Expr, a1_: *const Expr, a2_: *const Expr, a3_: *const Expr, a4_: *const Expr) !*const Expr {
        return app(self.arena, head, &[_]*const Expr{ a1_, a2_, a3_, a4_ });
    }
};

// ==========================================
// CatRule — 圏論的推論規則
// ==========================================

pub const CatRule = struct {
    name: []const u8,
    lhs: *const Expr,
    rhs: *const Expr,
    universals: []const *const Expr = &.{},
    domain: []const u8 = "general",
};

// ==========================================
// ContextEntry — コンテキストの項
// ==========================================

pub const ContextEntry = struct {
    name: []const u8,
    expr: *const Expr,
};

// ==========================================
// Goal — 証明ゴール
// ==========================================

pub const Goal = struct {
    context: []const ContextEntry = &.{},
    linear_context: []const ContextEntry = &.{},
    target: *const Expr,
};

// ==========================================
// LogicState — 論理状態
// ==========================================

pub const LogicState = struct {
    linear_context: []const ContextEntry = &.{},
    meta_counter: i32 = 0,
    induction_depth: u32 = 0,
    raa_depth: u32 = 0,
    path_level: u32 = 0,
};

// ==========================================
// InitialAlgebra — 帰納型定義
// ==========================================

pub const ArgType = enum { recursive, constant };

pub const ConstructorType = union(enum) {
    point: void,
    path: struct { from: *const Expr, to: *const Expr },
};

pub const ConstructorDef = struct {
    symbol: []const u8,
    arg_types: []const ArgType,
    ctor_type: ConstructorType = .point,
};

pub const InitialAlgebra = struct {
    name: []const u8,
    constructors: []const ConstructorDef,
    var_prefix: []const u8,
};

// ==========================================
// ProverConfig — 証明エンジンの設定
// ==========================================

pub const LemmaGenerationMode = enum {
    all,
    induction_only,
    equality_only,
    manual_only,
};

pub const ProverConfig = struct {
    classical: bool = false,
    rules: []const CatRule = &.{},
    algebras: []const InitialAlgebra = &.{},
    rewrite_hooks: []const @import("rewriter.zig").RewriteHook = &.{},
    max_raa: u32 = 2,
    max_induction: u32 = 2,
    max_path_level: u32 = 5,
    max_complexity: u32 = 200,
    generate_lemmas: bool = true,
    lemma_mode: LemmaGenerationMode = .equality_only,
    exclude_trivial_lemmas: bool = true,
};

// ==========================================
// Tree(T) — 汎用ツリー構造 (Types.scala移植)
// ==========================================

pub fn Tree(comptime T: type) type {
    return union(enum) {
        empty: void,
        node: Node,

        const Self = @This();

        pub const Node = struct {
            value: T,
            children: []const Self,
        };

        pub fn leaf(arena: Allocator, value: T) !Self {
            return .{ .node = .{
                .value = value,
                .children = try arena.alloc(Self, 0),
            } };
        }

        pub fn branch(arena: Allocator, value: T, children: []const Self) !Self {
            const owned = try arena.alloc(Self, children.len);
            @memcpy(owned, children);
            return .{ .node = .{
                .value = value,
                .children = owned,
            } };
        }
    };
}

// ==========================================
// SearchNode — 探索ノード (可視化用)
// ==========================================

pub const SearchNode = struct {
    goal: []const u8, // ゴール式の文字列表現
    rule_name: []const u8,
    status: enum { success, failure, in_progress },
    depth: u32,
};

// ==========================================
// ProofTree — 証明のツリー構造
// ==========================================

pub const ProofTree = union(enum) {
    node: struct {
        goal: *const Expr,
        rule_name: []const u8,
        children: []const ProofTree,
    },
    leaf: struct {
        goal: *const Expr,
        rule_name: []const u8,
    },
};

// ==========================================
// FailTrace — 証明の失敗トレース
// ==========================================

pub const FailTrace = struct {
    goal: Goal,
    reason: []const u8,
    depth: u32,
    children: []const FailTrace = &.{},
    failure_type: []const u8 = "Normal",
    attempted_rules: []const []const u8 = &.{},
};

// ==========================================
// ProofResult — 証明結果
// ==========================================

pub const ProofResult = struct {
    tree: ProofTree,
    generated_lemma: ?CatRule = null,
    search_tree: ?Tree(SearchNode) = null,
};

// ==========================================
// Subst — 置換 (MetaId → Expr)
// ==========================================

pub const MetaIdContext = struct {
    pub fn hash(_: MetaIdContext, key: MetaId) u64 {
        return key.hash();
    }
    pub fn eql(_: MetaIdContext, a: MetaId, b: MetaId) bool {
        return MetaId.eql(a, b);
    }
};

pub const Subst = std.HashMap(MetaId, *const Expr, MetaIdContext, std.hash_map.default_max_load_percentage);

// ==========================================
// ProofState — タクティクス用証明状態
// ==========================================

pub const ProofState = struct {
    goals: []const Goal,
    completed_proofs: []const ProofTree,
    original_goal: *const Expr,
    search_tree: ?Tree(SearchNode) = null,
    rules: []const CatRule = &.{},

    pub fn isSolved(self: *const ProofState) bool {
        return self.goals.len == 0;
    }

    pub fn currentGoal(self: *const ProofState) ?Goal {
        return if (self.goals.len > 0) self.goals[0] else null;
    }
};

// ==========================================
// テスト
// ==========================================

test "Expr basic construction and equality" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const a2 = try sym(arena, "A");

    try std.testing.expect(a.eql(a2));
    try std.testing.expect(!a.eql(b));
}

test "Expr App construction" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const and_sym = try sym(arena, "∧");
    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const and_ab = try app2(arena, and_sym, a, b);

    try std.testing.expectEqualStrings("∧", and_ab.headSymbol());
    try std.testing.expect(and_ab.complexity() > 1);
}

test "Expr contains" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const a = try sym(arena, "A");
    const b = try sym(arena, "B");
    const f = try sym(arena, "f");
    const fa = try app1(arena, f, a);

    try std.testing.expect(fa.contains(a));
    try std.testing.expect(!fa.contains(b));
}

test "MetaId equality" {
    const ids1 = [_]i32{ 1, 2, 3 };
    const ids2 = [_]i32{ 1, 2, 3 };
    const ids3 = [_]i32{ 1, 2 };

    const m1 = MetaId{ .ids = &ids1 };
    const m2 = MetaId{ .ids = &ids2 };
    const m3 = MetaId{ .ids = &ids3 };

    try std.testing.expect(MetaId.eql(m1, m2));
    try std.testing.expect(!MetaId.eql(m1, m3));
}

test "Tree construction" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const SearchTree = Tree(SearchNode);
    const leaf = try SearchTree.leaf(arena, .{
        .goal = "A ∧ B",
        .rule_name = "axiom",
        .status = .success,
        .depth = 0,
    });

    try std.testing.expect(leaf == .node);
    try std.testing.expectEqualStrings("axiom", leaf.node.value.rule_name);
}

test "Subst basic operations" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var subst = Subst.init(std.testing.allocator);
    defer subst.deinit();

    const ids = try arena.alloc(i32, 1);
    ids[0] = 0;
    const mid = MetaId{ .ids = ids };
    const a = try sym(arena, "A");

    try subst.put(mid, a);

    const result = subst.get(mid);
    try std.testing.expect(result != null);
    try std.testing.expect(result.?.eql(a));
}
