// ==========================================
// search.zig
// 証明探索エンジン (Prover.scala移植)
// 反復深化 + プラグインベースの探索
// ==========================================

const std = @import("std");
const Allocator = std.mem.Allocator;
const expr_mod = @import("expr.zig");
const unifier_mod = @import("unifier.zig");
const rewriter_mod = @import("rewriter.zig");
const Expr = expr_mod.Expr;
const MetaId = expr_mod.MetaId;
const Goal = expr_mod.Goal;
const CatRule = expr_mod.CatRule;
const ContextEntry = expr_mod.ContextEntry;
const LogicState = expr_mod.LogicState;
const ProverConfig = expr_mod.ProverConfig;
const ProofTree = expr_mod.ProofTree;
const ProofResult = expr_mod.ProofResult;
const FailTrace = expr_mod.FailTrace;
const SearchNode = expr_mod.SearchNode;
const Subst = expr_mod.Subst;
const Tree = expr_mod.Tree;
const sym = expr_mod.sym;
const var_ = expr_mod.var_;
const meta = expr_mod.meta;
const app = expr_mod.app;
const app1 = expr_mod.app1;
const app2 = expr_mod.app2;

/// プラグインインターフェース (関数ポインタ構造体)
pub const Plugin = struct {
    name: []const u8,
    priority: u32 = 100,
    provided_rules: []const CatRule = &.{},
    provided_algebras: []const expr_mod.InitialAlgebra = &.{},

    /// ゴールフック: ゴール式に対してブランチを生成
    goal_hooks: ?*const fn (args: HookArgs) HookError![]const Tree(SearchNode) = null,
    /// コンテキストフック: コンテキストに対してブランチを生成
    context_hooks: ?*const fn (args: HookArgs) HookError![]const Tree(SearchNode) = null,
    /// 正規化フック
    normalize_hook: ?*const fn (e: *const Expr, arena: Allocator) ?*const Expr = null,
};

pub const HookError = error{OutOfMemory, Timeout};

/// フック関数の引数
pub const HookArgs = struct {
    goal: *const Expr,
    context: []const ContextEntry,
    state: LogicState,
    subst: Subst,
    depth: u32,
    limit: u32,
    arena: Allocator,
    prover: *ProverEngine,
};

/// 証明探索エンジン
pub const ProverEngine = struct {
    config: ProverConfig,
    plugins: []const Plugin,
    arena: Allocator,
    meta_counter: i32 = 0,
    deadline_ms: i64 = 0,

    // キャッシュ
    // NOTE: Zigではアリーナ単位なのでprove()呼び出し毎にリセットされる

    /// 初期化
    pub fn init(config: ProverConfig, plugins: []const Plugin, arena: Allocator) ProverEngine {
        return .{
            .config = config,
            .plugins = plugins,
            .arena = arena,
        };
    }

    /// フレッシュメタ変数の生成
    pub fn freshMeta(self: *ProverEngine) !*const Expr {
        self.meta_counter += 1;
        return meta(self.arena, self.meta_counter);
    }

    /// 正規化
    pub fn normalize(self: *ProverEngine, e: *const Expr) !*const Expr {
        return rewriter_mod.normalize(e, self.config.rules, self.arena);
    }

    /// タイムアウトチェック
    pub fn checkDeadline(self: *ProverEngine) !void {
        if (self.deadline_ms > 0) {
            const now = std.time.milliTimestamp();
            if (now > self.deadline_ms) return error.Timeout;
        }
    }

    /// メイン証明関数
    pub fn prove(
        self: *ProverEngine,
        goal_expr: *const Expr,
        max_depth: u32,
        timeout_ms: i64,
    ) ProveError!ProveResult {
        self.deadline_ms = if (timeout_ms > 0) std.time.milliTimestamp() + timeout_ms else 0;

        const initial_goal = Goal{
            .target = goal_expr,
        };

        // 反復深化
        var d: u32 = 1;
        while (d <= max_depth) : (d += 1) {
            self.checkDeadline() catch return .{ .fail = .{
                .goal = initial_goal,
                .reason = "Timeout",
                .depth = d,
            } };

            const search_tree = try self.search(
                goal_expr,
                &.{},
                LogicState{},
                Subst.init(self.arena),
                0,
                d,
            );

            // 成功ノードを探す
            if (findSuccess(search_tree)) |node| {
                return .{ .success = .{
                    .tree = .{ .leaf = .{ .goal = goal_expr, .rule_name = node.rule_name } },
                    .search_tree = search_tree,
                } };
            }
        }

        return .{ .fail = .{
            .goal = initial_goal,
            .reason = "No proof found",
            .depth = max_depth,
        } };
    }

    /// 探索の本体
    pub fn search(
        self: *ProverEngine,
        goal: *const Expr,
        context: []const ContextEntry,
        state: LogicState,
        subst: Subst,
        depth: u32,
        limit: u32,
    ) ProveError!Tree(SearchNode) {
        self.checkDeadline() catch return makeFailNode(self.arena, goal, "Timeout", depth);

        // 現在のゴールを正規化
        const current_goal = try self.normalize(try unifier_mod.applySubst(goal, &subst, self.arena));

        // 深さ制限
        if (depth > limit) {
            return makeFailNode(self.arena, current_goal, "Limit reached", depth);
        }

        // 複雑さチェック
        if (current_goal.complexity() > self.config.max_complexity) {
            return makeFailNode(self.arena, current_goal, "Complexity limit", depth);
        }

        // プラグインからブランチを収集
        var branches: std.ArrayList(Tree(SearchNode)) = .{};

        for (self.plugins) |plugin| {
            self.checkDeadline() catch break;

            if (plugin.goal_hooks) |hook| {
                const hook_args = HookArgs{
                    .goal = current_goal,
                    .context = context,
                    .state = state,
                    .subst = subst,
                    .depth = depth,
                    .limit = limit,
                    .arena = self.arena,
                    .prover = self,
                };
                const results = try hook(hook_args);
                for (results) |r| {
                    try branches.append(self.arena, r);
                }
            }

            if (plugin.context_hooks) |hook| {
                const hook_args = HookArgs{
                    .goal = current_goal,
                    .context = context,
                    .state = state,
                    .subst = subst,
                    .depth = depth,
                    .limit = limit,
                    .arena = self.arena,
                    .prover = self,
                };
                const results = try hook(hook_args);
                for (results) |r| {
                    try branches.append(self.arena, r);
                }
            }
        }

        // 成功ブランチを探す
        var best_success: ?SearchNode = null;
        for (branches.items) |branch| {
            if (branch == .node and branch.node.value.status == .success) {
                best_success = branch.node.value;
                break;
            }
            // 子ノードも探索
            if (findSuccess(branch)) |node| {
                best_success = node;
                break;
            }
        }

        if (best_success) |s| {
            return Tree(SearchNode).leaf(self.arena, s);
        }

        // 失敗
        const fail_node = SearchNode{
            .goal = "failed",
            .rule_name = if (branches.items.len == 0) "failure" else "choice",
            .status = .failure,
            .depth = depth,
        };

        if (branches.items.len == 0) {
            return Tree(SearchNode).leaf(self.arena, fail_node);
        }

        return Tree(SearchNode).branch(self.arena, fail_node, branches.items);
    }

    /// ルール適用 (バックワード: RHSとゴールをユニファイ)
    pub fn applyRules(
        self: *ProverEngine,
        goal: *const Expr,
        subst: Subst,
    ) ![]const RuleApplication {
        var results: std.ArrayList(RuleApplication) = .{};
        const head = goal.headSymbol();

        for (self.config.rules) |rule| {
            if (!std.mem.eql(u8, rule.rhs.headSymbol(), head)) continue;

            // ルールをインスタンス化 (変数をメタ変数に置換)
            const inst_rule = try self.instantiateRule(rule);
            const unify_result = try unifier_mod.unify(goal, inst_rule.rhs, subst, self.arena);
            if (unify_result.first()) |s| {
                const lhs = try self.normalize(try unifier_mod.applySubst(inst_rule.lhs, &s, self.arena));
                try results.append(self.arena, .{
                    .new_goal = lhs,
                    .rule_name = rule.name,
                    .subst = s,
                });
            }
        }

        // プラグイン提供のルールも適用
        for (self.plugins) |plugin| {
            for (plugin.provided_rules) |rule| {
                if (!std.mem.eql(u8, rule.rhs.headSymbol(), head)) continue;
                const inst_rule = try self.instantiateRule(rule);
                const unify_result = try unifier_mod.unify(goal, inst_rule.rhs, subst, self.arena);
                if (unify_result.first()) |s| {
                    const lhs = try self.normalize(try unifier_mod.applySubst(inst_rule.lhs, &s, self.arena));
                    try results.append(self.arena, .{
                        .new_goal = lhs,
                        .rule_name = rule.name,
                        .subst = s,
                    });
                }
            }
        }

        return results.items;
    }

    /// 前方推論ルール適用
    pub fn forwardApplyRules(
        self: *ProverEngine,
        e: *const Expr,
        subst: Subst,
    ) ![]const ForwardResult {
        var results: std.ArrayList(ForwardResult) = .{};
        const head = e.headSymbol();

        for (self.config.rules) |rule| {
            if (!std.mem.eql(u8, rule.lhs.headSymbol(), head)) continue;
            const inst_rule = try self.instantiateRule(rule);
            const unify_result = try unifier_mod.unify(e, inst_rule.lhs, subst, self.arena);
            if (unify_result.first()) |s| {
                const rhs = try self.normalize(try unifier_mod.applySubst(inst_rule.rhs, &s, self.arena));
                try results.append(self.arena, .{
                    .result = rhs,
                    .rule_name = rule.name,
                    .subst = s,
                });
            }
        }

        return results.items;
    }

    /// ルールの変数をフレッシュメタ変数に置換
    fn instantiateRule(self: *ProverEngine, rule: CatRule) !CatRule {
        // 変数を収集
        var vars = std.StringHashMap(void).init(self.arena);
        try collectVars(rule.lhs, &vars);
        try collectVars(rule.rhs, &vars);

        if (vars.count() == 0) return rule;

        // 各変数にフレッシュメタ変数を割り当て
        var var_subst = std.StringHashMap(*const Expr).init(self.arena);
        var iter = vars.iterator();
        while (iter.next()) |entry| {
            const m = try self.freshMeta();
            try var_subst.put(entry.key_ptr.*, m);
        }

        return .{
            .name = rule.name,
            .lhs = try applyVarSubst(rule.lhs, &var_subst, self.arena),
            .rhs = try applyVarSubst(rule.rhs, &var_subst, self.arena),
            .universals = rule.universals,
            .domain = rule.domain,
        };
    }
};

/// ルール適用結果
pub const RuleApplication = struct {
    new_goal: *const Expr,
    rule_name: []const u8,
    subst: Subst,
};

/// 前方推論結果
pub const ForwardResult = struct {
    result: *const Expr,
    rule_name: []const u8,
    subst: Subst,
};

/// 証明結果
pub const ProveResult = union(enum) {
    success: struct {
        tree: ProofTree,
        search_tree: Tree(SearchNode),
    },
    fail: FailTrace,
};

pub const ProveError = error{ OutOfMemory, Timeout };

/// 成功ノードを探す
pub fn findSuccess(tree: Tree(SearchNode)) ?SearchNode {
    return switch (tree) {
        .empty => null,
        .node => |n| {
            if (n.value.status == .success) return n.value;
            for (n.children) |child| {
                if (findSuccess(child)) |s| return s;
            }
            return null;
        },
    };
}

/// 失敗ノードを作成
fn makeFailNode(arena_alloc: Allocator, goal: *const Expr, reason: []const u8, depth: u32) Tree(SearchNode) {
    _ = goal;
    return Tree(SearchNode).leaf(arena_alloc, .{
        .goal = reason,
        .rule_name = "failure",
        .status = .failure,
        .depth = depth,
    }) catch .empty;
}

/// 式から変数を収集
fn collectVars(e: *const Expr, result: *std.StringHashMap(void)) !void {
    switch (e.*) {
        .var_ => |n| try result.put(n, {}),
        .app => |a| {
            try collectVars(a.head, result);
            for (a.args) |arg| {
                try collectVars(arg, result);
            }
        },
        else => {},
    }
}

/// 変数マップに基づく置換
fn applyVarSubst(e: *const Expr, var_subst: *const std.StringHashMap(*const Expr), arena_alloc: Allocator) !*const Expr {
    return switch (e.*) {
        .var_ => |n| var_subst.get(n) orelse e,
        .app => |a| {
            const new_head = try applyVarSubst(a.head, var_subst, arena_alloc);
            const new_args = try arena_alloc.alloc(*const Expr, a.args.len);
            for (a.args, 0..) |arg, i| {
                new_args[i] = try applyVarSubst(arg, var_subst, arena_alloc);
            }
            const result = try arena_alloc.create(Expr);
            result.* = .{ .app = .{ .head = new_head, .args = new_args } };
            return result;
        },
        else => e,
    };
}

// ==========================================
// テスト
// ==========================================

test "ProverEngine initialization" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var engine = ProverEngine.init(ProverConfig{}, &.{}, arena);
    const m = try engine.freshMeta();
    try std.testing.expect(m.* == .meta);
}

test "ProverEngine normalize" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    var engine = ProverEngine.init(ProverConfig{}, &.{}, arena);
    const a = try sym(arena, "A");
    const result = try engine.normalize(a);
    try std.testing.expect(result.eql(a));
}

test "collectVars" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const x = try var_(arena, "x");
    const f = try sym(arena, "f");
    const fx = try app1(arena, f, x);

    var vars = std.StringHashMap(void).init(arena);
    try collectVars(fx, &vars);
    try std.testing.expect(vars.contains("x"));
    try std.testing.expect(!vars.contains("f"));
}

test "ProverEngine prove simple tautology" {
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // ⊤の証明 (プラグインなしだと失敗するが、エンジン自体は動く)
    var engine = ProverEngine.init(ProverConfig{}, &.{}, arena);
    const goal = try sym(arena, "⊤");
    const result = engine.prove(goal, 5, 1000) catch |err| switch (err) {
        error.Timeout => ProveResult{ .fail = .{ .goal = Goal{ .target = goal }, .reason = "Timeout", .depth = 0 } },
        else => return err,
    };
    // プラグインなしなので失敗するはず
    try std.testing.expect(result == .fail);
}
