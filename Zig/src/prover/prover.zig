// ==========================================
// prover.zig
// Romanesco証明器 モジュールルート
// ==========================================

pub const expr = @import("expr.zig");
pub const symbols = @import("symbols.zig");
pub const unifier = @import("unifier.zig");
pub const rewriter = @import("rewriter.zig");
pub const search = @import("search.zig");
pub const plugin = @import("plugin.zig");
pub const rules = @import("rules.zig");
pub const tactics = @import("tactics.zig");
pub const parser = @import("parser.zig");
pub const repl = @import("repl.zig");
pub const lemma_manager = @import("lemma_manager.zig");

// 主要型のre-export
pub const Expr = expr.Expr;
pub const MetaId = expr.MetaId;
pub const Goal = expr.Goal;
pub const CatRule = expr.CatRule;
pub const ProofTree = expr.ProofTree;
pub const ProofResult = expr.ProofResult;
pub const Subst = expr.Subst;
pub const ProverConfig = expr.ProverConfig;
pub const InitialAlgebra = expr.InitialAlgebra;
pub const InternedString = symbols.InternedString;
pub const StringPool = symbols.StringPool;
pub const Symbols = symbols.Symbols;

// 構築ヘルパー
pub const sym = expr.sym;
pub const var_ = expr.var_;
pub const meta = expr.meta;
pub const app = expr.app;
pub const app1 = expr.app1;
pub const app2 = expr.app2;
pub const Tree = expr.Tree;

// ユニファイア関数のre-export
pub const unify = unifier.unify;
pub const applySubst = unifier.applySubst;
pub const substVar = unifier.substVar;
pub const normalize = rewriter.normalize;
pub const parse = parser.parse;

// エンジン
pub const ProverEngine = search.ProverEngine;
pub const Plugin = search.Plugin;
pub const all_plugins = plugin.all_plugins;
pub const RuleBuilder = rules.RuleBuilder;

// 補題管理のre-export
pub const saveLemmas = lemma_manager.saveLemmas;
pub const loadLemmas = lemma_manager.loadLemmas;

test {
    _ = expr;
    _ = symbols;
    _ = unifier;
    _ = rewriter;
    _ = search;
    _ = plugin;
    _ = rules;
    _ = tactics;
    _ = parser;
    _ = repl;
    _ = lemma_manager;
    _ = @import("integration_test.zig");
}
