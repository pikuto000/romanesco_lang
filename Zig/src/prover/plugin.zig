// ==========================================
// plugin.zig
// プラグインレジストリ (PluginRegistry.scala移植)
// 全プラグインの登録と依存関係解決
// ==========================================

const std = @import("std");
const search_mod = @import("search.zig");
const Plugin = search_mod.Plugin;

// プラグインのインポート
pub const axiom = @import("plugins/axiom.zig");
pub const introduction = @import("plugins/introduction.zig");
pub const user_rule = @import("plugins/user_rule.zig");
pub const persistent = @import("plugins/persistent.zig");
pub const linear = @import("plugins/linear.zig");
pub const hott = @import("plugins/hott.zig");
pub const cubical = @import("plugins/cubical.zig");
pub const hoare = @import("plugins/hoare.zig");
pub const modal = @import("plugins/modal.zig");
pub const temporal = @import("plugins/temporal.zig");
pub const induction = @import("plugins/induction.zig");
pub const rewrite_plugin = @import("plugins/rewrite_plugin.zig");
pub const forward = @import("plugins/forward.zig");
pub const algebra = @import("plugins/algebra.zig");

/// デフォルトの全プラグイン (priority順にソート済み)
pub const all_plugins = [_]Plugin{
    axiom.plugin, // 10
    introduction.plugin, // 50
    linear.plugin, // 80
    persistent.plugin, // 100
    hott.plugin, // 120
    cubical.plugin, // 125
    modal.plugin, // 130
    temporal.plugin, // 140
    hoare.plugin, // 150
    rewrite_plugin.plugin, // 160
    algebra.plugin, // 170
    induction.plugin, // 180
    forward.plugin, // 190
    user_rule.plugin, // 200
};

/// 標準プラグインパック
pub const standard_pack = [_]Plugin{
    axiom.plugin,
    introduction.plugin,
    persistent.plugin,
    user_rule.plugin,
};

/// HoTTプラグインパック
pub const hott_pack = [_]Plugin{
    hott.plugin,
    cubical.plugin,
};

/// リソース論理プラグインパック
pub const resource_pack = [_]Plugin{
    linear.plugin,
    hoare.plugin,
};

/// モーダル・時相論理プラグインパック
pub const modal_temporal_pack = [_]Plugin{
    modal.plugin,
    temporal.plugin,
};

/// 高度推論プラグインパック
pub const advanced_pack = [_]Plugin{
    induction.plugin,
    rewrite_plugin.plugin,
    forward.plugin,
};

test "all plugins have unique names" {
    var names = std.StringHashMap(void).init(std.testing.allocator);
    defer names.deinit();
    for (&all_plugins) |*p| {
        try std.testing.expect(!names.contains(p.name));
        try names.put(p.name, {});
    }
}

test "all plugins are sorted by priority" {
    var prev_priority: u32 = 0;
    for (&all_plugins) |*p| {
        try std.testing.expect(p.priority >= prev_priority);
        prev_priority = p.priority;
    }
}
