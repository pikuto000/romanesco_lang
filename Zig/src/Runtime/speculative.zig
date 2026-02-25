const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const jit_mod = @import("jit.zig");
const codegen_mod = @import("codegen.zig");
const analyzer_mod = @import("analyzer.zig");
const optimizer_mod = @import("optimizer.zig");
const Op = vm.Op;
const Value = vm.Value;
const NativeEntry = vm.NativeEntry;

pub const TieredArtifact = struct {
    lib: std.DynLib,
    func: NativeEntry,
    tier: vm.Tier,
    path: []const u8, // Store the path to the DLL for cleanup
};

pub const SpeculativeExecutor = struct {
    allocator: Allocator,
    vm: vm.ProfilingVM,
    jit: jit_mod.JIT,
    optimizer: optimizer_mod.Optimizer,
    analyzer: analyzer_mod.RangeAnalyzer,
    codegen: codegen_mod.CodeGen,
    cpu_features: vm.CpuFeatures,
    
    // Config
    hot_threshold: usize = 5,
    promotion_threshold: usize = 20,
    keep_artifacts: bool = false, // Debug option
    
    // Functions and their tiered versions
    artifacts: std.ArrayList(TieredArtifact),

    pub fn init(allocator: Allocator) SpeculativeExecutor {
        return .{
            .allocator = allocator,
            .vm = vm.ProfilingVM.init(allocator),
            .jit = jit_mod.JIT.init(allocator),
            .optimizer = optimizer_mod.Optimizer.init(allocator),
            .analyzer = analyzer_mod.RangeAnalyzer.init(allocator),
            .codegen = codegen_mod.CodeGen.init(allocator),
            .cpu_features = vm.CpuFeatures.detect(),
            .artifacts = std.ArrayList(TieredArtifact).initCapacity(allocator, 0) catch unreachable,
        };
    }

    pub fn cleanupOldArtifacts(self: *SpeculativeExecutor) void {
        _ = self;
        const tmp_path = ".jit_tmp";
        if (std.fs.cwd().openDir(tmp_path, .{ .iterate = true })) |dir| {
            var d = dir;
            defer d.close();
            var iter = d.iterate();
            while (iter.next() catch null) |entry| {
                if (entry.kind == .file) {
                    d.deleteFile(entry.name) catch {};
                }
            }
        } else |_| {}
    }

    pub fn deinit(self: *SpeculativeExecutor) void {
        self.vm.deinit();
        for (self.artifacts.items) |*art| {
            art.lib.close();
            
            if (!self.keep_artifacts) {
                // Delete the DLL file
                std.fs.cwd().deleteFile(art.path) catch {};
                
                // Delete Windows specific companion files
                if (@import("builtin").os.tag == .windows) {
                    const base_path = art.path[0 .. art.path.len - 4]; // Remove .dll
                    var lib_buf: [128]u8 = undefined;
                    var exp_buf: [128]u8 = undefined;
                    const lib_path = std.fmt.bufPrint(&lib_buf, "{s}.lib", .{base_path}) catch "";
                    const exp_path = std.fmt.bufPrint(&exp_buf, "{s}.exp", .{base_path}) catch "";
                    if (lib_path.len > 0) std.fs.cwd().deleteFile(lib_path) catch {};
                    if (exp_path.len > 0) std.fs.cwd().deleteFile(exp_path) catch {};
                }
            }
            self.allocator.free(art.path);
        }
        self.artifacts.deinit(self.allocator);
    }

    pub fn execute(self: *SpeculativeExecutor, program: []const []const Op, code: []const Op) !Value {
        var profile = &self.vm.profile_data;
        try profile.record(0);
        
        const entry_count = profile.counts.get(0) orelse 0;

        // Dynamic Tiering: Check if we should promote to a higher tier
        const current_tier = if (self.artifacts.items.len > 0) self.artifacts.items[self.artifacts.items.len - 1].tier else 0;
        
        // Example: Tier N exists if entry_count >= threshold * N
        const next_tier = current_tier + 1;
        if (entry_count >= self.hot_threshold * next_tier) {
            std.debug.print("[SpeculativeExecutor] Promoting to Level {d} JIT...\n", .{next_tier});
            
            const optimized_code = try self.optimizer.optimize(code);
            defer {
                for (optimized_code) |op| {
                    switch (op) {
                        .make_closure => |o| self.allocator.free(o.captures),
                        .call => |o| self.allocator.free(o.args),
                        else => {},
                    }
                }
                self.allocator.free(optimized_code);
            }
            
            var analysis = try self.analyzer.analyze(optimized_code, profile, 0); // Assuming main block 0 for now
            defer analysis.deinit();
            
            // LEVEL 2+ SPECIALIZATION:
            if (next_tier >= 2) {
                // Look for stable values at entry (PC=0)
                var reg: u32 = 0;
                while (reg < 32) : (reg += 1) {
                    // If we saw this register have the same value > 80% of the time
                    if (profile.getStableValue(0, reg, (entry_count * 8) / 10)) |val| {
                        std.debug.print("  Specializing reg {d} with constant {d}\n", .{reg, val});
                        try analysis.stable_values.put(reg, val);
                    }
                }
            }

            const timestamp = std.time.nanoTimestamp();
            var name_buf: [64]u8 = undefined;
            const entry_name = try std.fmt.bufPrint(&name_buf, "tier_{d}_{d}", .{ next_tier, timestamp });
            
            const ir = try self.codegen.generate(program, analysis, entry_name, self.cpu_features);
            defer self.allocator.free(ir);
            
            var dll_name_buf: [128]u8 = undefined;
            const dll_name = try std.fmt.bufPrint(&dll_name_buf, ".jit_tmp/tier{d}_{d}.dll", .{ next_tier, timestamp });
            
            try self.jit.compile(ir, dll_name);
            var lib = try std.DynLib.open(dll_name);
            const entry_name_z = try self.allocator.dupeZ(u8, entry_name);
            defer self.allocator.free(entry_name_z);
            
            const func = lib.lookup(vm.NativeEntry, entry_name_z) orelse return error.SymbolNotFound;
            
            try self.artifacts.append(self.allocator, .{ 
                .lib = lib, 
                .func = func, 
                .tier = next_tier,
                .path = try self.allocator.dupe(u8, dll_name),
            });
        }

        // Execution Dispatch: Run the highest available tier
        if (self.artifacts.items.len > 0) {
            const art = self.artifacts.items[self.artifacts.items.len - 1];
            
            const regs = try self.allocator.alloc(Value, 32);
            defer {
                for (regs) |r| r.deinit(self.allocator);
                self.allocator.free(regs);
            }
            for (regs) |*r| r.* = .unit;

            const res_u64 = try self.jit.run(art.func, regs);
            
            // Check for Deoptimization
            if (res_u64 >= 0x80000000) {
                const pc = @as(u32, @intCast(res_u64 & 0x7FFFFFFF));
                std.debug.print("[SpeculativeExecutor] Deoptimization at PC {d}! Falling back to Interpreter...\n", .{pc});
                
                // Resume in interpreter from the deopt point
                // For now, our VM.run starts from 0. Let's use VM.exec directly.
                const res = try self.vm.vm.exec(code, regs, 0, profile);
                return res.val;
            }

            return Value{ .bits = res_u64 };
        }

        // TIER 0: Interpreter
        return try self.vm.run(code);
    }
};

test "SpeculativeExecutor Tiered demo" {
    const allocator = std.testing.allocator;
    var executor = SpeculativeExecutor.init(allocator);
    defer executor.deinit(); 
    
    const code = &[_]Op{
        .{ .load_const = .{ .dst = 0, .val = .{ .bits = 10 } } },
        .{ .load_const = .{ .dst = 1, .val = .{ .bits = 32 } } },
        .{ .add = .{ .dst = 2, .lhs = 0, .rhs = 1 } },
        .{ .ret = .{ .src = 2 } },
    };

    var i: usize = 0;
    while (i < 30) : (i += 1) {
        const res = try executor.execute(&[_][]const Op{code}, code);
        defer res.deinit(allocator);
        try std.testing.expectEqual(@as(u64, 42), res.bits);
    }
}

// test "SpeculativeExecutor Deoptimization" {
//     const allocator = std.testing.allocator;
//     var executor = SpeculativeExecutor.init(allocator);
//     defer executor.deinit(); 
//     
//     const type_code = &[_]Op{
//         .{ .add = .{ .dst = 1, .lhs = 0, .rhs = 0 } },
//         .{ .ret = .{ .src = 1 } },
//     };
//
//     // ... rest of test ...
// }
