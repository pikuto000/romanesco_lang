const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;

pub const JIT = struct {
    allocator: Allocator,
    keep_artifacts: bool = false,

    pub fn init(allocator: Allocator) JIT {
        return .{ .allocator = allocator };
    }

    pub fn compile(self: *JIT, ir: []const u8, dll_name: []const u8) !void {
        const timestamp = std.time.nanoTimestamp();
        var ll_name_buf: [128]u8 = undefined;
        const ll_name = try std.fmt.bufPrint(&ll_name_buf, "jit_{d}.ll", .{timestamp});
        
        const tmp_path = ".jit_tmp";
        std.fs.cwd().makeDir(tmp_path) catch |err| { if (err != error.PathAlreadyExists) return err; };
        var tmp_dir = try std.fs.cwd().openDir(tmp_path, .{});
        defer tmp_dir.close();

        {
            const ll_file = try tmp_dir.createFile(ll_name, .{});
            defer ll_file.close();
            try ll_file.writeAll(ir);
        }

        const cwd_path = try std.fs.cwd().realpathAlloc(self.allocator, ".");
        defer self.allocator.free(cwd_path);
        
        const ll_full_path = try std.fs.path.join(self.allocator, &[_][]const u8{ cwd_path, tmp_path, ll_name });
        defer self.allocator.free(ll_full_path);

        const argv = [_][]const u8{ "clang", "-shared", "-O3", "-Wno-override-module", "-o", dll_name, ll_full_path };
        
        var child = std.process.Child.init(&argv, self.allocator);
        const term = try child.spawnAndWait();
        switch (term) {
            .Exited => |code| if (code != 0) return error.CompilationFailed,
            else => return error.CompilationFailed,
        }
        
        if (!self.keep_artifacts) {
            tmp_dir.deleteFile(ll_name) catch {};
        }
    }

    pub fn run(self: *JIT, func: vm.NativeEntry, vm_regs: []Value) !u64 {
        const LLVMValue = vm.LLVMValue;
        const native_regs = try self.allocator.alloc(LLVMValue, vm_regs.len);
        defer self.allocator.free(native_regs);
        
        // Sync To Native
        for (vm_regs, 0..) |v, i| {
            native_regs[i] = switch (v) {
                .int => |n| .{ .tag = 6, .payload = @ptrFromInt(n) },
                .unit => .{ .tag = 5, .payload = null },
                else => .{ .tag = 0, .payload = null },
            };
        }
        
        const result = func(native_regs.ptr);
        
        // Sync From Native
        for (vm_regs, 0..) |*v, i| {
            const nv = native_regs[i];
            // Only update if changed or simple types
            if (nv.tag == 6) {
                v.deinit(self.allocator);
                v.* = .{ .int = @intFromPtr(nv.payload) };
            } else if (nv.tag == 5) {
                v.deinit(self.allocator);
                v.* = .unit;
            }
        }
        
        return result;
    }

    pub fn compileAndRun(self: *JIT, ir: []const u8, entry_name: []const u8, vm_regs: []Value) !u64 {
        const timestamp = std.time.nanoTimestamp();
        var dll_name_buf: [128]u8 = undefined;
        const dll_name = try std.fmt.bufPrint(&dll_name_buf, ".jit_tmp/jit_{d}.dll", .{timestamp});
        
        try self.compile(ir, dll_name);
        
        var lib: std.DynLib = undefined;
        var retry: usize = 0;
        while (retry < 15) : (retry += 1) {
            lib = std.DynLib.open(dll_name) catch |err| {
                if (err == error.AccessDenied and retry < 14) {
                    std.Thread.sleep(100 * std.time.ns_per_ms);
                    continue;
                }
                return err;
            };
            break;
        }
        defer {
            lib.close();
            if (!self.keep_artifacts) {
                std.fs.cwd().deleteFile(dll_name) catch {};
                if (@import("builtin").os.tag == .windows) {
                    const base_path = dll_name[0 .. dll_name.len - 4];
                    var l_buf: [128]u8 = undefined;
                    var e_buf: [128]u8 = undefined;
                    const l_path = std.fmt.bufPrint(&l_buf, "{s}.lib", .{base_path}) catch "";
                    const e_path = std.fmt.bufPrint(&e_buf, "{s}.exp", .{base_path}) catch "";
                    if (l_path.len > 0) std.fs.cwd().deleteFile(l_path) catch {};
                    if (e_path.len > 0) std.fs.cwd().deleteFile(e_path) catch {};
                }
            }
        }

        const entry_name_z = try self.allocator.dupeZ(u8, entry_name);
        defer self.allocator.free(entry_name_z);
        const func = lib.lookup(vm.NativeEntry, entry_name_z) orelse return error.SymbolNotFound;

        return try self.run(func, vm_regs);
    }
};
