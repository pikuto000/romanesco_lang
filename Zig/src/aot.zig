const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;
const analyzer = @import("analyzer.zig");
const codegen = @import("codegen.zig");
const loader = @import("loader.zig");

pub const AOTCompiler = struct {
    allocator: Allocator,
    codegen: codegen.CodeGen,
    analyzer: analyzer.RangeAnalyzer,

    pub fn init(allocator: Allocator) AOTCompiler {
        return .{
            .allocator = allocator,
            .codegen = codegen.CodeGen.init(allocator),
            .analyzer = analyzer.RangeAnalyzer.init(allocator),
        };
    }

    pub fn compile(self: *AOTCompiler, program: loader.LoadedProgram, profile: ?*vm.ProfileData, output_path: []const u8, cpu: vm.CpuFeatures) !void {
        var analysis = try self.analyzer.analyze(program.mainCode(), profile, 0);
        defer analysis.deinit();
        
        const ir_body = try self.codegen.generate(program.blocks, analysis, "romanesco_entry", cpu);
        defer self.allocator.free(ir_body);

        var ir_full = try std.ArrayList(u8).initCapacity(self.allocator, ir_body.len + 1024);
        defer ir_full.deinit(self.allocator);
        const writer = ir_full.writer(self.allocator);

        try writer.writeAll(ir_body);
        
        // Append Main Wrapper
        try writer.writeAll(
            \\
            \\@.str_out_fmt = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
            \\
            \\define i32 @main() {
            \\  %regs = alloca %Value, i32 32
            \\  %i_ptr = alloca i32
            \\  store i32 0, ptr %i_ptr
            \\  br label %init_loop
            \\init_loop:
            \\  %i = load i32, ptr %i_ptr
            \\  %cond = icmp ult i32 %i, 32
            \\  br i1 %cond, label %init_body, label %init_done
            \\init_body:
            \\  %p = getelementptr %Value, ptr %regs, i32 %i
            \\  call void @rt_init_unit(ptr %p)
            \\  %next = add i32 %i, 1
            \\  store i32 %next, ptr %i_ptr
            \\  br label %init_loop
            \\init_done:
            \\  ; Call entry point
            \\  %res_i64 = call i64 @romanesco_entry(ptr %regs)
            \\  ; Print result
            \\  %p_msg = call i32 (ptr, ...) @printf(ptr @.str_out_fmt, i64 %res_i64)
            \\  ret i32 0
            \\}
            \\
        );

        // Write IR to temp file
        const timestamp = std.time.nanoTimestamp();
        var ll_name_buf: [128]u8 = undefined;
        const ll_name = try std.fmt.bufPrint(&ll_name_buf, ".jit_tmp/aot_{d}.ll", .{timestamp});
        
        {
            const file = try std.fs.cwd().createFile(ll_name, .{});
            defer file.close();
            try file.writeAll(ir_full.items);
        }

        // Run Clang
        const argv = [_][]const u8{ "clang", "-O3", "-Wno-override-module", "-o", output_path, ll_name };
        var child = std.process.Child.init(&argv, self.allocator);
        const term = try child.spawnAndWait();
        
        switch (term) {
            .Exited => |code| if (code != 0) return error.CompilationFailed,
            else => return error.CompilationFailed,
        }
    }
};
