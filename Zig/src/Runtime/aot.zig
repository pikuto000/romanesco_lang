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
        return self.compileWithImports(program, profile, output_path, cpu, &[_][]const u8{});
    }

    /// extra_ll_paths: clang に追加で渡す外部 .ll ファイルのパス一覧
    pub fn compileWithImports(self: *AOTCompiler, program: loader.LoadedProgram, profile: ?*vm.ProfileData, output_path: []const u8, cpu: vm.CpuFeatures, extra_ll_paths: []const []const u8) !void {
        var analysis = try self.analyzer.analyze(program.mainCode(), profile, 0);
        defer analysis.deinit();

        const ir_body = try self.codegen.generate(program.blocks, analysis, "romanesco_entry", cpu);
        defer self.allocator.free(ir_body);

        var ir_full = try std.ArrayList(u8).initCapacity(self.allocator, ir_body.len + 512);
        defer ir_full.deinit(self.allocator);
        const writer = ir_full.writer(self.allocator);

        try writer.writeAll(ir_body);

        // Append Main Wrapper
        try writer.writeAll(
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
            \\  ret i32 0
            \\}
            \\
        );

        // Write IR to temp file
        const timestamp = std.time.nanoTimestamp();
        var ll_name_buf: [128]u8 = undefined;
        const ll_name = try std.fmt.bufPrint(&ll_name_buf, ".jit_tmp/aot_{d}.ll", .{timestamp});

        std.fs.cwd().makeDir(".jit_tmp") catch |err| { if (err != error.PathAlreadyExists) return err; };
        {
            const file = try std.fs.cwd().createFile(ll_name, .{});
            defer file.close();
            try file.writeAll(ir_full.items);
        }

        // Run Clang (追加 .ll ファイルを末尾に連結)
        var argv = try std.ArrayList([]const u8).initCapacity(self.allocator, 6 + extra_ll_paths.len);
        defer argv.deinit(self.allocator);
        try argv.appendSlice(self.allocator, &[_][]const u8{ "clang", "-O3", "-Wno-override-module", "-o", output_path, ll_name });
        for (extra_ll_paths) |p| try argv.append(self.allocator, p);

        var child = std.process.Child.init(argv.items, self.allocator);
        const term = try child.spawnAndWait();

        switch (term) {
            .Exited => |code| if (code != 0) return error.CompilationFailed,
            else => return error.CompilationFailed,
        }
    }

    /// 外部 LLVM IR を Lifter でバイトコード化し、AOT コンパイルする
    pub fn liftAndCompile(
        self: *AOTCompiler,
        ir_text: []const u8,
        output_path: []const u8,
        cpu: vm.CpuFeatures,
    ) !void {
        const lifter = @import("lifter.zig");
        var l = lifter.Lifter.init(self.allocator);
        var prog = try l.lift(ir_text);
        defer prog.deinit();

        try self.compile(prog, null, output_path, cpu);
    }
};
