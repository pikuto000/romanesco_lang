const std = @import("std");
const Zig = @import("Zig");
const vm = Zig.vm;
const loader = Zig.loader;
const speculative = Zig.speculative;
const aot = Zig.aot;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage:\n", .{});
        std.debug.print("  {s} <bytecode.rbc> [--keep]      (Run with JIT)\n", .{args[0]});
        std.debug.print("  {s} aot <input.rbc> <output.exe> (Compile to Native)\n", .{args[0]});
        return;
    }

    if (std.mem.eql(u8, args[1], "aot")) {
        if (args.len < 4) {
            std.debug.print("Usage: {s} aot <input.rbc> <output.exe>\n", .{args[0]});
            return;
        }
        const input_path = args[2];
        const output_path = args[3];
        
        std.debug.print("AOT Compiling {s} -> {s}...\n", .{input_path, output_path});
        
        var bl = loader.BytecodeLoader.init(allocator);
        var prog = try bl.loadFromFile(input_path);
        defer prog.deinit();
        
        var aot_comp = aot.AOTCompiler.init(allocator);
        const cpu = vm.CpuFeatures.detect();
        
        try aot_comp.compile(prog, null, output_path, cpu);
        std.debug.print("Successfully compiled to {s}\n", .{output_path});
        return;
    }

    var file_path: ?[]const u8 = null;
    var keep_artifacts: bool = false;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--keep")) {
            keep_artifacts = true;
        } else {
            file_path = arg;
        }
    }

    if (file_path == null) {
        std.debug.print("Usage: {s} [--keep] <bytecode.rbc>\n", .{args[0]});
        return;
    }

    std.debug.print("Loading {s}...\n", .{file_path.?});

    var bl = loader.BytecodeLoader.init(allocator);
    var prog = try bl.loadFromFile(file_path.?);
    defer prog.deinit();

    var executor = speculative.SpeculativeExecutor.init(allocator);
    executor.keep_artifacts = keep_artifacts;
    executor.jit.keep_artifacts = keep_artifacts;
    defer executor.deinit();

    if (!keep_artifacts) {
        executor.cleanupOldArtifacts();
    }
    // Lower threshold for testing
    executor.hot_threshold = 5;
    
    // Run the program multiple times to demonstrate speculative JIT
    var last_result: vm.Value = .unit;
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        if (i > 0) last_result.deinit(allocator);
        last_result = try executor.execute(prog.blocks, prog.mainCode());
    }
    defer last_result.deinit(allocator);

    switch (last_result) {
        .bits => |n| std.debug.print("Final Result: {d}\n", .{n}),
        .unit => std.debug.print("Final Result: ()\n", .{}),
        else => std.debug.print("Final Result: <complex value>\n", .{}),
    }
}
