const std = @import("std");
const Zig = @import("Zig");
const vm = Zig.vm;
const loader = Zig.loader;
const speculative = Zig.speculative;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

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
        .int => |n| std.debug.print("Final Result: {d}\n", .{n}),
        .unit => std.debug.print("Final Result: ()\n", .{}),
        else => std.debug.print("Final Result: <complex value>\n", .{}),
    }
}

test "simple test" {
    const gpa = std.testing.allocator;
    var list: std.ArrayList(i32) = .empty;
    defer list.deinit(gpa); // Try commenting this out and see if zig detects the memory leak!
    try list.append(gpa, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
