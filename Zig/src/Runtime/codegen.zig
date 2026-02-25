const std = @import("std");
const Allocator = std.mem.Allocator;
const vm = @import("vm.zig");
const Op = vm.Op;
const Value = vm.Value;
const analyzer = @import("analyzer.zig");
const RangeAnalysisResult = analyzer.RangeAnalysisResult;

pub const CodeGen = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) CodeGen {
        return .{ .allocator = allocator };
    }

    pub fn generate(self: *CodeGen, program: []const []const Op, analysis: RangeAnalysisResult, entry_name: []const u8, cpu: vm.CpuFeatures) ![]u8 {
        return self.generateWithImports(program, analysis, entry_name, cpu, &[_][]const u8{});
    }

    /// 外部LLVM IRファイルの内容を imports として渡すと、モジュールヘッダーを除去して末尾に追記する。
    pub fn generateWithImports(self: *CodeGen, program: []const []const Op, analysis: RangeAnalysisResult, entry_name: []const u8, cpu: vm.CpuFeatures, imports: []const []const u8) ![]u8 {
        var total_ops: usize = 0;
        for (program) |block| total_ops += block.len;
        const estimated = @max(4096, total_ops * 200 + runtime_implementation.len);
        var buffer = try std.ArrayList(u8).initCapacity(self.allocator, estimated);
        errdefer buffer.deinit(self.allocator);
        const writer = buffer.writer(self.allocator);

        // Header
        try writer.print("; ModuleID = '{s}'\n", .{entry_name});
        try writer.print("; SIMD: AVX2={any}, Neon={any}, Width={d}\n", .{cpu.has_avx2, cpu.has_neon, cpu.vector_width});
        try writer.writeAll("source_filename = \"romanesco_module\"\n");
        try writer.writeAll("target datalayout = \"e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n");
        if (@import("builtin").os.tag == .windows) {
            try writer.writeAll("target triple = \"x86_64-pc-windows-msvc\"\n");
        } else {
            try writer.writeAll("target triple = \"x86_64-pc-linux-gnu\"\n");
        }

        try writer.writeAll(
            \\
            \\%Value = type { i64, ptr }
            \\%Closure = type { ptr, ptr, i32 }
            \\%Pair = type { %Value, %Value }
            \\
            \\; SIMD Intrinsics
            \\declare <4 x i64> @llvm.x86.avx2.padd.q(<4 x i64>, <4 x i64>)
            \\declare <2 x i64> @llvm.aarch64.neon.addp.v2i64(<2 x i64>, <2 x i64>)
            \\
        );

        // Function generation for all blocks
        for (program, 0..) |block, i| {
            var block_name_buf: [64]u8 = undefined;
            const func_name = if (i == 0) entry_name else try std.fmt.bufPrint(&block_name_buf, "__block_{d}", .{i});
            try self.generateFunction(writer, program, block, analysis, func_name, i == 0, cpu);
        }

        // Runtime implementation
        try writer.writeAll(runtime_implementation);

        // External IR imports (ヘッダー行を除去して追記)
        for (imports) |import_ir| {
            try writer.writeAll("\n; --- imported IR ---\n");
            try writeStrippedImport(writer, import_ir);
        }

        return buffer.toOwnedSlice(self.allocator);
    }

    /// 外部LLVM IRからモジュールヘッダー行を除いた本体部分を writer に書き出す。
    /// 除外対象: ModuleIDコメント、source_filename、target datalayout/triple、型定義、SIMDイントリンシック宣言
    fn writeStrippedImport(writer: anytype, ir_text: []const u8) !void {
        var lines = std.mem.splitScalar(u8, ir_text, '\n');
        while (lines.next()) |line| {
            const t = std.mem.trim(u8, line, " \t\r");
            if (std.mem.startsWith(u8, t, "; ModuleID") or
                std.mem.startsWith(u8, t, "; SIMD") or
                std.mem.startsWith(u8, t, "source_filename") or
                std.mem.startsWith(u8, t, "target datalayout") or
                std.mem.startsWith(u8, t, "target triple") or
                std.mem.indexOf(u8, t, " = type ") != null or
                std.mem.startsWith(u8, t, "declare <"))
            {
                continue;
            }
            try writer.writeAll(line);
            try writer.writeAll("\n");
        }
    }

    const runtime_implementation =
        \\
        \\declare ptr @malloc(i64)
        \\declare void @free(ptr)
        \\declare void @exit(i32)
        \\declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)
        \\
        \\define void @rt_cleanup_value(%Value %v) {
        \\  %tag = extractvalue %Value %v, 0
        \\  %ptr = extractvalue %Value %v, 1
        \\  switch i64 %tag, label %done [
        \\    i64 1, label %free_cl
        \\    i64 2, label %free_pair
        \\    i64 3, label %free_sum
        \\    i64 4, label %free_sum
        \\    i64 7, label %free_wide
        \\  ]
        \\free_cl:
        \\  call void @free(ptr %ptr)
        \\  br label %done
        \\free_pair:
        \\  %v1 = load %Value, ptr %ptr
        \\  call void @rt_cleanup_value(%Value %v1)
        \\  %v2_ptr = getelementptr %Value, ptr %ptr, i32 1
        \\  %v2 = load %Value, ptr %v2_ptr
        \\  call void @rt_cleanup_value(%Value %v2)
        \\  call void @free(ptr %ptr)
        \\  br label %done
        \\free_sum:
        \\  %inner = load %Value, ptr %ptr
        \\  call void @rt_cleanup_value(%Value %inner)
        \\  call void @free(ptr %ptr)
        \\  br label %done
        \\free_wide:
        \\  call void @free(ptr %ptr)
        \\  br label %done
        \\done:
        \\  ret void
        \\}
        \\
        \\define i1 @rt_guard_tag(%Value %v, i64 %expected) {
        \\  %tag = extractvalue %Value %v, 0
        \\  %ok = icmp eq i64 %tag, %expected
        \\  %fail = xor i1 %ok, 1
        \\  ret i1 %fail
        \\}
        \\
        \\define i1 @rt_guard_value(%Value %v, i64 %expected) {
        \\  %tag = extractvalue %Value %v, 0
        \\  %is_int = icmp eq i64 %tag, 6
        \\  br i1 %is_int, label %check_val, label %fail
        \\check_val:
        \\  %ptr = extractvalue %Value %v, 1
        \\  %val = ptrtoint ptr %ptr to i64
        \\  %ok = icmp eq i64 %val, %expected
        \\  %fail_v = xor i1 %ok, 1
        \\  ret i1 %fail_v
        \\fail:
        \\  ret i1 1
        \\}
        \\
        \\define void @rt_make_int(ptr %out, i64 %n) {
        \\  %old = load %Value, ptr %out
        \\  call void @rt_cleanup_value(%Value %old)
        \\  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 6, ptr %p1
        \\  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  %ptr = inttoptr i64 %n to ptr
        \\  store ptr %ptr, ptr %p2
        \\  ret void
        \\}
        \\
        \\define i64 @rt_get_int(ptr %v) {
        \\  %p1 = getelementptr %Value, ptr %v, i32 0, i32 0
        \\  %tag = load i64, ptr %p1
        \\  %is_int = icmp eq i64 %tag, 6
        \\  br i1 %is_int, label %int_val, label %fail
        \\int_val:
        \\  %p2 = getelementptr %Value, ptr %v, i32 0, i32 1
        \\  %ptr = load ptr, ptr %p2
        \\  %n = ptrtoint ptr %ptr to i64
        \\  ret i64 %n
        \\fail:
        \\  ret i64 0
        \\}
        \\
        \\define void @rt_make_wide(ptr %out, ptr %limbs, i64 %count) {
        \\entry:
        \\  %old = load %Value, ptr %out
        \\  call void @rt_cleanup_value(%Value %old)
        \\  %data_bytes = mul i64 %count, 8
        \\  %total = add i64 %data_bytes, 8
        \\  %mem = call ptr @malloc(i64 %total)
        \\  store i64 %count, ptr %mem
        \\  %dst_limbs = getelementptr i8, ptr %mem, i64 8
        \\  call void @llvm.memcpy.p0.p0.i64(ptr %dst_limbs, ptr %limbs, i64 %data_bytes, i1 false)
        \\  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 7, ptr %p1
        \\  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  store ptr %mem, ptr %p2
        \\  ret void
        \\}
        \\
        \\define i64 @rt_get_wide_limb(ptr %v, i64 %idx) {
        \\entry:
        \\  %p2 = getelementptr %Value, ptr %v, i32 0, i32 1
        \\  %mem = load ptr, ptr %p2
        \\  %limbs_base = getelementptr i8, ptr %mem, i64 8
        \\  %limb_ptr = getelementptr i64, ptr %limbs_base, i64 %idx
        \\  %val = load i64, ptr %limb_ptr
        \\  ret i64 %val
        \\}
        \\
        \\define void @rt_make_unit(ptr %out) {
        \\  %old = load %Value, ptr %out
        \\  call void @rt_cleanup_value(%Value %old)
        \\  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 5, ptr %p1
        \\  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  store ptr null, ptr %p2
        \\  ret void
        \\}
        \\
        \\define void @rt_init_unit(ptr %out) {
        \\  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 5, ptr %p1
        \\  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  store ptr null, ptr %p2
        \\  ret void
        \\}
        \\
        \\define void @rt_make_pair(ptr %out, %Value %v1, %Value %v2) {
        \\  %old = load %Value, ptr %out
        \\  call void @rt_cleanup_value(%Value %old)
        \\  %mem = call ptr @malloc(i64 32)
        \\  store %Value %v1, ptr %mem
        \\  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
        \\  store %Value %v2, ptr %v2_ptr
        \\  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 2, ptr %p1
        \\  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  store ptr %mem, ptr %p2
        \\  ret void
        \\}
        \\
        \\define %Value @rt_proj1(ptr %src_ptr, %Value %pair) {
        \\  %mem = extractvalue %Value %pair, 1
        \\  %v1 = load %Value, ptr %mem
        \\  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
        \\  %v2 = load %Value, ptr %v2_ptr
        \\  store %Value %v2, ptr %src_ptr
        \\  ret %Value %v1
        \\}
        \\
        \\define %Value @rt_proj2(ptr %src_ptr, %Value %pair) {
        \\  %mem = extractvalue %Value %pair, 1
        \\  %v1 = load %Value, ptr %mem
        \\  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
        \\  %v2 = load %Value, ptr %v2_ptr
        \\  store %Value %v1, ptr %src_ptr
        \\  ret %Value %v2
        \\}
        \\
        \\define void @rt_make_sum(ptr %out, %Value %v, i64 %tag) {
        \\  %old = load %Value, ptr %out
        \\  call void @rt_cleanup_value(%Value %old)
        \\  %mem = call ptr @malloc(i64 16)
        \\  store %Value %v, ptr %mem
        \\  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 %tag, ptr %p1
        \\  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  store ptr %mem, ptr %p2
        \\  ret void
        \\}
        \\
        \\define ptr @rt_alloc_env(i32 %size) {
        \\  %total_size = mul i32 %size, 16
        \\  %real_size = add i32 %total_size, 8
        \\  %zext_size = zext i32 %real_size to i64
        \\  %mem = call ptr @malloc(i64 %zext_size)
        \\  store i32 %size, ptr %mem
        \\  ret ptr %mem
        \\}
        \\
        \\define void @rt_env_store(ptr %env, i32 %idx, %Value %v) {
        \\  %data = getelementptr i8, ptr %env, i32 8
        \\  %slot = getelementptr %Value, ptr %data, i32 %idx
        \\  store %Value %v, ptr %slot
        \\  ret void
        \\}
        \\
        \\define void @rt_make_closure(ptr %out, ptr %func, ptr %env, i32 %arity) {
        \\  %old = load %Value, ptr %out
        \\  call void @rt_cleanup_value(%Value %old)
        \\  %mem = call ptr @malloc(i64 24)
        \\  %p1 = getelementptr %Closure, ptr %mem, i32 0, i32 0
        \\  store ptr %func, ptr %p1
        \\  %p2 = getelementptr %Closure, ptr %mem, i32 0, i32 1
        \\  store ptr %env, ptr %p2
        \\  %p3 = getelementptr %Closure, ptr %mem, i32 0, i32 2
        \\  store i32 %arity, ptr %p3
        \\  %o1 = getelementptr %Value, ptr %out, i32 0, i32 0
        \\  store i64 1, ptr %o1
        \\  %o2 = getelementptr %Value, ptr %out, i32 0, i32 1
        \\  store ptr %mem, ptr %o2
        \\  ret void
        \\}
        \\
        \\define %Value @rt_call(%Value %clVal, ptr %args, i32 %num_args) {
        \\  %tag = extractvalue %Value %clVal, 0
        \\  %is_cl = icmp eq i64 %tag, 1
        \\  br i1 %is_cl, label %do_call, label %error
        \\do_call:
        \\  %clPtr = extractvalue %Value %clVal, 1
        \\  %fpp = getelementptr %Closure, ptr %clPtr, i32 0, i32 0
        \\  %func = load ptr, ptr %fpp
        \\  %epp = getelementptr %Closure, ptr %clPtr, i32 0, i32 1
        \\  %env = load ptr, ptr %epp
        \\  %res = call %Value %func(ptr %env, ptr %args, i32 %num_args)
        \\  ret %Value %res
        \\error:
        \\  ret %Value { i64 5, ptr null }
        \\}
        \\
        \\define void @rt_setup_regs(ptr %regs, i32 %count, ptr %env, ptr %args, i32 %num_args) {
        \\entry:
        \\  %is_env_null = icmp eq ptr %env, null
        \\  br i1 %is_env_null, label %args_pre_no_env, label %load_env
        \\load_env:
        \\  %env_size = load i32, ptr %env
        \\  %i_ptr = alloca i32
        \\  store i32 0, ptr %i_ptr
        \\  br label %env_loop
        \\env_loop:
        \\  %i = load i32, ptr %i_ptr
        \\  %cond = icmp ult i32 %i, %env_size
        \\  br i1 %cond, label %env_body, label %args_pre_env
        \\env_body:
        \\  %data = getelementptr i8, ptr %env, i32 8
        \\  %src_slot = getelementptr %Value, ptr %data, i32 %i
        \\  %v = load %Value, ptr %src_slot
        \\  %dst_slot = getelementptr %Value, ptr %regs, i32 %i
        \\  store %Value %v, ptr %dst_slot
        \\  %next_i = add i32 %i, 1
        \\  store i32 %next_i, ptr %i_ptr
        \\  br label %env_loop
        \\args_pre_no_env:
        \\  br label %args_pre
        \\args_pre_env:
        \\  br label %args_pre
        \\args_pre:
        \\  %final_env_size = phi i32 [ 0, %args_pre_no_env ], [ %env_size, %args_pre_env ]
        \\  %j_ptr = alloca i32
        \\  store i32 0, ptr %j_ptr
        \\  br label %args_loop
        \\args_loop:
        \\  %j = load i32, ptr %j_ptr
        \\  %cond2 = icmp ult i32 %j, %num_args
        \\  br i1 %cond2, label %args_body, label %done
        \\args_body:
        \\  %src_arg = getelementptr %Value, ptr %args, i32 %j
        \\  %v2 = load %Value, ptr %src_arg
        \\  %idx = add i32 %j, %final_env_size
        \\  %dst_arg = getelementptr %Value, ptr %regs, i32 %idx
        \\  store %Value %v2, ptr %dst_arg
        \\  %next_j = add i32 %j, 1
        \\  store i32 %next_j, ptr %j_ptr
        \\  br label %args_loop
        \\done:
        \\  ret void
        \\}
        \\
    ;

    fn generateFunction(self: *CodeGen, writer: anytype, program: []const []const Op, code: []const Op, analysis: RangeAnalysisResult, name: []const u8, is_main: bool, cpu: vm.CpuFeatures) !void {
        _ = cpu;
        _ = self;
        var max_reg: u32 = 0;
        for (code) |op| {
             switch (op) {
                .load_const => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .move => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .add => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .sub => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .mul => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .make_pair => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .proj1 => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .proj2 => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .make_inl => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .make_inr => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .case_op => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .call => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .make_closure => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .borrow => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .free => |o| if (o.reg > max_reg) { max_reg = o.reg; },
                .ret => |o| if (o.src > max_reg) { max_reg = o.src; },
                .ibin => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .icmp => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .load_bits => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .load_wide => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .sext => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .zext => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .trunc => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .itof => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .ftoi => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .fadd => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .fsub => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .fmul => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .fdiv => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .frem => |o| if (o.dst > max_reg) { max_reg = o.dst; },
                .fcmp => |o| if (o.dst > max_reg) { max_reg = o.dst; },
            }
        }
        
        if (is_main) {
             try writer.print("define dllexport i64 @{s}(ptr %external_regs) {{\n", .{name});
        } else {
             try writer.print("define %Value @{s}(ptr %env, ptr %args, i32 %num_args) {{\n", .{name});
        }
        
        try writer.writeAll("entry:\n");

        try writer.print("  %regs = alloca %Value, i32 {d}\n", .{max_reg + 1});
        if (is_main) {
             var i: u32 = 0;
             while (i <= max_reg) : (i += 1) {
                 try writer.print("  %r_ptr_init_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{i, i});
                 try writer.print("  call void @rt_init_unit(ptr %r_ptr_init_{d})\n", .{i});
             }
        } else {
             try writer.print("  call void @rt_setup_regs(ptr %regs, i32 {d}, ptr %env, ptr %args, i32 %num_args)\n", .{max_reg + 1});
        }

        // AGGRESSIVE RECURSION UNROLLING
        // Check if the block is tail-recursive
        var is_tail_rec = false;
        if (code.len >= 2) {
            const last = code[code.len-1];
            const prev = code[code.len-2];
            if (last == .ret and prev == .call) {
                // If it's calling itself (we'd need current_block_idx from generate loop)
                // For now, let's enable loop header
                try writer.writeAll("  br label %loop_header\n\nloop_header:\n");
                is_tail_rec = true;
            }
        }

        // Deopt point
        try writer.writeAll("  br label %body\n\ndeopt_exit:\n");
        const deopt_pc_val = 0x80000000; // Tag for deopt
        if (is_main) {
            try writer.print("  %deopt_val = or i64 0, {d}\n", .{deopt_pc_val});
            try writer.print("  ret i64 %deopt_val\n", .{});
        } else {
            // For helper blocks, return a dummy value with a special tag if we want to signal deopt
            // For now, let's just return Unit (tag 5)
            try writer.writeAll("  ret %Value { i64 5, ptr null }\n");
        }
        try writer.writeAll("\nbody:\n");

        var temp_counter: u32 = 0;
        const next_temp = struct {
            ptr: *u32,
            fn call(ptr: *u32) u32 {
                const v = ptr.*;
                ptr.* += 1;
                return v;
            }
        }.call;

        // Tiered Optimization: Guard stable values
        var stable_it = analysis.stable_values.iterator();
        while (stable_it.next()) |entry| {
            const reg = entry.key_ptr.*;
            const val = entry.value_ptr.*;
            try writer.print("  %stable_ptr_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{reg, reg});
            try writer.print("  %stable_v_{d} = load %Value, ptr %stable_ptr_{d}\n", .{reg, reg});
            const f_stable = next_temp(&temp_counter);
            try writer.print("  %v{d} = call i1 @rt_guard_value(%Value %stable_v_{d}, i64 {d})\n", .{f_stable, reg, val});
            const b_label = next_temp(&temp_counter);
            try writer.print("  br i1 %v{d}, label %deopt_exit, label %body_stable_{d}\n", .{f_stable, b_label});
            try writer.print("body_stable_{d}:\n", .{b_label});
        }

        for (code, 0..) |op, pc| {
            switch (op) {
                .load_const => |lc| {
                    try writer.print("  %r_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{lc.dst, pc, lc.dst});
                    if (lc.val == .bits) {
                        try writer.print("  call void @rt_make_int(ptr %r_ptr_{d}_{d}, i64 {d})\n", .{lc.dst, pc, @as(i64, @bitCast(lc.val.bits))});
                    } else if (lc.val == .unit) {
                        try writer.print("  call void @rt_make_unit(ptr %r_ptr_{d}_{d})\n", .{lc.dst, pc});
                    }
                },
                .move => |m| {
                     try writer.print("  %src_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.src, pc, m.src});
                     try writer.print("  %dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.dst, pc, m.dst});
                     const v = next_temp(&temp_counter);
                     try writer.print("  %v{d} = load %Value, ptr %src_ptr_{d}_{d}\n", .{v, m.src, pc});
                     try writer.print("  store %Value %v{d}, ptr %dst_ptr_{d}_{d}\n", .{v, m.dst, pc});
                     try writer.print("  call void @rt_init_unit(ptr %src_ptr_{d}_{d})\n", .{m.src, pc});
                },
                .add => |a| {
                    const w = analysis.bitWidth(a.dst);
                    const l_stable = analysis.stable_values.get(a.lhs);
                    const r_stable = analysis.stable_values.get(a.rhs);

                    const l_val = next_temp(&temp_counter);
                    if (l_stable) |v| {
                        try writer.print("  %v{d} = add i64 0, {d}\n", .{l_val, v});
                    } else if (analysis.isProvenBits(a.lhs)) {
                        try writer.print("  %lhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{a.lhs, pc, a.lhs});
                        try writer.print("  %v{d} = call i64 @rt_get_int(ptr %lhs_ptr_{d}_{d})\n", .{l_val, a.lhs, pc});
                    } else {
                        try writer.print("  %lhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{a.lhs, pc, a.lhs});
                        try writer.print("  %v_lhs_{d}_{d} = load %Value, ptr %lhs_ptr_{d}_{d}\n", .{next_temp(&temp_counter), pc, a.lhs, pc});
                        const f1 = next_temp(&temp_counter);
                        try writer.print("  %v{d} = call i1 @rt_guard_tag(%Value %v_lhs_{d}_{d}, i64 6)\n", .{f1, temp_counter-2, pc});
                        const b1 = next_temp(&temp_counter);
                        try writer.print("  br i1 %v{d}, label %deopt_exit, label %body_add_l_{d}_{d}\n", .{f1, b1, pc});
                        try writer.print("body_add_l_{d}_{d}:\n", .{b1, pc});
                        try writer.print("  %v{d} = call i64 @rt_get_int(ptr %lhs_ptr_{d}_{d})\n", .{l_val, a.lhs, pc});
                    }

                    const r_val = next_temp(&temp_counter);
                    if (r_stable) |v| {
                        try writer.print("  %v{d} = add i64 0, {d}\n", .{r_val, v});
                    } else if (analysis.isProvenBits(a.rhs)) {
                        try writer.print("  %rhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{a.rhs, pc, a.rhs});
                        try writer.print("  %v{d} = call i64 @rt_get_int(ptr %rhs_ptr_{d}_{d})\n", .{r_val, a.rhs, pc});
                    } else {
                        try writer.print("  %rhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{a.rhs, pc, a.rhs});
                        try writer.print("  %v_rhs_{d}_{d} = load %Value, ptr %rhs_ptr_{d}_{d}\n", .{next_temp(&temp_counter), pc, a.rhs, pc});
                        const f2 = next_temp(&temp_counter);
                        try writer.print("  %v{d} = call i1 @rt_guard_tag(%Value %v_rhs_{d}_{d}, i64 6)\n", .{f2, temp_counter-2, pc});
                        const b2 = next_temp(&temp_counter);
                        try writer.print("  br i1 %v{d}, label %deopt_exit, label %body_add_r_{d}_{d}\n", .{f2, b2, pc});
                        try writer.print("body_add_r_{d}_{d}:\n", .{b2, pc});
                        try writer.print("  %v{d} = call i64 @rt_get_int(ptr %rhs_ptr_{d}_{d})\n", .{r_val, a.rhs, pc});
                    }

                    const res_temp = next_temp(&temp_counter);
                    if (w < 64) {
                        const l_trunc = next_temp(&temp_counter);
                        const r_trunc = next_temp(&temp_counter);
                        try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{l_trunc, l_val, w});
                        try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{r_trunc, r_val, w});
                        const op_res = next_temp(&temp_counter);
                        try writer.print("  %v{d} = add i{d} %v{d}, %v{d}\n", .{op_res, w, l_trunc, r_trunc});
                        try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{res_temp, w, op_res});
                    } else {
                        try writer.print("  %v{d} = add i64 %v{d}, %v{d}\n", .{res_temp, l_val, r_val});
                    }
                    try writer.print("  %dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{a.dst, pc, a.dst});
                    try writer.print("  call void @rt_make_int(ptr %dst_ptr_{d}_{d}, i64 %v{d})\n", .{a.dst, pc, res_temp});
                    if (l_stable == null) try writer.print("  call void @rt_init_unit(ptr %lhs_ptr_{d}_{d})\n", .{a.lhs, pc});
                    if (r_stable == null) try writer.print("  call void @rt_init_unit(ptr %rhs_ptr_{d}_{d})\n", .{a.rhs, pc});
                },
                .sub => |s| {
                    const w = analysis.bitWidth(s.dst);
                    try writer.print("  %lhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{s.lhs, pc, s.lhs});
                    try writer.print("  %rhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{s.rhs, pc, s.rhs});
                    const l_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call i64 @rt_get_int(ptr %lhs_ptr_{d}_{d})\n", .{l_val, s.lhs, pc});
                    const r_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call i64 @rt_get_int(ptr %rhs_ptr_{d}_{d})\n", .{r_val, s.rhs, pc});
                    const res_temp = next_temp(&temp_counter);
                    if (w < 64) {
                        const l_trunc = next_temp(&temp_counter);
                        const r_trunc = next_temp(&temp_counter);
                        try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{l_trunc, l_val, w});
                        try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{r_trunc, r_val, w});
                        const op_res = next_temp(&temp_counter);
                        try writer.print("  %v{d} = sub i{d} %v{d}, %v{d}\n", .{op_res, w, l_trunc, r_trunc});
                        try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{res_temp, w, op_res});
                    } else {
                        try writer.print("  %v{d} = sub i64 %v{d}, %v{d}\n", .{res_temp, l_val, r_val});
                    }
                    try writer.print("  %dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{s.dst, pc, s.dst});
                    try writer.print("  call void @rt_make_int(ptr %dst_ptr_{d}_{d}, i64 %v{d})\n", .{s.dst, pc, res_temp});
                    try writer.print("  call void @rt_init_unit(ptr %lhs_ptr_{d}_{d})\n", .{s.lhs, pc});
                    try writer.print("  call void @rt_init_unit(ptr %rhs_ptr_{d}_{d})\n", .{s.rhs, pc});
                },
                .mul => |m| {
                    const w = analysis.bitWidth(m.dst);
                    try writer.print("  %lhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.lhs, pc, m.lhs});
                    try writer.print("  %rhs_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.rhs, pc, m.rhs});
                    const l_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call i64 @rt_get_int(ptr %lhs_ptr_{d}_{d})\n", .{l_val, m.lhs, pc});
                    const r_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call i64 @rt_get_int(ptr %rhs_ptr_{d}_{d})\n", .{r_val, m.rhs, pc});
                    const res_temp = next_temp(&temp_counter);
                    if (w < 64) {
                        const l_trunc = next_temp(&temp_counter);
                        const r_trunc = next_temp(&temp_counter);
                        try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{l_trunc, l_val, w});
                        try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{r_trunc, r_val, w});
                        const op_res = next_temp(&temp_counter);
                        try writer.print("  %v{d} = mul i{d} %v{d}, %v{d}\n", .{op_res, w, l_trunc, r_trunc});
                        try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{res_temp, w, op_res});
                    } else {
                        try writer.print("  %v{d} = mul i64 %v{d}, %v{d}\n", .{res_temp, l_val, r_val});
                    }
                    try writer.print("  %dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.dst, pc, m.dst});
                    try writer.print("  call void @rt_make_int(ptr %dst_ptr_{d}_{d}, i64 %v{d})\n", .{m.dst, pc, res_temp});
                    try writer.print("  call void @rt_make_unit(ptr %lhs_ptr_{d}_{d})\n", .{m.lhs, pc});
                    try writer.print("  call void @rt_init_unit(ptr %rhs_ptr_{d}_{d})\n", .{m.rhs, pc});
                },
                .make_pair => |mp| {
                    try writer.print("  %f_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{mp.fst, pc, mp.fst});
                    try writer.print("  %s_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{mp.snd, pc, mp.snd});
                    const f_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %f_ptr_{d}_{d}\n", .{f_val, mp.fst, pc});
                    const s_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %s_ptr_{d}_{d}\n", .{s_val, mp.snd, pc});
                    try writer.print("  %dst_ptr_mp_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{mp.dst, pc, mp.dst});
                    if (analysis.doesEscape(mp.dst)) {
                        try writer.print("  call void @rt_make_pair(ptr %dst_ptr_mp_{d}_{d}, %Value %v{d}, %Value %v{d})\n", .{mp.dst, pc, f_val, s_val});
                    } else {
                        const pair_mem = next_temp(&temp_counter);
                        try writer.print("  %v{d} = alloca %Pair\n", .{pair_mem});
                        const f_slot = next_temp(&temp_counter);
                        try writer.print("  %v{d} = getelementptr %Pair, ptr %v{d}, i32 0, i32 0\n", .{f_slot, pair_mem});
                        try writer.print("  store %Value %v{d}, ptr %v{d}\n", .{f_val, f_slot});
                        const s_slot = next_temp(&temp_counter);
                        try writer.print("  %v{d} = getelementptr %Pair, ptr %v{d}, i32 0, i32 1\n", .{s_slot, pair_mem});
                        try writer.print("  store %Value %v{d}, ptr %v{d}\n", .{s_val, s_slot});
                        const tag_ptr = next_temp(&temp_counter);
                        try writer.print("  %v{d} = getelementptr %Value, ptr %dst_ptr_mp_{d}_{d}, i32 0, i32 0\n", .{tag_ptr, mp.dst, pc});
                        try writer.print("  store i64 2, ptr %v{d}\n", .{tag_ptr});
                        const val_ptr = next_temp(&temp_counter);
                        try writer.print("  %v{d} = getelementptr %Value, ptr %dst_ptr_mp_{d}_{d}, i32 0, i32 1\n", .{val_ptr, mp.dst, pc});
                        try writer.print("  store ptr %v{d}, ptr %v{d}\n", .{pair_mem, val_ptr});
                    }
                    try writer.print("  call void @rt_init_unit(ptr %f_ptr_{d}_{d})\n", .{mp.fst, pc});
                    try writer.print("  call void @rt_init_unit(ptr %s_ptr_{d}_{d})\n", .{mp.snd, pc});
                },
                .proj1 => |p| {
                    try writer.print("  %src_ptr_p1_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{p.src, pc, p.src});
                    const p_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %src_ptr_p1_{d}_{d}\n", .{p_val, p.src, pc});
                    const res = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call %Value @rt_proj1(ptr %src_ptr_p1_{d}_{d}, %Value %v{d})\n", .{res, p.src, pc, p_val});
                    try writer.print("  %dst_ptr_p1_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{p.dst, pc, p.dst});
                    try writer.print("  store %Value %v{d}, ptr %dst_ptr_p1_{d}_{d}\n", .{res, p.dst, pc});
                },
                .proj2 => |p| {
                    try writer.print("  %src_ptr_p2_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{p.src, pc, p.src});
                    const p_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %src_ptr_p2_{d}_{d}\n", .{p_val, p.src, pc});
                    const res = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call %Value @rt_proj2(ptr %src_ptr_p2_{d}_{d}, %Value %v{d})\n", .{res, p.src, pc, p_val});
                    try writer.print("  %dst_ptr_p2_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{p.dst, pc, p.dst});
                    try writer.print("  store %Value %v{d}, ptr %dst_ptr_p2_{d}_{d}\n", .{res, p.dst, pc});
                },
                .make_inl => |m| {
                    try writer.print("  %src_ptr_inl_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.src, pc, m.src});
                    const v = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %src_ptr_inl_{d}_{d}\n", .{v, m.src, pc});
                    try writer.print("  %dst_ptr_inl_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.dst, pc, m.dst});
                    try writer.print("  call void @rt_make_sum(ptr %dst_ptr_inl_{d}_{d}, %Value %v{d}, i64 3)\n", .{m.dst, pc, v});
                    try writer.print("  call void @rt_init_unit(ptr %src_ptr_inl_{d}_{d})\n", .{m.src, pc});
                },
                .make_inr => |m| {
                    try writer.print("  %src_ptr_inr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.src, pc, m.src});
                    const v = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %src_ptr_inr_{d}_{d}\n", .{v, m.src, pc});
                    try writer.print("  %dst_ptr_inr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{m.dst, pc, m.dst});
                    try writer.print("  call void @rt_make_sum(ptr %dst_ptr_inr_{d}_{d}, %Value %v{d}, i64 4)\n", .{m.dst, pc, v});
                    try writer.print("  call void @rt_init_unit(ptr %src_ptr_inr_{d}_{d})\n", .{m.src, pc});
                },
                .case_op => |c| {
                    try writer.print("  %scr_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{c.scrutinee, pc, c.scrutinee});
                    const scr_v = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %scr_ptr_{d}_{d}\n", .{scr_v, c.scrutinee, pc});
                    const tag = next_temp(&temp_counter);
                    try writer.print("  %v{d} = extractvalue %Value %v{d}, 0\n", .{tag, scr_v});
                    const payload = next_temp(&temp_counter);
                    try writer.print("  %v{d} = extractvalue %Value %v{d}, 1\n", .{payload, scr_v});
                    const inner = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %v{d}\n", .{inner, payload});
                    const is_inl = next_temp(&temp_counter);
                    try writer.print("  %v{d} = icmp eq i64 %v{d}, 3\n", .{is_inl, tag});
                    const label_id = next_temp(&temp_counter);
                    var inl_lab: [32]u8 = undefined; var inr_lab: [32]u8 = undefined; var end_lab: [32]u8 = undefined;
                    _ = try std.fmt.bufPrint(&inl_lab, "case_inl_{d}_{d}", .{label_id, pc});
                    _ = try std.fmt.bufPrint(&inr_lab, "case_inr_{d}_{d}", .{label_id, pc});
                    _ = try std.fmt.bufPrint(&end_lab, "case_end_{d}_{d}", .{label_id, pc});
                    try writer.print("  br i1 %v{d}, label %{s}, label %{s}\n", .{is_inl, inl_lab, inr_lab});
                    const arg_buf = next_temp(&temp_counter);
                    try writer.print("  %v{d} = alloca %Value, i32 1\n", .{arg_buf});
                    try writer.print("  store %Value %v{d}, ptr %v{d}\n", .{inner, arg_buf});
                    const res_ptr = next_temp(&temp_counter);
                    try writer.print("  %v{d} = alloca %Value\n", .{res_ptr});
                    var inl_idx: usize = 0; var inr_idx: usize = 0;
                    for (program, 0..) |b, i| {
                        if (b.ptr == c.inl_branch.ptr) inl_idx = i;
                        if (b.ptr == c.inr_branch.ptr) inr_idx = i;
                    }
                    try writer.print("{s}:\n", .{inl_lab});
                    const res_inl = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call %Value @__block_{d}(ptr null, ptr %v{d}, i32 1)\n", .{res_inl, inl_idx, arg_buf});
                    try writer.print("  store %Value %v{d}, ptr %v{d}\n", .{res_inl, res_ptr});
                    try writer.print("  br label %{s}\n", .{end_lab});
                    try writer.print("{s}:\n", .{inr_lab});
                    const res_inr = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call %Value @__block_{d}(ptr null, ptr %v{d}, i32 1)\n", .{res_inr, inr_idx, arg_buf});
                    try writer.print("  store %Value %v{d}, ptr %v{d}\n", .{res_inr, res_ptr});
                    try writer.print("  br label %{s}\n", .{end_lab});
                    try writer.print("{s}:\n", .{end_lab});
                    const final_res = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %v{d}\n", .{final_res, res_ptr});
                    try writer.print("  %dst_ptr_case_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{c.dst, pc, c.dst});
                    try writer.print("  store %Value %v{d}, ptr %dst_ptr_case_{d}_{d}\n", .{final_res, c.dst, pc});
                    try writer.print("  call void @rt_init_unit(ptr %scr_ptr_{d}_{d})\n", .{c.scrutinee, pc});
                },
                .ret => |r| {
                     try writer.print("  %ret_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{r.src, pc, r.src});
                     if (is_main) {
                         const val_i64 = next_temp(&temp_counter);
                         try writer.print("  %v{d} = call i64 @rt_get_int(ptr %ret_ptr_{d}_{d})\n", .{val_i64, r.src, pc});
                         try writer.print("  ret i64 %v{d}\n", .{val_i64});
                     } else {
                         const ret_val = next_temp(&temp_counter);
                         try writer.print("  %v{d} = load %Value, ptr %ret_ptr_{d}_{d}\n", .{ret_val, r.src, pc});
                         try writer.print("  ret %Value %v{d}\n", .{ret_val});
                     }
                },
                .make_closure => |mc| {
                    var block_idx: usize = 0; var found = false;
                    for (program, 0..) |b, i| { if (b.ptr == mc.body.ptr) { block_idx = i; found = true; break; } }
                    if (!found) continue;
                    var closure_name_buf: [64]u8 = undefined;
                    const func_ptr_name = try std.fmt.bufPrint(&closure_name_buf, "__block_{d}", .{block_idx});
                    const env_ptr = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call ptr @rt_alloc_env(i32 {d})\n", .{env_ptr, mc.captures.len});
                    for (mc.captures, 0..) |cap_idx, i| {
                        try writer.print("  %cap_src_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{cap_idx, pc, cap_idx});
                        const cap_v = next_temp(&temp_counter);
                        try writer.print("  %v{d} = load %Value, ptr %cap_src_{d}_{d}\n", .{cap_v, cap_idx, pc});
                        try writer.print("  call void @rt_env_store(ptr %v{d}, i32 {d}, %Value %v{d})\n", .{env_ptr, i, cap_v});
                    }
                    try writer.print("  %mc_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{mc.dst, pc, mc.dst});
                    try writer.print("  call void @rt_make_closure(ptr %mc_dst_ptr_{d}_{d}, ptr @{s}, ptr %v{d}, i32 {d})\n", .{mc.dst, pc, func_ptr_name, env_ptr, mc.arity});
                },
                .call => |c| {
                    try writer.print("  %f_ptr_call_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{c.func, pc, c.func});
                    const f_val = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %f_ptr_call_{d}_{d}\n", .{f_val, c.func, pc});
                    const args_ptr = next_temp(&temp_counter);
                    try writer.print("  %v{d} = alloca %Value, i32 {d}\n", .{args_ptr, c.args.len});
                    for (c.args, 0..) |arg_idx, i| {
                        try writer.print("  %arg_src_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{arg_idx, pc, arg_idx});
                        const arg_v = next_temp(&temp_counter);
                        try writer.print("  %v{d} = load %Value, ptr %arg_src_{d}_{d}\n", .{arg_v, arg_idx, pc});
                        const arg_dst_ptr = next_temp(&temp_counter);
                        try writer.print("  %v{d} = getelementptr %Value, ptr %v{d}, i32 {d}\n", .{arg_dst_ptr, args_ptr, i});
                        try writer.print("  store %Value %v{d}, ptr %v{d}\n", .{arg_v, arg_dst_ptr});
                    }
                    const res = next_temp(&temp_counter);
                    try writer.print("  %v{d} = call %Value @rt_call(%Value %v{d}, ptr %v{d}, i32 {d})\n", .{res, f_val, args_ptr, c.args.len});
                    try writer.print("  %dst_ptr_call_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{c.dst, pc, c.dst});
                    try writer.print("  store %Value %v{d}, ptr %dst_ptr_call_{d}_{d}\n", .{res, c.dst, pc});
                },
                .borrow => |b| {
                    try writer.print("  %src_ptr_br_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{b.src, pc, b.src});
                    const v = next_temp(&temp_counter);
                    try writer.print("  %v{d} = load %Value, ptr %src_ptr_br_{d}_{d}\n", .{v, b.src, pc});
                    try writer.print("  %dst_ptr_br_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{b.dst, pc, b.dst});
                    try writer.print("  store %Value %v{d}, ptr %dst_ptr_br_{d}_{d}\n", .{v, b.dst, pc});
                },
                .free => |f| {
                    try writer.print("  %reg_ptr_fr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{f.reg, pc, f.reg});
                    try writer.print("  call void @rt_make_unit(ptr %reg_ptr_fr_{d}_{d})\n", .{f.reg, pc});
                },
                .ibin => |o| {
                    const llvm_op: []const u8 = switch (o.op) {
                        .add => "add", .sub => "sub", .mul => "mul", .sdiv => "sdiv", .udiv => "udiv",
                        .srem => "srem", .urem => "urem", .and_ => "and", .or_ => "or", .xor_ => "xor",
                        .shl => "shl", .lshr => "lshr", .ashr => "ashr",
                    };
                    const w = o.width;
                    try writer.print("  %ibin_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %ibin_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    if (w <= 64) {
                        const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %ibin_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                        const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %ibin_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                        const res = next_temp(&temp_counter);
                        if (w < 64) {
                            const lt = next_temp(&temp_counter); const rt_t = next_temp(&temp_counter);
                            try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{lt, lv, w});
                            try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{rt_t, rv, w});
                            const op_r = next_temp(&temp_counter); try writer.print("  %v{d} = {s} i{d} %v{d}, %v{d}\n", .{op_r, llvm_op, w, lt, rt_t});
                            try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{res, w, op_r});
                        } else { try writer.print("  %v{d} = {s} i64 %v{d}, %v{d}\n", .{res, llvm_op, lv, rv}); }
                        try writer.print("  %ibin_dst_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                        try writer.print("  call void @rt_make_int(ptr %ibin_dst_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, res});
                    } else {
                        const lc = (w + 63) / 64;
                        var lhs_acc = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_lhs_{d}_{d}, i64 0)\n", .{lhs_acc, o.lhs, pc});
                        const lhs_e0 = next_temp(&temp_counter); try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{lhs_e0, lhs_acc, w});
                        lhs_acc = lhs_e0;
                        for (1..lc) |li| {
                            const lr = next_temp(&temp_counter); const le = next_temp(&temp_counter); const ls = next_temp(&temp_counter); const la = next_temp(&temp_counter);
                            try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_lhs_{d}_{d}, i64 {d})\n", .{lr, o.lhs, pc, li});
                            try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{le, lr, w}); try writer.print("  %v{d} = shl i{d} %v{d}, {d}\n", .{ls, w, le, li * 64});
                            try writer.print("  %v{d} = or i{d} %v{d}, %v{d}\n", .{la, w, lhs_acc, ls}); lhs_acc = la;
                        }
                        var rhs_acc = next_temp(&temp_counter); const is_shift = o.op == .shl or o.op == .lshr or o.op == .ashr;
                        if (is_shift) {
                            try writer.print("  %v{d} = call i64 @rt_get_int(ptr %ibin_rhs_{d}_{d})\n", .{rhs_acc, o.rhs, pc});
                            const rhs_e = next_temp(&temp_counter); try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{rhs_e, rhs_acc, w}); rhs_acc = rhs_e;
                        } else {
                            try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_rhs_{d}_{d}, i64 0)\n", .{rhs_acc, o.rhs, pc});
                            const rhs_e0 = next_temp(&temp_counter); try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{rhs_e0, rhs_acc, w}); rhs_acc = rhs_e0;
                            for (1..lc) |li| {
                                const rr = next_temp(&temp_counter); const re = next_temp(&temp_counter); const rs = next_temp(&temp_counter); const ra = next_temp(&temp_counter);
                                try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %ibin_rhs_{d}_{d}, i64 {d})\n", .{rr, o.rhs, pc, li});
                                try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{re, rr, w}); try writer.print("  %v{d} = shl i{d} %v{d}, {d}\n", .{rs, w, re, li * 64});
                                try writer.print("  %v{d} = or i{d} %v{d}, %v{d}\n", .{ra, w, rhs_acc, rs}); rhs_acc = ra;
                            }
                        }
                        const op_res = next_temp(&temp_counter); try writer.print("  %v{d} = {s} i{d} %v{d}, %v{d}\n", .{op_res, llvm_op, w, lhs_acc, rhs_acc});
                        try writer.print("  %ibin_wide_{d}_{d} = alloca [{d} x i64]\n", .{o.dst, pc, lc});
                        for (0..lc) |li| {
                            const to_trunc = if (li == 0) op_res else blk: { const s = next_temp(&temp_counter); try writer.print("  %v{d} = lshr i{d} %v{d}, {d}\n", .{s, w, op_res, li * 64}); break :blk s; };
                            const tr = next_temp(&temp_counter); const gp = next_temp(&temp_counter);
                            try writer.print("  %v{d} = trunc i{d} %v{d} to i64\n", .{tr, w, to_trunc}); try writer.print("  %v{d} = getelementptr [{d} x i64], ptr %ibin_wide_{d}_{d}, i32 0, i32 {d}\n", .{gp, lc, o.dst, pc, li});
                            try writer.print("  store i64 %v{d}, ptr %v{d}\n", .{tr, gp});
                        }
                        try writer.print("  %ibin_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                        try writer.print("  call void @rt_make_wide(ptr %ibin_dst_ptr_{d}_{d}, ptr %ibin_wide_{d}_{d}, i64 {d})\n", .{o.dst, pc, o.dst, pc, lc});
                    }
                },
                .icmp => |o| {
                    const llvm_pred: []const u8 = switch (o.pred) { .eq => "eq", .ne => "ne", .slt => "slt", .sle => "sle", .sgt => "sgt", .sge => "sge", .ult => "ult", .ule => "ule", .ugt => "ugt", .uge => "uge" };
                    const w = o.width;
                    try writer.print("  %icmp_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %icmp_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const cmp_id = next_temp(&temp_counter);
                    if (w <= 64) {
                        const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %icmp_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                        const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %icmp_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                        if (w < 64) {
                            const lt = next_temp(&temp_counter); const rt_t = next_temp(&temp_counter);
                            try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{lt, lv, w}); try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{rt_t, rv, w});
                            try writer.print("  %v{d} = icmp {s} i{d} %v{d}, %v{d}\n", .{cmp_id, llvm_pred, w, lt, rt_t});
                        } else { try writer.print("  %v{d} = icmp {s} i64 %v{d}, %v{d}\n", .{cmp_id, llvm_pred, lv, rv}); }
                    } else {
                        const lc = (w + 63) / 64;
                        var lhs_acc = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %icmp_lhs_{d}_{d}, i64 0)\n", .{lhs_acc, o.lhs, pc});
                        const lhs_e0 = next_temp(&temp_counter); try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{lhs_e0, lhs_acc, w}); lhs_acc = lhs_e0;
                        for (1..lc) |li| {
                            const lr = next_temp(&temp_counter); const le = next_temp(&temp_counter); const ls = next_temp(&temp_counter); const la = next_temp(&temp_counter);
                            try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %icmp_lhs_{d}_{d}, i64 {d})\n", .{lr, o.lhs, pc, li});
                            try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{le, lr, w}); try writer.print("  %v{d} = shl i{d} %v{d}, {d}\n", .{ls, w, le, li * 64});
                            try writer.print("  %v{d} = or i{d} %v{d}, %v{d}\n", .{la, w, lhs_acc, ls}); lhs_acc = la;
                        }
                        var rhs_acc = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %icmp_rhs_{d}_{d}, i64 0)\n", .{rhs_acc, o.rhs, pc});
                        const rhs_e0 = next_temp(&temp_counter); try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{rhs_e0, rhs_acc, w}); rhs_acc = rhs_e0;
                        for (1..lc) |li| {
                            const rr = next_temp(&temp_counter); const re = next_temp(&temp_counter); const rs = next_temp(&temp_counter); const ra = next_temp(&temp_counter);
                            try writer.print("  %v{d} = call i64 @rt_get_wide_limb(ptr %icmp_rhs_{d}_{d}, i64 {d})\n", .{rr, o.rhs, pc, li});
                            try writer.print("  %v{d} = zext i64 %v{d} to i{d}\n", .{re, rr, w}); try writer.print("  %v{d} = shl i{d} %v{d}, {d}\n", .{rs, w, re, li * 64});
                            try writer.print("  %v{d} = or i{d} %v{d}, %v{d}\n", .{ra, w, rhs_acc, rs}); rhs_acc = ra;
                        }
                        try writer.print("  %v{d} = icmp {s} i{d} %v{d}, %v{d}\n", .{cmp_id, llvm_pred, w, lhs_acc, rhs_acc});
                    }
                    const bid = next_temp(&temp_counter);
                    var t_lab_buf: [64]u8 = undefined; var f_lab_buf: [64]u8 = undefined; var e_lab_buf: [64]u8 = undefined;
                    const t_lab = try std.fmt.bufPrint(&t_lab_buf, "icmp_t_{d}_{d}", .{bid, pc});
                    const f_lab = try std.fmt.bufPrint(&f_lab_buf, "icmp_f_{d}_{d}", .{bid, pc});
                    const e_lab = try std.fmt.bufPrint(&e_lab_buf, "icmp_end_{d}_{d}", .{bid, pc});
                    try writer.print("  %icmp_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  br i1 %v{d}, label %{s}, label %{s}\n", .{cmp_id, t_lab, f_lab});
                    try writer.print("{s}:\n", .{t_lab});
                    const ut = next_temp(&temp_counter); try writer.print("  %v{d} = alloca %Value\n", .{ut}); try writer.print("  call void @rt_init_unit(ptr %v{d})\n", .{ut});
                    const uvt = next_temp(&temp_counter); try writer.print("  %v{d} = load %Value, ptr %v{d}\n", .{uvt, ut});
                    try writer.print("  call void @rt_make_sum(ptr %icmp_dst_ptr_{d}_{d}, %Value %v{d}, i64 3)\n", .{o.dst, pc, uvt}); try writer.print("  br label %{s}\n", .{e_lab});
                    try writer.print("{s}:\n", .{f_lab});
                    const uf = next_temp(&temp_counter); try writer.print("  %v{d} = alloca %Value\n", .{uf}); try writer.print("  call void @rt_init_unit(ptr %v{d})\n", .{uf});
                    const uvf = next_temp(&temp_counter); try writer.print("  %v{d} = load %Value, ptr %v{d}\n", .{uvf, uf});
                    try writer.print("  call void @rt_make_sum(ptr %icmp_dst_ptr_{d}_{d}, %Value %v{d}, i64 4)\n", .{o.dst, pc, uvf}); try writer.print("  br label %{s}\n", .{e_lab});
                    try writer.print("{s}:\n", .{e_lab});
                },
                .load_bits => |o| {
                    try writer.print("  %lb_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %lb_dst_ptr_{d}_{d}, i64 {d})\n", .{o.dst, pc, @as(i64, @bitCast(o.val))});
                },
                .load_wide => |o| {
                    const w = o.width; const lc = (w + 63) / 64;
                    try writer.print("  %lw_arr_{d}_{d} = alloca [{d} x i64]\n", .{o.dst, pc, lc});
                    for (0..lc) |li| {
                        const val: i64 = @bitCast(if (li < o.limbs.len) o.limbs[li] else 0);
                        const gep = next_temp(&temp_counter); try writer.print("  %v{d} = getelementptr [{d} x i64], ptr %lw_arr_{d}_{d}, i32 0, i32 {d}\n", .{gep, lc, o.dst, pc, li});
                        try writer.print("  store i64 {d}, ptr %v{d}\n", .{val, gep});
                    }
                    try writer.print("  %lw_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_wide(ptr %lw_dst_ptr_{d}_{d}, ptr %lw_arr_{d}_{d}, i64 {d})\n", .{o.dst, pc, o.dst, pc, lc});
                },
                .sext => |o| {
                    try writer.print("  %sext_src_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.src, pc, o.src});
                    const sv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %sext_src_ptr_{d}_{d})\n", .{sv, o.src, pc});
                    const tr = next_temp(&temp_counter); try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{tr, sv, o.from});
                    const se = next_temp(&temp_counter); try writer.print("  %v{d} = sext i{d} %v{d} to i64\n", .{se, o.from, tr});
                    try writer.print("  %sext_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %sext_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, se});
                },
                .zext => |o| {
                    try writer.print("  %zext_src_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.src, pc, o.src});
                    const sv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %zext_src_ptr_{d}_{d})\n", .{sv, o.src, pc});
                    const tr = next_temp(&temp_counter); try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{tr, sv, o.from});
                    const ze = next_temp(&temp_counter); try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{ze, o.from, tr});
                    try writer.print("  %zext_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %zext_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, ze});
                },
                .trunc => |o| {
                    try writer.print("  %trunc_src_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.src, pc, o.src});
                    const sv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %trunc_src_ptr_{d}_{d})\n", .{sv, o.src, pc});
                    const tr = next_temp(&temp_counter); try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{tr, sv, o.to});
                    const ze = next_temp(&temp_counter); try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{ze, o.to, tr});
                    try writer.print("  %trunc_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %trunc_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, ze});
                },
                .itof => |o| {
                    const conv_op: []const u8 = if (o.signed) "sitofp" else "uitofp";
                    try writer.print("  %itof_src_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.src, pc, o.src});
                    const sv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %itof_src_ptr_{d}_{d})\n", .{sv, o.src, pc});
                    const fp = next_temp(&temp_counter);
                    if (o.width < 64) {
                        const tr = next_temp(&temp_counter); try writer.print("  %v{d} = trunc i64 %v{d} to i{d}\n", .{tr, sv, o.width});
                        try writer.print("  %v{d} = {s} i{d} %v{d} to double\n", .{fp, conv_op, o.width, tr});
                    } else { try writer.print("  %v{d} = {s} i64 %v{d} to double\n", .{fp, conv_op, sv}); }
                    const bi = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast double %v{d} to i64\n", .{bi, fp});
                    try writer.print("  %itof_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %itof_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, bi});
                },
                .ftoi => |o| {
                    const conv_op: []const u8 = if (o.signed) "fptosi" else "fptoui";
                    try writer.print("  %ftoi_src_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.src, pc, o.src});
                    const sv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %ftoi_src_ptr_{d}_{d})\n", .{sv, o.src, pc});
                    const fp = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{fp, sv});
                    const iv = next_temp(&temp_counter);
                    if (o.width < 64) {
                        try writer.print("  %v{d} = {s} double %v{d} to i{d}\n", .{iv, conv_op, fp, o.width});
                        const ze = next_temp(&temp_counter); try writer.print("  %v{d} = zext i{d} %v{d} to i64\n", .{ze, o.width, iv});
                        try writer.print("  %ftoi_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                        try writer.print("  call void @rt_make_int(ptr %ftoi_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, ze});
                    } else {
                        try writer.print("  %v{d} = {s} double %v{d} to i64\n", .{iv, conv_op, fp});
                        try writer.print("  %ftoi_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                        try writer.print("  call void @rt_make_int(ptr %ftoi_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, iv});
                    }
                },
                .fadd => |o| {
                    try writer.print("  %fadd_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %fadd_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fadd_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                    const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fadd_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                    const lf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{lf, lv});
                    const rf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{rf, rv});
                    const res = next_temp(&temp_counter); try writer.print("  %v{d} = fadd double %v{d}, %v{d}\n", .{res, lf, rf});
                    const bi = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast double %v{d} to i64\n", .{bi, res});
                    try writer.print("  %fadd_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %fadd_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, bi});
                },
                .fsub => |o| {
                    try writer.print("  %fsub_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %fsub_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fsub_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                    const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fsub_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                    const lf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{lf, lv});
                    const rf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{rf, rv});
                    const res = next_temp(&temp_counter); try writer.print("  %v{d} = fsub double %v{d}, %v{d}\n", .{res, lf, rf});
                    const bi = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast double %v{d} to i64\n", .{bi, res});
                    try writer.print("  %fsub_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %fsub_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, bi});
                },
                .fmul => |o| {
                    try writer.print("  %fmul_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %fmul_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fmul_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                    const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fmul_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                    const lf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{lf, lv});
                    const rf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{rf, rv});
                    const res = next_temp(&temp_counter); try writer.print("  %v{d} = fmul double %v{d}, %v{d}\n", .{res, lf, rf});
                    const bi = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast double %v{d} to i64\n", .{bi, res});
                    try writer.print("  %fmul_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %fmul_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, bi});
                },
                .fdiv => |o| {
                    try writer.print("  %fdiv_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %fdiv_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fdiv_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                    const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fdiv_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                    const lf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{lf, lv});
                    const rf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{rf, rv});
                    const res = next_temp(&temp_counter); try writer.print("  %v{d} = fdiv double %v{d}, %v{d}\n", .{res, lf, rf});
                    const bi = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast double %v{d} to i64\n", .{bi, res});
                    try writer.print("  %fdiv_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %fdiv_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, bi});
                },
                .frem => |o| {
                    try writer.print("  %frem_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %frem_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %frem_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                    const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %frem_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                    const lf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{lf, lv});
                    const rf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{rf, rv});
                    const res = next_temp(&temp_counter); try writer.print("  %v{d} = frem double %v{d}, %v{d}\n", .{res, lf, rf});
                    const bi = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast double %v{d} to i64\n", .{bi, res});
                    try writer.print("  %frem_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  call void @rt_make_int(ptr %frem_dst_ptr_{d}_{d}, i64 %v{d})\n", .{o.dst, pc, bi});
                },
                .fcmp => |o| {
                    const llvm_pred: []const u8 = switch (o.pred) { .oeq => "oeq", .one => "one", .olt => "olt", .ole => "ole", .ogt => "ogt", .oge => "oge", .ord => "ord", .uno => "uno" };
                    try writer.print("  %fcmp_lhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.lhs, pc, o.lhs});
                    try writer.print("  %fcmp_rhs_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.rhs, pc, o.rhs});
                    const lv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fcmp_lhs_{d}_{d})\n", .{lv, o.lhs, pc});
                    const rv = next_temp(&temp_counter); try writer.print("  %v{d} = call i64 @rt_get_int(ptr %fcmp_rhs_{d}_{d})\n", .{rv, o.rhs, pc});
                    const lf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{lf, lv});
                    const rf = next_temp(&temp_counter); try writer.print("  %v{d} = bitcast i64 %v{d} to double\n", .{rf, rv});
                    const cmp_id = next_temp(&temp_counter); try writer.print("  %v{d} = fcmp {s} double %v{d}, %v{d}\n", .{cmp_id, llvm_pred, lf, rf});
                    const bid = next_temp(&temp_counter);
                    var t_lab_buf: [64]u8 = undefined; var f_lab_buf: [64]u8 = undefined; var e_lab_buf: [64]u8 = undefined;
                    const t_lab = try std.fmt.bufPrint(&t_lab_buf, "fcmp_t_{d}_{d}", .{bid, pc});
                    const f_lab = try std.fmt.bufPrint(&f_lab_buf, "fcmp_f_{d}_{d}", .{bid, pc});
                    const e_lab = try std.fmt.bufPrint(&e_lab_buf, "fcmp_end_{d}_{d}", .{bid, pc});
                    try writer.print("  %fcmp_dst_ptr_{d}_{d} = getelementptr %Value, ptr %regs, i32 {d}\n", .{o.dst, pc, o.dst});
                    try writer.print("  br i1 %v{d}, label %{s}, label %{s}\n", .{cmp_id, t_lab, f_lab});
                    try writer.print("{s}:\n", .{t_lab});
                    const ut = next_temp(&temp_counter); try writer.print("  %v{d} = alloca %Value\n", .{ut}); try writer.print("  call void @rt_init_unit(ptr %v{d})\n", .{ut});
                    const uvt = next_temp(&temp_counter); try writer.print("  %v{d} = load %Value, ptr %v{d}\n", .{uvt, ut});
                    try writer.print("  call void @rt_make_sum(ptr %fcmp_dst_ptr_{d}_{d}, %Value %v{d}, i64 3)\n", .{o.dst, pc, uvt}); try writer.print("  br label %{s}\n", .{e_lab});
                    try writer.print("{s}:\n", .{f_lab});
                    const uf = next_temp(&temp_counter); try writer.print("  %v{d} = alloca %Value\n", .{uf}); try writer.print("  call void @rt_init_unit(ptr %v{d})\n", .{uf});
                    const uvf = next_temp(&temp_counter); try writer.print("  %v{d} = load %Value, ptr %v{d}\n", .{uvf, uf});
                    try writer.print("  call void @rt_make_sum(ptr %fcmp_dst_ptr_{d}_{d}, %Value %v{d}, i64 4)\n", .{o.dst, pc, uvf}); try writer.print("  br label %{s}\n", .{e_lab});
                    try writer.print("{s}:\n", .{e_lab});
                },
            }
        }
        try writer.writeAll("  unreachable\n");
        try writer.writeAll("}\n");
    }
};
