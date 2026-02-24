; ModuleID = 'tier_1_1771902296552918000'
source_filename = "romanesco_module"
target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

%Value = type { i64, ptr }
%Closure = type { ptr, ptr, i32 }
define dllexport i64 @tier_1_1771902296552918000(ptr %external_regs) {
entry:
  %regs = alloca %Value, i32 3
  %r_ptr_0 = getelementptr %Value, ptr %regs, i32 0
  call void @rt_init_unit(ptr %r_ptr_0)
  %r_ptr_1 = getelementptr %Value, ptr %regs, i32 1
  call void @rt_init_unit(ptr %r_ptr_1)
  %r_ptr_2 = getelementptr %Value, ptr %regs, i32 2
  call void @rt_init_unit(ptr %r_ptr_2)
  br label %body

deopt_exit:
  %deopt_val = or i64 0, 2147483648
  ret i64 %deopt_val

body:
  %v0 = call ptr @rt_alloc_env(i32 0)
  %dst_ptr_0 = getelementptr %Value, ptr %regs, i32 0
  call void @rt_make_closure(ptr %dst_ptr_0, ptr @__block_1, ptr %v0, i32 1)
  %r_1 = getelementptr %Value, ptr %regs, i32 1
  call void @rt_make_int(ptr %r_1, i64 2)
  %f_ptr_0 = getelementptr %Value, ptr %regs, i32 0
  %v1 = load %Value, ptr %f_ptr_0
  %v2 = alloca %Value, i32 1
  %arg_src_1 = getelementptr %Value, ptr %regs, i32 1
  %v3 = load %Value, ptr %arg_src_1
  %v4 = getelementptr %Value, ptr %v2, i32 0
  store %Value %v3, ptr %v4
  %v5 = call %Value @rt_call(%Value %v1, ptr %v2, i32 1)
  %dst_ptr_2 = getelementptr %Value, ptr %regs, i32 2
  store %Value %v5, ptr %dst_ptr_2
  %ret_ptr_2 = getelementptr %Value, ptr %regs, i32 2
  %v6 = call i64 @rt_get_int(ptr %ret_ptr_2)
  ret i64 %v6
  unreachable
}
define %Value @__block_1(ptr %env, ptr %args, i32 %num_args) {
entry:
  %regs = alloca %Value, i32 3
  call void @rt_setup_regs(ptr %regs, i32 3, ptr %env, ptr %args, i32 %num_args)
  br label %body

deopt_exit:
  ret %Value { i64 5, ptr null }

body:
  %r_1 = getelementptr %Value, ptr %regs, i32 1
  call void @rt_make_int(ptr %r_1, i64 40)
  %lhs_ptr_0 = getelementptr %Value, ptr %regs, i32 0
  %rhs_ptr_1 = getelementptr %Value, ptr %regs, i32 1
  %v_lhs_1 = load %Value, ptr %lhs_ptr_0
  %v2 = call i1 @rt_guard_tag(%Value %v_lhs_1, i64 6)
  br i1 %v2, label %deopt_exit, label %body_add_l_3_1
body_add_l_3_1:
  %v0 = call i64 @rt_get_int(ptr %lhs_ptr_0)
  %v_rhs_5 = load %Value, ptr %rhs_ptr_1
  %v6 = call i1 @rt_guard_tag(%Value %v_rhs_5, i64 6)
  br i1 %v6, label %deopt_exit, label %body_add_r_7_1
body_add_r_7_1:
  %v4 = call i64 @rt_get_int(ptr %rhs_ptr_1)
  %v8 = add i64 %v0, %v4
  %dst_ptr_2 = getelementptr %Value, ptr %regs, i32 2
  call void @rt_make_int(ptr %dst_ptr_2, i64 %v8)
  call void @rt_init_unit(ptr %lhs_ptr_0)
  call void @rt_init_unit(ptr %rhs_ptr_1)
  %ret_ptr_2 = getelementptr %Value, ptr %regs, i32 2
  %v9 = load %Value, ptr %ret_ptr_2
  ret %Value %v9
  unreachable
}

declare ptr @malloc(i64)
declare void @free(ptr)
declare i32 @printf(ptr, ...)
declare void @exit(i32)

@.str_int_fmt = private unnamed_addr constant [6 x i8] c"%lld\0A\00"
@.str_deopt_msg = private unnamed_addr constant [35 x i8] c"Deoptimization at PC %d triggered\0A\00"
@.str_reg_dump = private unnamed_addr constant [24 x i8] c"REG %d TAG %lld VAL %p\0A\00"
@.str_deopt_start = private unnamed_addr constant [19 x i8] c"DEOPT_STATE_START\0A\00"
@.str_deopt_end = private unnamed_addr constant [17 x i8] c"DEOPT_STATE_END\0A\00"

define void @rt_cleanup_value(%Value %v) {
  %tag = extractvalue %Value %v, 0
  %ptr = extractvalue %Value %v, 1
  switch i64 %tag, label %done [
    i64 1, label %free_cl
    i64 2, label %free_pair
    i64 3, label %free_sum
    i64 4, label %free_sum
  ]
free_cl:
  call void @free(ptr %ptr)
  br label %done
free_pair:
  %v1 = load %Value, ptr %ptr
  call void @rt_cleanup_value(%Value %v1)
  %v2_ptr = getelementptr %Value, ptr %ptr, i32 1
  %v2 = load %Value, ptr %v2_ptr
  call void @rt_cleanup_value(%Value %v2)
  call void @free(ptr %ptr)
  br label %done
free_sum:
  %inner = load %Value, ptr %ptr
  call void @rt_cleanup_value(%Value %inner)
  call void @free(ptr %ptr)
  br label %done
done:
  ret void
}

define void @rt_dump_regs(ptr %regs, i32 %count) {
  %start_msg = call i32 (ptr, ...) @printf(ptr @.str_deopt_start)
  %i_ptr = alloca i32
  store i32 0, ptr %i_ptr
  br label %loop
loop:
  %i = load i32, ptr %i_ptr
  %cond = icmp ult i32 %i, %count
  br i1 %cond, label %body, label %done
body:
  %ptr = getelementptr %Value, ptr %regs, i32 %i
  %tag_ptr = getelementptr %Value, ptr %ptr, i32 0, i32 0
  %tag = load i64, ptr %tag_ptr
  %val_ptr = getelementptr %Value, ptr %ptr, i32 0, i32 1
  %val = load ptr, ptr %val_ptr
  %dump_msg = call i32 (ptr, ...) @printf(ptr @.str_reg_dump, i32 %i, i64 %tag, ptr %val)
  %next = add i32 %i, 1
  store i32 %next, ptr %i_ptr
  br label %loop
done:
  %end_msg = call i32 (ptr, ...) @printf(ptr @.str_deopt_end)
  ret void
}

define i1 @rt_guard_tag(%Value %v, i64 %expected) {
  %tag = extractvalue %Value %v, 0
  %ok = icmp eq i64 %tag, %expected
  %fail = xor i1 %ok, 1
  ret i1 %fail
}

define i1 @rt_guard_value(%Value %v, i64 %expected) {
  %tag = extractvalue %Value %v, 0
  %is_int = icmp eq i64 %tag, 6
  br i1 %is_int, label %check_val, label %fail
check_val:
  %ptr = extractvalue %Value %v, 1
  %val = ptrtoint ptr %ptr to i64
  %ok = icmp eq i64 %val, %expected
  %fail_v = xor i1 %ok, 1
  ret i1 %fail_v
fail:
  ret i1 1
}

define void @rt_make_int(ptr %out, i64 %n) {
  %old = load %Value, ptr %out
  call void @rt_cleanup_value(%Value %old)
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 6, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  %ptr = inttoptr i64 %n to ptr
  store ptr %ptr, ptr %p2
  ret void
}

define i64 @rt_get_int(ptr %v) {
  %p1 = getelementptr %Value, ptr %v, i32 0, i32 0
  %tag = load i64, ptr %p1
  %is_int = icmp eq i64 %tag, 6
  br i1 %is_int, label %int_val, label %fail
int_val:
  %p2 = getelementptr %Value, ptr %v, i32 0, i32 1
  %ptr = load ptr, ptr %p2
  %n = ptrtoint ptr %ptr to i64
  ret i64 %n
fail:
  ret i64 0
}

define void @rt_make_unit(ptr %out) {
  %old = load %Value, ptr %out
  call void @rt_cleanup_value(%Value %old)
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 5, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr null, ptr %p2
  ret void
}

define void @rt_init_unit(ptr %out) {
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 5, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr null, ptr %p2
  ret void
}

define void @rt_make_pair(ptr %out, %Value %v1, %Value %v2) {
  %old = load %Value, ptr %out
  call void @rt_cleanup_value(%Value %old)
  %mem = call ptr @malloc(i64 32)
  store %Value %v1, ptr %mem
  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
  store %Value %v2, ptr %v2_ptr
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 2, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %p2
  ret void
}

define %Value @rt_proj1(ptr %src_ptr, %Value %pair) {
  %mem = extractvalue %Value %pair, 1
  %v1 = load %Value, ptr %mem
  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
  %v2 = load %Value, ptr %v2_ptr
  store %Value %v2, ptr %src_ptr
  ret %Value %v1
}

define %Value @rt_proj2(ptr %src_ptr, %Value %pair) {
  %mem = extractvalue %Value %pair, 1
  %v1 = load %Value, ptr %mem
  %v2_ptr = getelementptr %Value, ptr %mem, i32 1
  %v2 = load %Value, ptr %v2_ptr
  store %Value %v1, ptr %src_ptr
  ret %Value %v2
}

define void @rt_make_sum(ptr %out, %Value %v, i64 %tag) {
  %old = load %Value, ptr %out
  call void @rt_cleanup_value(%Value %old)
  %mem = call ptr @malloc(i64 16)
  store %Value %v, ptr %mem
  %p1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 %tag, ptr %p1
  %p2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %p2
  ret void
}

define ptr @rt_alloc_env(i32 %size) {
  %total_size = mul i32 %size, 16
  %real_size = add i32 %total_size, 8
  %zext_size = zext i32 %real_size to i64
  %mem = call ptr @malloc(i64 %zext_size)
  store i32 %size, ptr %mem
  ret ptr %mem
}

define void @rt_env_store(ptr %env, i32 %idx, %Value %v) {
  %data = getelementptr i8, ptr %env, i32 8
  %slot = getelementptr %Value, ptr %data, i32 %idx
  store %Value %v, ptr %slot
  ret void
}

define void @rt_make_closure(ptr %out, ptr %func, ptr %env, i32 %arity) {
  %old = load %Value, ptr %out
  call void @rt_cleanup_value(%Value %old)
  %mem = call ptr @malloc(i64 24)
  %p1 = getelementptr %Closure, ptr %mem, i32 0, i32 0
  store ptr %func, ptr %p1
  %p2 = getelementptr %Closure, ptr %mem, i32 0, i32 1
  store ptr %env, ptr %p2
  %p3 = getelementptr %Closure, ptr %mem, i32 0, i32 2
  store i32 %arity, ptr %p3
  %o1 = getelementptr %Value, ptr %out, i32 0, i32 0
  store i64 1, ptr %o1
  %o2 = getelementptr %Value, ptr %out, i32 0, i32 1
  store ptr %mem, ptr %o2
  ret void
}

define %Value @rt_call(%Value %clVal, ptr %args, i32 %num_args) {
  %tag = extractvalue %Value %clVal, 0
  %is_cl = icmp eq i64 %tag, 1
  br i1 %is_cl, label %do_call, label %error
do_call:
  %clPtr = extractvalue %Value %clVal, 1
  %fpp = getelementptr %Closure, ptr %clPtr, i32 0, i32 0
  %func = load ptr, ptr %fpp
  %epp = getelementptr %Closure, ptr %clPtr, i32 0, i32 1
  %env = load ptr, ptr %epp
  %res = call %Value %func(ptr %env, ptr %args, i32 %num_args)
  ret %Value %res
error:
  ret %Value { i64 5, ptr null }
}

define void @rt_setup_regs(ptr %regs, i32 %count, ptr %env, ptr %args, i32 %num_args) {
entry:
  %is_env_null = icmp eq ptr %env, null
  br i1 %is_env_null, label %args_pre_no_env, label %load_env
load_env:
  %env_size = load i32, ptr %env
  %i_ptr = alloca i32
  store i32 0, ptr %i_ptr
  br label %env_loop
env_loop:
  %i = load i32, ptr %i_ptr
  %cond = icmp ult i32 %i, %env_size
  br i1 %cond, label %env_body, label %args_pre_env
env_body:
  %data = getelementptr i8, ptr %env, i32 8
  %src_slot = getelementptr %Value, ptr %data, i32 %i
  %v = load %Value, ptr %src_slot
  %dst_slot = getelementptr %Value, ptr %regs, i32 %i
  store %Value %v, ptr %dst_slot
  %next_i = add i32 %i, 1
  store i32 %next_i, ptr %i_ptr
  br label %env_loop
args_pre_no_env:
  br label %args_pre
args_pre_env:
  br label %args_pre
args_pre:
  %final_env_size = phi i32 [ 0, %args_pre_no_env ], [ %env_size, %args_pre_env ]
  %j_ptr = alloca i32
  store i32 0, ptr %j_ptr
  br label %args_loop
args_loop:
  %j = load i32, ptr %j_ptr
  %cond2 = icmp ult i32 %j, %num_args
  br i1 %cond2, label %args_body, label %done
args_body:
  %src_arg = getelementptr %Value, ptr %args, i32 %j
  %v2 = load %Value, ptr %src_arg
  %idx = add i32 %j, %final_env_size
  %dst_arg = getelementptr %Value, ptr %regs, i32 %idx
  store %Value %v2, ptr %dst_arg
  %next_j = add i32 %j, 1
  store i32 %next_j, ptr %j_ptr
  br label %args_loop
done:
  ret void
}
