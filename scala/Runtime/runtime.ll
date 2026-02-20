; ==========================================
; runtime.ll
; Romanesco バイトコードVM ランタイムライブラリ
; ==========================================
;
; Value型: { i8 tag, ptr payload }
;   tag 0 = IntVal   (payload: i64へのポインタ)
;   tag 1 = Literal  (payload: i8文字列ポインタ)
;   tag 2 = Closure  (payload: { ptr funcPtr, ptr env, i32 arity }へのポインタ)
;   tag 3 = Pair     (payload: { %Value, %Value }へのポインタ)
;   tag 4 = Inl      (payload: %Valueへのポインタ)
;   tag 5 = Inr      (payload: %Valueへのポインタ)
;   tag 6 = Unit     (payload: null)

%Value = type { i8, ptr }
%Closure = type { ptr, ptr, i32 }
%Pair = type { %Value, %Value }

; 外部関数
declare ptr @malloc(i64)
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)

; ==========================================
; rt_make_int: i64 → Value(tag=0)
; ==========================================
define %Value @rt_make_int(i64 %n) {
  %ptr = call ptr @malloc(i64 8)
  store i64 %n, ptr %ptr
  %v1 = insertvalue %Value undef, i8 0, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

; ==========================================
; rt_make_literal: ptr → Value(tag=1)
; ==========================================
define %Value @rt_make_literal(ptr %s) {
  %v1 = insertvalue %Value undef, i8 1, 0
  %v2 = insertvalue %Value %v1, ptr %s, 1
  ret %Value %v2
}

; ==========================================
; rt_make_unit: → Value(tag=6)
; ==========================================
define %Value @rt_make_unit() {
  %v1 = insertvalue %Value undef, i8 6, 0
  %v2 = insertvalue %Value %v1, ptr null, 1
  ret %Value %v2
}

; ==========================================
; rt_make_pair: (Value, Value) → Value(tag=3)
; ==========================================
define %Value @rt_make_pair(%Value %a, %Value %b) {
  %ptr = call ptr @malloc(i64 24)

  %p1 = getelementptr %Pair, ptr %ptr, i32 0, i32 0
  store %Value %a, ptr %p1

  %p2 = getelementptr %Pair, ptr %ptr, i32 0, i32 1
  store %Value %b, ptr %p2

  %v1 = insertvalue %Value undef, i8 3, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

; ==========================================
; rt_proj1: Value(Pair) → Value
; ==========================================
define %Value @rt_proj1(%Value %p) {
  %ptr = extractvalue %Value %p, 1
  %vptr = getelementptr %Pair, ptr %ptr, i32 0, i32 0
  %v = load %Value, ptr %vptr
  ret %Value %v
}

; ==========================================
; rt_proj2: Value(Pair) → Value
; ==========================================
define %Value @rt_proj2(%Value %p) {
  %ptr = extractvalue %Value %p, 1
  %vptr = getelementptr %Pair, ptr %ptr, i32 0, i32 1
  %v = load %Value, ptr %vptr
  ret %Value %v
}

; ==========================================
; rt_make_inl: Value → Value(tag=4)
; ==========================================
define %Value @rt_make_inl(%Value %inner) {
  %ptr = call ptr @malloc(i64 16)
  store %Value %inner, ptr %ptr
  %v1 = insertvalue %Value undef, i8 4, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

; ==========================================
; rt_make_inr: Value → Value(tag=5)
; ==========================================
define %Value @rt_make_inr(%Value %inner) {
  %ptr = call ptr @malloc(i64 16)
  store %Value %inner, ptr %ptr
  %v1 = insertvalue %Value undef, i8 5, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

; ==========================================
; rt_get_tag: Value → i8
; ==========================================
define i8 @rt_get_tag(%Value %v) {
  %tag = extractvalue %Value %v, 0
  ret i8 %tag
}

; ==========================================
; rt_get_inner: Value(Inl/Inr) → Value
; ==========================================
define %Value @rt_get_inner(%Value %v) {
  %ptr = extractvalue %Value %v, 1
  %inner = load %Value, ptr %ptr
  ret %Value %inner
}

; ==========================================
; rt_make_closure: (funcPtr, env, arity) → Value(tag=2)
; ==========================================
define %Value @rt_make_closure(ptr %func, ptr %env, i32 %arity) {
  %ptr = call ptr @malloc(i64 24)

  %f = getelementptr %Closure, ptr %ptr, i32 0, i32 0
  store ptr %func, ptr %f

  %e = getelementptr %Closure, ptr %ptr, i32 0, i32 1
  store ptr %env, ptr %e

  %a = getelementptr %Closure, ptr %ptr, i32 0, i32 2
  store i32 %arity, ptr %a

  %v1 = insertvalue %Value undef, i8 2, 0
  %v2 = insertvalue %Value %v1, ptr %ptr, 1
  ret %Value %v2
}

; ==========================================
; rt_apply: (closure, arg) → Value
; ==========================================
define %Value @rt_apply(%Value %fn, %Value %arg) {
entry:
  %ptr = extractvalue %Value %fn, 1

  %fpp = getelementptr %Closure, ptr %ptr, i32 0, i32 0
  %funcPtr = load ptr, ptr %fpp

  %epp = getelementptr %Closure, ptr %ptr, i32 0, i32 1
  %env = load ptr, ptr %epp

  %app = getelementptr %Closure, ptr %ptr, i32 0, i32 2
  %arity = load i32, ptr %app

  %is_one = icmp eq i32 %arity, 1
  br i1 %is_one, label %direct_call, label %partial_apply

direct_call:
  %result = call %Value %funcPtr(ptr %env, %Value %arg)
  ret %Value %result

partial_apply:
  %env_size = load i32, ptr %env

  %new_size = add i32 %env_size, 1
  %new_env = call ptr @rt_alloc_env(i32 %new_size)

  %src_data = getelementptr i8, ptr %env, i32 4
  %dst_data = getelementptr i8, ptr %new_env, i32 4
  %copy_bytes = mul i32 %env_size, 16
  %copy_i64 = zext i32 %copy_bytes to i64
  call void @llvm.memcpy.p0.p0.i64(ptr %dst_data, ptr %src_data, i64 %copy_i64, i1 false)

  call void @rt_env_store(ptr %new_env, i32 %env_size, %Value %arg)

  %new_arity = sub i32 %arity, 1
  %new_closure = call %Value @rt_make_closure(ptr %funcPtr, ptr %new_env, i32 %new_arity)
  ret %Value %new_closure
}

; ==========================================
; rt_alloc_env: i32 → ptr
; ==========================================
define ptr @rt_alloc_env(i32 %n) {
  %val_size = mul i32 %n, 16
  %total_i32 = add i32 %val_size, 4
  %total = zext i32 %total_i32 to i64
  %ptr = call ptr @malloc(i64 %total)
  store i32 %n, ptr %ptr
  ret ptr %ptr
}

; ==========================================
; rt_env_store: (env, index, value) → void
; ==========================================
define void @rt_env_store(ptr %env, i32 %idx, %Value %val) {
  %data = getelementptr i8, ptr %env, i32 4
  %vptr = getelementptr %Value, ptr %data, i32 %idx
  store %Value %val, ptr %vptr
  ret void
}

; ==========================================
; rt_env_load: (env, index) → Value
; ==========================================
define %Value @rt_env_load(ptr %env, i32 %idx) {
  %data = getelementptr i8, ptr %env, i32 4
  %vptr = getelementptr %Value, ptr %data, i32 %idx
  %v = load %Value, ptr %vptr
  ret %Value %v
}
