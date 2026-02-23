// ==========================================
// runtime.c
// Romanesco ネイティブランタイム (Pointer-based Out-parameter版)
// ==========================================

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    int64_t tag;
    void* ptr;
} Value;

typedef struct {
    Value first;
    Value second;
} Pair;

void rt_make_int(Value* out, int64_t n) {
    out->tag = 6;
    out->ptr = (void*)n;
}

int64_t rt_get_int(Value* v) {
    if (v->tag == 6) {
        return (int64_t)v->ptr;
    }
    return 0;
}

void rt_make_unit(Value* out) {
    out->tag = 5;
    out->ptr = NULL;
}

void rt_make_pair(Value* out, Value* a, Value* b) {
    Pair* p = (Pair*)malloc(sizeof(Pair));
    p->first = *a;
    p->second = *b;
    out->tag = 2;
    out->ptr = p;
}

void rt_proj1(Value* out, Value* p) {
    Pair* pair = (Pair*)p->ptr;
    *out = pair->first;
}

void rt_proj2(Value* out, Value* p) {
    Pair* pair = (Pair*)p->ptr;
    *out = pair->second;
}

void rt_free_value(Value* v) {
    if (v->tag == 2) {
        free(v->ptr);
    }
}

void rt_print_int(int64_t n) {
    printf("%lld\n", (long long)n);
}

void rt_make_literal(Value* out, char* s) {
    out->tag = 0;
    out->ptr = s;
}

// ダミー
void rt_setup_regs() {}
void* rt_alloc_env(int s) { return NULL; }
void rt_env_store(void* e, int i, Value* v) {}
void rt_make_closure(Value* out, void* f, void* e, int a) { rt_make_unit(out); }
void rt_call(Value* out, Value* f, void* a, int n) { rt_make_unit(out); }
