# Zig Porting Plan: Advanced Runtime & Speculative JIT

This document outlines the roadmap for porting the advanced Romanesco runtime features from Scala to Zig.

## Phase 1: Bytecode Optimization & Analysis
Porting the static analysis and optimization passes to improve VM performance and prepare for JIT.

1.  **`optimizer.zig`**:
    *   **BytecodeOptimizer**: Constant folding, Dead Code Elimination (DCE), and move simplification.
    *   **RegisterAllocator**: Liveness analysis and register re-assignment to minimize register usage.
2.  **`analyzer.zig`**:
    *   **RangeAnalyzer**: Bit-width inference (i1~i64) based on profiling data.
    *   **EscapeAnalysis**: Identifying registers that can be stack-allocated vs. heap-allocated.

## Phase 2: LLVM Backend & JIT
Implementing the high-performance native execution engine.

1.  **`codegen.zig`**:
    *   Translate optimized bytecode to LLVM IR.
    *   Integrate bit-width info from `RangeAnalyzer`.
    *   Implement efficient closure calling conventions and environment management.
2.  **`jit.zig`**:
    *   Dynamic compilation using `clang -shared`.
    *   Loading native binaries using `std.DynLib`.
    *   **OSR (On-Stack Replacement)**: Synchronizing VM registers with native memory for seamless execution handover.

## Phase 3: Speculative Execution
Connecting the VM and JIT.

1.  **`SpeculativeExecutor.zig`**:
    *   Hot-spot detection logic.
    *   Deoptimization (Deopt) mechanism to fall back to VM when guards fail.
    *   Integration with `ProfilingVM`.

## Integration Strategy
- Keep `vm.zig` as the reference interpreter.
- New components will reside in `Zig/src/`.
- Maintain a comprehensive test suite in `Zig/src/tests/` to ensure parity with Scala results.

## Phase 4: Deep Refinement & Polish
Maximizing the potential of the Zig runtime.

1.  **Complete Codegen**: Implement full instruction set support in `codegen.zig` including nested structures and branching.
2.  **Robust Native Runtime**: Enhance the embedded LLVM runtime with proper memory management and ABI-safe data transfer.
3.  **Liveness-based Optimization**: Implement true register reuse in `optimizer.zig`.
4.  **JIT Binary Caching**: Cache compiled shared libraries to avoid Clang overhead on every call.
5.  **Deoptimization Guards**: Implement speculative guards that trigger fallback to interpreter when type assumptions are violated.
