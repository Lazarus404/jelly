# Jelly VM performance notes

This document records Jelly’s performance goals, the design choices made to support them, and what to compare against when we benchmark.

## Goals

- **Small + predictable**: keep the VM small, with minimal opcode count and simple data structures.
- **Fast interpreter**: tight dispatch loop, fixed-width decode, typed registers (no per-slot runtime tags).
- **Correctness first**: no undefined behavior shortcuts; keep conversions explicit; validate bytecode strictly.
- **Portable**: run efficiently on 32-bit and 64-bit systems.

## What makes Jelly fast (by design)

- **Typed register file (Hashlink-style)**:
  - Each function carries a `reg_types[]` table (module-local type IDs).
  - The interpreter uses those static kinds to decide how to load/store each register.
  - There is **no dynamic tag per register slot** at runtime.

- **Boxing only at boundaries (Jello-style)**:
  - `Dynamic` is a boxed/tagged `jelly_value`.
  - Typed registers store unboxed primitives/pointers; **spill** is boxed only.
  - The compiler inserts `JOP_TO_DYN` / `JOP_FROM_DYN_*` where needed.

- **Fixed-width instructions (8 bytes)**:
  - `op,a,b,c,imm32` keeps decode simple and branch-light.
  - We can consider compression later if it’s worth the complexity.

- **“Wide” typed slots for 64-bit values**:
  - `I64`/`F64` occupy 8-byte slots with 8-byte alignment in the typed register frame layout.
  - The implementation uses `memcpy` loads/stores to avoid alignment UB.

- **Strict bytecode validation**:
  - Loader + semantic validator reject malformed modules early, reducing defensive checks in the hot loop.

## Baseline comparisons (what to measure against)

Comparisons should be framed around:

- **Interpreter throughput** (insns/sec) on tight loops and arithmetic kernels.
- **Allocation rate + GC overhead** (collections/sec, live bytes, pause time).
- **Dynamic boundary costs** (`ToDyn` / `FromDyn_*`, spill push/pop).

Reference points:

- **Lua 5.4**:
  - Register-based interpreter with a highly optimized dispatch loop.
  - Dynamic typing implies more runtime tagging and type checks than Jelly’s typed registers.

- **Python (CPython)**:
  - Stack-based VM with heavy object model overhead and reference counting.
  - Great baseline for “dynamic object” costs, but not directly comparable instruction-for-instruction.

## Current “known hot” areas

- **Dispatch loop** in `vm/src/vm/exec.c`
  - Minimize branches in per-op handlers.
  - Prefer straight-line typed loads/stores.

- **Object/map operations** (`Object` / atom keyed)
  - Hashing, probing, and rehash behavior under GC pressure.

- **Dynamic boundaries**
  - `JOP_TO_DYN`, `JOP_FROM_DYN_*`, spill push/pop.

## How we should benchmark (next steps)

Short-term (microbench-style):

- Tight loops: integer add/sub/mul, float add/mul, conversion kernels.
- Match lowering stress: `KINDOF` + `SWITCH_KIND` dispatch.
- Container kernels: `Array` get/set, `bytes` get/set, `List` head/tail.

Longer-term (macrobench-style):

- End-to-end bytecode programs (compiler-emitted), measured for wall time + allocations + GC.

## Notes / cautions

- Benchmark with and without sanitizers.
- Prefer `-O3` for performance runs, but keep CI at safer flags.
- Avoid conflating “fewer opcodes” with “faster”; measure decode/dispatch costs directly.
