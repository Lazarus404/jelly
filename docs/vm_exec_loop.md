# `vm_exec_loop`: interpreter loop and correctness

This document explains the VM’s main execution loop implementation (`vm_exec_loop`) and the correctness invariants it must preserve. Performance improvements are only acceptable when they preserve these invariants and remain testable against a reference path.

## Background: where dispatch overhead comes from

The VM executes a classic fetch–decode–execute loop. A common performance pitfall is paying _extra_ C-level overhead per bytecode instruction (e.g., an additional function call layer) that does not contribute to language semantics.

`vm_exec_loop` exists to:

- Centralize the interpreter loop in one place.
- Allow a small set of very common opcodes to be handled inline (avoiding an extra call layer).
- Preserve a canonical reference implementation for all opcodes.

## Design: hot-path inlining with a canonical fallback

Implementation: `vm/src/vm/ops/exec_loop.c`

- The loop fetches `insns[pc]`, increments `pc`, then executes the opcode.
- A subset of frequently executed opcodes is implemented inline in `vm_exec_loop`.
- All other opcodes fall back to the existing canonical dispatcher (`op_dispatch`), which remains the authoritative implementation.

This is not “benchmark-specific specialization”: the selection is based on VM-wide frequency (const/mov/jumps/int arith/call/ret) and applies to any program.

## Correctness invariants

`vm_exec_loop` must preserve:

- **Single-step semantics**: each instruction is fetched from the current top frame at `pc`, then `pc` is advanced exactly once (unless a control-flow instruction modifies it).
- **Stack discipline**:
  - `CALL` pushes a new `call_frame` and transfers control to callee `pc=0`.
  - `RET` pops the current frame and returns to the caller with the value placed into the caller destination register (with boxing/unboxing only when required by types).
- **Typed-register semantics**:
  - Registers are unboxed and typed by `reg_types[]`.
  - Stores must respect the register’s type kind (e.g. masking/truncation for small integer types).
- **Trap/exception semantics**:
  - When an opcode traps/throws, the VM sets `vm->exc_pending` and `vm_exc_dispatch` decides whether execution continues (caught) or unwinds to a trap return.
  - The interpreter must not “swallow” pending exceptions; it must re-check and dispatch consistently.
- **GC visibility**:
  - All heap references remain reachable through existing roots (frames/spill/const caches/etc.). `vm_exec_loop` must not bypass root publication or change object lifetimes.

## Reference mode (conformance / debugging)

The VM supports a build-time reference mode that disables hot-op inlining in `vm_exec_loop`:

- CMake option: `JELLYVM_REFERENCE_INTERP=ON`
- Effect: `vm_exec_loop` executes _all_ opcodes via `op_dispatch` (canonical path), preserving minimal duplicated semantics.

This is intended for:

- Debugging subtle interpreter correctness issues.
- Comparing results between “fast” and “reference” configurations on the same bytecode corpus.

Note: reference mode is expected to be **significantly slower**. That slowdown is the point: it provides a semantics-first configuration that reduces duplication and makes it easier to bisect correctness issues.

## Validation strategy

Recommended checks when changing interpreter/dispatch behavior:

- Run the full test suite in both configurations:
  - `JELLYVM_REFERENCE_INTERP=ON`
  - `JELLYVM_REFERENCE_INTERP=OFF`
- Run representative programs that stress:
  - control flow (`JMP`, `JMP_IF`)
  - exceptions (`TRY/ENDTRY/THROW`)
  - dynamic boundaries (`TO_DYN` / `FROM_DYN_*`)
  - container mutation ops (array/bytes/object set)

The intent is to keep a “known correct” baseline and ensure optimizations don’t change observable behavior.
