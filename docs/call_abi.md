## Jelly call ABI and capture layout (compiler ↔ VM contract)

This document describes the **register layout invariants** that `jellyc` must respect when emitting bytecode for the Jelly VM.

### Terms

- **vreg**: IR virtual register (`IrFunction.vreg_types` index)
- **preg**: bytecode physical register (`reg_types[]` index)
- **`reg_types[]`**: per-function static register type table used by the VM validator and GC

### Parameter registers (callee)

- The callee’s parameters live in **pregs `0..param_count`**.
- Their types are fixed and must match `reg_types[0..param_count]`.

### Call argument windows (caller)

The ISA uses **contiguous argument windows**:

- `CALL a b c imm` uses args from `rB..r(B+c-1)`
- `CALLR a b c imm` uses args from `rImm..r(imm+c-1)`

In IR, calls carry:

- `arg_base`: base **vreg** of a contiguous vreg window `[arg_base .. arg_base+nargs)`
- `sig_id`: signature describing each arg type

In codegen, `jellyc` allocates one contiguous physical **arg block** per signature and _pins_ each call’s argument window vregs to the corresponding physical block.

### Closures and capture registers (callee)

The ISA closure constructor:

- `CLOSURE a b c imm` captures `rB..r(B+c-1)` into a function object.

On `CALLR`, the VM copies captures into the callee’s register file:

- If the module enables **`CAP_START`** (`JELLY_BC_FEAT_CAP_START`), the VM copies captures starting at `callee.cap_start`.
- Otherwise it falls back to trailing placement: `cap_start = nregs - ncaps`.

`jellyc` therefore:

- Assigns dedicated capture slots for captured values (callee frame).
- Emits a correct per-function `cap_start` value when the feature is enabled.

### CAP_START feature bit

The module must enable `CAP_START` when captures exist, even if some functions have `cap_start == 0`
(e.g. a paramless function with captures).

### Spilling and pinned windows

- The spill stack is **boxed-only**: only `Dynamic` values are spillable.
- Any vreg pinned to an ABI window (params, call arg windows, closure capture windows, callee capture slots) must **never** be spilled.

### References

- `docs/ISA.md` (CALL/CALLR/CLOSURE semantics)
- `docs/vm_architecture.md` (typed regs and `reg_types[]`)
- `jellyc/src/codegen/phase2.rs` (pinning and invariants)
