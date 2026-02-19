## IR in `jellyc` (current state)

`jellyc` has a real typed, block-structured IR (`jellyc/src/ir.rs`) that sits between the AST frontend and bytecode emission (`jellyc/src/ir_codegen.rs`).

### Virtual instructions (`VInsn`)

In `jellyc/src/jlyb.rs`, codegen produces:

- a list of typed virtual registers (vregs)
- a linear stream of virtual instructions `VInsn { op, a, b, c, imm }`

Each vreg has a **static type id** (`vtypes[vreg] -> type_id`) that corresponds to the module type table entry.

This IR stays close to VM bytecode:

- most `op` values correspond 1:1 to Jelly VM opcodes
- jump and exception handler targets are stored as relative deltas in `imm`
- some late-lowering steps (like call argument placement) are deferred to the final emission phase

### Typing invariant

The compiler enforces the VM invariant that **a physical register never changes type** by:

- tracking a type id for every vreg
- running a type-aware register allocator
- emitting a `reg_types` table for each function

### Join points and “multi-def”

Most vregs are treated as single-definition values.

For control-flow joins that need a merged value (e.g. `if` / `try`), the compiler creates a destination vreg and explicitly marks it as allowing multi-def (implemented today as multiple `MOV` writes into the same destination vreg across branches).

This is a transitional approach until a real SSA-based IR + phi insertion/elimination is introduced.

### Calls and contiguous argument windows

The VM requires `CALLR` arguments to live in a contiguous physical register range `imm..imm+nargs`.

In the IR backend:

- lowering constructs a contiguous vreg window `[arg_base .. arg_base+nargs)` for each call
- emission pins those vregs to a per-signature contiguous “arg block” in the caller’s register file
- `CALLR` is emitted directly (no per-call instruction expansion), so jump/try deltas stay stable

### Register allocation

`jellyc/src/regalloc.rs` implements a linear-scan allocator over the virtual instruction stream, using per-instruction use/def sets recorded during codegen.

Allocation is **type-aware**:

- vregs are grouped by type id
- each type group is allocated into a disjoint physical register range
- this ensures no physical register is reused across different static types

### Planned evolution

The long-term direction is a real, block-structured typed IR with explicit CFG and explicit dynamic boundaries (`ToDyn` / `FromDyn_*`), with SSA joins handled via phi insertion and elimination. This document will be updated as that IR is introduced.
