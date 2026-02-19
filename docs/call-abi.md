## Call ABI (current `jellyc` / Jelly bytecode convention)

This document captures the calling convention as it exists today in the VM bytecode format and as used by `jellyc`.

### `CALLR` instruction shape

`CALLR` is encoded with:

- **`a`**: destination register for the return value
- **`b`**: callee register (must have a `Function` static type)
- **`c`**: `nargs` (0–255)
- **`imm`**: `arg_start` (the first register of the contiguous argument window)

The callee reads its arguments from physical registers:

- `arg_start + 0`
- `arg_start + 1`
- ...
- `arg_start + (nargs - 1)`

### Parameter registers in the callee

In the callee function body, parameter registers are:

- physical register indices `0..nargs-1`
- typed by the callee’s `reg_types` table

For compiler-emitted function literals (MVP), `jellyc` builds the function with:

- `reg_types[0..nargs-1] = parameter types`
- additional registers appended as needed for temporaries

### Return values

The return value is written into the caller’s destination register `a`.

### How `jellyc` satisfies the contiguous arg window

Today `jellyc` satisfies the contiguous arg window by **pinning** a call’s argument vregs to a
per-signature contiguous “arg block” in the caller’s physical register file:

- lowering creates a contiguous vreg window `[arg_base .. arg_base+nargs)` for each call
- during emission, those vregs are assigned to the signature’s arg block registers
- `CALLR` is emitted with `imm = arg_block_start`

This preserves the VM’s contiguous-args convention without inserting extra `MOV*` at each callsite.

### Not implemented yet

- `this` binding / method call lowering
- closures and capture environments
- varargs / rest parameters
