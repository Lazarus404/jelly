# Small Numeric Types: I8, I16, F16

Jelly supports narrow numeric types **I8**, **I16**, and **F16** alongside the standard I32, I64, F32, and F64. These types enable smaller constants and reduced register pressure while keeping the VM and compiler simple.

## Storage Model

All small types use **32-bit slots** in the register frame, consistent with I32 and F32:

| Type | Slot size | Storage                                       | Range / semantics |
| ---- | --------- | --------------------------------------------- | ----------------- |
| I8   | 4 bytes   | Low 8 bits, sign-extended on load             | -128..127         |
| I16  | 4 bytes   | Low 16 bits, sign-extended on load            | -32768..32767     |
| I32  | 4 bytes   | Full 32 bits                                  | -2^31..2^31-1     |
| I64  | 8 bytes   | Full 64 bits                                  | (unchanged)       |
| F16  | 4 bytes   | IEEE 754 binary16 in low 16 bits, zero-padded | ±6e-8..65504      |
| F32  | 4 bytes   | IEEE 754 binary32                             | (unchanged)       |
| F64  | 8 bytes   | IEEE 754 binary64                             | (unchanged)       |

The VM loads I8/I16 via `vm_load_u32`, masks to 8/16 bits, and sign-extends when needed. Stores truncate and mask before write. F16 is stored as `uint16_t` in the low half of the slot; arithmetic converts to F32, computes, and converts back.

## Literals

Numeric literals can carry type suffixes:

- **`42i8`** — I8 literal (must be in -128..127)
- **`1000i16`** — I16 literal (must be in -32768..32767)
- **`0.5f16`** — F16 literal (IEEE 754 half-precision)

Unannotated integer literals (`42`) are inferred from context when assigned to an I8 or I16 variable. Unannotated float literals (`0.5`) in F16 context become F16.

## Numeric Promotion

Binary arithmetic promotes operands to a common type:

- **Integers:** I8 < I16 < I32 < I64
- **Floats:** F16 < F32 < F64

For example, `i8 + i16` promotes to I16; `i8 + i32` promotes to I32. Assigning a wider type to a narrower one (e.g. I32 → I8) inserts an explicit truncation.

## Conversions

| From | To            | Operation                          |
| ---- | -------------- | ----------------------------------- |
| I8   | I16, I32, I64  | Sign-extend                         |
| I16  | I8             | Truncate                            |
| I16  | I32, I64       | Sign-extend                         |
| I32  | I8, I16        | Truncate                            |
| I32  | I64            | Sign-extend                         |
| F16  | F32, F64       | Expand (software conversion)        |
| F32  | F16            | Narrow (may lose precision)         |
| F32  | F64            | Expand                              |
| F64  | F16, F32       | Narrow                              |

## Boxing and Dynamic

I8, I16, and F16 box into `Dynamic` like I32: they become tagged immediates or boxed heap objects as needed. Unboxing uses `FROM_DYN_I8`, `FROM_DYN_I16`, and `FROM_DYN_F16` with runtime type checks.

## Immediate-in-Opcode

When the second operand of an arithmetic or comparison op is a small constant in range -128..127, the compiler may emit an immediate form (e.g. `ADD_I32_IMM`) that encodes the value in the instruction, avoiding a register load.
