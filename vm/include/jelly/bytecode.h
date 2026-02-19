#ifndef JELLY_BYTECODE_H
#define JELLY_BYTECODE_H

/*
  Minimal Jelly bytecode structures (register-based, statically typed).

  Encoding decisions will be finalized as we implement the loader, but the
  invariants are:
  - same bytecode on 32-bit and 64-bit
  - per-function reg typing (nregs <= 256, reg_types[nregs])
  - typed constant pools and/or typed literal opcodes
  - spill stack is boxed `jelly_value` only (compiler inserts ToDyn/FromDyn*)
*/

#include <stdint.h>
#include <stddef.h>

#include "type.h"

#define JELLY_BC_MAGIC 0x4A4C5942u /* 'JLYB' */

typedef enum jelly_bc_feature_flags {
  JELLY_BC_FEAT_NONE = 0,
  // If set, module header includes i64/f64 const pool counts and the module
  // includes const pool data (loaded before the function table).
  JELLY_BC_FEAT_CONST64 = 1u << 0,
  // If set, module header includes bytes const pool count and the module
  // includes bytes const pool data (loaded before the function table).
  JELLY_BC_FEAT_CONSTBYTES = 1u << 1,
  // reserved for future (e.g. debug info, compression, etc.)
} jelly_bc_feature_flags;

typedef enum jelly_op {
  // --- control / misc
  JOP_NOP = 0,
  JOP_RET = 1,          // ret rX

  // --- moves
  JOP_MOV = 2,          // mov rD, rS  (types must match)

  // --- calls (MVP)
  // Calling convention:
  // - callee's argument registers are r0..r(nargs-1)
  // - call-site provides args in a contiguous range in the caller frame
  JOP_CALL = 3,         // rA = call func_index=imm, args start=rB, nargs=rC
  JOP_CALLR = 4,        // rA = call rB(function), args start=imm, nargs=rC

  // --- typed consts
  JOP_CONST_I32 = 10,   // rD = i32 imm32
  JOP_CONST_BOOL = 11,  // rD = bool imm8
  JOP_CONST_ATOM = 12,  // rD = atom pool_index (u32)
  JOP_CONST_FUN = 13,   // rD:function = fun(func_index=imm)
  JOP_CONST_NULL = 96,  // rD:Dynamic = null
  JOP_CONST_F32 = 18,   // rD = f32 bitcast imm32
  JOP_CONST_I64 = 19,   // rD = i64 const_pool[imm]
  JOP_CONST_F64 = 45,   // rD = f64 const_pool[imm]
  JOP_CONST_BYTES = 57, // rD:bytes = bytes_const_pool[imm]

  // --- control flow (MVP)
  // Jumps use signed relative deltas in `imm`, relative to *next* pc (post-fetch).
  JOP_JMP = 14,         // pc += (int32)imm
  JOP_JMP_IF = 15,      // if(rA != 0) pc += (int32)imm

  // --- closures (MVP)
  // Captures are boxed at capture-time. At call-time, captures are copied into
  // the callee's trailing registers: [nregs - ncaps, ..., nregs-1].
  JOP_CLOSURE = 16,     // rA:function = closure(func_index=imm, caps start=rB, ncaps=rC)
  JOP_BIND_THIS = 17,   // rA:function = bind_this(rB:function, this=rC)

  // --- typed arithmetic (MVP)
  JOP_ADD_I32 = 20,     // rD = rA + rB
  JOP_SUB_I32 = 21,
  JOP_MUL_I32 = 22,
  JOP_ADD_F32 = 23,
  JOP_SUB_F32 = 24,
  JOP_MUL_F32 = 25,
  JOP_ADD_F64 = 26,
  JOP_SUB_F64 = 27,
  JOP_MUL_F64 = 28,
  // 64-bit integer arithmetic (wraps modulo 2^64)
  JOP_ADD_I64 = 104,    // rD = rA + rB
  JOP_SUB_I64 = 105,
  JOP_MUL_I64 = 106,

  // --- typed conversions (MVP)
  JOP_SEXT_I64 = 30,      // rA:I64 = sext_i32(rB:I32)
  JOP_I32_FROM_I64 = 31,  // rA:I32 = trunc_i64(rB:I64)
  JOP_F64_FROM_I32 = 32,  // rA:F64 = (double)rB:I32
  JOP_I32_FROM_F64 = 33,  // rA:I32 = (int32)rB:F64 (trunc toward 0)
  JOP_F64_FROM_I64 = 39,  // rA:F64 = (double)rB:I64
  JOP_I64_FROM_F64 = 44,  // rA:I64 = (int64)rB:F64 (trunc toward 0)
  JOP_F32_FROM_I32 = 35,  // rA:F32 = (float)rB:I32
  JOP_I32_FROM_F32 = 36,  // rA:I32 = (int32)rB:F32 (trunc toward 0)

  // --- compares (MVP)
  JOP_EQ_I32 = 34,      // rA:bool = (rB == rC) (i32)
  JOP_EQ_F32 = 37,      // rA:bool = (rB == rC) (f32)
  JOP_EQ_F64 = 38,      // rA:bool = (rB == rC) (f64)
  JOP_EQ_I64 = 107,     // rA:bool = (rB == rC) (i64)
  JOP_LT_I32 = 65,      // rA:bool = (rB < rC) (signed i32)

  // --- boxing boundaries
  JOP_TO_DYN = 40,      // rD:Dynamic = box(rS)
  JOP_FROM_DYN_I32 = 41,// rD:I32 = unbox_i32(rS:Dynamic) (checked)
  JOP_FROM_DYN_I64 = 42,// rD:I64 = unbox_i64(rS:Dynamic) (checked)
  JOP_FROM_DYN_F64 = 43,// rD:F64 = unbox_f64(rS:Dynamic) (checked)
  JOP_FROM_DYN_BOOL = 46,// rD:Bool = unbox_bool(rS:Dynamic) (checked)
  JOP_FROM_DYN_ATOM = 47,// rD:Atom = unbox_atom(rS:Dynamic) (checked)
  JOP_FROM_DYN_PTR = 48, // rD:PtrKind = unbox_ptrkind(rS:Dynamic) (checked; kind inferred from dst reg type)

  // --- spill (boxed value only)
  JOP_SPILL_PUSH = 50,  // push (Dynamic) rS onto spill stack
  JOP_SPILL_POP = 51,   // pop  into (Dynamic) rD

  // --- unary ops (typed)
  JOP_NEG_I32 = 52,     // rA:I32 = -rB:I32
  JOP_NEG_I64 = 53,     // rA:I64 = -rB:I64
  JOP_NEG_F32 = 54,     // rA:F32 = -rB:F32
  JOP_NEG_F64 = 55,     // rA:F64 = -rB:F64
  JOP_NOT_BOOL = 56,    // rA:Bool = !rB:Bool

  // --- exceptions (minimal VM-level facility)
  // TRY/ENDTRY manage a handler stack; THROW raises a payload (Dynamic).
  // Traps (bounds/null/type mismatch) are also raised through this facility when a handler exists.
  JOP_TRY = 58,         // try rA:Dynamic, catch_delta=imm (relative to next pc)
  JOP_ENDTRY = 59,      // endtry
  JOP_THROW = 64,       // throw rA:Dynamic

  // --- identity / match helpers
  JOP_PHYSEQ = 60,      // rA:bool = phys_eq(rB, rC)
  JOP_KINDOF = 61,      // rA:I32 = kindof(rB:Dynamic) (see ISA.md for codes)
  // Jump-table helper for match dispatch on KINDOF codes.
  // Semantics: reads `rA` (an I32 kind code) and jumps to the first matching case.
  // The next `rB` instructions must be `JOP_CASE_KIND` entries (data-only).
  JOP_SWITCH_KIND = 62, // switch_kind rA(kind), ncases=rB, default_delta=imm (relative to end of case table)
  JOP_CASE_KIND = 63,   // case_kind kind_u8=a, delta=imm (relative to end of case table)

  // --- immutable list (fast path)
  JOP_LIST_NIL = 70,    // rA:List<T> = nil
  JOP_LIST_CONS = 71,   // rA:List<T> = cons(rB:T, rC:List<T>)
  JOP_LIST_HEAD = 72,   // rA:T = head(rB:List<T>)
  JOP_LIST_TAIL = 73,   // rA:List<T> = tail(rB:List<T>)
  JOP_LIST_IS_NIL = 74, // rA:bool = is_nil(rB:List<T>)

  // --- Array<T> (MVP boxed backing store)
  JOP_ARRAY_NEW = 80,   // rA:Array<T> = new(len=rB:I32)
  JOP_ARRAY_LEN = 81,   // rA:I32 = len(rB:Array<T>)
  JOP_ARRAY_GET = 82,   // rA:T = rB[rC:I32]
  JOP_ARRAY_SET = 83,   // rB[rC:I32] = rA:T

  // --- bytes
  JOP_BYTES_NEW = 90,   // rA:bytes = new(len=rB:I32)
  JOP_BYTES_LEN = 91,   // rA:I32 = len(rB:bytes)
  JOP_BYTES_GET_U8 = 92,// rA:I32 = bytes_get_u8(rB:bytes, rC:I32)
  JOP_BYTES_SET_U8 = 93,// bytes_set_u8(rB:bytes, rC:I32, rA:I32)
  JOP_BYTES_CONCAT2 = 94, // rA:bytes = concat2(rB:bytes, rC:bytes)
  JOP_BYTES_CONCAT_MANY = 95, // rA:bytes = concat_many(rB:Array<bytes>)

  // --- object/map (atom -> value)
  JOP_OBJ_NEW = 100,       // rA:Object = new()
  JOP_OBJ_HAS_ATOM = 101,  // rA:bool = has(rB:Object, atom_id=imm)
  JOP_OBJ_GET_ATOM = 102,  // rA:T = get(rB:Object, atom_id=imm)
  JOP_OBJ_SET_ATOM = 103,  // set(rB:Object, atom_id=imm, rA:T)
  JOP_OBJ_GET = 108,       // rA:T = get(rB:Object, atom_id=rC:Atom)
  JOP_OBJ_SET = 109,       // set(rB:Object, atom_id=rC:Atom, rA:T)
} jelly_op;

// Reserved atom IDs (the compiler guarantees these live at stable indices
// at the start of every module's atom table).
#define JELLY_ATOM___PROTO__ 0u
#define JELLY_ATOM_INIT 1u

// A simple fixed-width instruction for the interpreter MVP.
// (Keeps decoding fast; we can compress later.)
typedef struct jelly_insn {
  uint8_t op;
  uint8_t a;   // usually dst
  uint8_t b;   // usually src1
  uint8_t c;   // usually src2 / small imm
  uint32_t imm;// imm32 / pool index (meaning depends on op)
} jelly_insn;

typedef struct jelly_bc_function {
  uint32_t nregs;               // <= 256
  const jelly_type_id* reg_types; // length nregs
  uint32_t ninsns;
  const jelly_insn* insns;
} jelly_bc_function;

typedef struct jelly_bc_module {
  uint32_t version;
  uint32_t features; // jelly_bc_feature_flags

  // types
  uint32_t ntypes;
  const jelly_type_entry* types;

  // function signatures
  uint32_t nsigs;
  const jelly_fun_sig* sigs;

  // atoms (interned)
  // Stored as UTF-8, NUL-terminated strings. IDs are stable 0..natoms-1.
  uint32_t natoms;
  const char* atoms_data;        // contiguous storage (owned by module)
  const char* const* atoms;      // length natoms; points into atoms_data (owned)

  // const pools (optional; see JELLY_BC_FEAT_CONST64)
  uint32_t nconst_i64;
  const int64_t* const_i64;
  uint32_t nconst_f64;
  const double* const_f64;

  // bytes const pool (optional; see JELLY_BC_FEAT_CONSTBYTES)
  uint32_t nconst_bytes;
  const uint32_t* const_bytes_len;  // length nconst_bytes
  const uint32_t* const_bytes_off;  // length nconst_bytes; offsets into const_bytes_data
  const uint8_t* const_bytes_data;  // contiguous bytes storage

  // functions
  uint32_t nfuncs;
  const jelly_bc_function* funcs;
  uint32_t entry; // index into funcs
} jelly_bc_module;

#endif /* JELLY_BYTECODE_H */

