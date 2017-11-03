/**
 * Copyright 2017 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef JELLY_H
#define JELLY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <inttypes.h>
#include <stdarg.h>

// `char16_t`/`char32_t` are normally provided by `<uchar.h>` in C11, but some
// libcs/SDKs (notably some Apple SDK configurations) don't ship that header.
#if !defined(__cplusplus)
#  if defined(__CHAR16_TYPE__) && defined(__CHAR32_TYPE__)
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
#  else
typedef uint16_t char16_t;
typedef uint32_t char32_t;
#  endif
#endif

#define JELLY_VERSION 0x011000u
#define JELLY_WSIZE ((uint32_t)sizeof(void*))
#define JELLY_IS_64 ((UINTPTR_MAX) > 0xffffffffu)
#define JELLY_PTR_FMT "%" PRIxPTR
#define JELLY_THREAD_LOCAL _Thread_local

#ifndef JELLY_API
#define JELLY_API extern
#endif

typedef intptr_t int_val;
typedef int64_t int64;
typedef uint64_t uint64;
typedef char16_t uchar;
#define USTR(str) u##str

/* --- type.h --- */
typedef uint32_t jelly_type_id;

typedef enum jelly_type_kind {
  /* Keep in sync with `jellyc/src/jlyb/format.rs` (TypeKind). */
  JELLY_T_BOOL = 1,
  JELLY_T_ATOM = 2,
  JELLY_T_I8 = 3,
  JELLY_T_I16 = 4,
  JELLY_T_I32 = 5,
  JELLY_T_I64 = 6,
  JELLY_T_F16 = 7,
  JELLY_T_F32 = 8,
  JELLY_T_F64 = 9,
  JELLY_T_BYTES = 10,
  JELLY_T_LIST = 11,
  JELLY_T_ARRAY = 12,
  JELLY_T_OBJECT = 13,
  JELLY_T_FUNCTION = 14,
  JELLY_T_ABSTRACT = 15,
  JELLY_T_DYNAMIC = 16,
} jelly_type_kind;

typedef struct jelly_type_entry {
  jelly_type_kind kind;
  union {
    struct { jelly_type_id elem; } unary;
    struct { uint32_t sig_id; } fun;
    struct { uint32_t abstract_id; } abs;
  } as;
} jelly_type_entry;

typedef struct jelly_fun_sig {
  jelly_type_id ret;
  uint16_t nargs;
  const jelly_type_id* args;
} jelly_fun_sig;

/* --- value.h --- */
typedef uintptr_t jelly_value;

enum {
  JELLY_TAG_PTR  = 0x0u,
  JELLY_TAG_I32  = 0x1u,
  JELLY_TAG_ATOM = 0x2u,
  JELLY_TAG_BOOL = 0x3u,
  JELLY_TAG_NULL = 0x4u,
};

static inline jelly_value jelly_make_null(void) {
  return (jelly_value)JELLY_TAG_NULL;
}
static inline int jelly_is_null(jelly_value v) {
  return (unsigned)(v & 0x7u) == JELLY_TAG_NULL;
}
static inline jelly_value jelly_make_bool(int b) {
  return ((jelly_value)((b != 0) ? 1u : 0u) << 3) | (jelly_value)JELLY_TAG_BOOL;
}
static inline int jelly_is_bool(jelly_value v) {
  return (unsigned)(v & 0x7u) == JELLY_TAG_BOOL;
}
static inline int jelly_as_bool(jelly_value v) {
  return (int)((v >> 3) & 1u);
}
static inline jelly_value jelly_make_atom(uint32_t atom_id) {
  return ((jelly_value)atom_id << 3) | (jelly_value)JELLY_TAG_ATOM;
}
static inline int jelly_is_atom(jelly_value v) {
  return (unsigned)(v & 0x7u) == JELLY_TAG_ATOM;
}
static inline uint32_t jelly_as_atom(jelly_value v) {
  return (uint32_t)(v >> 3);
}
static inline jelly_value jelly_make_i32(int32_t x) {
  return ((jelly_value)(uintptr_t)(uint32_t)x << 3) | (jelly_value)JELLY_TAG_I32;
}
static inline int jelly_is_i32(jelly_value v) {
  return (unsigned)(v & 0x7u) == JELLY_TAG_I32;
}
static inline int32_t jelly_as_i32(jelly_value v) {
  return (int32_t)(uint32_t)(v >> 3);
}
static inline int jelly_is_ptr(jelly_value v) {
  return (unsigned)(v & 0x7u) == JELLY_TAG_PTR;
}
static inline void* jelly_as_ptr(jelly_value v) {
  return (void*)(uintptr_t)v;
}
static inline jelly_value jelly_from_ptr(void* p) {
  return (jelly_value)(uintptr_t)p;
}

typedef enum jelly_obj_kind {
  JELLY_OBJ_BYTES = 1,
  JELLY_OBJ_FUNCTION = 2,
  JELLY_OBJ_LIST = 3,
  JELLY_OBJ_ARRAY = 4,
  JELLY_OBJ_OBJECT = 5,
  JELLY_OBJ_ABSTRACT = 6,
  JELLY_OBJ_BOX_I64 = 7,
  JELLY_OBJ_BOX_F64 = 8,
  JELLY_OBJ_BOX_F32 = 9,
  JELLY_OBJ_BOX_F16 = 10,
} jelly_obj_kind;

typedef struct jelly_obj_header {
  uint32_t kind;
  uint32_t type_id;
} jelly_obj_header;

static inline const jelly_obj_header* jelly_obj_header_of(jelly_value v) {
  return (const jelly_obj_header*)jelly_as_ptr(v);
}
static inline uint32_t jelly_obj_kind_of(jelly_value v) {
  return jelly_obj_header_of(v)->kind;
}

/* --- bytecode.h --- */
#define JELLY_BC_MAGIC 0x4A4C5942u

typedef enum jelly_bc_feature_flags {
  JELLY_BC_FEAT_NONE = 0,
  JELLY_BC_FEAT_CONST64 = 1u << 0,
  JELLY_BC_FEAT_CONSTBYTES = 1u << 1,
  JELLY_BC_FEAT_CAP_START = 1u << 2,
} jelly_bc_feature_flags;

typedef enum jelly_op {
  JOP_NOP = 0,
  /* Control / misc */
  JOP_RET = 1,

  /* Moves */
  JOP_MOV = 2,

  /* Calls / closures */
  JOP_CALL = 3,
  JOP_CALLR = 4,
  JOP_CONST_FUN = 5,
  JOP_CLOSURE = 6,
  JOP_BIND_THIS = 7,

  /* Typed constants */
  JOP_CONST_I32 = 8,
  JOP_CONST_I8_IMM = 9,
  JOP_CONST_BOOL = 10,
  JOP_CONST_NULL = 11,
  JOP_CONST_ATOM = 12,
  JOP_CONST_F16 = 13,
  JOP_CONST_F32 = 14,
  JOP_CONST_I64 = 15,
  JOP_CONST_F64 = 16,
  JOP_CONST_BYTES = 17,

  /* Bytes helpers */
  JOP_BYTES_CONCAT2 = 18,
  JOP_BYTES_CONCAT_MANY = 19,

  /* Control flow */
  JOP_JMP = 20,
  JOP_JMP_IF = 21,

  /* Exceptions */
  JOP_TRY = 22,
  JOP_ENDTRY = 23,
  JOP_THROW = 24,

  /* Debug */
  JOP_ASSERT = 25,

  /* Arithmetic (ints) */
  JOP_ADD_I32 = 26,
  JOP_SUB_I32 = 27,
  JOP_MUL_I32 = 28,
  JOP_DIV_I32 = 29,
  JOP_ADD_I32_IMM = 30,
  JOP_SUB_I32_IMM = 31,
  JOP_MUL_I32_IMM = 32,
  JOP_ADD_I64 = 33,
  JOP_SUB_I64 = 34,
  JOP_MUL_I64 = 35,
  JOP_DIV_I64 = 36,

  /* Arithmetic (floats) */
  JOP_ADD_F16 = 37,
  JOP_SUB_F16 = 38,
  JOP_MUL_F16 = 39,
  JOP_ADD_F32 = 40,
  JOP_SUB_F32 = 41,
  JOP_MUL_F32 = 42,
  JOP_DIV_F32 = 43,
  JOP_ADD_F64 = 44,
  JOP_SUB_F64 = 45,
  JOP_MUL_F64 = 46,
  JOP_DIV_F64 = 47,

  /* Unary */
  JOP_NEG_I32 = 48,
  JOP_NEG_I64 = 49,
  JOP_NEG_F32 = 50,
  JOP_NEG_F64 = 51,
  JOP_NOT_BOOL = 52,

  /* Comparisons */
  JOP_EQ_I32 = 53,
  JOP_LT_I32 = 54,
  JOP_EQ_I32_IMM = 55,
  JOP_LT_I32_IMM = 56,
  JOP_EQ_I64 = 57,
  JOP_EQ_F32 = 58,
  JOP_EQ_F64 = 59,

  /* Boxing/unboxing boundary + spill */
  JOP_TO_DYN = 60,
  JOP_FROM_DYN_I8 = 61,
  JOP_FROM_DYN_I16 = 62,
  JOP_FROM_DYN_I32 = 63,
  JOP_FROM_DYN_I64 = 64,
  JOP_FROM_DYN_F16 = 65,
  JOP_FROM_DYN_F32 = 66,
  JOP_FROM_DYN_F64 = 67,
  JOP_FROM_DYN_BOOL = 68,
  JOP_FROM_DYN_ATOM = 69,
  JOP_FROM_DYN_PTR = 70,
  JOP_SPILL_PUSH = 71,
  JOP_SPILL_POP = 72,

  /* Conversions / width changes */
  JOP_SEXT_I64 = 73,
  JOP_I32_FROM_I64 = 74,
  JOP_F64_FROM_I32 = 75,
  JOP_I32_FROM_F64 = 76,
  JOP_F64_FROM_I64 = 77,
  JOP_I64_FROM_F64 = 78,
  JOP_F32_FROM_I32 = 79,
  JOP_I32_FROM_F32 = 80,
  JOP_F64_FROM_F32 = 81,
  JOP_F32_FROM_F64 = 82,
  JOP_F32_FROM_I64 = 83,
  JOP_I64_FROM_F32 = 84,
  JOP_SEXT_I16 = 85,
  JOP_TRUNC_I8 = 86,
  JOP_TRUNC_I16 = 87,
  JOP_F16_FROM_F32 = 88,
  JOP_F32_FROM_F16 = 89,
  JOP_F16_FROM_I32 = 90,
  JOP_I32_FROM_F16 = 91,

  /* Identity */
  JOP_PHYSEQ = 92,

  /* Match / type introspection */
  JOP_KINDOF = 93,
  JOP_SWITCH_KIND = 94,
  JOP_CASE_KIND = 95,

  /* Containers */
  JOP_LIST_NIL = 96,
  JOP_LIST_CONS = 97,
  JOP_LIST_HEAD = 98,
  JOP_LIST_TAIL = 99,
  JOP_LIST_IS_NIL = 100,
  JOP_ARRAY_NEW = 101,
  JOP_ARRAY_LEN = 102,
  JOP_ARRAY_GET = 103,
  JOP_ARRAY_SET = 104,
  JOP_BYTES_NEW = 105,
  JOP_BYTES_LEN = 106,
  JOP_BYTES_GET_U8 = 107,
  JOP_BYTES_SET_U8 = 108,
  JOP_OBJ_NEW = 109,
  JOP_OBJ_HAS_ATOM = 110,
  JOP_OBJ_GET_ATOM = 111,
  JOP_OBJ_SET_ATOM = 112,
  JOP_OBJ_GET = 113,
  JOP_OBJ_SET = 114,
  /* Comparisons (extended) */
  JOP_LT_I64 = 115,
  JOP_LT_F32 = 116,
  JOP_LT_F64 = 117,
} jelly_op;

#define JELLY_ATOM___PROTO__ 0u
#define JELLY_ATOM_INIT 1u

typedef struct jelly_insn {
  uint8_t op;
  uint8_t a;
  uint8_t b;
  uint8_t c;
  uint32_t imm;
} jelly_insn;

typedef struct jelly_bc_function {
  uint32_t nregs;
  uint32_t cap_start; /* first capture slot; used when JELLY_BC_FEAT_CAP_START */
  const jelly_type_id* reg_types;
  uint32_t ninsns;
  const jelly_insn* insns;
} jelly_bc_function;

struct jelly_bc_module;
typedef struct jelly_bc_module jelly_bc_module;

/* --- loader.h --- */
typedef enum jelly_bc_error {
  JELLY_BC_OK = 0,
  JELLY_BC_EOF,
  JELLY_BC_BAD_MAGIC,
  JELLY_BC_UNSUPPORTED_VERSION,
  JELLY_BC_BAD_FORMAT,
  JELLY_BC_OUT_OF_MEMORY,
} jelly_bc_error;

typedef struct jelly_bc_result {
  jelly_bc_error err;
  const char* msg;
  size_t offset;
} jelly_bc_result;

jelly_bc_result jelly_bc_read(const uint8_t* data, size_t size, jelly_bc_module** out);
void jelly_bc_free(jelly_bc_module* m);

/* --- vm.h --- */
typedef enum jelly_exec_status {
  JELLY_EXEC_OK = 0,
  JELLY_EXEC_TRAP = 1,
} jelly_exec_status;

typedef enum jelly_trap_code {
  JELLY_TRAP_NONE = 0,
  JELLY_TRAP_TYPE_MISMATCH = 1,
  JELLY_TRAP_BOUNDS = 2,
  JELLY_TRAP_NULL_DEREF = 3,
  JELLY_TRAP_THROWN = 4,
  JELLY_TRAP_STACK_OVERFLOW = 5,
} jelly_trap_code;

struct jelly_vm;
typedef struct jelly_vm jelly_vm;

jelly_vm* jelly_vm_create(void);
void jelly_vm_destroy(jelly_vm* vm);
size_t jelly_slot_align(jelly_type_kind k);
size_t jelly_slot_size(jelly_type_kind k);
jelly_value jelly_vm_exec(jelly_vm* vm, const jelly_bc_module* m);
jelly_exec_status jelly_vm_exec_status(jelly_vm* vm, const jelly_bc_module* m, jelly_value* out);
void jelly_vm_clear_trap(jelly_vm* vm);
jelly_trap_code jelly_vm_last_trap_code(const jelly_vm* vm);
const char* jelly_vm_last_trap_msg(const jelly_vm* vm);

/* --- list.h --- */
typedef struct jelly_list {
  jelly_obj_header h;
  jelly_value head;
  struct jelly_list* tail;
} jelly_list;

jelly_list* jelly_list_cons(struct jelly_vm* vm, uint32_t type_id, jelly_value head, jelly_list* tail);

/* --- array.h --- */
typedef struct jelly_array {
  jelly_obj_header h;
  uint32_t length;
  jelly_value* data;
} jelly_array;

jelly_array* jelly_array_new(struct jelly_vm* vm, uint32_t type_id, uint32_t length);

/* --- bytes.h --- */
typedef struct jelly_bytes {
  jelly_obj_header h;
  uint32_t length;
  uint8_t data[];
} jelly_bytes;

jelly_bytes* jelly_bytes_new(struct jelly_vm* vm, uint32_t type_id, uint32_t length);

/* --- object.h --- */
typedef struct jelly_object {
  jelly_obj_header h;
  struct jelly_object* proto;
  uint32_t cap;
  uint32_t len;
  uint32_t* keys;
  jelly_value* vals;
  uint8_t* states;
} jelly_object;

jelly_object* jelly_object_new(struct jelly_vm* vm, uint32_t type_id);
int jelly_object_has(jelly_object* o, uint32_t atom_id);
jelly_value jelly_object_get(jelly_object* o, uint32_t atom_id);
void jelly_object_set(jelly_object* o, uint32_t atom_id, jelly_value v);

/* --- function.h --- */
typedef struct jelly_function {
  jelly_obj_header h;
  uint32_t func_index;
  uint32_t ncaps;
  jelly_value bound_this;
  jelly_value caps[];
} jelly_function;

jelly_function* jelly_function_new(struct jelly_vm* vm, uint32_t type_id, uint32_t func_index);
jelly_function* jelly_closure_new(struct jelly_vm* vm, uint32_t type_id, uint32_t func_index, uint32_t ncaps, const jelly_value* caps);
jelly_function* jelly_function_bind_this(struct jelly_vm* vm, uint32_t type_id, const jelly_function* f, jelly_value bound_this);

/* --- abstract.h --- */
typedef void (*jelly_abstract_finalizer)(void* payload);

typedef struct jelly_abstract {
  jelly_obj_header h;
  void* payload;
  jelly_abstract_finalizer finalizer;
} jelly_abstract;

jelly_abstract* jelly_abstract_new(struct jelly_vm* vm, uint32_t type_id, void* payload);
jelly_abstract* jelly_abstract_new_finalized(struct jelly_vm* vm, uint32_t type_id, void* payload, jelly_abstract_finalizer finalizer);

/* --- box.h --- */
typedef struct jelly_box_i64 {
  jelly_obj_header h;
  int64_t value;
} jelly_box_i64;

typedef struct jelly_box_f64 {
  jelly_obj_header h;
  double value;
} jelly_box_f64;

typedef struct jelly_box_f32 {
  jelly_obj_header h;
  float value;
} jelly_box_f32;

typedef struct jelly_box_f16 {
  jelly_obj_header h;
  uint16_t value; /* IEEE 754 binary16 bits */
} jelly_box_f16;

jelly_box_i64* jelly_box_i64_new(struct jelly_vm* vm, uint32_t type_id, int64_t v);
jelly_box_f64* jelly_box_f64_new(struct jelly_vm* vm, uint32_t type_id, double v);
jelly_box_f32* jelly_box_f32_new(struct jelly_vm* vm, uint32_t type_id, float v);
jelly_box_f16* jelly_box_f16_new(struct jelly_vm* vm, uint32_t type_id, uint16_t bits);

static inline int jelly_is_box_i64(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_I64;
}
static inline int jelly_is_box_f64(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_F64;
}
static inline int jelly_is_box_f32(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_F32;
}
static inline int64_t jelly_as_box_i64(jelly_value v) {
  return ((const jelly_box_i64*)jelly_as_ptr(v))->value;
}
static inline double jelly_as_box_f64(jelly_value v) {
  return ((const jelly_box_f64*)jelly_as_ptr(v))->value;
}
static inline float jelly_as_box_f32(jelly_value v) {
  return ((const jelly_box_f32*)jelly_as_ptr(v))->value;
}
static inline int jelly_is_box_f16(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_F16;
}
static inline uint16_t jelly_as_box_f16(jelly_value v) {
  return ((const jelly_box_f16*)jelly_as_ptr(v))->value;
}

/* --- gc.h --- */
void jelly_gc_init(struct jelly_vm* vm);
void jelly_gc_shutdown(struct jelly_vm* vm);
void* jelly_gc_alloc(struct jelly_vm* vm, size_t size);
void jelly_gc_push_root(struct jelly_vm* vm, jelly_value v);
void jelly_gc_pop_roots(struct jelly_vm* vm, uint32_t n);
void jelly_gc_collect(struct jelly_vm* vm);

/* --- UTF-16 helpers --- */
JELLY_API double utod(const uchar* str, uchar** end);
JELLY_API int utoi(const uchar* str, uchar** end);
JELLY_API int ustrlen(const uchar* str);
JELLY_API uchar* ustrdup(const uchar* str);
JELLY_API int ucmp(const uchar* a, const uchar* b);
JELLY_API int utostr(char* out, int out_size, const uchar* str);
JELLY_API int usprintf(uchar* out, int out_size, const uchar* fmt, ...);
JELLY_API int uvszprintf(uchar* out, int out_size, const uchar* fmt, va_list arglist);
JELLY_API void uprintf(const uchar* fmt, const uchar* str);

/* --- Debug/attributes --- */
#ifdef JELLY_DEBUG
#define jelly_debug_break() abort()
#else
#define jelly_debug_break() ((void)0)
#endif

#define JELLY_NO_RETURN(decl) noreturn decl
#define JELLY_UNREACHABLE() abort()

#endif /* JELLY_H */
