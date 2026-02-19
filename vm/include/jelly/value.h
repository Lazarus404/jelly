#ifndef JELLY_VALUE_H
#define JELLY_VALUE_H

/*
  Boxed/tagged runtime value (Jello-style reference).

  Typed registers/locals are generally *unboxed*; this `jelly_value` is for:
  - Dynamic / polymorphic storage
  - spill stack (MVP policy: spill stores boxed values only)
  - object references
  - ABI boundaries (builtins/FFI later)

  Design: portable low-bit tagging on uintptr_t.
  - immediates: small i32, atom id, bool, null
  - pointers: heap objects (self-identifying header)
  - wide numeric (i64/f64): boxed heap objects (keeps tagging simple + portable)
*/

#include <stdint.h>
#include <stddef.h>

typedef uintptr_t jelly_value;

// Low-bit tag scheme (requires heap pointers aligned to >= 8 bytes).
// We only rely on typical malloc alignment; keep it explicit in allocators later.
enum {
  JELLY_TAG_PTR  = 0x0u, // ...000
  JELLY_TAG_I32  = 0x1u, // ...001  (signed 32-bit immediate)
  JELLY_TAG_ATOM = 0x2u, // ...010  (u32 atom id immediate)
  JELLY_TAG_BOOL = 0x3u, // ...011  (0/1 payload)
  JELLY_TAG_NULL = 0x4u, // ...100  (no payload)
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
  // Store as sign-extended in the upper bits.
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

// --- Heap object headers ------------------------------------------------------

typedef enum jelly_obj_kind {
  JELLY_OBJ_BYTES = 1,
  JELLY_OBJ_FUNCTION = 2,
  JELLY_OBJ_LIST = 3,
  JELLY_OBJ_ARRAY = 4,
  JELLY_OBJ_OBJECT = 5,
  JELLY_OBJ_ABSTRACT = 6,
  // boxed wide numerics (for Dynamic):
  JELLY_OBJ_BOX_I64 = 7,
  JELLY_OBJ_BOX_F64 = 8,
  JELLY_OBJ_BOX_F32 = 9,
} jelly_obj_kind;

typedef struct jelly_obj_header {
  uint32_t kind;     // jelly_obj_kind
  uint32_t type_id;  // optional: module type id (0 if unknown/not applicable)
} jelly_obj_header;

static inline const jelly_obj_header* jelly_obj_header_of(jelly_value v) {
  return (const jelly_obj_header*)jelly_as_ptr(v);
}

static inline uint32_t jelly_obj_kind_of(jelly_value v) {
  return jelly_obj_header_of(v)->kind;
}

#endif /* JELLY_VALUE_H */

