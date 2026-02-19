#ifndef JELLY_TYPE_H
#define JELLY_TYPE_H

/*
  Typed VM type system (Hashlink-style).

  - Type IDs are module-local indices.
  - Per-function reg typing is `reg_types[reg_id] -> type_id`.
  - Typed registers/locals are NOT runtime-tagged per slot.
*/

#include <stdint.h>

typedef uint32_t jelly_type_id;

typedef enum jelly_type_kind {
  JELLY_T_I8 = 1,
  JELLY_T_I16 = 2,
  JELLY_T_I32 = 3,
  JELLY_T_I64 = 4,
  JELLY_T_F32 = 5,
  JELLY_T_F64 = 6,
  JELLY_T_BOOL = 7,
  JELLY_T_ATOM = 8,
  JELLY_T_BYTES = 9,
  JELLY_T_DYNAMIC = 10,   // boxed/tagged jelly_value
  JELLY_T_FUNCTION = 11,  // refers to a signature entry
  JELLY_T_LIST = 12,      // List<T>
  JELLY_T_ARRAY = 13,     // Array<T>
  JELLY_T_OBJECT = 14,    // Map/Object (atom -> value for MVP)
  JELLY_T_ABSTRACT = 15,  // Abstract(id)
} jelly_type_kind;

typedef struct jelly_type_entry {
  jelly_type_kind kind;
  union {
    struct {
      jelly_type_id elem;
    } unary; // LIST/ARRAY

    struct {
      uint32_t sig_id; // index into signature table
    } fun;

    struct {
      uint32_t abstract_id; // user-defined abstract ID (module-local)
    } abs;
  } as;
} jelly_type_entry;

// Function signatures live in a separate table for compactness.
// (This mirrors the idea that function types are structural and shareable.)
typedef struct jelly_fun_sig {
  jelly_type_id ret;
  uint16_t nargs;
  // followed by nargs * jelly_type_id in serialized form; in-memory can point to array
  const jelly_type_id* args;
} jelly_fun_sig;

#endif /* JELLY_TYPE_H */

