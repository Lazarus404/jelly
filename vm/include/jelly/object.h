#ifndef JELLY_OBJECT_H
#define JELLY_OBJECT_H

/*
  Object/map runtime representation (MVP).

  - Keys are atoms (u32 IDs).
  - Values are boxed `jelly_value` for polymorphism.
  - Uses open addressing with linear probing.
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef struct jelly_object {
  jelly_obj_header h;   // kind=JELLY_OBJ_OBJECT
  // Fast-path prototype link (optional).
  //
  // Semantics are still defined in terms of the `__proto__` atom property; this is a cache that
  // is maintained by the interpreter only when a module reserves atom 0 for "__proto__".
  struct jelly_object* proto;
  uint32_t cap;         // power-of-two
  uint32_t len;
  uint32_t* keys;       // length cap
  jelly_value* vals;    // length cap
  uint8_t* states;      // 0=empty,1=full,2=tomb
} jelly_object;

jelly_object* jelly_object_new(struct jelly_vm* vm, uint32_t type_id);

int jelly_object_has(jelly_object* o, uint32_t atom_id);
jelly_value jelly_object_get(jelly_object* o, uint32_t atom_id); // returns null if missing
void jelly_object_set(jelly_object* o, uint32_t atom_id, jelly_value v);

#endif /* JELLY_OBJECT_H */

