#ifndef JELLY_ARRAY_H
#define JELLY_ARRAY_H

/*
  Array<T> runtime representation (MVP).

  - Backing store is boxed `jelly_value[]` for simplicity and correctness.
  - This supports any element type (typed regs box/unbox at the boundary).
  - Later we can add specialized numeric arrays without changing bytecode.
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef struct jelly_array {
  jelly_obj_header h;   // kind=JELLY_OBJ_ARRAY
  uint32_t length;
  jelly_value* data;    // length elements, boxed
} jelly_array;

jelly_array* jelly_array_new(struct jelly_vm* vm, uint32_t type_id, uint32_t length);

#endif /* JELLY_ARRAY_H */

