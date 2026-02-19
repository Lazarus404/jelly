#ifndef JELLY_BYTES_H
#define JELLY_BYTES_H

/*
  Bytes runtime representation (MVP).

  - Heap object, self-identifying via header.kind = JELLY_OBJ_BYTES
  - Backing store is a flexible array (single allocation).
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef struct jelly_bytes {
  jelly_obj_header h; // kind=JELLY_OBJ_BYTES
  uint32_t length;
  uint8_t data[];
} jelly_bytes;

jelly_bytes* jelly_bytes_new(struct jelly_vm* vm, uint32_t type_id, uint32_t length);

#endif /* JELLY_BYTES_H */

