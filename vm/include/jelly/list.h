#ifndef JELLY_LIST_H
#define JELLY_LIST_H

/*
  Immutable list (cons cell) runtime representation.

  - Designed for speed: structural sharing, O(1) head/tail.
  - Nodes are immutable once allocated.
  - Payload is boxed `jelly_value` for MVP (works with any element type).
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef struct jelly_list {
  jelly_obj_header h;      // kind=JELLY_OBJ_LIST
  jelly_value head;        // boxed payload
  struct jelly_list* tail; // NULL = empty list
} jelly_list;

// Allocate a new cons cell.
jelly_list* jelly_list_cons(struct jelly_vm* vm, uint32_t type_id, jelly_value head, jelly_list* tail);

#endif /* JELLY_LIST_H */

