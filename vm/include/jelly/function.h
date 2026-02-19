#ifndef JELLY_FUNCTION_H
#define JELLY_FUNCTION_H

/*
  Function runtime representation (MVP).

  First-class functions + closures:
  - A function value is a heap object that points at a bytecode function index.
  - It may also carry captured values (boxed `jelly_value`), and an optional
    bound receiver ("this") that is passed as argument 0 on call.
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef struct jelly_function {
  jelly_obj_header h; // kind=JELLY_OBJ_FUNCTION
  uint32_t func_index;
  uint32_t ncaps;
  jelly_value bound_this; // null means unbound
  jelly_value caps[];     // length ncaps (inline, same allocation)
} jelly_function;

jelly_function* jelly_function_new(struct jelly_vm* vm, uint32_t type_id, uint32_t func_index);
jelly_function* jelly_closure_new(struct jelly_vm* vm, uint32_t type_id, uint32_t func_index, uint32_t ncaps, const jelly_value* caps);
jelly_function* jelly_function_bind_this(struct jelly_vm* vm, uint32_t type_id, const jelly_function* f, jelly_value bound_this);

#endif /* JELLY_FUNCTION_H */

