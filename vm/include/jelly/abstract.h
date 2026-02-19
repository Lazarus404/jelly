#ifndef JELLY_ABSTRACT_H
#define JELLY_ABSTRACT_H

/*
  Abstract runtime representation (MVP).

  An Abstract is a heap object with a user/module-defined abstract_id encoded in
  its type entry (JELLY_T_ABSTRACT). The payload pointer is opaque to the VM.
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef void (*jelly_abstract_finalizer)(void* payload);

typedef struct jelly_abstract {
  jelly_obj_header h; // kind=JELLY_OBJ_ABSTRACT
  void* payload;
  jelly_abstract_finalizer finalizer; // may be NULL
} jelly_abstract;

jelly_abstract* jelly_abstract_new(struct jelly_vm* vm, uint32_t type_id, void* payload);
jelly_abstract* jelly_abstract_new_finalized(struct jelly_vm* vm, uint32_t type_id, void* payload, jelly_abstract_finalizer finalizer);

#endif /* JELLY_ABSTRACT_H */

