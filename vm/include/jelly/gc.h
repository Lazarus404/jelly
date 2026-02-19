#ifndef JELLY_GC_H
#define JELLY_GC_H

/*
  Non-moving stop-the-world mark/sweep GC (MVP).

  The GC is precise: it traces from explicit VM roots:
  - active call frames (typed register scanning)
  - spill stack (boxed jelly_value)
  - temporary root stack (boxed jelly_value)
*/

#include <stddef.h>
#include <stdint.h>

#include "value.h"

struct jelly_vm;

void jelly_gc_init(struct jelly_vm* vm);
void jelly_gc_shutdown(struct jelly_vm* vm);

// Allocate GC-managed heap memory for an object.
void* jelly_gc_alloc(struct jelly_vm* vm, size_t size);

// Temporary roots (shadow stack) for values not yet stored in registers/heap.
void jelly_gc_push_root(struct jelly_vm* vm, jelly_value v);
void jelly_gc_pop_roots(struct jelly_vm* vm, uint32_t n);

// Force a collection (primarily for tests).
void jelly_gc_collect(struct jelly_vm* vm);

#endif /* JELLY_GC_H */

