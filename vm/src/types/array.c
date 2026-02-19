#include <jelly/array.h>
#include <jelly/gc.h>

#include <stdlib.h>
#include <string.h>

jelly_array* jelly_array_new(struct jelly_vm* vm, uint32_t type_id, uint32_t length) {
  jelly_array* a = (jelly_array*)jelly_gc_alloc(vm, sizeof(jelly_array));
  a->h.kind = (uint32_t)JELLY_OBJ_ARRAY;
  a->h.type_id = type_id;
  a->length = length;
  if(length == 0) {
    a->data = NULL;
  } else {
    a->data = (jelly_value*)malloc(sizeof(jelly_value) * (size_t)length);
    if(!a->data) abort();
    // Initialize to null (boxed).
    for(uint32_t i = 0; i < length; i++) a->data[i] = jelly_make_null();
  }
  return a;
}

