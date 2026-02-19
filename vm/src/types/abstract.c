#include <jelly/abstract.h>
#include <jelly/gc.h>

jelly_abstract* jelly_abstract_new(struct jelly_vm* vm, uint32_t type_id, void* payload) {
  return jelly_abstract_new_finalized(vm, type_id, payload, NULL);
}

jelly_abstract* jelly_abstract_new_finalized(struct jelly_vm* vm, uint32_t type_id, void* payload, jelly_abstract_finalizer finalizer) {
  jelly_abstract* a = (jelly_abstract*)jelly_gc_alloc(vm, sizeof(jelly_abstract));
  a->h.kind = (uint32_t)JELLY_OBJ_ABSTRACT;
  a->h.type_id = type_id;
  a->payload = payload;
  a->finalizer = finalizer;
  return a;
}

