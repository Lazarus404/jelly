#include <jelly/list.h>
#include <jelly/gc.h>

jelly_list* jelly_list_cons(struct jelly_vm* vm, uint32_t type_id, jelly_value head, jelly_list* tail) {
  jelly_list* n = (jelly_list*)jelly_gc_alloc(vm, sizeof(jelly_list));
  n->h.kind = (uint32_t)JELLY_OBJ_LIST;
  n->h.type_id = type_id;
  n->head = head;
  n->tail = tail;
  return n;
}

