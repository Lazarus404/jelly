#include <jelly/bytes.h>
#include <jelly/gc.h>

#include <string.h>

jelly_bytes* jelly_bytes_new(struct jelly_vm* vm, uint32_t type_id, uint32_t length) {
  size_t total = sizeof(jelly_bytes) + (size_t)length;
  jelly_bytes* b = (jelly_bytes*)jelly_gc_alloc(vm, total);
  b->h.kind = (uint32_t)JELLY_OBJ_BYTES;
  b->h.type_id = type_id;
  b->length = length;
  if(length) memset(b->data, 0, length);
  return b;
}

