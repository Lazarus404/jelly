#include <jelly/box.h>
#include <jelly/gc.h>

jelly_box_i64* jelly_box_i64_new(struct jelly_vm* vm, uint32_t type_id, int64_t v) {
  jelly_box_i64* b = (jelly_box_i64*)jelly_gc_alloc(vm, sizeof(jelly_box_i64));
  b->h.kind = (uint32_t)JELLY_OBJ_BOX_I64;
  b->h.type_id = type_id;
  b->value = v;
  return b;
}

jelly_box_f64* jelly_box_f64_new(struct jelly_vm* vm, uint32_t type_id, double v) {
  jelly_box_f64* b = (jelly_box_f64*)jelly_gc_alloc(vm, sizeof(jelly_box_f64));
  b->h.kind = (uint32_t)JELLY_OBJ_BOX_F64;
  b->h.type_id = type_id;
  b->value = v;
  return b;
}

jelly_box_f32* jelly_box_f32_new(struct jelly_vm* vm, uint32_t type_id, float v) {
  jelly_box_f32* b = (jelly_box_f32*)jelly_gc_alloc(vm, sizeof(jelly_box_f32));
  b->h.kind = (uint32_t)JELLY_OBJ_BOX_F32;
  b->h.type_id = type_id;
  b->value = v;
  return b;
}

