#ifndef JELLY_BOX_H
#define JELLY_BOX_H

/*
  Boxed wide numerics for Dynamic/spill.

  We keep i64/f64 as heap objects so `jelly_value` tagging stays portable.
*/

#include <stdint.h>

#include "value.h"

struct jelly_vm;

typedef struct jelly_box_i64 {
  jelly_obj_header h; // kind=JELLY_OBJ_BOX_I64
  int64_t value;
} jelly_box_i64;

typedef struct jelly_box_f64 {
  jelly_obj_header h; // kind=JELLY_OBJ_BOX_F64
  double value;
} jelly_box_f64;

typedef struct jelly_box_f32 {
  jelly_obj_header h; // kind=JELLY_OBJ_BOX_F32
  float value;
} jelly_box_f32;

jelly_box_i64* jelly_box_i64_new(struct jelly_vm* vm, uint32_t type_id, int64_t v);
jelly_box_f64* jelly_box_f64_new(struct jelly_vm* vm, uint32_t type_id, double v);
jelly_box_f32* jelly_box_f32_new(struct jelly_vm* vm, uint32_t type_id, float v);

static inline int jelly_is_box_i64(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_I64;
}

static inline int jelly_is_box_f64(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_F64;
}

static inline int jelly_is_box_f32(jelly_value v) {
  return jelly_is_ptr(v) && jelly_obj_kind_of(v) == (uint32_t)JELLY_OBJ_BOX_F32;
}

static inline int64_t jelly_as_box_i64(jelly_value v) {
  return ((const jelly_box_i64*)jelly_as_ptr(v))->value;
}

static inline double jelly_as_box_f64(jelly_value v) {
  return ((const jelly_box_f64*)jelly_as_ptr(v))->value;
}

static inline float jelly_as_box_f32(jelly_value v) {
  return ((const jelly_box_f32*)jelly_as_ptr(v))->value;
}

#endif /* JELLY_BOX_H */

