#include <jelly/function.h>
#include <jelly/gc.h>

#include <string.h>

jelly_function* jelly_function_new(struct jelly_vm* vm, uint32_t type_id, uint32_t func_index) {
  jelly_function* fn = (jelly_function*)jelly_gc_alloc(vm, sizeof(jelly_function));
  fn->h.kind = (uint32_t)JELLY_OBJ_FUNCTION;
  fn->h.type_id = type_id;
  fn->func_index = func_index;
  fn->ncaps = 0;
  fn->bound_this = jelly_make_null();
  return fn;
}

jelly_function* jelly_closure_new(struct jelly_vm* vm, uint32_t type_id, uint32_t func_index, uint32_t ncaps, const jelly_value* caps) {
  jelly_function* fn = (jelly_function*)jelly_gc_alloc(vm, sizeof(jelly_function) + sizeof(jelly_value) * (size_t)ncaps);
  fn->h.kind = (uint32_t)JELLY_OBJ_FUNCTION;
  fn->h.type_id = type_id;
  fn->func_index = func_index;
  fn->ncaps = ncaps;
  fn->bound_this = jelly_make_null();
  if(ncaps) memcpy(fn->caps, caps, sizeof(jelly_value) * (size_t)ncaps);
  return fn;
}

jelly_function* jelly_function_bind_this(struct jelly_vm* vm, uint32_t type_id, const jelly_function* f, jelly_value bound_this) {
  if(!f) return NULL;
  jelly_function* fn = (jelly_function*)jelly_gc_alloc(vm, sizeof(jelly_function) + sizeof(jelly_value) * (size_t)f->ncaps);
  fn->h.kind = (uint32_t)JELLY_OBJ_FUNCTION;
  fn->h.type_id = type_id;
  fn->func_index = f->func_index;
  fn->ncaps = f->ncaps;
  fn->bound_this = bound_this;
  if(fn->ncaps) memcpy(fn->caps, f->caps, sizeof(jelly_value) * (size_t)fn->ncaps);
  return fn;
}

