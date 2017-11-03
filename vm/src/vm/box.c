/**
 * Copyright 2017 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <jelly/internal.h>

#include <stdlib.h>
#include <math.h>

int vm_checked_f64_to_i32(jelly_vm* vm, double x, uint32_t* out_u32) {
  if(!isfinite(x)) {
    vm->trap_code = JELLY_TRAP_TYPE_MISMATCH;
    vm->trap_msg = "f64->i32 conversion overflow";
    vm->exc_pending = 1;
    vm->exc_payload = jelly_make_i32((int32_t)JELLY_TRAP_TYPE_MISMATCH);
    return 0;
  }
  if(x > 2147483647.0 || x < -2147483648.0) {
    vm->trap_code = JELLY_TRAP_TYPE_MISMATCH;
    vm->trap_msg = "f64->i32 conversion overflow";
    vm->exc_pending = 1;
    vm->exc_payload = jelly_make_i32((int32_t)JELLY_TRAP_TYPE_MISMATCH);
    return 0;
  }
  int32_t v = (int32_t)x;
  *out_u32 = (uint32_t)v;
  return 1;
}

int vm_checked_f64_to_i64(jelly_vm* vm, double x, int64_t* out_i64) {
  if(!isfinite(x)) {
    vm->trap_code = JELLY_TRAP_TYPE_MISMATCH;
    vm->trap_msg = "f64->i64 conversion overflow";
    vm->exc_pending = 1;
    vm->exc_payload = jelly_make_i32((int32_t)JELLY_TRAP_TYPE_MISMATCH);
    return 0;
  }
  if(x > 9223372036854775807.0 || x < -9223372036854775808.0) {
    vm->trap_code = JELLY_TRAP_TYPE_MISMATCH;
    vm->trap_msg = "f64->i64 conversion overflow";
    vm->exc_pending = 1;
    vm->exc_payload = jelly_make_i32((int32_t)JELLY_TRAP_TYPE_MISMATCH);
    return 0;
  }
  *out_i64 = (int64_t)x;
  return 1;
}

uint32_t vm_expected_obj_kind_for_typed_ptr(jelly_type_kind k) {
  switch(k) {
    case JELLY_T_BYTES: return (uint32_t)JELLY_OBJ_BYTES;
    case JELLY_T_FUNCTION: return (uint32_t)JELLY_OBJ_FUNCTION;
    case JELLY_T_LIST: return (uint32_t)JELLY_OBJ_LIST;
    case JELLY_T_ARRAY: return (uint32_t)JELLY_OBJ_ARRAY;
    case JELLY_T_OBJECT: return (uint32_t)JELLY_OBJ_OBJECT;
    case JELLY_T_ABSTRACT: return (uint32_t)JELLY_OBJ_ABSTRACT;
    default: return 0;
  }
}

uint32_t vm_kindof_dynamic(jelly_value v) {
  if(jelly_is_null(v)) return 0u;
  if(jelly_is_bool(v)) return 1u;
  if(jelly_is_i32(v)) return 2u;
  if(jelly_is_atom(v)) return 3u;
  if(!jelly_is_ptr(v)) return 0x7fffffffu;
  switch(jelly_obj_kind_of(v)) {
    case JELLY_OBJ_BYTES: return 4u;
    case JELLY_OBJ_FUNCTION: return 5u;
    case JELLY_OBJ_LIST: return 6u;
    case JELLY_OBJ_ARRAY: return 7u;
    case JELLY_OBJ_OBJECT: return 8u;
    case JELLY_OBJ_ABSTRACT: return 9u;
    case JELLY_OBJ_BOX_I64: return 10u;
    case JELLY_OBJ_BOX_F64: return 11u;
    case JELLY_OBJ_BOX_F32: return 12u;
    case JELLY_OBJ_BOX_F16: return 13u;
    default: return 0x7fffffffu;
  }
}

jelly_value vm_box_from_typed(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf, uint32_t r) {
  switch(vm_reg_kind(m, f, r)) {
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
      return jelly_make_i32((int32_t)vm_load_u32(rf, r));
    case JELLY_T_F32: {
      uint32_t type_id = f->reg_types[r];
      jelly_box_f32* b = jelly_box_f32_new(vm, type_id, vm_load_f32(rf, r));
      return jelly_from_ptr(b);
    }
    case JELLY_T_F16: {
      uint32_t type_id = f->reg_types[r];
      uint16_t bits = vm_load_f16_bits(rf, r);
      jelly_box_f16* b = jelly_box_f16_new(vm, type_id, bits);
      return jelly_from_ptr(b);
    }
    case JELLY_T_I64: {
      uint32_t type_id = f->reg_types[r];
      jelly_box_i64* b = jelly_box_i64_new(vm, type_id, vm_load_i64(rf, r));
      return jelly_from_ptr(b);
    }
    case JELLY_T_BOOL:
      return jelly_make_bool((int)(vm_load_u32(rf, r) != 0));
    case JELLY_T_ATOM:
      return jelly_make_atom(vm_load_u32(rf, r));
    case JELLY_T_F64: {
      uint32_t type_id = f->reg_types[r];
      jelly_box_f64* b = jelly_box_f64_new(vm, type_id, vm_load_f64(rf, r));
      return jelly_from_ptr(b);
    }
    case JELLY_T_DYNAMIC:
      return vm_load_val(rf, r);
    case JELLY_T_BYTES:
    case JELLY_T_FUNCTION:
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
    case JELLY_T_OBJECT:
    case JELLY_T_ABSTRACT: {
      void* p = vm_load_ptr(rf, r);
      if(p == NULL) return jelly_make_null();
      return jelly_from_ptr(p);
    }
    default:
      jelly_vm_panic();
      return jelly_make_null();
  }
}

static void unbox_to_i32(reg_frame* rf, uint32_t dst, jelly_value v) {
  if(!jelly_is_i32(v)) jelly_vm_panic();
  vm_store_u32(rf, dst, (uint32_t)jelly_as_i32(v));
}

void vm_store_from_boxed(const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf, uint32_t dst, jelly_value v) {
  switch(vm_reg_kind(m, f, dst)) {
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
      if(jelly_is_null(v)) {
        vm_store_u32(rf, dst, 0);
        return;
      }
      if(jelly_is_i32(v)) {
        vm_store_u32(rf, dst, (uint32_t)jelly_as_i32(v));
        return;
      }
      if(jelly_is_box_f64(v)) {
        int32_t x = (int32_t)jelly_as_box_f64(v);
        vm_store_u32(rf, dst, (uint32_t)x);
        return;
      }
      if(jelly_is_box_i64(v)) {
        int32_t x = (int32_t)jelly_as_box_i64(v);
        vm_store_u32(rf, dst, (uint32_t)x);
        return;
      }
      unbox_to_i32(rf, dst, v);
      return;
    case JELLY_T_F32:
      if(jelly_is_i32(v)) {
        vm_store_f32(rf, dst, (float)jelly_as_i32(v));
        return;
      }
      if(jelly_is_null(v)) {
        vm_store_f32(rf, dst, 0.0f);
        return;
      }
      if(jelly_is_box_f64(v)) {
        vm_store_f32(rf, dst, (float)jelly_as_box_f64(v));
        return;
      }
      if(!jelly_is_box_f32(v)) jelly_vm_panic();
      vm_store_f32(rf, dst, jelly_as_box_f32(v));
      return;
    case JELLY_T_F16:
      if(jelly_is_i32(v)) {
        vm_store_f16_bits(rf, dst, vm_f32_to_f16_bits((float)jelly_as_i32(v)));
        return;
      }
      if(jelly_is_null(v)) {
        vm_store_f16_bits(rf, dst, 0);
        return;
      }
      if(jelly_is_box_f64(v)) {
        vm_store_f16_bits(rf, dst, vm_f32_to_f16_bits((float)jelly_as_box_f64(v)));
        return;
      }
      if(!jelly_is_box_f16(v)) jelly_vm_panic();
      vm_store_f16_bits(rf, dst, jelly_as_box_f16(v));
      return;
    case JELLY_T_I64:
      if(jelly_is_i32(v)) {
        vm_store_i64(rf, dst, (int64_t)jelly_as_i32(v));
        return;
      }
      if(jelly_is_null(v)) {
        vm_store_i64(rf, dst, 0);
        return;
      }
      if(jelly_is_box_f64(v)) {
        vm_store_i64(rf, dst, (int64_t)jelly_as_box_f64(v));
        return;
      }
      if(!jelly_is_box_i64(v)) jelly_vm_panic();
      vm_store_i64(rf, dst, jelly_as_box_i64(v));
      return;
    case JELLY_T_BOOL:
      if(!jelly_is_bool(v)) jelly_vm_panic();
      vm_store_u32(rf, dst, (uint32_t)jelly_as_bool(v));
      return;
    case JELLY_T_ATOM:
      if(!jelly_is_atom(v)) jelly_vm_panic();
      vm_store_u32(rf, dst, jelly_as_atom(v));
      return;
    case JELLY_T_F64:
      if(jelly_is_i32(v)) {
        vm_store_f64(rf, dst, (double)jelly_as_i32(v));
        return;
      }
      if(jelly_is_null(v)) {
        vm_store_f64(rf, dst, 0.0);
        return;
      }
      if(jelly_is_box_f32(v)) {
        vm_store_f64(rf, dst, (double)jelly_as_box_f32(v));
        return;
      }
      if(!jelly_is_box_f64(v)) jelly_vm_panic();
      vm_store_f64(rf, dst, jelly_as_box_f64(v));
      return;
    case JELLY_T_DYNAMIC:
      vm_store_val(rf, dst, v);
      return;
    case JELLY_T_BYTES:
    case JELLY_T_FUNCTION:
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
    case JELLY_T_OBJECT:
    case JELLY_T_ABSTRACT:
      if(jelly_is_null(v)) {
        vm_store_ptr(rf, dst, NULL);
        return;
      }
      if(!jelly_is_ptr(v)) jelly_vm_panic();
      vm_store_ptr(rf, dst, jelly_as_ptr(v));
      return;
    default:
      jelly_vm_panic();
      return;
  }
}
