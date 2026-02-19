#include <jelly/vm.h>
#include <jelly/list.h>
#include <jelly/array.h>
#include <jelly/bytes.h>
#include <jelly/object.h>
#include <jelly/function.h>
#include <jelly/abstract.h>
#include <jelly/box.h>
#include <jelly/gc.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

static void jelly_vm_panic(void) {
  abort();
}

static jelly_exec_status jelly_vm_trap(jelly_vm* vm, jelly_trap_code code, const char* msg) {
  if(vm) {
    vm->trap_code = code;
    vm->trap_msg = msg;
    vm->exc_pending = 1;
    vm->exc_payload = jelly_make_i32((int32_t)code);
  }
  return JELLY_EXEC_TRAP;
}

static int checked_f64_to_i32(jelly_vm* vm, double x, uint32_t* out_u32) {
  if(!isfinite(x)) {
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "f64->i32 conversion overflow");
    return 0;
  }
  // Truncate toward 0, but trap if not representable.
  if(x > 2147483647.0 || x < -2147483648.0) {
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "f64->i32 conversion overflow");
    return 0;
  }
  int32_t v = (int32_t)x;
  *out_u32 = (uint32_t)v;
  return 1;
}

static int checked_f64_to_i64(jelly_vm* vm, double x, int64_t* out_i64) {
  if(!isfinite(x)) {
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "f64->i64 conversion overflow");
    return 0;
  }
  // Truncate toward 0, but trap if not representable.
  // Use inclusive limits so boundary values are allowed.
  if(x > 9223372036854775807.0 || x < -9223372036854775808.0) {
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "f64->i64 conversion overflow");
    return 0;
  }
  *out_i64 = (int64_t)x;
  return 1;
}

size_t jelly_slot_align(jelly_type_kind k) {
  switch(k) {
    case JELLY_T_I64:
    case JELLY_T_F64:
      return 8;
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
    case JELLY_T_F32:
    case JELLY_T_BOOL:
    case JELLY_T_ATOM:
      return 4;
    case JELLY_T_DYNAMIC:
      return sizeof(jelly_value);
    case JELLY_T_BYTES:
    case JELLY_T_FUNCTION:
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
    case JELLY_T_OBJECT:
    case JELLY_T_ABSTRACT:
      return sizeof(void*);
    default:
      return sizeof(void*);
  }
}

size_t jelly_slot_size(jelly_type_kind k) {
  switch(k) {
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
    case JELLY_T_F32:
    case JELLY_T_BOOL:
    case JELLY_T_ATOM:
      // Store as 32-bit slots (I8/I16 are range-constrained i32 for MVP).
      return 4;
    case JELLY_T_I64:
    case JELLY_T_F64:
      return 8;
    case JELLY_T_DYNAMIC:
      return sizeof(jelly_value);
    case JELLY_T_BYTES:
    case JELLY_T_FUNCTION:
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
    case JELLY_T_OBJECT:
    case JELLY_T_ABSTRACT:
      return sizeof(void*);
    default:
      return sizeof(void*);
  }
}

static size_t align_up(size_t x, size_t a) {
  return (x + (a - 1u)) & ~(a - 1u);
}

typedef struct reg_frame {
  uint8_t* mem;
  uint32_t* off; // nregs offsets
  uint32_t nregs;
} reg_frame;

static void rf_free(reg_frame* rf) {
  if(!rf) return;
  free(rf->mem);
  free(rf->off);
  rf->mem = NULL;
  rf->off = NULL;
  rf->nregs = 0;
}

static jelly_type_kind reg_kind(const jelly_bc_module* m, const jelly_bc_function* f, uint32_t r) {
  jelly_type_id tid = f->reg_types[r];
  return m->types[tid].kind;
}

static void* reg_ptr(reg_frame* rf, uint32_t r) {
  return (void*)(rf->mem + rf->off[r]);
}

static uint32_t load_u32(reg_frame* rf, uint32_t r) {
  uint32_t v;
  memcpy(&v, reg_ptr(rf, r), sizeof(v));
  return v;
}

static void store_u32(reg_frame* rf, uint32_t r, uint32_t v) {
  memcpy(reg_ptr(rf, r), &v, sizeof(v));
}

static float load_f32(reg_frame* rf, uint32_t r) {
  float v;
  memcpy(&v, reg_ptr(rf, r), sizeof(v));
  return v;
}

static void store_f32(reg_frame* rf, uint32_t r, float v) {
  memcpy(reg_ptr(rf, r), &v, sizeof(v));
}

static int32_t load_i32ish(reg_frame* rf, uint32_t r) {
  return (int32_t)load_u32(rf, r);
}

static int64_t load_i64(reg_frame* rf, uint32_t r) {
  int64_t v;
  memcpy(&v, reg_ptr(rf, r), sizeof(v));
  return v;
}

static void store_i64(reg_frame* rf, uint32_t r, int64_t v) {
  memcpy(reg_ptr(rf, r), &v, sizeof(v));
}

static double load_f64(reg_frame* rf, uint32_t r) {
  double v;
  memcpy(&v, reg_ptr(rf, r), sizeof(v));
  return v;
}

static void store_f64(reg_frame* rf, uint32_t r, double v) {
  memcpy(reg_ptr(rf, r), &v, sizeof(v));
}

static jelly_value load_val(reg_frame* rf, uint32_t r) {
  jelly_value v;
  memcpy(&v, reg_ptr(rf, r), sizeof(v));
  return v;
}

static void store_val(reg_frame* rf, uint32_t r, jelly_value v) {
  memcpy(reg_ptr(rf, r), &v, sizeof(v));
}

static void spill_push(jelly_vm* vm, jelly_value v) {
  if(vm->spill_len == vm->spill_cap) {
    uint32_t new_cap = vm->spill_cap ? vm->spill_cap * 2u : 16u;
    jelly_value* nv = (jelly_value*)realloc(vm->spill, (size_t)new_cap * sizeof(jelly_value));
    if(!nv) jelly_vm_panic();
    vm->spill = nv;
    vm->spill_cap = new_cap;
  }
  vm->spill[vm->spill_len++] = v;
}

static jelly_value spill_pop(jelly_vm* vm) {
  if(vm->spill_len == 0) jelly_vm_panic();
  return vm->spill[--vm->spill_len];
}

static jelly_value box_from_typed(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf, uint32_t r) {
  switch(reg_kind(m, f, r)) {
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
      return jelly_make_i32((int32_t)load_u32(rf, r));
    case JELLY_T_F32: {
      uint32_t type_id = f->reg_types[r];
      jelly_box_f32* b = jelly_box_f32_new(vm, type_id, load_f32(rf, r));
      return jelly_from_ptr(b);
    }
    case JELLY_T_I64: {
      uint32_t type_id = f->reg_types[r];
      jelly_box_i64* b = jelly_box_i64_new(vm, type_id, load_i64(rf, r));
      return jelly_from_ptr(b);
    }
    case JELLY_T_BOOL:
      return jelly_make_bool((int)(load_u32(rf, r) != 0));
    case JELLY_T_ATOM:
      return jelly_make_atom(load_u32(rf, r));
    case JELLY_T_F64: {
      uint32_t type_id = f->reg_types[r];
      jelly_box_f64* b = jelly_box_f64_new(vm, type_id, load_f64(rf, r));
      return jelly_from_ptr(b);
    }
    case JELLY_T_DYNAMIC:
      return load_val(rf, r);
    case JELLY_T_BYTES:
    case JELLY_T_FUNCTION:
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
    case JELLY_T_OBJECT:
    case JELLY_T_ABSTRACT: {
      void* p = NULL;
      memcpy(&p, reg_ptr(rf, r), sizeof(void*));
      if(p == NULL) return jelly_make_null();
      return jelly_from_ptr(p);
    }
    default:
      jelly_vm_panic();
      return jelly_make_null();
  }
}

static void store_ptr(reg_frame* rf, uint32_t r, void* p) {
  memcpy(reg_ptr(rf, r), &p, sizeof(void*));
}

static uint32_t expected_obj_kind_for_typed_ptr(jelly_type_kind k) {
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

static uint32_t kindof_dynamic(jelly_value v) {
  // Stable kind codes for match lowering:
  // 0 null, 1 bool, 2 i32, 3 atom,
  // 4 bytes, 5 function, 6 list, 7 array, 8 object, 9 abstract,
  // 10 box_i64, 11 box_f64, 12 box_f32,
  // 0x7fffffff unknown/unsupported pointer kind.
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
    default: return 0x7fffffffu;
  }
}

static void* load_ptr(reg_frame* rf, uint32_t r) {
  void* p = NULL;
  memcpy(&p, reg_ptr(rf, r), sizeof(void*));
  return p;
}

static void unbox_to_i32(reg_frame* rf, uint32_t dst, jelly_value v) {
  if(!jelly_is_i32(v)) jelly_vm_panic();
  store_u32(rf, dst, (uint32_t)jelly_as_i32(v));
}

static void store_from_boxed(const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf, uint32_t dst, jelly_value v) {
  switch(reg_kind(m, f, dst)) {
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
      unbox_to_i32(rf, dst, v);
      return;
    case JELLY_T_F32:
      if(!jelly_is_box_f32(v)) jelly_vm_panic();
      store_f32(rf, dst, jelly_as_box_f32(v));
      return;
    case JELLY_T_I64:
      if(jelly_is_i32(v)) {
        store_i64(rf, dst, (int64_t)jelly_as_i32(v));
        return;
      }
      if(!jelly_is_box_i64(v)) jelly_vm_panic();
      store_i64(rf, dst, jelly_as_box_i64(v));
      return;
    case JELLY_T_BOOL:
      if(!jelly_is_bool(v)) jelly_vm_panic();
      store_u32(rf, dst, (uint32_t)jelly_as_bool(v));
      return;
    case JELLY_T_ATOM:
      if(!jelly_is_atom(v)) jelly_vm_panic();
      store_u32(rf, dst, jelly_as_atom(v));
      return;
    case JELLY_T_F64:
      if(jelly_is_i32(v)) {
        store_f64(rf, dst, (double)jelly_as_i32(v));
        return;
      }
      if(!jelly_is_box_f64(v)) jelly_vm_panic();
      store_f64(rf, dst, jelly_as_box_f64(v));
      return;
    case JELLY_T_DYNAMIC:
      store_val(rf, dst, v);
      return;
    case JELLY_T_BYTES:
    case JELLY_T_FUNCTION:
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
    case JELLY_T_OBJECT:
    case JELLY_T_ABSTRACT:
      if(jelly_is_null(v)) {
        store_ptr(rf, dst, NULL);
        return;
      }
      if(!jelly_is_ptr(v)) jelly_vm_panic();
      store_ptr(rf, dst, jelly_as_ptr(v));
      return;
    default:
      jelly_vm_panic();
      return;
  }
}

static void copy_arg_strict(const jelly_bc_module* m,
                            const jelly_bc_function* caller_f, reg_frame* caller_rf, uint32_t src,
                            const jelly_bc_function* callee_f, reg_frame* callee_rf, uint32_t dst) {
  if(caller_f->reg_types[src] != callee_f->reg_types[dst]) {
    fprintf(stderr,
            "vm panic: call arg type mismatch (caller reg %u type %u) -> (callee reg %u type %u)\n",
            src, caller_f->reg_types[src], dst, callee_f->reg_types[dst]);
    jelly_vm_panic();
  }
  jelly_type_kind k = reg_kind(m, callee_f, dst);
  size_t sz = jelly_slot_size(k);
  memmove(reg_ptr(callee_rf, dst), reg_ptr(caller_rf, src), sz);
}

typedef struct call_frame {
  const jelly_bc_function* f;
  reg_frame rf;
  uint32_t pc;
  uint32_t caller_dst; // destination register in caller
  uint8_t has_caller;
} call_frame;

typedef struct exc_handler {
  uint32_t frame_index; // call frame index to land in
  uint32_t catch_pc;    // absolute pc in that frame
  uint32_t dst_reg;     // Dynamic register to receive payload
} exc_handler;

static void exc_push(jelly_vm* vm, uint32_t frame_index, uint32_t catch_pc, uint32_t dst_reg) {
  exc_handler** hs = (exc_handler**)&vm->exc_handlers;
  if(vm->exc_handlers_len == vm->exc_handlers_cap) {
    uint32_t ncap = vm->exc_handlers_cap ? (vm->exc_handlers_cap * 2u) : 16u;
    exc_handler* nh = (exc_handler*)realloc(*hs, sizeof(exc_handler) * (size_t)ncap);
    if(!nh) jelly_vm_panic();
    *hs = nh;
    vm->exc_handlers_cap = ncap;
  }
  exc_handler* h = &((exc_handler*)vm->exc_handlers)[vm->exc_handlers_len++];
  h->frame_index = frame_index;
  h->catch_pc = catch_pc;
  h->dst_reg = dst_reg;
}

static int exc_pop(jelly_vm* vm, exc_handler* out) {
  if(vm->exc_handlers_len == 0) return 0;
  exc_handler* hs = (exc_handler*)vm->exc_handlers;
  *out = hs[--vm->exc_handlers_len];
  return 1;
}

static void exc_pop_for_frame(jelly_vm* vm, uint32_t frame_index) {
  exc_handler* hs = (exc_handler*)vm->exc_handlers;
  while(vm->exc_handlers_len > 0 && hs[vm->exc_handlers_len - 1u].frame_index == frame_index) {
    vm->exc_handlers_len--;
  }
}

static void unwind_all_frames(jelly_vm* vm) {
  if(!vm || !vm->call_frames) return;
  call_frame* frames = (call_frame*)vm->call_frames;
  for(uint32_t i = 0; i < vm->call_frames_len; i++) {
    rf_free(&frames[i].rf);
  }
  free(vm->call_frames);
  vm->call_frames = NULL;
  vm->call_frames_len = 0;
  vm->call_frames_cap = 0;
}

static void build_frame_layout(const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf) {
  rf->nregs = f->nregs;
  rf->off = (uint32_t*)malloc(sizeof(uint32_t) * (size_t)rf->nregs);
  if(!rf->off) jelly_vm_panic();

  size_t total = 0;
  for(uint32_t i = 0; i < rf->nregs; i++) {
    jelly_type_kind k = reg_kind(m, f, i);
    size_t a = jelly_slot_align(k);
    size_t s = jelly_slot_size(k);
    total = align_up(total, a);
    rf->off[i] = (uint32_t)total;
    total += s;
  }

  rf->mem = (uint8_t*)calloc(1, total);
  if(!rf->mem) { rf_free(rf); jelly_vm_panic(); }
}

static void init_args_and_caps(jelly_vm* vm, const jelly_bc_module* m,
                               const jelly_bc_function* callee_f, reg_frame* callee_rf,
                               const jelly_bc_function* caller_f, reg_frame* caller_rf,
                               uint32_t first_arg, uint32_t nargs,
                               const jelly_function* funobj) {
  uint32_t arg_base = 0;
  if(funobj && !jelly_is_null(funobj->bound_this)) {
    if(callee_rf->nregs < 1) jelly_vm_panic();
    store_from_boxed(m, callee_f, callee_rf, 0, funobj->bound_this);
    arg_base = 1;
  }

  if(arg_base + nargs > callee_rf->nregs) jelly_vm_panic();
  for(uint32_t i = 0; i < nargs; i++) {
    copy_arg_strict(m, caller_f, caller_rf, first_arg + i, callee_f, callee_rf, arg_base + i);
  }

  if(funobj && funobj->ncaps) {
    if(funobj->ncaps > callee_rf->nregs) jelly_vm_panic();
    uint32_t cap_start = callee_rf->nregs - funobj->ncaps;
    if(cap_start < arg_base + nargs) jelly_vm_panic();
    for(uint32_t i = 0; i < funobj->ncaps; i++) {
      store_from_boxed(m, callee_f, callee_rf, cap_start + i, funobj->caps[i]);
    }
  }
}

static void push_frame(jelly_vm* vm,
                       const jelly_bc_module* m,
                       const jelly_bc_function* callee_f,
                       const jelly_bc_function* caller_f,
                       uint32_t caller_frame_index,
                       uint32_t caller_dst,
                       uint32_t first_arg,
                       uint32_t nargs,
                       const jelly_function* funobj,
                       uint8_t has_caller) {
  call_frame** frames = (call_frame**)&vm->call_frames;
  uint32_t* len = &vm->call_frames_len;
  uint32_t* cap = &vm->call_frames_cap;

  if(*len == *cap) {
    uint32_t ncap = *cap ? (*cap * 2u) : 16u;
    call_frame* nf = (call_frame*)realloc(*frames, sizeof(call_frame) * (size_t)ncap);
    if(!nf) jelly_vm_panic();
    *frames = nf;
    *cap = ncap;
  }

  call_frame* fr = &(*frames)[(*len)++];
  memset(fr, 0, sizeof(*fr));
  fr->f = callee_f;
  fr->pc = 0;
  fr->caller_dst = caller_dst;
  fr->has_caller = has_caller;
  build_frame_layout(m, callee_f, &fr->rf);
  if(has_caller) {
    if(caller_frame_index >= *len - 1u) jelly_vm_panic();
    call_frame* caller = &(*frames)[caller_frame_index];
    init_args_and_caps(vm, m, callee_f, &fr->rf, caller_f, &caller->rf, first_arg, nargs, funobj);
  }
}

static jelly_exec_status exec_entry(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* entry, jelly_value* out) {
  vm->call_frames = NULL;
  vm->call_frames_len = 0;
  vm->call_frames_cap = 0;

  free(vm->exc_handlers);
  vm->exc_handlers = NULL;
  vm->exc_handlers_len = 0;
  vm->exc_handlers_cap = 0;
  vm->exc_pending = 0;
  vm->exc_payload = jelly_make_null();

  // Root frame has no caller and no args.
  push_frame(vm, m, entry, entry, 0, 0, 0, 0, NULL, 0);

  for(;;) {
    if(vm->exc_pending) goto exception_dispatch;
    if(vm->call_frames_len == 0) {
      free(vm->call_frames);
      vm->call_frames = NULL;
      vm->call_frames_cap = 0;
      free(vm->exc_handlers);
      vm->exc_handlers = NULL;
      vm->exc_handlers_cap = 0;
      if(out) *out = jelly_make_null();
      return JELLY_EXEC_OK;
    }

    call_frame* frames = (call_frame*)vm->call_frames;
    call_frame* fr = &frames[vm->call_frames_len - 1u];
    const jelly_bc_function* f = fr->f;
    const jelly_type_entry* types = m->types;
    const jelly_insn* insns = f->insns;

    if(fr->pc >= f->ninsns) jelly_vm_panic();
    const jelly_insn* ins = &insns[fr->pc++];

    switch((jelly_op)ins->op) {
      case JOP_NOP:
        break;
      case JOP_MOV: {
        uint32_t a = ins->a, b = ins->b;
        // Bytecode validator guarantees reg_types[a] == reg_types[b].
        jelly_type_kind k = types[f->reg_types[a]].kind;
        size_t sz = jelly_slot_size(k);
        memmove(reg_ptr(&fr->rf, a), reg_ptr(&fr->rf, b), sz);
        break;
      }
      case JOP_CONST_I32:
        store_u32(&fr->rf, ins->a, ins->imm);
        break;
      case JOP_CONST_BOOL:
        store_u32(&fr->rf, ins->a, (uint32_t)(ins->c & 1u));
        break;
      case JOP_CONST_ATOM:
        store_u32(&fr->rf, ins->a, ins->imm);
        break;
      case JOP_CONST_NULL:
        store_val(&fr->rf, ins->a, jelly_make_null());
        break;
      case JOP_CONST_F32: {
        uint32_t bits = ins->imm;
        float fv;
        memcpy(&fv, &bits, sizeof(fv));
        store_f32(&fr->rf, ins->a, fv);
        break;
      }
      case JOP_CONST_I64: {
        // Validated by loader.
        store_i64(&fr->rf, ins->a, m->const_i64[ins->imm]);
        break;
      }
      case JOP_CONST_F64: {
        // Validated by loader.
        store_f64(&fr->rf, ins->a, m->const_f64[ins->imm]);
        break;
      }
      case JOP_CONST_BYTES: {
        // Validated by loader.
        const uint32_t idx = ins->imm;
        uint32_t len = 0;
        uint32_t off = 0;
        if(idx < m->nconst_bytes) {
          len = m->const_bytes_len[idx];
          off = m->const_bytes_off[idx];
        } else {
          // Should never happen due to loader + validator.
          (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "const_bytes out of range");
          break;
        }
        uint32_t type_id = f->reg_types[ins->a];
        jelly_bytes* b = jelly_bytes_new(vm, type_id, len);
        if(len > 0) memcpy(b->data, m->const_bytes_data + off, len);
        store_ptr(&fr->rf, ins->a, b);
        break;
      }
      case JOP_CONST_FUN: {
        uint32_t type_id = f->reg_types[ins->a];
        jelly_function* fn = jelly_function_new(vm, type_id, ins->imm);
        store_ptr(&fr->rf, ins->a, fn);
        break;
      }
      case JOP_CLOSURE: {
        uint32_t type_id = f->reg_types[ins->a];
        uint32_t ncaps = (uint32_t)ins->c;
        jelly_value tmp[256];
        for(uint32_t i = 0; i < ncaps; i++) {
          tmp[i] = box_from_typed(vm, m, f, &fr->rf, (uint32_t)ins->b + i);
          jelly_gc_push_root(vm, tmp[i]);
        }
        jelly_function* clo = jelly_closure_new(vm, type_id, ins->imm, ncaps, tmp);
        jelly_gc_pop_roots(vm, ncaps);
        store_ptr(&fr->rf, ins->a, clo);
        break;
      }
      case JOP_BIND_THIS: {
        jelly_function* base = (jelly_function*)load_ptr(&fr->rf, ins->b);
        if(!base) jelly_vm_panic();
        if(base->h.kind != (uint32_t)JELLY_OBJ_FUNCTION) jelly_vm_panic();
        jelly_value thisv = box_from_typed(vm, m, f, &fr->rf, ins->c);
        jelly_gc_push_root(vm, thisv);
        uint32_t type_id = f->reg_types[ins->a];
        jelly_function* bound = jelly_function_bind_this(vm, type_id, base, thisv);
        jelly_gc_pop_roots(vm, 1);
        store_ptr(&fr->rf, ins->a, bound);
        break;
      }
      case JOP_JMP: {
        int32_t d = (int32_t)ins->imm;
        int32_t npc = (int32_t)fr->pc + d;
        if(npc < 0 || npc > (int32_t)f->ninsns) jelly_vm_panic();
        fr->pc = (uint32_t)npc;
        break;
      }
      case JOP_JMP_IF: {
        uint32_t cond = load_u32(&fr->rf, ins->a);
        if(cond != 0) {
          int32_t d = (int32_t)ins->imm;
          int32_t npc = (int32_t)fr->pc + d;
          if(npc < 0 || npc > (int32_t)f->ninsns) jelly_vm_panic();
          fr->pc = (uint32_t)npc;
        }
        break;
      }
      case JOP_TRY: {
        uint32_t frame_index = vm->call_frames_len - 1u;
        int32_t d = (int32_t)ins->imm;
        int32_t catch_pc = (int32_t)fr->pc + d;
        if(catch_pc < 0 || catch_pc > (int32_t)f->ninsns) jelly_vm_panic();
        exc_push(vm, frame_index, (uint32_t)catch_pc, (uint32_t)ins->a);
        break;
      }
      case JOP_ENDTRY: {
        uint32_t frame_index = vm->call_frames_len - 1u;
        exc_handler top;
        if(!exc_pop(vm, &top)) jelly_vm_panic();
        if(top.frame_index != frame_index) jelly_vm_panic();
        break;
      }
      case JOP_THROW: {
        jelly_value payload = load_val(&fr->rf, ins->a);
        vm->trap_code = JELLY_TRAP_THROWN;
        vm->trap_msg = "unhandled throw";
        vm->exc_pending = 1;
        vm->exc_payload = payload;
        goto exception_dispatch;
      }
      case JOP_SWITCH_KIND: {
        // Table layout:
        //   SWITCH_KIND rA(kind), ncases=rB, default_delta=imm
        //   CASE_KIND kind_u8=a, delta=imm
        //   ... (ncases entries)
        //
        // All deltas are relative to the pc *after* the case table.
        uint32_t kind = load_u32(&fr->rf, ins->a);
        uint32_t ncases = (uint32_t)ins->b;
        uint32_t table_first = fr->pc; // pc after fetch points at first case entry
        uint32_t table_end = table_first + ncases;
        if(table_end > f->ninsns) jelly_vm_panic();

        int32_t d = (int32_t)ins->imm;
        for(uint32_t i = 0; i < ncases; i++) {
          const jelly_insn* ci = &insns[table_first + i];
          if((uint32_t)ci->a == kind) {
            d = (int32_t)ci->imm;
            break;
          }
        }
        int32_t npc = (int32_t)table_end + d;
        if(npc < 0 || npc > (int32_t)f->ninsns) jelly_vm_panic();
        fr->pc = (uint32_t)npc;
        break;
      }
      case JOP_CASE_KIND:
        // Data-only; should never execute if bytecode is validated.
        jelly_vm_panic();
      case JOP_NEG_I32: {
        // Defined wraparound modulo 2^32 (avoid C signed overflow UB for INT32_MIN).
        uint32_t x = load_u32(&fr->rf, ins->b);
        store_u32(&fr->rf, ins->a, (uint32_t)0u - x);
        break;
      }
      case JOP_NEG_I64: {
        // Defined wraparound modulo 2^64 (avoid C signed overflow UB for INT64_MIN).
        uint64_t x = (uint64_t)load_i64(&fr->rf, ins->b);
        store_i64(&fr->rf, ins->a, (int64_t)(0ull - x));
        break;
      }
      case JOP_NEG_F32: {
        float x = load_f32(&fr->rf, ins->b);
        store_f32(&fr->rf, ins->a, -x);
        break;
      }
      case JOP_NEG_F64: {
        double x = load_f64(&fr->rf, ins->b);
        store_f64(&fr->rf, ins->a, -x);
        break;
      }
      case JOP_NOT_BOOL: {
        uint32_t x = load_u32(&fr->rf, ins->b);
        store_u32(&fr->rf, ins->a, (uint32_t)(x == 0));
        break;
      }
      case JOP_CALL: {
        if(ins->imm >= m->nfuncs) jelly_vm_panic();
        const jelly_bc_function* cf = &m->funcs[ins->imm];
        uint32_t first = ins->b;
        uint32_t na = ins->c;
        if(first + na > fr->rf.nregs) jelly_vm_panic();
        uint32_t caller_i = vm->call_frames_len - 1u;
        push_frame(vm, m, cf, f, caller_i, ins->a, first, na, NULL, 1);
        break;
      }
      case JOP_CALLR: {
        jelly_function* fn = (jelly_function*)load_ptr(&fr->rf, ins->b);
        if(!fn) jelly_vm_panic();
        if(fn->h.kind != (uint32_t)JELLY_OBJ_FUNCTION) jelly_vm_panic();
        if(fn->func_index >= m->nfuncs) jelly_vm_panic();
        const jelly_bc_function* cf = &m->funcs[fn->func_index];
        uint32_t first = ins->imm;
        uint32_t na = ins->c;
        if(first + na > fr->rf.nregs) jelly_vm_panic();
        uint32_t caller_i = vm->call_frames_len - 1u;
        push_frame(vm, m, cf, f, caller_i, ins->a, first, na, fn, 1);
        break;
      }
      case JOP_ADD_I32: {
        // Defined wraparound modulo 2^32 (avoid C signed overflow UB).
        uint32_t a = load_u32(&fr->rf, ins->b);
        uint32_t b = load_u32(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, a + b);
        break;
      }
      case JOP_ADD_F32: {
        float a = load_f32(&fr->rf, ins->b);
        float b = load_f32(&fr->rf, ins->c);
        store_f32(&fr->rf, ins->a, a + b);
        break;
      }
      case JOP_ADD_F64: {
        double a = load_f64(&fr->rf, ins->b);
        double b = load_f64(&fr->rf, ins->c);
        store_f64(&fr->rf, ins->a, a + b);
        break;
      }
      case JOP_EQ_I32: {
        int32_t a = load_i32ish(&fr->rf, ins->b);
        int32_t b = load_i32ish(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, (uint32_t)(a == b));
        break;
      }
      case JOP_LT_I32: {
        int32_t a = load_i32ish(&fr->rf, ins->b);
        int32_t b = load_i32ish(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, (uint32_t)(a < b));
        break;
      }
      case JOP_EQ_F32: {
        float a = load_f32(&fr->rf, ins->b);
        float b = load_f32(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, (uint32_t)(a == b));
        break;
      }
      case JOP_EQ_F64: {
        double a = load_f64(&fr->rf, ins->b);
        double b = load_f64(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, (uint32_t)(a == b));
        break;
      }
      case JOP_SEXT_I64: {
        int32_t x = (int32_t)load_u32(&fr->rf, ins->b);
        store_i64(&fr->rf, ins->a, (int64_t)x);
        break;
      }
      case JOP_I32_FROM_I64: {
        // Defined wraparound/truncation modulo 2^32.
        uint64_t x = (uint64_t)load_i64(&fr->rf, ins->b);
        store_u32(&fr->rf, ins->a, (uint32_t)x);
        break;
      }
      case JOP_F64_FROM_I32: {
        int32_t x = (int32_t)load_u32(&fr->rf, ins->b);
        store_f64(&fr->rf, ins->a, (double)x);
        break;
      }
      case JOP_I32_FROM_F64: {
        double x = load_f64(&fr->rf, ins->b);
        uint32_t out_u32 = 0;
        if(!checked_f64_to_i32(vm, x, &out_u32)) break;
        store_u32(&fr->rf, ins->a, out_u32);
        break;
      }
      case JOP_F64_FROM_I64: {
        int64_t x = load_i64(&fr->rf, ins->b);
        store_f64(&fr->rf, ins->a, (double)x);
        break;
      }
      case JOP_I64_FROM_F64: {
        double x = load_f64(&fr->rf, ins->b);
        int64_t out_i64 = 0;
        if(!checked_f64_to_i64(vm, x, &out_i64)) break;
        store_i64(&fr->rf, ins->a, out_i64);
        break;
      }
      case JOP_F32_FROM_I32: {
        int32_t x = (int32_t)load_u32(&fr->rf, ins->b);
        store_f32(&fr->rf, ins->a, (float)x);
        break;
      }
      case JOP_I32_FROM_F32: {
        float x = load_f32(&fr->rf, ins->b);
        uint32_t out_u32 = 0;
        if(!checked_f64_to_i32(vm, (double)x, &out_u32)) break;
        store_u32(&fr->rf, ins->a, out_u32);
        break;
      }
      case JOP_SUB_I32: {
        // Defined wraparound modulo 2^32 (avoid C signed overflow UB).
        uint32_t a = load_u32(&fr->rf, ins->b);
        uint32_t b = load_u32(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, a - b);
        break;
      }
      case JOP_SUB_F32: {
        float a = load_f32(&fr->rf, ins->b);
        float b = load_f32(&fr->rf, ins->c);
        store_f32(&fr->rf, ins->a, a - b);
        break;
      }
      case JOP_SUB_F64: {
        double a = load_f64(&fr->rf, ins->b);
        double b = load_f64(&fr->rf, ins->c);
        store_f64(&fr->rf, ins->a, a - b);
        break;
      }
      case JOP_MUL_I32: {
        // Defined wraparound modulo 2^32 (avoid C signed overflow UB).
        uint32_t a = load_u32(&fr->rf, ins->b);
        uint32_t b = load_u32(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, a * b);
        break;
      }
      case JOP_ADD_I64: {
        uint64_t a = (uint64_t)load_i64(&fr->rf, ins->b);
        uint64_t b = (uint64_t)load_i64(&fr->rf, ins->c);
        store_i64(&fr->rf, ins->a, (int64_t)(a + b));
        break;
      }
      case JOP_SUB_I64: {
        uint64_t a = (uint64_t)load_i64(&fr->rf, ins->b);
        uint64_t b = (uint64_t)load_i64(&fr->rf, ins->c);
        store_i64(&fr->rf, ins->a, (int64_t)(a - b));
        break;
      }
      case JOP_MUL_I64: {
        uint64_t a = (uint64_t)load_i64(&fr->rf, ins->b);
        uint64_t b = (uint64_t)load_i64(&fr->rf, ins->c);
        store_i64(&fr->rf, ins->a, (int64_t)(a * b));
        break;
      }
      case JOP_EQ_I64: {
        int64_t a = load_i64(&fr->rf, ins->b);
        int64_t b = load_i64(&fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, (uint32_t)(a == b));
        break;
      }
      case JOP_MUL_F32: {
        float a = load_f32(&fr->rf, ins->b);
        float b = load_f32(&fr->rf, ins->c);
        store_f32(&fr->rf, ins->a, a * b);
        break;
      }
      case JOP_MUL_F64: {
        double a = load_f64(&fr->rf, ins->b);
        double b = load_f64(&fr->rf, ins->c);
        store_f64(&fr->rf, ins->a, a * b);
        break;
      }
      case JOP_TO_DYN: {
        jelly_value v = box_from_typed(vm, m, f, &fr->rf, ins->b);
        store_val(&fr->rf, ins->a, v);
        break;
      }
      case JOP_FROM_DYN_I32: {
        jelly_value v = load_val(&fr->rf, ins->b);
        if(!jelly_is_i32(v)) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_i32 type mismatch"); goto exception_dispatch; }
        store_u32(&fr->rf, ins->a, (uint32_t)jelly_as_i32(v));
        break;
      }
      case JOP_FROM_DYN_I64: {
        jelly_value v = load_val(&fr->rf, ins->b);
        if(jelly_is_i32(v)) {
          store_i64(&fr->rf, ins->a, (int64_t)jelly_as_i32(v));
          break;
        }
        if(!jelly_is_box_i64(v)) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_i64 type mismatch"); goto exception_dispatch; }
        store_i64(&fr->rf, ins->a, jelly_as_box_i64(v));
        break;
      }
      case JOP_FROM_DYN_F64: {
        jelly_value v = load_val(&fr->rf, ins->b);
        if(jelly_is_i32(v)) {
          store_f64(&fr->rf, ins->a, (double)jelly_as_i32(v));
          break;
        }
        if(!jelly_is_box_f64(v)) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_f64 type mismatch"); goto exception_dispatch; }
        store_f64(&fr->rf, ins->a, jelly_as_box_f64(v));
        break;
      }
      case JOP_FROM_DYN_BOOL: {
        jelly_value v = load_val(&fr->rf, ins->b);
        if(!jelly_is_bool(v)) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_bool type mismatch"); goto exception_dispatch; }
        store_u32(&fr->rf, ins->a, (uint32_t)jelly_as_bool(v));
        break;
      }
      case JOP_FROM_DYN_ATOM: {
        jelly_value v = load_val(&fr->rf, ins->b);
        if(!jelly_is_atom(v)) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_atom type mismatch"); goto exception_dispatch; }
        store_u32(&fr->rf, ins->a, jelly_as_atom(v));
        break;
      }
      case JOP_FROM_DYN_PTR: {
        jelly_value v = load_val(&fr->rf, ins->b);
        jelly_type_kind dk = reg_kind(m, f, ins->a);
        uint32_t want = expected_obj_kind_for_typed_ptr(dk);
        if(want == 0) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_ptr dst not pointer-kind"); goto exception_dispatch; }
        if(jelly_is_null(v)) {
          store_ptr(&fr->rf, ins->a, NULL);
          break;
        }
        if(!jelly_is_ptr(v)) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_ptr type mismatch"); goto exception_dispatch; }
        if(jelly_obj_kind_of(v) != want) { (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "from_dyn_ptr kind mismatch"); goto exception_dispatch; }
        store_ptr(&fr->rf, ins->a, jelly_as_ptr(v));
        break;
      }
      case JOP_SPILL_PUSH: {
        jelly_value v = load_val(&fr->rf, ins->a);
        spill_push(vm, v);
        break;
      }
      case JOP_SPILL_POP: {
        jelly_value v = spill_pop(vm);
        store_val(&fr->rf, ins->a, v);
        break;
      }
      case JOP_PHYSEQ: {
        jelly_value vb = box_from_typed(vm, m, f, &fr->rf, ins->b);
        jelly_value vc = box_from_typed(vm, m, f, &fr->rf, ins->c);
        store_u32(&fr->rf, ins->a, (uint32_t)(vb == vc));
        break;
      }
      case JOP_KINDOF: {
        jelly_value v = load_val(&fr->rf, ins->b);
        store_u32(&fr->rf, ins->a, kindof_dynamic(v));
        break;
      }
      case JOP_LIST_NIL:
        store_ptr(&fr->rf, ins->a, NULL);
        break;
      case JOP_LIST_CONS: {
        jelly_value head = box_from_typed(vm, m, f, &fr->rf, ins->b);
        jelly_gc_push_root(vm, head);
        jelly_list* tail = (jelly_list*)load_ptr(&fr->rf, ins->c);
        uint32_t type_id = f->reg_types[ins->a];
        jelly_list* node = jelly_list_cons(vm, type_id, head, tail);
        jelly_gc_pop_roots(vm, 1);
        store_ptr(&fr->rf, ins->a, node);
        break;
      }
      case JOP_LIST_HEAD: {
        jelly_list* node = (jelly_list*)load_ptr(&fr->rf, ins->b);
        if(!node) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "list_head on nil"); goto exception_dispatch; }
        store_from_boxed(m, f, &fr->rf, ins->a, node->head);
        break;
      }
      case JOP_LIST_TAIL: {
        jelly_list* node = (jelly_list*)load_ptr(&fr->rf, ins->b);
        if(!node) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "list_tail on nil"); goto exception_dispatch; }
        store_ptr(&fr->rf, ins->a, node->tail);
        break;
      }
      case JOP_LIST_IS_NIL: {
        void* p = load_ptr(&fr->rf, ins->b);
        store_u32(&fr->rf, ins->a, (uint32_t)(p == NULL));
        break;
      }
      case JOP_ARRAY_NEW: {
        uint32_t len = load_u32(&fr->rf, ins->b);
        uint32_t type_id = f->reg_types[ins->a];
        jelly_array* a = jelly_array_new(vm, type_id, len);
        store_ptr(&fr->rf, ins->a, a);
        break;
      }
      case JOP_ARRAY_LEN: {
        jelly_array* a = (jelly_array*)load_ptr(&fr->rf, ins->b);
        if(!a) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "array_len on null"); goto exception_dispatch; }
        store_u32(&fr->rf, ins->a, a->length);
        break;
      }
      case JOP_ARRAY_GET: {
        jelly_array* a = (jelly_array*)load_ptr(&fr->rf, ins->b);
        if(!a) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "array_get on null"); goto exception_dispatch; }
        uint32_t idx = load_u32(&fr->rf, ins->c);
        if(idx >= a->length) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "array_get index out of bounds"); goto exception_dispatch; }
        store_from_boxed(m, f, &fr->rf, ins->a, a->data[idx]);
        break;
      }
      case JOP_ARRAY_SET: {
        jelly_array* a = (jelly_array*)load_ptr(&fr->rf, ins->b);
        if(!a) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "array_set on null"); goto exception_dispatch; }
        uint32_t idx = load_u32(&fr->rf, ins->c);
        if(idx >= a->length) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "array_set index out of bounds"); goto exception_dispatch; }
        jelly_value v = box_from_typed(vm, m, f, &fr->rf, ins->a);
        a->data[idx] = v;
        break;
      }
      case JOP_BYTES_NEW: {
        uint32_t len = load_u32(&fr->rf, ins->b);
        uint32_t type_id = f->reg_types[ins->a];
        jelly_bytes* b = jelly_bytes_new(vm, type_id, len);
        store_ptr(&fr->rf, ins->a, b);
        break;
      }
      case JOP_BYTES_LEN: {
        jelly_bytes* b = (jelly_bytes*)load_ptr(&fr->rf, ins->b);
        if(!b) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_len on null"); goto exception_dispatch; }
        store_u32(&fr->rf, ins->a, b->length);
        break;
      }
      case JOP_BYTES_GET_U8: {
        jelly_bytes* b = (jelly_bytes*)load_ptr(&fr->rf, ins->b);
        if(!b) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_get_u8 on null"); goto exception_dispatch; }
        uint32_t idx = load_u32(&fr->rf, ins->c);
        if(idx >= b->length) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "bytes_get_u8 index out of bounds"); goto exception_dispatch; }
        store_u32(&fr->rf, ins->a, (uint32_t)b->data[idx]);
        break;
      }
      case JOP_BYTES_SET_U8: {
        jelly_bytes* b = (jelly_bytes*)load_ptr(&fr->rf, ins->b);
        if(!b) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_set_u8 on null"); goto exception_dispatch; }
        uint32_t idx = load_u32(&fr->rf, ins->c);
        if(idx >= b->length) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "bytes_set_u8 index out of bounds"); goto exception_dispatch; }
        uint32_t v = load_u32(&fr->rf, ins->a);
        if(v > 255u) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "bytes_set_u8 value out of range"); goto exception_dispatch; }
        b->data[idx] = (uint8_t)v;
        break;
      }
      case JOP_BYTES_CONCAT2: {
        jelly_bytes* x = (jelly_bytes*)load_ptr(&fr->rf, ins->b);
        if(!x) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_concat2 on null lhs"); goto exception_dispatch; }
        jelly_bytes* y = (jelly_bytes*)load_ptr(&fr->rf, ins->c);
        if(!y) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_concat2 on null rhs"); goto exception_dispatch; }
        uint64_t total64 = (uint64_t)x->length + (uint64_t)y->length;
        if(total64 > 0xffffffffu) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "bytes_concat2 length overflow"); goto exception_dispatch; }
        uint32_t total = (uint32_t)total64;
        uint32_t type_id = f->reg_types[ins->a];
        jelly_bytes* outb = jelly_bytes_new(vm, type_id, total);
        jelly_gc_push_root(vm, jelly_from_ptr(outb));
        if(x->length) memcpy(outb->data, x->data, x->length);
        if(y->length) memcpy(outb->data + x->length, y->data, y->length);
        store_ptr(&fr->rf, ins->a, outb);
        jelly_gc_pop_roots(vm, 1);
        break;
      }
      case JOP_BYTES_CONCAT_MANY: {
        jelly_array* parts = (jelly_array*)load_ptr(&fr->rf, ins->b);
        if(!parts) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_concat_many on null"); goto exception_dispatch; }

        uint64_t total64 = 0;
        for(uint32_t i = 0; i < parts->length; i++) {
          jelly_value pv = parts->data[i];
          if(jelly_is_null(pv)) { (void)jelly_vm_trap(vm, JELLY_TRAP_NULL_DEREF, "bytes_concat_many element is null"); goto exception_dispatch; }
          if(!jelly_is_ptr(pv) || jelly_obj_kind_of(pv) != (uint32_t)JELLY_OBJ_BYTES) {
            (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "bytes_concat_many element not bytes");
            goto exception_dispatch;
          }
          jelly_bytes* b = (jelly_bytes*)jelly_as_ptr(pv);
          total64 += (uint64_t)b->length;
          if(total64 > 0xffffffffu) { (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "bytes_concat_many length overflow"); goto exception_dispatch; }
        }

        uint32_t total = (uint32_t)total64;
        uint32_t type_id = f->reg_types[ins->a];
        jelly_bytes* outb = jelly_bytes_new(vm, type_id, total);
        jelly_gc_push_root(vm, jelly_from_ptr(outb));
        uint32_t w = 0;
        for(uint32_t i = 0; i < parts->length; i++) {
          jelly_bytes* b = (jelly_bytes*)jelly_as_ptr(parts->data[i]);
          if(b->length) {
            memcpy(outb->data + w, b->data, b->length);
            w += b->length;
          }
        }
        store_ptr(&fr->rf, ins->a, outb);
        jelly_gc_pop_roots(vm, 1);
        break;
      }
      case JOP_OBJ_NEW: {
        uint32_t type_id = f->reg_types[ins->a];
        jelly_object* o = jelly_object_new(vm, type_id);
        store_ptr(&fr->rf, ins->a, o);
        break;
      }
      case JOP_OBJ_HAS_ATOM: {
        jelly_object* o = (jelly_object*)load_ptr(&fr->rf, ins->b);
        if(!o) jelly_vm_panic();
        int has = jelly_object_has(o, ins->imm);
        store_u32(&fr->rf, ins->a, (uint32_t)(has != 0));
        break;
      }
      case JOP_OBJ_GET_ATOM: {
        jelly_object* o = (jelly_object*)load_ptr(&fr->rf, ins->b);
        if(!o) jelly_vm_panic();
        uint32_t atom_id = ins->imm;

        // Back-compat: only enable prototype delegation when the module reserves
        // atom 0 for "__proto__" (compiler-enforced for jellyc-emitted modules).
        int proto_enabled = 0;
        if(m->natoms > JELLY_ATOM___PROTO__ && m->atoms[JELLY_ATOM___PROTO__]) {
          proto_enabled = (strcmp(m->atoms[JELLY_ATOM___PROTO__], "__proto__") == 0);
        }
        if(!proto_enabled) {
          jelly_value v = jelly_object_get(o, atom_id);
          store_from_boxed(m, f, &fr->rf, ins->a, v);
          break;
        }

        // `__proto__` is a raw link; do not traverse when querying it directly.
        if(atom_id == JELLY_ATOM___PROTO__) {
          jelly_value v = jelly_object_get(o, atom_id);
          store_from_boxed(m, f, &fr->rf, ins->a, v);
          break;
        }

        // Prototype delegation: walk `__proto__` chain on miss.
        jelly_object* cur = o;
        jelly_value v = jelly_make_null();
        for(uint32_t steps = 0;; steps++) {
          if(jelly_object_has(cur, atom_id)) {
            v = jelly_object_get(cur, atom_id);
            break;
          }
          jelly_object* p = cur->proto;
          if(!p) break;
          cur = p;
          if(steps > 1024u) {
            (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "prototype chain too deep");
            goto exception_dispatch;
          }
        }
        store_from_boxed(m, f, &fr->rf, ins->a, v);
        break;
      }
      case JOP_OBJ_SET_ATOM: {
        jelly_object* o = (jelly_object*)load_ptr(&fr->rf, ins->b);
        if(!o) jelly_vm_panic();
        jelly_value v = box_from_typed(vm, m, f, &fr->rf, ins->a);
        uint32_t atom_id = ins->imm;
        jelly_object_set(o, atom_id, v);
        // Maintain prototype cache when enabled and setting `__proto__`.
        if(atom_id == JELLY_ATOM___PROTO__) {
          int proto_enabled = 0;
          if(m->natoms > JELLY_ATOM___PROTO__ && m->atoms[JELLY_ATOM___PROTO__]) {
            proto_enabled = (strcmp(m->atoms[JELLY_ATOM___PROTO__], "__proto__") == 0);
          }
          if(proto_enabled) {
            if(jelly_is_ptr(v)) {
              void* p = jelly_as_ptr(v);
              if(p) {
                jelly_obj_header* h = (jelly_obj_header*)p;
                if(h->kind == (uint32_t)JELLY_OBJ_OBJECT) {
                  o->proto = (jelly_object*)p;
                } else {
                  o->proto = NULL;
                }
              } else {
                o->proto = NULL;
              }
            } else {
              o->proto = NULL;
            }
          }
        }
        break;
      }
      case JOP_OBJ_GET: {
        jelly_object* o = (jelly_object*)load_ptr(&fr->rf, ins->b);
        if(!o) jelly_vm_panic();
        uint32_t atom_id = load_u32(&fr->rf, ins->c);

        if(atom_id >= m->natoms) {
          (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "obj_get atom id out of range");
          goto exception_dispatch;
        }

        // Back-compat: only enable prototype delegation when the module reserves
        // atom 0 for "__proto__" (compiler-enforced for jellyc-emitted modules).
        int proto_enabled = 0;
        if(m->natoms > JELLY_ATOM___PROTO__ && m->atoms[JELLY_ATOM___PROTO__]) {
          proto_enabled = (strcmp(m->atoms[JELLY_ATOM___PROTO__], "__proto__") == 0);
        }
        if(!proto_enabled) {
          jelly_value v = jelly_object_get(o, atom_id);
          store_from_boxed(m, f, &fr->rf, ins->a, v);
          break;
        }

        // `__proto__` is a raw link; do not traverse when querying it directly.
        if(atom_id == JELLY_ATOM___PROTO__) {
          jelly_value v = jelly_object_get(o, atom_id);
          store_from_boxed(m, f, &fr->rf, ins->a, v);
          break;
        }

        // Prototype delegation: walk `__proto__` chain on miss.
        jelly_object* cur = o;
        jelly_value v = jelly_make_null();
        for(uint32_t steps = 0;; steps++) {
          if(jelly_object_has(cur, atom_id)) {
            v = jelly_object_get(cur, atom_id);
            break;
          }
          jelly_object* p = cur->proto;
          if(!p) break;
          cur = p;
          if(steps > 1024u) {
            (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "prototype chain too deep");
            goto exception_dispatch;
          }
        }

        store_from_boxed(m, f, &fr->rf, ins->a, v);
        break;
      }
      case JOP_OBJ_SET: {
        jelly_object* o = (jelly_object*)load_ptr(&fr->rf, ins->b);
        if(!o) jelly_vm_panic();
        uint32_t atom_id = load_u32(&fr->rf, ins->c);

        if(atom_id >= m->natoms) {
          (void)jelly_vm_trap(vm, JELLY_TRAP_BOUNDS, "obj_set atom id out of range");
          goto exception_dispatch;
        }

        jelly_value v = box_from_typed(vm, m, f, &fr->rf, ins->a);
        jelly_object_set(o, atom_id, v);

        // Maintain prototype cache when enabled and setting `__proto__` dynamically.
        if(atom_id == JELLY_ATOM___PROTO__) {
          int proto_enabled = 0;
          if(m->natoms > JELLY_ATOM___PROTO__ && m->atoms[JELLY_ATOM___PROTO__]) {
            proto_enabled = (strcmp(m->atoms[JELLY_ATOM___PROTO__], "__proto__") == 0);
          }
          if(proto_enabled) {
            if(jelly_is_ptr(v)) {
              void* p = jelly_as_ptr(v);
              if(p) {
                jelly_obj_header* h = (jelly_obj_header*)p;
                if(h->kind == (uint32_t)JELLY_OBJ_OBJECT) {
                  o->proto = (jelly_object*)p;
                } else {
                  o->proto = NULL;
                }
              } else {
                o->proto = NULL;
              }
            } else {
              o->proto = NULL;
            }
          }
        }
        break;
      }
      case JOP_RET: {
        jelly_value ret = box_from_typed(vm, m, f, &fr->rf, ins->a);
        uint32_t caller_dst = fr->caller_dst;
        uint8_t has_caller = fr->has_caller;
        exc_pop_for_frame(vm, vm->call_frames_len - 1u);
        rf_free(&fr->rf);
        vm->call_frames_len--;
        if(!has_caller) {
          free(vm->call_frames);
          vm->call_frames = NULL;
          vm->call_frames_len = 0;
          vm->call_frames_cap = 0;
          free(vm->exc_handlers);
          vm->exc_handlers = NULL;
          vm->exc_handlers_len = 0;
          vm->exc_handlers_cap = 0;
          if(out) *out = ret;
          return JELLY_EXEC_OK;
        }
        if(vm->call_frames_len == 0) jelly_vm_panic();
        frames = (call_frame*)vm->call_frames;
        call_frame* caller = &frames[vm->call_frames_len - 1u];
        store_from_boxed(m, caller->f, &caller->rf, caller_dst, ret);
        break;
      }
      default:
        jelly_vm_panic();
        break;
    }

    if(vm->exc_pending) goto exception_dispatch;
    continue;

exception_dispatch:
    if(!vm->exc_pending) continue;

    if(vm->exc_handlers_len == 0) {
      unwind_all_frames(vm);
      free(vm->exc_handlers);
      vm->exc_handlers = NULL;
      vm->exc_handlers_len = 0;
      vm->exc_handlers_cap = 0;
      return JELLY_EXEC_TRAP;
    }

    exc_handler h;
    if(!exc_pop(vm, &h)) jelly_vm_panic();

    // Unwind frames above handler target.
    while(vm->call_frames_len - 1u > h.frame_index) {
      uint32_t idx = vm->call_frames_len - 1u;
      frames = (call_frame*)vm->call_frames;
      exc_pop_for_frame(vm, idx);
      rf_free(&frames[idx].rf);
      vm->call_frames_len--;
    }

    if(vm->call_frames_len == 0) jelly_vm_panic();
    frames = (call_frame*)vm->call_frames;
    call_frame* target = &frames[h.frame_index];
    target->pc = h.catch_pc;
    store_val(&target->rf, h.dst_reg, vm->exc_payload);

    vm->exc_pending = 0;
    vm->exc_payload = jelly_make_null();
    jelly_vm_clear_trap(vm);
    continue;
  }
}

jelly_exec_status jelly_vm_exec_status(jelly_vm* vm, const jelly_bc_module* m, jelly_value* out) {
  if(!vm || !m || !m->funcs || m->entry >= m->nfuncs) jelly_vm_panic();
  jelly_vm_clear_trap(vm);
  const jelly_bc_function* f = &m->funcs[m->entry];
  jelly_gc_init(vm);
  vm->running_module = m;
  jelly_exec_status st = exec_entry(vm, m, f, out);
  vm->running_module = NULL;
  if(st != JELLY_EXEC_OK) {
    unwind_all_frames(vm);
  }
  return st;
}

jelly_value jelly_vm_exec(jelly_vm* vm, const jelly_bc_module* m) {
  jelly_value out = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(vm, m, &out);
  if(st != JELLY_EXEC_OK) jelly_vm_panic();
  return out;
}

