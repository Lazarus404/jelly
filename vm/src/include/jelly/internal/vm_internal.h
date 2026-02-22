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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
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

// Internal VM runtime structs shared across translation units.
// Not part of the public embedder API.
#ifndef JELLY_INTERNAL_VM_INTERNAL_H
#define JELLY_INTERNAL_VM_INTERNAL_H

#include <stdint.h>

#include <jelly.h>

/* Opaque in public API; full definition here for VM implementation. */
struct jelly_bc_module {
  uint32_t version;
  uint32_t features;
  uint32_t ntypes;
  const jelly_type_entry* types;
  uint32_t nsigs;
  const jelly_fun_sig* sigs;
  uint32_t natoms;
  const char* atoms_data;
  const char* const* atoms;
  uint32_t nconst_i64;
  const int64_t* const_i64;
  uint32_t nconst_f64;
  const double* const_f64;
  uint32_t nconst_bytes;
  const uint32_t* const_bytes_len;
  const uint32_t* const_bytes_off;
  const uint8_t* const_bytes_data;
  uint32_t nfuncs;
  const jelly_bc_function* funcs;
  uint32_t entry;
};

struct jelly_vm {
  jelly_value* spill;
  uint32_t spill_len;
  uint32_t spill_cap;

  /* Safety limits */
  uint64_t fuel_limit;
  uint64_t fuel_remaining;
  uint32_t max_bytes_len;
  uint32_t max_array_len;

  void* gc_objects;
  size_t gc_bytes_live;
  size_t gc_next_collect;

  jelly_value* gc_roots;
  uint32_t gc_roots_len;
  uint32_t gc_roots_cap;

  uint64_t gc_collections;
  uint64_t gc_freed_objects;

  void* call_frames;
  uint32_t call_frames_len;
  uint32_t call_frames_cap;
  uint32_t call_frames_max;

  void* const_fun_cache;
  uint32_t const_fun_cache_len;

  /* Frame layout cache + register frame memory pool (performance-critical for calls). */
  void* frame_layouts; /* frame_layout[] (len = m->nfuncs) for running_module */
  uint32_t frame_layouts_len;
  const jelly_bc_module* frame_layouts_mod;
  void* rf_free_list; /* rf_mem_block* */

  const jelly_bc_module* running_module;

  void* exc_handlers;
  uint32_t exc_handlers_len;
  uint32_t exc_handlers_cap;
  uint8_t exc_pending;
  jelly_value exc_payload;

  jelly_trap_code trap_code;
  const char* trap_msg;
};

typedef struct reg_frame {
  uint8_t* mem;
  const uint32_t* off; /* nregs offsets */
  uint32_t nregs;
  uint32_t total;      /* total bytes in mem */
  uint8_t off_shared;  /* 1 if `off` points into cached layout */
} reg_frame;

typedef struct frame_layout {
  uint32_t nregs;
  uint32_t total;
  uint32_t* off;
} frame_layout;

typedef struct rf_mem_block {
  struct rf_mem_block* next;
  uint32_t size;
} rf_mem_block;

typedef struct call_frame {
  const jelly_bc_function* f;
  reg_frame rf;
  uint32_t pc;
  uint32_t caller_dst; // destination register in caller
  uint32_t exc_base;   // exc_handlers_len at frame entry (O(1) cleanup)
  uint8_t has_caller;
} call_frame;

typedef struct exc_handler {
  uint32_t frame_index;
  uint32_t catch_pc;
  uint32_t dst_reg;
  uint8_t trap_only; /* 1: traps only (do not catch THROWN), 0: catch everything */
} exc_handler;

/* --- exec context (for opcode handlers) --- */
typedef struct exec_ctx {
  jelly_vm* vm;
  const jelly_bc_module* m;
  const jelly_bc_function* f;
  call_frame* fr;
  call_frame* frames;
  jelly_value* out;
} exec_ctx;

typedef enum op_result {
  OP_CONTINUE,
  OP_RETURN,   /* exec_entry should return JELLY_EXEC_OK */
  OP_TRAP,     /* goto exception_dispatch */
} op_result;

typedef op_result (*op_handler_fn)(exec_ctx* ctx, const jelly_insn* ins);

/* --- trap/panic (used by interp + ops) --- */
jelly_exec_status jelly_vm_trap(jelly_vm* vm, jelly_trap_code code, const char* msg);
void jelly_vm_panic(void);

/* --- reg.c --- */
jelly_type_kind vm_reg_kind(const jelly_bc_module* m, const jelly_bc_function* f, uint32_t r);
void* vm_reg_ptr(reg_frame* rf, uint32_t r);
uint32_t vm_load_u32(reg_frame* rf, uint32_t r);
void vm_store_u32(reg_frame* rf, uint32_t r, uint32_t v);
void vm_store_u32_masked(reg_frame* rf, uint32_t r, uint32_t v, jelly_type_kind k);
float vm_load_f32(reg_frame* rf, uint32_t r);
void vm_store_f32(reg_frame* rf, uint32_t r, float v);
int32_t vm_load_i32ish(reg_frame* rf, uint32_t r);
int64_t vm_load_i64(reg_frame* rf, uint32_t r);
void vm_store_i64(reg_frame* rf, uint32_t r, int64_t v);
double vm_load_f64(reg_frame* rf, uint32_t r);
void vm_store_f64(reg_frame* rf, uint32_t r, double v);
uint16_t vm_load_f16_bits(reg_frame* rf, uint32_t r);
void vm_store_f16_bits(reg_frame* rf, uint32_t r, uint16_t bits);
float vm_f16_bits_to_f32(uint16_t bits);
uint16_t vm_f32_to_f16_bits(float f);
jelly_value vm_load_val(reg_frame* rf, uint32_t r);
void vm_store_val(reg_frame* rf, uint32_t r, jelly_value v);
void* vm_load_ptr(reg_frame* rf, uint32_t r);
void vm_store_ptr(reg_frame* rf, uint32_t r, void* p);

/* --- spill.c --- */
void vm_spill_push(jelly_vm* vm, jelly_value v);
jelly_value vm_spill_pop(jelly_vm* vm);

/* --- box.c --- */
jelly_value vm_box_from_typed(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf, uint32_t r);
void vm_store_from_boxed(const jelly_bc_module* m, const jelly_bc_function* f, reg_frame* rf, uint32_t dst, jelly_value v);
int vm_checked_f64_to_i32(jelly_vm* vm, double x, uint32_t* out_u32);
int vm_checked_f64_to_i64(jelly_vm* vm, double x, int64_t* out_i64);
uint32_t vm_kindof_dynamic(jelly_value v);
uint32_t vm_expected_obj_kind_for_typed_ptr(jelly_type_kind k);

/* --- call.c --- */
void vm_copy_arg_strict(const jelly_bc_module* m,
                        const jelly_bc_function* caller_f, reg_frame* caller_rf, uint32_t src,
                        const jelly_bc_function* callee_f, reg_frame* callee_rf, uint32_t dst);
void vm_init_args_and_caps(jelly_vm* vm, const jelly_bc_module* m,
                           const jelly_bc_function* callee_f, reg_frame* callee_rf,
                           const jelly_bc_function* caller_f, reg_frame* caller_rf,
                           uint32_t first_arg, uint32_t nargs,
                           const jelly_function* funobj);

/* --- frame.c --- */
void vm_rf_release(jelly_vm* vm, reg_frame* rf);
const frame_layout* vm_get_frame_layout(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* f);
void vm_frame_cache_shutdown(jelly_vm* vm);
int vm_push_frame(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* callee_f,
                  const jelly_bc_function* caller_f, uint32_t caller_frame_index, uint32_t caller_dst,
                  uint32_t first_arg, uint32_t nargs, const jelly_function* funobj, uint8_t has_caller);

/* --- ops/ops_builtins.c --- */
#define JELLY_NATIVE_BUILTIN_COUNT 1u
int jelly_is_native_builtin(uint32_t func_index);
void jelly_invoke_native_builtin(exec_ctx* ctx, const jelly_insn* ins, uint32_t func_index, uint32_t first_arg_reg);

/* --- exc.c --- */
void vm_exc_push(jelly_vm* vm, uint32_t frame_index, uint32_t catch_pc, uint32_t dst_reg, uint8_t trap_only);
int vm_exc_pop(jelly_vm* vm, exc_handler* out);
void vm_exc_pop_for_frame(jelly_vm* vm, uint32_t frame_index);
void vm_unwind_all_frames(jelly_vm* vm);
/* Returns 1 if exec_entry should return JELLY_EXEC_TRAP, 0 to continue loop. */
int vm_exc_dispatch(jelly_vm* vm, jelly_value* out);

/* --- ops/dispatch.c --- */
/* Legacy single-instruction dispatch (used as fallback). */
op_result op_dispatch(exec_ctx* ctx, const jelly_insn* ins);
/* Interpreter loop (runs until completion/trap). */
jelly_exec_status vm_exec_loop(exec_ctx* ctx);

#endif /* JELLY_INTERNAL_VM_INTERNAL_H */

