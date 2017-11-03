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
#include <stdio.h>
#include <stdlib.h>

op_result op_closure(exec_ctx* ctx, const jelly_insn* ins) {
  jelly_vm* vm = ctx->vm;
  const jelly_bc_module* m = ctx->m;
  const jelly_bc_function* f = ctx->f;
  call_frame* fr = ctx->fr;

  uint32_t type_id = f->reg_types[ins->a];
  uint32_t ncaps = (uint32_t)ins->c;
  jelly_value tmp[256];
  for(uint32_t i = 0; i < ncaps; i++) {
    tmp[i] = vm_box_from_typed(vm, m, f, &fr->rf, (uint32_t)ins->b + i);
    jelly_gc_push_root(vm, tmp[i]);
  }
  jelly_function* clo = jelly_closure_new(vm, type_id, ins->imm, ncaps, tmp);
  jelly_gc_pop_roots(vm, ncaps);
  vm_store_ptr(&fr->rf, ins->a, clo);
  if(getenv("JELLY_TRACE_CLOSURE")) {
    uint32_t func_idx = (uint32_t)(f - m->funcs);
    fprintf(stderr, "[JELLY_TRACE] Closure: func=%u dst_reg=%u cap_src_reg=%u ncaps=%u target_func=%u\n",
            (unsigned)func_idx, (unsigned)ins->a, (unsigned)ins->b, (unsigned)ncaps, (unsigned)ins->imm);
  }
  return OP_CONTINUE;
}

op_result op_bind_this(exec_ctx* ctx, const jelly_insn* ins) {
  jelly_vm* vm = ctx->vm;
  const jelly_bc_module* m = ctx->m;
  const jelly_bc_function* f = ctx->f;
  call_frame* fr = ctx->fr;

  jelly_function* base = (jelly_function*)vm_load_ptr(&fr->rf, ins->b);
  if(!base) jelly_vm_panic();
  if(base->h.kind != (uint32_t)JELLY_OBJ_FUNCTION) jelly_vm_panic();
  jelly_value thisv = vm_box_from_typed(vm, m, f, &fr->rf, ins->c);
  jelly_gc_push_root(vm, thisv);
  uint32_t type_id = f->reg_types[ins->a];
  jelly_function* bound = jelly_function_bind_this(vm, type_id, base, thisv);
  jelly_gc_pop_roots(vm, 1);
  vm_store_ptr(&fr->rf, ins->a, bound);
  return OP_CONTINUE;
}

op_result op_call(exec_ctx* ctx, const jelly_insn* ins) {
  jelly_vm* vm = ctx->vm;
  const jelly_bc_module* m = ctx->m;
  const jelly_bc_function* f = ctx->f;
  call_frame* fr = ctx->fr;

  uint32_t fi = ins->imm;
  if(jelly_is_native_builtin(fi)) {
    jelly_invoke_native_builtin(ctx, ins, fi, ins->b);
    return OP_CONTINUE;
  }
  uint32_t bytecode_idx = fi - 1u; /* 0=native, 1+=m->funcs[0+] */
  if(bytecode_idx >= m->nfuncs) jelly_vm_panic();
  const jelly_bc_function* cf = &m->funcs[bytecode_idx];
  uint32_t first = ins->b;
  uint32_t na = ins->c;
  if(first + na > fr->rf.nregs) jelly_vm_panic();
  uint32_t caller_i = vm->call_frames_len - 1u;
  if(!vm_push_frame(vm, m, cf, f, caller_i, ins->a, first, na, NULL, 1)) return OP_CONTINUE;
  return OP_CONTINUE;
}

op_result op_callr(exec_ctx* ctx, const jelly_insn* ins) {
  jelly_vm* vm = ctx->vm;
  const jelly_bc_module* m = ctx->m;
  const jelly_bc_function* f = ctx->f;
  call_frame* fr = ctx->fr;

  jelly_value v = vm_load_val(&fr->rf, ins->b);
  if(getenv("JELLY_TRACE_CLOSURE")) {
    uint32_t func_idx = (uint32_t)(f - m->funcs);
    fprintf(stderr, "[JELLY_TRACE] CallR: func=%u pc=%u callee_reg=%u val=0x%zx is_ptr=%d is_null=%d cap_start=%u nregs=%u\n",
            (unsigned)func_idx, (unsigned)(fr->pc > 0 ? fr->pc - 1 : 0), (unsigned)ins->b, (size_t)v,
            jelly_is_ptr(v) ? 1 : 0, jelly_is_null(v) ? 1 : 0,
            (unsigned)f->cap_start, (unsigned)fr->rf.nregs);
  }
  if(!jelly_is_ptr(v) || jelly_is_null(v)) {
    uint32_t func_idx = (uint32_t)(f - m->funcs);
    uint32_t pc = fr->pc > 0 ? fr->pc - 1 : 0;
    static char buf[128];
    (void)snprintf(buf, sizeof buf, "callr callee not a function (func=%u pc=%u reg=%u val=0x%zx)",
                   (unsigned)func_idx, (unsigned)pc, (unsigned)ins->b, (size_t)v);
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, buf);
    return OP_CONTINUE;
  }
  jelly_function* fn = (jelly_function*)jelly_as_ptr(v);
  if(!fn) {
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "callr callee is null");
    return OP_CONTINUE;
  }
  if(fn->h.kind != (uint32_t)JELLY_OBJ_FUNCTION) {
    (void)jelly_vm_trap(vm, JELLY_TRAP_TYPE_MISMATCH, "callr callee not a function");
    return OP_CONTINUE;
  }
  uint32_t fi = fn->func_index;
  if(jelly_is_native_builtin(fi)) {
    jelly_invoke_native_builtin(ctx, ins, fi, ins->imm);
    return OP_CONTINUE;
  }
  uint32_t bytecode_idx = fi - 1u;
  if(bytecode_idx >= m->nfuncs) jelly_vm_panic();
  const jelly_bc_function* cf = &m->funcs[bytecode_idx];
  uint32_t first = ins->imm;
  uint32_t na = ins->c;
  if(first + na > fr->rf.nregs) jelly_vm_panic();
  uint32_t caller_i = vm->call_frames_len - 1u;
  if(!vm_push_frame(vm, m, cf, f, caller_i, ins->a, first, na, fn, 1)) return OP_CONTINUE;
  return OP_CONTINUE;
}
