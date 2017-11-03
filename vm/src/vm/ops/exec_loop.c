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

#include <string.h>

jelly_exec_status vm_exec_loop(exec_ctx* ctx) {
  if(!ctx || !ctx->vm || !ctx->m) jelly_vm_panic();
  jelly_vm* vm = ctx->vm;
  const jelly_bc_module* m = ctx->m;
  jelly_value* out = ctx->out;

  for(;;) {
  CHECK_EXC:
    if(vm->exc_pending) {
      if(vm_exc_dispatch(vm, out)) return JELLY_EXEC_TRAP;
    }
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
#ifndef NDEBUG
    if(fr->pc >= f->ninsns) jelly_vm_panic();
#endif
    const jelly_insn* ins = &f->insns[fr->pc++];
    ctx->f = f;
    ctx->fr = fr;
    ctx->frames = frames;

#if defined(JELLYVM_REFERENCE_INTERP)
    // Reference mode: execute everything via the canonical dispatcher.
    // This is slower, but minimizes duplicated semantics and is useful for conformance testing.
    op_result r = op_dispatch(ctx, ins);
    if(r == OP_RETURN) return JELLY_EXEC_OK;
    if(r == OP_TRAP) goto CHECK_EXC;
#else
    // Hot opcodes are handled inline here to avoid paying an extra `op_dispatch()` call
    // per instruction. Everything else falls back to `op_dispatch()`.
    switch((jelly_op)ins->op) {
      case JOP_CONST_I32: {
        vm_store_u32(&fr->rf, ins->a, ins->imm);
        break;
      }
      case JOP_CONST_BOOL: {
        vm_store_u32(&fr->rf, ins->a, (uint32_t)(ins->c & 1u));
        break;
      }
      case JOP_MOV: {
        const jelly_type_entry* types = m->types;
        uint32_t a = ins->a, b = ins->b;
        jelly_type_kind k = types[f->reg_types[a]].kind;
        size_t sz = jelly_slot_size(k);
        memmove(vm_reg_ptr(&fr->rf, a), vm_reg_ptr(&fr->rf, b), sz);
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
        uint32_t cond = vm_load_u32(&fr->rf, ins->a);
        if(cond != 0) {
          int32_t d = (int32_t)ins->imm;
          int32_t npc = (int32_t)fr->pc + d;
          if(npc < 0 || npc > (int32_t)f->ninsns) jelly_vm_panic();
          fr->pc = (uint32_t)npc;
        }
        break;
      }
      case JOP_ADD_I32: {
        jelly_type_kind k = m->types[f->reg_types[ins->a]].kind;
        uint32_t a = vm_load_u32(&fr->rf, ins->b);
        uint32_t b = vm_load_u32(&fr->rf, ins->c);
        uint32_t v = a + b;
        if(k == JELLY_T_I32) {
          vm_store_u32(&fr->rf, ins->a, v);
        } else {
          vm_store_u32_masked(&fr->rf, ins->a, v, k);
        }
        break;
      }
      case JOP_SUB_I32: {
        jelly_type_kind k = m->types[f->reg_types[ins->a]].kind;
        uint32_t a = vm_load_u32(&fr->rf, ins->b);
        uint32_t b = vm_load_u32(&fr->rf, ins->c);
        uint32_t v = a - b;
        if(k == JELLY_T_I32) {
          vm_store_u32(&fr->rf, ins->a, v);
        } else {
          vm_store_u32_masked(&fr->rf, ins->a, v, k);
        }
        break;
      }
      case JOP_EQ_I32: {
        int32_t a = (int32_t)vm_load_u32(&fr->rf, ins->b);
        int32_t b = (int32_t)vm_load_u32(&fr->rf, ins->c);
        vm_store_u32(&fr->rf, ins->a, (uint32_t)((a == b) ? 1 : 0));
        break;
      }
      case JOP_LT_I32: {
        int32_t a = (int32_t)vm_load_u32(&fr->rf, ins->b);
        int32_t b = (int32_t)vm_load_u32(&fr->rf, ins->c);
        vm_store_u32(&fr->rf, ins->a, (uint32_t)((a < b) ? 1 : 0));
        break;
      }
      case JOP_CONST_FUN: {
        if(ins->imm >= vm->const_fun_cache_len) jelly_vm_panic();
        jelly_function** cache = (jelly_function**)vm->const_fun_cache;
        jelly_function* fn = cache[ins->imm];
        if(!fn) {
          uint32_t type_id = f->reg_types[ins->a];
          fn = jelly_function_new(vm, type_id, ins->imm);
          cache[ins->imm] = fn;
        }
        vm_store_ptr(&fr->rf, ins->a, fn);
        break;
      }
      case JOP_CALL: {
        uint32_t fi = ins->imm;
        if(jelly_is_native_builtin(fi)) {
          jelly_invoke_native_builtin(ctx, ins, fi, ins->b);
        } else {
          uint32_t bytecode_idx = fi - 1u;
          if(bytecode_idx >= m->nfuncs) jelly_vm_panic();
          const jelly_bc_function* cf = &m->funcs[bytecode_idx];
          uint32_t first = ins->b;
          uint32_t na = ins->c;
          if(first + na > fr->rf.nregs) jelly_vm_panic();
          uint32_t caller_i = vm->call_frames_len - 1u;
          if(!vm_push_frame(vm, m, cf, f, caller_i, ins->a, first, na, NULL, 1)) goto CHECK_EXC;
        }
        break;
      }
      case JOP_RET: {
        uint32_t caller_dst = fr->caller_dst;
        uint8_t has_caller = fr->has_caller;
        if(fr->exc_base > vm->exc_handlers_len) jelly_vm_panic();
        vm->exc_handlers_len = fr->exc_base;

        if(!has_caller) {
          jelly_value ret = vm_box_from_typed(vm, m, f, &fr->rf, ins->a);
          vm_rf_release(vm, &fr->rf);
          vm->call_frames_len--;
          free(vm->call_frames);
          vm->call_frames = NULL;
          vm->call_frames_len = 0;
          vm->call_frames_cap = 0;
          free(vm->const_fun_cache);
          vm->const_fun_cache = NULL;
          vm->const_fun_cache_len = 0;
          free(vm->exc_handlers);
          vm->exc_handlers = NULL;
          vm->exc_handlers_len = 0;
          vm->exc_handlers_cap = 0;
          if(out) *out = ret;
          return JELLY_EXEC_OK;
        }

        if(vm->call_frames_len < 2u) jelly_vm_panic();
        call_frame* caller = &frames[vm->call_frames_len - 2u];

        uint32_t ret_tid = f->reg_types[ins->a];
        if(caller->f->reg_types[caller_dst] == ret_tid) {
          jelly_type_kind k = m->types[ret_tid].kind;
          size_t sz = jelly_slot_size(k);
          memmove(vm_reg_ptr(&caller->rf, caller_dst), vm_reg_ptr(&fr->rf, ins->a), sz);
          vm_rf_release(vm, &fr->rf);
          vm->call_frames_len--;
          break;
        }

        jelly_value ret = vm_box_from_typed(vm, m, f, &fr->rf, ins->a);
        vm_rf_release(vm, &fr->rf);
        vm->call_frames_len--;
        vm_store_from_boxed(m, caller->f, &caller->rf, caller_dst, ret);
        break;
      }
      default: {
        op_result r = op_dispatch(ctx, ins);
        if(r == OP_RETURN) return JELLY_EXEC_OK;
        if(r == OP_TRAP) goto CHECK_EXC;
        break;
      }
    }
#endif
  }
}

