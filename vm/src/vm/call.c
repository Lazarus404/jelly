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
#include <string.h>

void vm_copy_arg_strict(const jelly_bc_module* m,
                        const jelly_bc_function* caller_f, reg_frame* caller_rf, uint32_t src,
                        const jelly_bc_function* callee_f, reg_frame* callee_rf, uint32_t dst) {
  // Keep strict type checking in debug builds; in release, rely on bytecode validity.
#ifndef NDEBUG
  if(caller_f->reg_types[src] != callee_f->reg_types[dst]) {
    jelly_vm_panic();
  }
#endif

  const uint32_t tid = callee_f->reg_types[dst];
  jelly_type_kind k = m->types[tid].kind;

  uint8_t* dstp = callee_rf->mem + callee_rf->off[dst];
  const uint8_t* srcp = caller_rf->mem + caller_rf->off[src];

  // Fast path: 32-bit "value in u32 slot" types (common in numeric code like fib).
  if(k == JELLY_T_BOOL || k == JELLY_T_ATOM || k == JELLY_T_I8 || k == JELLY_T_I16 || k == JELLY_T_I32) {
    *(uint32_t*)dstp = *(const uint32_t*)srcp;
    return;
  }

  size_t sz = jelly_slot_size(k);
  memmove(dstp, srcp, sz);
}

void vm_init_args_and_caps(jelly_vm* vm, const jelly_bc_module* m,
                           const jelly_bc_function* callee_f, reg_frame* callee_rf,
                           const jelly_bc_function* caller_f, reg_frame* caller_rf,
                           uint32_t first_arg, uint32_t nargs,
                           const jelly_function* funobj) {
  uint32_t arg_base = 0;
  if(funobj && !jelly_is_null(funobj->bound_this)) {
    if(callee_rf->nregs < 1) jelly_vm_panic();
    vm_store_from_boxed(m, callee_f, callee_rf, 0, funobj->bound_this);
    arg_base = 1;
  }

  if(arg_base + nargs > callee_rf->nregs) jelly_vm_panic();
  for(uint32_t i = 0; i < nargs; i++) {
    vm_copy_arg_strict(m, caller_f, caller_rf, first_arg + i, callee_f, callee_rf, arg_base + i);
  }

  if(funobj && funobj->ncaps) {
    if(funobj->ncaps > callee_rf->nregs) jelly_vm_panic();
    uint32_t cap_start = (m->features & (uint32_t)JELLY_BC_FEAT_CAP_START) && callee_f->cap_start < callee_rf->nregs
      ? callee_f->cap_start
      : callee_rf->nregs - funobj->ncaps;
    if(getenv("JELLY_TRACE_CLOSURE")) {
      uint32_t callee_idx = (uint32_t)(callee_f - m->funcs);
      fprintf(stderr, "[JELLY_TRACE] vm_init_args_and_caps: callee_func=%u ncaps=%u cap_start=%u nregs=%u feat=%u callee_f->cap_start=%u\n",
              (unsigned)callee_idx, (unsigned)funobj->ncaps, (unsigned)cap_start,
              (unsigned)callee_rf->nregs, (unsigned)((m->features & (uint32_t)JELLY_BC_FEAT_CAP_START) ? 1 : 0),
              (unsigned)callee_f->cap_start);
    }
    if(cap_start < arg_base + nargs) jelly_vm_panic();
    for(uint32_t i = 0; i < funobj->ncaps; i++) {
      vm_store_from_boxed(m, callee_f, callee_rf, cap_start + i, funobj->caps[i]);
    }
  }
}
