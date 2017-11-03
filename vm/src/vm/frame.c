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

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static size_t align_up(size_t x, size_t a) {
  return (x + (a - 1u)) & ~(a - 1u);
}

static void vm_free_frame_layouts(jelly_vm* vm) {
  if(!vm) return;
  frame_layout* ls = (frame_layout*)vm->frame_layouts;
  for(uint32_t i = 0; i < vm->frame_layouts_len; i++) {
    free(ls[i].off);
    ls[i].off = NULL;
    ls[i].nregs = 0;
    ls[i].total = 0;
  }
  free(vm->frame_layouts);
  vm->frame_layouts = NULL;
  vm->frame_layouts_len = 0;
  vm->frame_layouts_mod = NULL;
}

static void vm_free_rf_pool(jelly_vm* vm) {
  if(!vm) return;
  rf_mem_block* b = (rf_mem_block*)vm->rf_free_list;
  while(b) {
    rf_mem_block* next = b->next;
    free(b);
    b = next;
  }
  vm->rf_free_list = NULL;
}

void vm_frame_cache_shutdown(jelly_vm* vm) {
  vm_free_frame_layouts(vm);
  vm_free_rf_pool(vm);
}

static void vm_prepare_frame_layouts(jelly_vm* vm, const jelly_bc_module* m) {
  if(!vm || !m) return;
  if(vm->frame_layouts_mod == m) return;
  vm_free_frame_layouts(vm);
  vm->frame_layouts_len = m->nfuncs;
  vm->frame_layouts = calloc((size_t)m->nfuncs, sizeof(frame_layout));
  if(!vm->frame_layouts) jelly_vm_panic();
  vm->frame_layouts_mod = m;
}

static uint8_t* vm_rf_alloc_mem(jelly_vm* vm, uint32_t size) {
  if(size == 0) return NULL;
  rf_mem_block** pp = (rf_mem_block**)&vm->rf_free_list;
  rf_mem_block* b = (rf_mem_block*)vm->rf_free_list;
  while(b) {
    if(b->size == size) {
      *pp = b->next;
      return (uint8_t*)(b + 1);
    }
    pp = &b->next;
    b = b->next;
  }
  rf_mem_block* nb = (rf_mem_block*)malloc(sizeof(rf_mem_block) + (size_t)size);
  if(!nb) jelly_vm_panic();
  nb->next = NULL;
  nb->size = size;
  return (uint8_t*)(nb + 1);
}

void vm_rf_release(jelly_vm* vm, reg_frame* rf) {
  if(!rf) return;
  if(rf->mem) {
    rf_mem_block* b = ((rf_mem_block*)rf->mem) - 1;
    b->next = (rf_mem_block*)vm->rf_free_list;
    vm->rf_free_list = b;
  }
  if(rf->off && !rf->off_shared) {
    free((void*)rf->off);
  }
  rf->mem = NULL;
  rf->off = NULL;
  rf->nregs = 0;
  rf->total = 0;
  rf->off_shared = 0;
}

const frame_layout* vm_get_frame_layout(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* f) {
  if(!vm || !m || !f) jelly_vm_panic();
  vm_prepare_frame_layouts(vm, m);
  if(vm->frame_layouts_len != m->nfuncs) jelly_vm_panic();

  ptrdiff_t idx = f - m->funcs;
  if(idx < 0 || (uint32_t)idx >= m->nfuncs) jelly_vm_panic();
  frame_layout* fl = &((frame_layout*)vm->frame_layouts)[(uint32_t)idx];
  if(fl->off) return fl;

  fl->nregs = f->nregs;
  fl->off = (uint32_t*)malloc(sizeof(uint32_t) * (size_t)fl->nregs);
  if(!fl->off) jelly_vm_panic();

  size_t total = 0;
  for(uint32_t i = 0; i < fl->nregs; i++) {
    jelly_type_kind k = vm_reg_kind(m, f, i);
    size_t a = jelly_slot_align(k);
    size_t s = jelly_slot_size(k);
    total = align_up(total, a);
    fl->off[i] = (uint32_t)total;
    total += s;
  }
  fl->total = (uint32_t)total;
  return fl;
}

int vm_push_frame(jelly_vm* vm,
                  const jelly_bc_module* m,
                  const jelly_bc_function* callee_f,
                  const jelly_bc_function* caller_f,
                  uint32_t caller_frame_index,
                  uint32_t caller_dst,
                  uint32_t first_arg,
                  uint32_t nargs,
                  const jelly_function* funobj,
                  uint8_t has_caller) {
  if(vm->call_frames_max && vm->call_frames_len >= vm->call_frames_max) {
    vm->trap_code = JELLY_TRAP_STACK_OVERFLOW;
    vm->trap_msg = "stack overflow";
    vm->exc_pending = 1;
    vm->exc_payload = jelly_make_i32((int32_t)JELLY_TRAP_STACK_OVERFLOW);
    return 0;
  }
  call_frame* frames = (call_frame*)vm->call_frames;
  if(vm->call_frames_len == vm->call_frames_cap) {
    uint32_t ncap = vm->call_frames_cap ? (vm->call_frames_cap * 2u) : 16u;
    call_frame* nf = (call_frame*)realloc(frames, sizeof(call_frame) * (size_t)ncap);
    if(!nf) jelly_vm_panic();
    frames = nf;
    vm->call_frames = nf;
    vm->call_frames_cap = ncap;
  }

  call_frame* fr = &frames[vm->call_frames_len++];
  fr->f = callee_f;
  fr->pc = 0;
  fr->caller_dst = caller_dst;
  fr->exc_base = vm->exc_handlers_len;
  fr->has_caller = has_caller;
  const frame_layout* fl = vm_get_frame_layout(vm, m, callee_f);
  fr->rf.nregs = fl->nregs;
  fr->rf.off = fl->off;
  fr->rf.total = fl->total;
  fr->rf.off_shared = 1u;
  fr->rf.mem = vm_rf_alloc_mem(vm, fl->total);
  // Frame memory blocks are reused; clear them to keep GC root scanning safe.
  // Also initialize Dynamic slots to `null` (tagged), not "null pointer".
  if(fr->rf.mem && fl->total) {
    memset(fr->rf.mem, 0, (size_t)fl->total);
    for(uint32_t r = 0; r < fl->nregs; r++) {
      uint32_t tid = callee_f->reg_types[r];
      if(m->types[tid].kind == JELLY_T_DYNAMIC) {
        jelly_value v = jelly_make_null();
        memcpy(fr->rf.mem + fl->off[r], &v, sizeof(v));
      }
    }
  }
  if(has_caller) {
    if(caller_frame_index >= vm->call_frames_len - 1u) jelly_vm_panic();
    call_frame* caller = &frames[caller_frame_index];
    // Fast path: common case for hot code (no bound `this`, no captures).
    // If caller/callee types match, copy args directly into callee regs.
    // This preserves semantics because arguments are passed by value (typed-slot copy).
    if(!funobj && nargs > 0u && nargs <= 4u) {
#ifndef NDEBUG
      if(first_arg + nargs > caller->rf.nregs) jelly_vm_panic();
#endif
      uint8_t ok = 1u;
      for(uint32_t i = 0; i < nargs; i++) {
        uint32_t src = first_arg + i;
        uint32_t dst = i;
        uint32_t src_tid = caller_f->reg_types[src];
        uint32_t dst_tid = callee_f->reg_types[dst];
        if(src_tid != dst_tid) {
          ok = 0u;
          break;
        }
        jelly_type_kind k = m->types[dst_tid].kind;
        size_t sz = jelly_slot_size(k);
        uint8_t* dstp = fr->rf.mem + fr->rf.off[dst];
        const uint8_t* srcp = caller->rf.mem + caller->rf.off[src];
        if(sz == 4) {
          *(uint32_t*)dstp = *(const uint32_t*)srcp;
        } else {
          memmove(dstp, srcp, sz);
        }
      }
      if(ok) return 1;
    }
    vm_init_args_and_caps(vm, m, callee_f, &fr->rf, caller_f, &caller->rf, first_arg, nargs, funobj);
  }
  return 1;
}
