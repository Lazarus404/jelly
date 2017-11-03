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

static jelly_exec_status exec_entry(jelly_vm* vm, const jelly_bc_module* m, const jelly_bc_function* entry, jelly_value* out) {
  vm->call_frames = NULL;
  vm->call_frames_len = 0;
  vm->call_frames_cap = 0;
  free(vm->const_fun_cache);
  vm->const_fun_cache = NULL;
  vm->const_fun_cache_len = 0;
  if(m && m->nfuncs) {
    size_t cache_len = (size_t)JELLY_NATIVE_BUILTIN_COUNT + (size_t)m->nfuncs;
    vm->const_fun_cache = calloc(cache_len, sizeof(void*));
    if(!vm->const_fun_cache) jelly_vm_panic();
    vm->const_fun_cache_len = (uint32_t)cache_len;
  }
  free(vm->exc_handlers);
  vm->exc_handlers = NULL;
  vm->exc_handlers_len = 0;
  vm->exc_handlers_cap = 0;
  vm->exc_pending = 0;
  vm->exc_payload = jelly_make_null();
  if(!vm_push_frame(vm, m, entry, entry, 0, 0, 0, 0, NULL, 0)) return JELLY_EXEC_TRAP;

  exec_ctx ctx = { .vm = vm, .m = m, .out = out };
  return vm_exec_loop(&ctx);
}

jelly_exec_status jelly_vm_exec_status(jelly_vm* vm, const jelly_bc_module* m, jelly_value* out) {
  if(!vm || !m || !m->funcs || m->entry >= m->nfuncs) jelly_vm_panic();
  jelly_vm_clear_trap(vm);
  const jelly_bc_function* f = &m->funcs[m->entry];
  jelly_gc_init(vm);
  vm->running_module = m;
  jelly_exec_status st = exec_entry(vm, m, f, out);
  vm->running_module = NULL;
  if(st != JELLY_EXEC_OK) vm_unwind_all_frames(vm);
  return st;
}

jelly_value jelly_vm_exec(jelly_vm* vm, const jelly_bc_module* m) {
  jelly_value out = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(vm, m, &out);
  if(st != JELLY_EXEC_OK) jelly_vm_panic();
  return out;
}
