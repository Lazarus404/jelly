#include <jelly.h>

#include <jelly/vm.h>
#include <jelly/gc.h>

#include <string.h>
#include <stdlib.h>

// Placeholder translation unit for the focused VM.
// As the VM grows, core state/opcodes/dispatch will live under `src/`.

int jellyvm_dummy_symbol_to_keep_archive(void);
int jellyvm_dummy_symbol_to_keep_archive(void) {
	return (int)JELLY_VERSION;
}

void jelly_vm_init(jelly_vm* vm) {
  if(!vm) return;
  memset(vm, 0, sizeof(*vm));
  // Leave gc_next_collect=0 here to allow embedders to configure it before first exec.
}

void jelly_vm_shutdown(jelly_vm* vm) {
  if(!vm) return;
  jelly_gc_shutdown(vm);
  free(vm->spill);
  vm->spill = NULL;
  vm->spill_len = 0;
  vm->spill_cap = 0;

  // `exec_entry` frees call frames on normal completion, but shut down defensively.
  free(vm->call_frames);
  vm->call_frames = NULL;
  vm->call_frames_len = 0;
  vm->call_frames_cap = 0;

  free(vm->exc_handlers);
  vm->exc_handlers = NULL;
  vm->exc_handlers_len = 0;
  vm->exc_handlers_cap = 0;
  vm->exc_pending = 0;
  vm->exc_payload = jelly_make_null();

  vm->running_module = NULL;
}

