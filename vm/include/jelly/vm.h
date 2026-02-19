#ifndef JELLY_VM_H
#define JELLY_VM_H

/*
  VM API + typed register storage rules.

  MVP interpreter will:
  - allocate a register frame sized by function.nregs (<=256)
  - interpret typed slots using reg_types[]
  - execute bytecode instructions
  - use boxed spill stack (jelly_value only)
*/

#include <stdint.h>
#include <stddef.h>

#include "type.h"
#include "value.h"
#include "bytecode.h"

typedef enum jelly_exec_status {
  JELLY_EXEC_OK = 0,
  JELLY_EXEC_TRAP = 1,
} jelly_exec_status;

typedef enum jelly_trap_code {
  JELLY_TRAP_NONE = 0,
  JELLY_TRAP_TYPE_MISMATCH = 1,
  JELLY_TRAP_BOUNDS = 2,
  JELLY_TRAP_NULL_DEREF = 3,
  JELLY_TRAP_THROWN = 4,
} jelly_trap_code;

typedef struct jelly_vm {
  // Spill stack stores boxed values only (Dynamic).
  jelly_value* spill;
  uint32_t spill_len;
  uint32_t spill_cap;

  // --- GC (MVP)
  void* gc_objects;            // internal GC list head
  size_t gc_bytes_live;        // bytes currently allocated on GC heap
  size_t gc_next_collect;      // allocation threshold to trigger a collection (0 = disabled)

  jelly_value* gc_roots;       // temporary root stack
  uint32_t gc_roots_len;
  uint32_t gc_roots_cap;

  // Debug/stats (useful for tests and tuning).
  uint64_t gc_collections;
  uint64_t gc_freed_objects;

  // Active interpreter frames (for precise GC root enumeration).
  void* call_frames;           // internal array (exec.c)
  uint32_t call_frames_len;
  uint32_t call_frames_cap;

  const jelly_bc_module* running_module; // non-NULL while executing

  // --- exceptions (VM-level try/catch/throw; also used to catch runtime traps)
  void* exc_handlers;           // internal array (vm/interp.c)
  uint32_t exc_handlers_len;
  uint32_t exc_handlers_cap;
  uint8_t exc_pending;
  jelly_value exc_payload;      // valid when exc_pending!=0

  // --- last trap (when exec_status returns JELLY_EXEC_TRAP)
  jelly_trap_code trap_code;
  const char* trap_msg; // static string owned by VM/runtime
} jelly_vm;

// Lifecycle helpers for embedders (C/C++).
// Jelly is designed to allow multiple independent VM instances in one process.
// All mutable runtime state must live in `jelly_vm` (no global singletons).
void jelly_vm_init(jelly_vm* vm);
void jelly_vm_shutdown(jelly_vm* vm);

// Typed slot layout helpers.
// These are purely about storage sizing/alignment for typed registers.
size_t jelly_slot_align(jelly_type_kind k);
size_t jelly_slot_size(jelly_type_kind k);

// Execute a module entry function (MVP shape; will evolve).
jelly_value jelly_vm_exec(jelly_vm* vm, const jelly_bc_module* m);

// Execute module entry with explicit status.
// On `JELLY_EXEC_TRAP`, the VM records `trap_code` + `trap_msg`.
jelly_exec_status jelly_vm_exec_status(jelly_vm* vm, const jelly_bc_module* m, jelly_value* out);

static inline void jelly_vm_clear_trap(jelly_vm* vm) {
  if(!vm) return;
  vm->trap_code = JELLY_TRAP_NONE;
  vm->trap_msg = NULL;
}

static inline jelly_trap_code jelly_vm_last_trap_code(const jelly_vm* vm) {
  return vm ? vm->trap_code : JELLY_TRAP_NONE;
}

static inline const char* jelly_vm_last_trap_msg(const jelly_vm* vm) {
  return vm ? vm->trap_msg : NULL;
}

#endif /* JELLY_VM_H */

