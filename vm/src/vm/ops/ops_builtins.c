/**
 * Copyright 2017 - Jahred Love
 *
 * Native builtins: C implementations for operations that cannot be done
 * correctly or efficiently in pure bytecode (e.g. Math.sqrt).
 */

#include <jelly/internal.h>
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* Must match jellyc/src/jlyb/format.rs NATIVE_BUILTIN_* constants. */
#define JELLY_NATIVE_BUILTIN_MATH_SQRT 0u
#define JELLY_NATIVE_BUILTIN_SYSTEM_EXIT 1u
#define JELLY_NATIVE_BUILTIN_I32_TO_BYTES 2u
#define JELLY_NATIVE_BUILTIN_F64_TO_BYTES 3u
#define JELLY_NATIVE_BUILTIN_COUNT 4u

/* math_sqrt(x: F64) -> F64. arg_reg = first arg, dst_reg = result. */
static void native_math_sqrt(exec_ctx* ctx, uint32_t dst_reg, uint32_t arg_reg) {
  call_frame* fr = ctx->fr;
  double x = vm_load_f64(&fr->rf, arg_reg);
  double y = sqrt(x);
  vm_store_f64(&fr->rf, dst_reg, y);
}

/* System.exit() -> never returns. Exit code 123 signals REPL to exit. */
static void native_system_exit(exec_ctx* ctx, uint32_t dst_reg, uint32_t arg_reg) {
  (void)ctx;
  (void)dst_reg;
  (void)arg_reg;
  exit(123);
}

/* I32.to_bytes(x: I32) -> Bytes. Converts integer to decimal string (UTF-8). */
static void native_i32_to_bytes(exec_ctx* ctx, uint32_t dst_reg, uint32_t arg_reg) {
  int32_t x = (int32_t)vm_load_u32(&ctx->fr->rf, arg_reg);
  char buf[16];
  int n = snprintf(buf, sizeof buf, "%" PRId32, x);
  if (n < 0 || (size_t)n >= sizeof buf) n = 0;
  jelly_vm* vm = ctx->vm;
  const jelly_bc_function* f = ctx->fr->f;
  uint32_t type_id = f->reg_types[dst_reg];
  jelly_bytes* b = jelly_bytes_new(vm, type_id, (uint32_t)n);
  if (b && n > 0) {
    for (int i = 0; i < n; i++) b->data[i] = (uint8_t)buf[i];
  }
  vm_store_ptr(&ctx->fr->rf, dst_reg, b);
}

/* F64.to_bytes(x: F64) -> Bytes. Converts double to string (UTF-8), %g format. */
static void native_f64_to_bytes(exec_ctx* ctx, uint32_t dst_reg, uint32_t arg_reg) {
  double x = vm_load_f64(&ctx->fr->rf, arg_reg);
  char buf[64];
  int n = snprintf(buf, sizeof buf, "%g", x);
  if (n < 0 || (size_t)n >= sizeof buf) n = 0;
  jelly_vm* vm = ctx->vm;
  const jelly_bc_function* f = ctx->fr->f;
  uint32_t type_id = f->reg_types[dst_reg];
  jelly_bytes* b = jelly_bytes_new(vm, type_id, (uint32_t)n);
  if (b && n > 0) {
    for (int i = 0; i < n; i++) b->data[i] = (uint8_t)buf[i];
  }
  vm_store_ptr(&ctx->fr->rf, dst_reg, b);
}

int jelly_is_native_builtin(uint32_t func_index) {
  return func_index < JELLY_NATIVE_BUILTIN_COUNT;
}

/* Invoke native builtin. For JOP_CALL: first_arg=ins->b. For JOP_CALLR: first_arg=ins->imm. */
void jelly_invoke_native_builtin(exec_ctx* ctx, const jelly_insn* ins, uint32_t func_index, uint32_t first_arg_reg) {
  if(func_index >= JELLY_NATIVE_BUILTIN_COUNT) jelly_vm_panic();
  if(func_index == JELLY_NATIVE_BUILTIN_MATH_SQRT) {
    native_math_sqrt(ctx, ins->a, first_arg_reg);
    return;
  }
  if(func_index == JELLY_NATIVE_BUILTIN_SYSTEM_EXIT) {
    native_system_exit(ctx, ins->a, first_arg_reg);
    return;
  }
  if(func_index == JELLY_NATIVE_BUILTIN_I32_TO_BYTES) {
    native_i32_to_bytes(ctx, ins->a, first_arg_reg);
    return;
  }
  if(func_index == JELLY_NATIVE_BUILTIN_F64_TO_BYTES) {
    native_f64_to_bytes(ctx, ins->a, first_arg_reg);
    return;
  }
  jelly_vm_panic();
}
