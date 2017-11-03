/**
 * Copyright 2017 - Jahred Love
 *
 * Native builtins: C implementations for operations that cannot be done
 * correctly or efficiently in pure bytecode (e.g. Math.sqrt).
 */

#include <jelly/internal.h>
#include <math.h>

/* Must match jellyc/src/jlyb/format.rs NATIVE_BUILTIN_* constants. */
#define JELLY_NATIVE_BUILTIN_MATH_SQRT 0u
#define JELLY_NATIVE_BUILTIN_COUNT 1u

/* math_sqrt(x: F64) -> F64. arg_reg = first arg, dst_reg = result. */
static void native_math_sqrt(exec_ctx* ctx, uint32_t dst_reg, uint32_t arg_reg) {
  call_frame* fr = ctx->fr;
  double x = vm_load_f64(&fr->rf, arg_reg);
  double y = sqrt(x);
  vm_store_f64(&fr->rf, dst_reg, y);
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
  jelly_vm_panic();
}
