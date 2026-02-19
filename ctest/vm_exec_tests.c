#include <jelly/loader.h>
#include <jelly/vm.h>
#include <jelly/gc.h>
#include <jelly/object.h>
#include <jelly/abstract.h>
#include <jelly/array.h>
#include <jelly/list.h>
#include <jelly/box.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures = 0;

static void failf(const char* test, const char* msg) {
  fprintf(stderr, "[FAIL] %s: %s\n", test, msg);
  failures++;
}

static void assert_true(const char* test, int cond, const char* msg) {
  if(!cond) failf(test, msg);
}

static void assert_i32_eq(const char* test, jelly_value v, int32_t want, const char* msg) {
  if(!jelly_is_i32(v) || jelly_as_i32(v) != want) {
    failf(test, msg);
  }
}

static void wr_u32(uint8_t* p, uint32_t v) {
  p[0] = (uint8_t)(v & 0xffu);
  p[1] = (uint8_t)((v >> 8) & 0xffu);
  p[2] = (uint8_t)((v >> 16) & 0xffu);
  p[3] = (uint8_t)((v >> 24) & 0xffu);
}

static void wr_u64(uint8_t* p, uint64_t v) {
  p[0] = (uint8_t)(v & 0xffu);
  p[1] = (uint8_t)((v >> 8) & 0xffu);
  p[2] = (uint8_t)((v >> 16) & 0xffu);
  p[3] = (uint8_t)((v >> 24) & 0xffu);
  p[4] = (uint8_t)((v >> 32) & 0xffu);
  p[5] = (uint8_t)((v >> 40) & 0xffu);
  p[6] = (uint8_t)((v >> 48) & 0xffu);
  p[7] = (uint8_t)((v >> 56) & 0xffu);
}

static void wr_type(uint8_t* buf, size_t* off, uint8_t kind, uint32_t p0) {
  buf[(*off)++] = kind;
  buf[(*off)++] = 0;
  buf[(*off)++] = 0;
  buf[(*off)++] = 0;
  wr_u32(buf + *off, p0); *off += 4;
  wr_u32(buf + *off, 0); *off += 4;
}

static void wr_insn(uint8_t* buf, size_t* off, uint8_t op, uint8_t a, uint8_t b, uint8_t c, uint32_t imm) {
  buf[(*off)++] = op;
  buf[(*off)++] = a;
  buf[(*off)++] = b;
  buf[(*off)++] = c;
  wr_u32(buf + *off, imm); *off += 4;
}

static void wr_sig(uint8_t* buf, size_t* off, uint32_t ret_type, uint16_t nargs, const uint32_t* args) {
  // u32 ret; u16 nargs; u16 pad; then nargs*u32 args
  wr_u32(buf + *off, ret_type); *off += 4;
  buf[(*off)++] = (uint8_t)(nargs & 0xffu);
  buf[(*off)++] = (uint8_t)((nargs >> 8) & 0xffu);
  buf[(*off)++] = 0;
  buf[(*off)++] = 0;
  for(uint16_t i = 0; i < nargs; i++) {
    wr_u32(buf + *off, args[i]); *off += 4;
  }
}

static void abs_finalizer_inc(void* payload) {
  uint32_t* c = (uint32_t*)payload;
  (*c)++;
}

static int run_exec_add_i32(void) {
  const char* t = "exec_add_i32";
  uint8_t buf[512];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4; // version
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types[0] = I32
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // function0: nregs=3, ninsns=4
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 4); off += 4;
  // reg types
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=1; r1=2; r2=r0+r1; ret r2
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 2, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 3, "result is 3");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_const_bytes_len_and_get_u8(void) {
  const char* t = "exec_const_bytes_len_and_get_u8";
  uint8_t buf[512];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4; // version
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONSTBYTES); off += 4; // features
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry
  wr_u32(buf + off, 1); off += 4; // nconst_bytes

  // types: bytes (0), i32 (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // const_bytes[0] = "hi"
  wr_u32(buf + off, 2); off += 4;
  buf[off++] = (uint8_t)'h';
  buf[off++] = (uint8_t)'i';

  // function0: nregs=4, ninsns=6
  // r0:bytes, r1:i32, r2:i32, r3:i32
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_BYTES, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_LEN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_GET_U8, 3, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 3, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 107, "len + get_u8 matches");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_bytes_concat2_and_get_u8(void) {
  const char* t = "exec_bytes_concat2_and_get_u8";
  uint8_t buf[1024];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4; // version
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONSTBYTES); off += 4; // features
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry
  wr_u32(buf + off, 2); off += 4; // nconst_bytes

  // types: bytes (0), i32 (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // const_bytes[0] = "hi" ; const_bytes[1] = "!"
  wr_u32(buf + off, 2); off += 4; buf[off++] = (uint8_t)'h'; buf[off++] = (uint8_t)'i';
  wr_u32(buf + off, 1); off += 4; buf[off++] = (uint8_t)'!';

  // function0: nregs=6, ninsns=8
  // r0:bytes, r1:bytes, r2:bytes(out), r3:i32(idx2), r4:i32(u8), r5:i32(len)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 8); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_BYTES, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_BYTES, 1, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_CONCAT2, 2, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_GET_U8, 4, 2, 3, 0);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_LEN, 5, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 5, 5, 4, 0); // 3 + '!'(33) = 36
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 36, "len + last byte");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_bytes_concat_many_and_len(void) {
  const char* t = "exec_bytes_concat_many_and_len";
  uint8_t buf[1536];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONSTBYTES); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // nconst_bytes

  // types: bytes(0), i32(1), Array<bytes>(2)
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_ARRAY, 0);

  // const_bytes: "a" "bb" "ccc"
  wr_u32(buf + off, 1); off += 4; buf[off++] = (uint8_t)'a';
  wr_u32(buf + off, 2); off += 4; buf[off++] = (uint8_t)'b'; buf[off++] = (uint8_t)'b';
  wr_u32(buf + off, 3); off += 4; buf[off++] = (uint8_t)'c'; buf[off++] = (uint8_t)'c'; buf[off++] = (uint8_t)'c';

  // function0 regs:
  // r0:bytes, r1:bytes, r2:bytes, r3:Array<bytes>,
  // r4:i32(len=3), r5:i32(0), r6:i32(1), r7:i32(2),
  // r8:bytes(out), r9:i32(outlen)
  wr_u32(buf + off, 10); off += 4;
  wr_u32(buf + off, 14); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_BYTES, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_BYTES, 1, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_BYTES, 2, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 3);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_NEW, 3, 4, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 5, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 6, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 7, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 0, 3, 5, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 1, 3, 6, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 2, 3, 7, 0);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_CONCAT_MANY, 8, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_LEN, 9, 8, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 9, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 6, "concat_many length");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_spill_roundtrip(void) {
  const char* t = "exec_spill_roundtrip";
  uint8_t buf[512];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Dynamic (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // function0: nregs=2, ninsns=5
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 5); off += 4;
  // reg0:I32, reg1:Dynamic
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  // r0=123; r1=ToDyn(r0); spill_push r1; spill_pop r1; ret r1 (boxed -> i32)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 123);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_PUSH, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_POP, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 123, "roundtrips via spill");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_list_head_tail(void) {
  const char* t = "exec_list_head_tail";
  uint8_t buf[768];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), List<I32> (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_LIST, 0); // elem type id = 0
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // function0: nregs=5, ninsns=10
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 10); off += 4;
  // r0: List<I32>, r1: I32, r2: List<I32>, r3: bool, r4: bool
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 2); off += 4;

  // r0 = nil
  wr_insn(buf, &off, (uint8_t)JOP_LIST_NIL, 0, 0, 0, 0);
  // r1 = 7
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 7);
  // r0 = cons(r1, r0)
  wr_insn(buf, &off, (uint8_t)JOP_LIST_CONS, 0, 1, 0, 0);
  // r1 = 9
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 9);
  // r0 = cons(r1, r0)  => list [9,7]
  wr_insn(buf, &off, (uint8_t)JOP_LIST_CONS, 0, 1, 0, 0);
  // r2 = tail(r0)
  wr_insn(buf, &off, (uint8_t)JOP_LIST_TAIL, 2, 0, 0, 0);
  // r3 = phys_eq(r2, r0)  (should be false)
  wr_insn(buf, &off, (uint8_t)JOP_PHYSEQ, 3, 2, 0, 0);
  // r4 = is_nil(r2) (should be false)
  wr_insn(buf, &off, (uint8_t)JOP_LIST_IS_NIL, 4, 2, 0, 0);
  // r1 = head(r2) => should be 7
  wr_insn(buf, &off, (uint8_t)JOP_LIST_HEAD, 1, 2, 0, 0);
  // ret r1
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 7, "head(tail([9,7])) is 7");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_list_is_nil(void) {
  const char* t = "exec_list_is_nil";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), List<I32> (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_LIST, 0); // elem type id = 0
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // function0: nregs=2, ninsns=3
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 1); off += 4; // r0: list
  wr_u32(buf + off, 2); off += 4; // r1: bool

  wr_insn(buf, &off, (uint8_t)JOP_LIST_NIL, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_LIST_IS_NIL, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_true(t, jelly_is_bool(v) && jelly_as_bool(v) == 1, "nil list is_nil => true");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_array_set_get_len(void) {
  const char* t = "exec_array_set_get_len";
  uint8_t buf[1024];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Array<I32> (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_ARRAY, 0); // elem = 0
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // function0: regs:
  // r0:Array<I32>, r1:I32(len), r2:I32(idx), r3:I32(tmp), r4:I32(out), r5:I32(len_out)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 12); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4; // r0 array
  wr_u32(buf + off, 0); off += 4; // r1 i32
  wr_u32(buf + off, 0); off += 4; // r2 i32
  wr_u32(buf + off, 0); off += 4; // r3 i32
  wr_u32(buf + off, 0); off += 4; // r4 i32
  wr_u32(buf + off, 0); off += 4; // r5 i32

  // r1 = 2 (len)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  // r0 = array_new(r1)
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_NEW, 0, 1, 0, 0);
  // idx=0; val=7; set
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 3, 0, 2, 0); // arr[idx]=val
  // idx=1; val=9; set
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 9);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 3, 0, 2, 0);
  // r5 = len(r0)
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_LEN, 5, 0, 0, 0);
  // r4 = get r0[0]
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_GET, 4, 0, 2, 0);
  // ret r4 (should be 7)
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 7, "array_get returns 7");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_array_len(void) {
  const char* t = "exec_array_len";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Array<I32> (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_ARRAY, 0);

  // regs: r0:Array, r1:i32(len), r2:i32(out)
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_NEW, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_LEN, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &out);
  assert_true(t, st == JELLY_EXEC_OK, "exec returns OK");
  if(st != JELLY_EXEC_OK) {
    fprintf(stderr, "[TRAP] %s: code=%u msg=%s\n", t, (unsigned)jelly_vm_last_trap_code(&vm), jelly_vm_last_trap_msg(&vm) ? jelly_vm_last_trap_msg(&vm) : "(null)");
    jelly_vm_shutdown(&vm);
    jelly_bc_free(&m);
    return 1;
  }
  assert_i32_eq(t, out, 2, "array_len returns 2");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_bytes_set_get_len(void) {
  const char* t = "exec_bytes_set_get_len";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), bytes (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);

  // regs: r0:bytes, r1:i32(len), r2:i32(idx), r3:i32(val)
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 10); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4; // r0 bytes
  wr_u32(buf + off, 0); off += 4; // r1 i32
  wr_u32(buf + off, 0); off += 4; // r2 i32
  wr_u32(buf + off, 0); off += 4; // r3 i32

  // len=2; new; set idx0=65('A'); set idx1=66('B'); get idx1; ret
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 65);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_SET_U8, 3, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 66);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_SET_U8, 3, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_GET_U8, 3, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 3, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &out);
  assert_true(t, st == JELLY_EXEC_OK, "exec returns OK");
  if(st != JELLY_EXEC_OK) {
    fprintf(stderr, "[TRAP] %s: code=%u msg=%s\n", t, (unsigned)jelly_vm_last_trap_code(&vm), jelly_vm_last_trap_msg(&vm) ? jelly_vm_last_trap_msg(&vm) : "(null)");
    jelly_vm_shutdown(&vm);
    jelly_bc_free(&m);
    return 1;
  }
  assert_i32_eq(t, out, 66, "bytes get_u8 returns 66");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_obj_get_atom(void) {
  const char* t = "exec_obj_get_atom";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Object (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_OBJECT, 0);

  // atoms: one empty atom (id 0)
  wr_u32(buf + off, 0); off += 4;

  // regs: r0:Object, r1:I32(val), r2:I32(out)
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 5); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // obj=new; val=7; set(atom=123,val); out=get(atom=123); ret out
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_NEW, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_SET_ATOM, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_GET_ATOM, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 7, "object get_atom returns 7");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_obj_has_atom(void) {
  const char* t = "exec_obj_has_atom";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Object (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_OBJECT, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // atoms: one empty atom (id 0)
  wr_u32(buf + off, 0); off += 4;

  // regs: r0:Object, r1:I32(val), r2:Bool(out)
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 5); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4;

  // obj=new; val=7; set(atom=123,val); out=has(atom=123); ret out
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_NEW, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_SET_ATOM, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_HAS_ATOM, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_true(t, jelly_is_bool(v) && jelly_as_bool(v) == 1, "object has_atom => true");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_call_direct_i32(void) {
  const char* t = "exec_call_direct_i32";
  uint8_t buf[1024];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32 (0)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // func0 (caller): nregs=3, ninsns=4
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  // r2 = call func1 (imm=1), args start=r0 (b=0), nargs=2 (c=2)
  wr_insn(buf, &off, (uint8_t)JOP_CALL, 2, 0, 2, 1);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  // func1 (callee): nregs=3, ninsns=2
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 2, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 3, "call returns 3");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_call_indirect_i32(void) {
  const char* t = "exec_call_indirect_i32";
  uint8_t buf[1024];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 1); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32 (0), Function(sig0) (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_FUNCTION, 0);

  // sig0: ret I32, nargs=2, args [I32, I32]
  const uint32_t args2[2] = {0, 0};
  wr_sig(buf, &off, 0, 2, args2);

  // func0 (caller): regs r0:I32 r1:I32 r2:fun r3:I32(out)
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_FUN, 2, 0, 0, 1); // fun -> func1
  // r3 = callr r2, args start=r0(imm=0), nargs=2 (c=2)
  wr_insn(buf, &off, (uint8_t)JOP_CALLR, 3, 2, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 3, 0, 0, 0);

  // func1 (callee): nregs=3, ninsns=2, regs [I32,I32,I32]
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 2, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 3, "callr returns 3");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_closure_capture_and_this(void) {
  const char* t = "exec_closure_capture_and_this";
  uint8_t buf[1024];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 1); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32 (0), Function(sig0) (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_FUNCTION, 0);

  // sig0: ret I32, nargs=2, args [I32, I32]   (this, arg)
  const uint32_t args2[2] = {0, 0};
  wr_sig(buf, &off, 0, 2, args2);

  // func0 (caller): regs
  // r0:I32(this=10), r1:I32(arg=2), r2:I32(cap=3), r3:fun(clo), r4:fun(bound), r5:I32(out)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 7); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 10);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 3);
  // r3 = closure(func1, capture r2)
  wr_insn(buf, &off, (uint8_t)JOP_CLOSURE, 3, 2, 1, 1);
  // r4 = bind_this(r3, r0)
  wr_insn(buf, &off, (uint8_t)JOP_BIND_THIS, 4, 3, 0, 0);
  // r5 = callr r4, args start=r1 (imm=1), nargs=1 (c=1)
  wr_insn(buf, &off, (uint8_t)JOP_CALLR, 5, 4, 1, 1);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  // func1 (callee): nregs=3 (r0:this, r1:arg, r2:capture)
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  // r1 = r0 + r1; r1 = r1 + r2; ret r1
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 15, "bound this + capture works");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_closure_capture_i64_aggressive_gc(void) {
  const char* t = "exec_closure_capture_i64_aggressive_gc";
  uint8_t buf[2048];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 1); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32 (0), I64 (1), Function(sig0) (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_FUNCTION, 0);

  // sig0: ret I32, nargs=2, args [I32, I32]   (this, arg)
  const uint32_t args2[2] = {0, 0};
  wr_sig(buf, &off, 0, 2, args2);

  // func0 (caller):
  // r0:I32(this=10), r1:I32(arg=2), r2:I32(cap_i32=3), r3:I64(cap_i64),
  // r4:fun(clo), r5:fun(bound), r6:I32(out)
  wr_u32(buf + off, 7); off += 4;
  wr_u32(buf + off, 8); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 10);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 3);
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 3, 2, 0, 0);
  // r4 = closure(func1, capture r3 (I64))
  wr_insn(buf, &off, (uint8_t)JOP_CLOSURE, 4, 3, 1, 1);
  // r5 = bind_this(r4, r0)
  wr_insn(buf, &off, (uint8_t)JOP_BIND_THIS, 5, 4, 0, 0);
  // r6 = callr r5, args start=r1 (imm=1), nargs=1 (c=1)
  wr_insn(buf, &off, (uint8_t)JOP_CALLR, 6, 5, 1, 1);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 6, 0, 0, 0);

  // func1 (callee): nregs=2(args)+1(cap)=3  => r0:this, r1:arg, r2:cap_i64
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  // r1 = r0 + r1
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 0, 1, 0);
  // r0 = trunc_i64(r2)
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 0, 2, 0, 0);
  // r1 = r1 + r0
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 0, 0);
  // ret r1
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  vm.gc_next_collect = 1; // collect extremely often
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 15, "aggressive GC keeps capture i64 box alive");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_gc_cycle_reclaim_object(void) {
  const char* t = "gc_cycle_reclaim_object";

  jelly_vm vm = {0};
  vm.gc_next_collect = 1; // aggressive
  jelly_gc_init(&vm);

  // Build two objects referencing each other.
  jelly_object* a = jelly_object_new(&vm, 0);
  // Root during setup to prevent GC-triggered reclamation.
  jelly_gc_push_root(&vm, jelly_from_ptr(a));
  jelly_object* b = jelly_object_new(&vm, 0);
  jelly_gc_push_root(&vm, jelly_from_ptr(b));
  jelly_object_set(a, 1, jelly_from_ptr(b));
  jelly_object_set(b, 2, jelly_from_ptr(a));

  // Keep only `a` rooted; it should keep `b` alive through the cycle.
  jelly_gc_pop_roots(&vm, 1);
  jelly_gc_collect(&vm);
  uint64_t freed_after_root = vm.gc_freed_objects;

  // Unroot and force collection; both should be reclaimed (cycle).
  jelly_gc_pop_roots(&vm, 1);
  jelly_gc_collect(&vm);

  assert_true(t, vm.gc_freed_objects >= freed_after_root + 2, "cycle reclaimed after unroot");

  jelly_vm_shutdown(&vm);
  return failures == 0 ? 0 : 1;
}

static int run_gc_abstract_finalizer_called_once(void) {
  const char* t = "gc_abstract_finalizer_called_once";
  uint32_t* abs_finalized = (uint32_t*)malloc(sizeof(uint32_t));
  assert_true(t, abs_finalized != NULL, "malloc counter");
  *abs_finalized = 0;

  jelly_vm vm = {0};
  vm.gc_next_collect = 1;
  jelly_gc_init(&vm);

  // Make an abstract with a finalizer.
  jelly_abstract* a = jelly_abstract_new_finalized(&vm, 0, abs_finalized, abs_finalizer_inc);

  // Keep it alive across a collection: finalizer must NOT run.
  jelly_gc_push_root(&vm, jelly_from_ptr(a));
  jelly_gc_collect(&vm);
  assert_true(t, *abs_finalized == 0, "finalizer not called while rooted");

  // Drop root and collect: finalizer should run exactly once.
  jelly_gc_pop_roots(&vm, 1);
  jelly_gc_collect(&vm);
  assert_true(t, *abs_finalized == 1, "finalizer called once after unroot");

  // Extra collections should not call it again.
  jelly_gc_collect(&vm);
  assert_true(t, *abs_finalized == 1, "finalizer not called twice");

  free(abs_finalized);
  jelly_vm_shutdown(&vm);
  return failures == 0 ? 0 : 1;
}

static int run_gc_mixed_container_graph_reclaim(void) {
  const char* t = "gc_mixed_container_graph_reclaim";

  jelly_vm vm = {0};
  vm.gc_next_collect = 1; // aggressive
  jelly_gc_init(&vm);

  // Build a mixed graph with a cycle:
  //   obj["arr"] -> arr
  //   arr[0]     -> obj   (cycle)
  //   obj["list"]-> list
  //   list.head  -> box_i64
  //
  // We root intermediates during setup so aggressive GC can't collect them
  // before they're linked into the graph.
  jelly_object* obj = jelly_object_new(&vm, 0);
  jelly_gc_push_root(&vm, jelly_from_ptr(obj));

  jelly_array* arr = jelly_array_new(&vm, 0, 1);
  jelly_gc_push_root(&vm, jelly_from_ptr(arr));

  jelly_box_i64* b = jelly_box_i64_new(&vm, 0, 123);
  jelly_value vb = jelly_from_ptr(b);
  jelly_gc_push_root(&vm, vb);

  jelly_list* list = jelly_list_cons(&vm, 0, vb, NULL);
  jelly_gc_push_root(&vm, jelly_from_ptr(list));

  // Link graph.
  arr->data[0] = jelly_from_ptr(obj);     // cycle back to obj
  jelly_object_set(obj, 10, jelly_from_ptr(arr));
  jelly_object_set(obj, 11, jelly_from_ptr(list));

  // Drop setup roots except the main `obj` root.
  jelly_gc_pop_roots(&vm, 3);

  // Collect: graph should remain alive, and we should be able to traverse it.
  uint64_t freed_before = vm.gc_freed_objects;
  jelly_gc_collect(&vm);
  assert_true(t, vm.gc_freed_objects == freed_before, "nothing freed while rooted");

  jelly_value v_arr = jelly_object_get(obj, 10);
  assert_true(t, jelly_is_ptr(v_arr), "arr is a pointer value");
  jelly_array* arr2 = (jelly_array*)jelly_as_ptr(v_arr);
  assert_true(t, arr2 && arr2->h.kind == (uint32_t)JELLY_OBJ_ARRAY, "arr kind ok");

  jelly_value v_obj_back = arr2->data[0];
  assert_true(t, jelly_is_ptr(v_obj_back) && jelly_as_ptr(v_obj_back) == (void*)obj, "cycle obj <-> arr intact");

  jelly_value v_list = jelly_object_get(obj, 11);
  assert_true(t, jelly_is_ptr(v_list), "list is a pointer value");
  jelly_list* list2 = (jelly_list*)jelly_as_ptr(v_list);
  assert_true(t, list2 && list2->h.kind == (uint32_t)JELLY_OBJ_LIST, "list kind ok");
  assert_true(t, jelly_is_box_i64(list2->head) && jelly_as_box_i64(list2->head) == 123, "boxed i64 survives");

  // Unroot and collect: all 4 objects (obj, arr, list, box) should be freed.
  jelly_gc_pop_roots(&vm, 1);
  jelly_gc_collect(&vm);
  assert_true(t, vm.gc_freed_objects >= freed_before + 4, "mixed graph reclaimed after unroot");

  jelly_vm_shutdown(&vm);
  return failures == 0 ? 0 : 1;
}

static int run_gc_object_rehash_under_aggressive_gc(void) {
  const char* t = "gc_object_rehash_under_aggressive_gc";

  jelly_vm vm = {0};
  vm.gc_next_collect = 1; // aggressive
  jelly_gc_init(&vm);

  jelly_object* obj = jelly_object_new(&vm, 0);
  jelly_gc_push_root(&vm, jelly_from_ptr(obj));

  // Insert enough entries to force multiple rehashes, while allocating a GC box
  // per entry. We must root the box until it is stored into the rooted object.
  const uint32_t n = 200;
  for(uint32_t i = 0; i < n; i++) {
    jelly_value boxv = jelly_from_ptr(jelly_box_i64_new(&vm, 0, (int64_t)(1000 + (int64_t)i)));
    jelly_gc_push_root(&vm, boxv);
    jelly_object_set(obj, i + 1u, boxv);
    jelly_gc_pop_roots(&vm, 1);
  }

  uint64_t freed_before = vm.gc_freed_objects;
  jelly_gc_collect(&vm);
  assert_true(t, vm.gc_freed_objects == freed_before, "nothing freed while rooted");

  // Drop the root and collect; object + all boxes should be reclaimed.
  jelly_gc_pop_roots(&vm, 1);
  jelly_gc_collect(&vm);
  assert_true(t, vm.gc_freed_objects >= freed_before + 1u + (uint64_t)n, "rehash-heavy object graph reclaimed");

  jelly_vm_shutdown(&vm);
  return failures == 0 ? 0 : 1;
}

static int run_exec_spill_roots_survive_aggressive_gc(void) {
  const char* t = "exec_spill_roots_survive_aggressive_gc";
  uint8_t buf[2048];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 5); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32(0), I64(1), Dynamic(2), bytes(3), Bool(4)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs:
  // r0:i32(src=-5)
  // r1:i64(wide)
  // r2:dyn(tmp dyn for spill push)
  // r3:dyn(tmp dyn for spill pop)
  // r4:i32(loop i)
  // r5:i32(one)
  // r6:i32(zero)
  // r7:i32(bytes_len=1)
  // r8:bytes(tmp allocation)
  // r9:bool(cond)
  // r10:i64(out wide)
  // r11:i32(out)
  wr_u32(buf + off, 12); off += 4;
  wr_u32(buf + off, 18); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;  // r0 i32
  wr_u32(buf + off, 1); off += 4;  // r1 i64
  wr_u32(buf + off, 2); off += 4;  // r2 dyn
  wr_u32(buf + off, 2); off += 4;  // r3 dyn
  wr_u32(buf + off, 0); off += 4;  // r4 i32
  wr_u32(buf + off, 0); off += 4;  // r5 i32
  wr_u32(buf + off, 0); off += 4;  // r6 i32
  wr_u32(buf + off, 0); off += 4;  // r7 i32
  wr_u32(buf + off, 3); off += 4;  // r8 bytes
  wr_u32(buf + off, 4); off += 4;  // r9 bool
  wr_u32(buf + off, 1); off += 4;  // r10 i64
  wr_u32(buf + off, 0); off += 4;  // r11 i32

  // Build an i64, box to Dynamic, spill it.
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, (uint32_t)(int32_t)-5); // 0
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 1, 0, 0, 0);                     // 1
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);                        // 2
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_PUSH, 2, 0, 0, 0);                    // 3

  // Loop allocating bytes to trigger frequent GC.
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 50);                    // 4 i=50
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 5, 0, 0, 1);                     // 5 one=1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 6, 0, 0, 0);                     // 6 zero=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 7, 0, 0, 1);                     // 7 bytes_len=1
  // loop:
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW, 8, 7, 0, 0);                     // 8 alloc bytes (discarded each iter)
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32, 4, 4, 5, 0);                       // 9 i -= 1
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 9, 4, 6, 0);                        // 10 cond = (i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 9, 0, 0, 1);                        // 11 if cond goto end
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, (uint32_t)(int32_t)-5);        // 12 goto loop (back to 8)

  // end: pop spilled dyn, unbox to i64, return i32
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_POP, 3, 0, 0, 0);                     // 13
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I64, 10, 3, 0, 0);                 // 14
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 11, 10, 0, 0);                // 15
  wr_insn(buf, &off, (uint8_t)JOP_RET, 11, 0, 0, 0);                          // 16
  wr_insn(buf, &off, (uint8_t)JOP_NOP, 0, 0, 0, 0);                           // 17 padding

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  vm.gc_next_collect = 1;
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, -5, "spilled boxed i64 survives aggressive GC");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_i64_dyn_roundtrip(void) {
  const char* t = "exec_i64_dyn_roundtrip";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), I64 (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I32, r1:I64, r2:Dynamic, r3:I64, r4:I32(out)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 6); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=-5; r1=sext_i64(r0); r2=ToDyn(r1); r3=FromDyn_I64(r2); r4=trunc_i32(r3); ret r4
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, (uint32_t)(int32_t)-5);
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I64, 3, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, -5, "i64 dyn roundtrip keeps value");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_from_dyn_bool(void) {
  const char* t = "exec_from_dyn_bool";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: Bool (0), Dynamic (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:Bool, r1:Dynamic, r2:Bool
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 4); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_BOOL, 0, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_BOOL, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_true(t, jelly_is_bool(v) && jelly_as_bool(v) == 1, "from_dyn_bool roundtrip");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_from_dyn_atom(void) {
  const char* t = "exec_from_dyn_atom";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: Atom (0), Dynamic (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_ATOM, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // atoms: one empty atom (id 0)
  wr_u32(buf + off, 0); off += 4;

  // regs: r0:Atom, r1:Dynamic, r2:Atom
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 4); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_ATOM, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_ATOM, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 2, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_true(t, jelly_is_atom(v) && jelly_as_atom(v) == 0u, "from_dyn_atom roundtrip");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_from_dyn_ptr_bytes(void) {
  const char* t = "exec_from_dyn_ptr_bytes";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), bytes (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I32(len), r1:bytes, r2:Dynamic, r3:bytes, r4:I32(out)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 6); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_PTR, 3, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_LEN, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 2, "from_dyn_ptr bytes roundtrip");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_trap_bytes_get_oob(void) {
  const char* t = "exec_trap_bytes_get_oob";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), bytes (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);

  // regs: r0:bytes, r1:i32(len), r2:i32(idx), r3:i32(out)
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 4); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // len=1; new; idx=9; get => trap
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 9);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_GET_U8, 3, 0, 2, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &out);
  assert_true(t, st == JELLY_EXEC_TRAP, "exec returns TRAP");
  assert_true(t, jelly_vm_last_trap_code(&vm) == JELLY_TRAP_BOUNDS, "trap is BOUNDS");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_trap_from_dyn_bool_mismatch(void) {
  const char* t = "exec_trap_from_dyn_bool_mismatch";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Dynamic (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs: r0:i32, r1:dyn, r2:bool
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 3); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_BOOL, 2, 1, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &out);
  assert_true(t, st == JELLY_EXEC_TRAP, "exec returns TRAP");
  assert_true(t, jelly_vm_last_trap_code(&vm) == JELLY_TRAP_TYPE_MISMATCH, "trap is TYPE_MISMATCH");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_try_catch_catches_trap_bytes_oob(void) {
  const char* t = "exec_try_catch_catches_trap_bytes_oob";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), bytes (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:bytes, r1:i32(len), r2:i32(idx), r3:i32(tmp), r4:dyn(caught)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 8); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4;

  // len=1; new; idx=9; try{ get } endtry; ret tmp; catch: ret caught
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 9);
  wr_insn(buf, &off, (uint8_t)JOP_TRY, 4, 0, 0, 3); // catch at insn 7 (delta from next pc=4)
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_GET_U8, 3, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ENDTRY, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 3, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, (int32_t)JELLY_TRAP_BOUNDS, "caught payload is trap code");
  assert_true(t, jelly_vm_last_trap_code(&vm) == JELLY_TRAP_NONE, "trap cleared after catch");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_try_catch_catches_throw_payload(void) {
  const char* t = "exec_try_catch_catches_throw_payload";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Dynamic (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:dyn(caught), r1:i32, r2:dyn(payload)
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 6); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  // try { payload=123; throw payload } endtry ; catch: ret caught
  wr_insn(buf, &off, (uint8_t)JOP_TRY, 0, 0, 0, 4); // catch at insn 5 (delta from next pc=1)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 123);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_THROW, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ENDTRY, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 0, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 123, "caught payload is thrown value");
  assert_true(t, jelly_vm_last_trap_code(&vm) == JELLY_TRAP_NONE, "trap cleared after catch");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_kindof_i32_and_null(void) {
  const char* t = "exec_kindof_i32_and_null";
  uint8_t buf[768];
  size_t off = 0;

  // Types: I32 (0), List<I32> (1), Dynamic (2)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_LIST, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs:
  // r0:I32, r1:Dynamic, r2:I32(kind), r3:List<I32>, r4:Dynamic, r5:I32(kind)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 10); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // dyn from i32 => kind 2
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 2, 1, 0, 0);

  // dyn from nil list => null => kind 0
  wr_insn(buf, &off, (uint8_t)JOP_LIST_NIL, 3, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 5, 4, 0, 0);

  // return (kind2 * 1000 + kind5) => 2000
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 1000);
  wr_insn(buf, &off, (uint8_t)JOP_MUL_I32, 0, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 0, 0, 5, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 0, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 2000, "kindof(i32)=2 and kindof(null)=0");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_match_when_and_fallthrough_i32(void) {
  const char* t = "exec_match_when_and_fallthrough_i32";
  uint8_t buf[1024];
  size_t off = 0;

  // types: I32 (0), Bool (1), Dynamic (2)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs:
  // r0:I32 tmp
  // r1:Dynamic subject
  // r2:I32 kind
  // r3:Bool cond
  // r4:I32 subj_i32
  // r5:I32 result
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 20); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // subject = ToDyn(7)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);

  // kind = KINDOF(subject)
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 2, 1, 0, 0);

  // arm1: type i32 (kind==2) AND when(subject==7) then { /*no expr*/ } fallthrough
  // cond = (kind == 2)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 3, 2, 0, 0);
  // if cond goto arm1_when else goto arm2_check
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 3, 0, 0, 1);  // -> arm1_when
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 6);     // -> arm2_check

  // arm1_when:
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I32, 4, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 3, 4, 0, 0);
  // if when true goto arm1_body else goto arm2_check
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 3, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 1); // to arm2_check

  // arm1_body: statement-only (no expr) => explicit fallthrough to arm2_check
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 0); // (no-op fallthrough; next insn is arm2_check)

  // arm2_check: if kind==2 then result=42 else result=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 3, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 3, 0, 0, 2); // -> arm2_body
  // default body:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 5, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  // arm2_body:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 5, 0, 0, 42);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 42, "arm1 falls through; arm2 returns 42");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_match_object_prop_and_when(void) {
  const char* t = "exec_match_object_prop_and_when";
  uint8_t buf[1400];
  size_t off = 0;

  // types: I32 (0), Bool (1), Object (2), Dynamic (3)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_OBJECT, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // atom table: one empty atom (id 0)
  wr_u32(buf + off, 0); off += 4;

  // regs:
  // r0:Object, r1:I32 tmp, r2:Dynamic subj, r3:I32 kind, r4:Bool cond, r5:I32 got, r6:I32 result
  wr_u32(buf + off, 7); off += 4;
  wr_u32(buf + off, 21); off += 4; // ninsns
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // obj = new(); set(atom0, 5); subj = ToDyn(obj)
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_NEW, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 5);
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_SET_ATOM, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 0, 0, 0);

  // kind = KINDOF(subj)
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 3, 2, 0, 0);

  // arm: kind==object(8) AND has(atom0) AND when(get(atom0)==5) => result=1 else result=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 8);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 4, 3, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 10); // default

  // has(atom0)?
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_HAS_ATOM, 4, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 7); // default

  // got = get(atom0); when(got==5)
  wr_insn(buf, &off, (uint8_t)JOP_OBJ_GET_ATOM, 5, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 5);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 4, 5, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 2); // default

  // body: result=1; ret
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 6, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 6, 0, 0, 0);

  // default:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 6, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 6, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 1, "object prop+when arm returns 1");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_match_list_head_and_when(void) {
  const char* t = "exec_match_list_head_and_when";
  uint8_t buf[2048];
  size_t off = 0;

  // types: I32(0), Bool(1), List<I32>(2), Dynamic(3)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_LIST, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs:
  // r0:List<I32>, r1:I32 tmp, r2:Dynamic subj, r3:I32 kind, r4:Bool cond, r5:List<I32> as_list, r6:I32 head, r7:I32 result
  wr_u32(buf + off, 8); off += 4;
  wr_u32(buf + off, 23); off += 4; // ninsns
  wr_u32(buf + off, 2); off += 4; // r0: List<I32>
  wr_u32(buf + off, 0); off += 4; // r1: I32
  wr_u32(buf + off, 3); off += 4; // r2: Dynamic
  wr_u32(buf + off, 0); off += 4; // r3: I32
  wr_u32(buf + off, 1); off += 4; // r4: Bool
  wr_u32(buf + off, 2); off += 4; // r5: List<I32>
  wr_u32(buf + off, 0); off += 4; // r6: I32
  wr_u32(buf + off, 0); off += 4; // r7: I32

  // Build list [9,7] in r0
  wr_insn(buf, &off, (uint8_t)JOP_LIST_NIL, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_LIST_CONS, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 9);
  wr_insn(buf, &off, (uint8_t)JOP_LIST_CONS, 0, 1, 0, 0);

  // subj = ToDyn(r0)
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 0, 0, 0);
  // kind = kindof(subj)
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 3, 2, 0, 0);

  // arm1: kind==list(6) and when(head==9) => result=1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 6);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 4, 3, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 10); // default

  // as_list = FromDyn_Ptr(subj)
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_PTR, 5, 2, 0, 0);
  // guard non-nil
  wr_insn(buf, &off, (uint8_t)JOP_LIST_IS_NIL, 4, 5, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 7); // if nil -> default

  // head = head(as_list); when(head == 9)
  wr_insn(buf, &off, (uint8_t)JOP_LIST_HEAD, 6, 5, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 9);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 4, 6, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 2); // default

  // arm1 body
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 7, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 7, 0, 0, 0);

  // default: result=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 7, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 7, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 1, "list head==9 arm returns 1");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_match_array_len_index_and_when(void) {
  const char* t = "exec_match_array_len_index_and_when";
  uint8_t buf[4096];
  size_t off = 0;

  // types: I32(0), Bool(1), Array<I32>(2), Dynamic(3)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_ARRAY, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs:
  // r0:Array<I32>, r1:I32 tmp, r2:I32 idx, r3:Dynamic subj, r4:I32 kind, r5:Bool cond,
  // r6:Array<I32> as_arr, r7:I32 len, r8:I32 v0, r9:I32 result
  wr_u32(buf + off, 10); off += 4;
  wr_u32(buf + off, 27); off += 4; // ninsns
  wr_u32(buf + off, 2); off += 4; // r0: Array<I32>
  wr_u32(buf + off, 0); off += 4; // r1: I32
  wr_u32(buf + off, 0); off += 4; // r2: I32
  wr_u32(buf + off, 3); off += 4; // r3: Dynamic
  wr_u32(buf + off, 0); off += 4; // r4: I32
  wr_u32(buf + off, 1); off += 4; // r5: Bool
  wr_u32(buf + off, 2); off += 4; // r6: Array<I32>
  wr_u32(buf + off, 0); off += 4; // r7: I32
  wr_u32(buf + off, 0); off += 4; // r8: I32
  wr_u32(buf + off, 0); off += 4; // r9: I32

  // Build array len=2 with arr[0]=5
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_NEW, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 5);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 1, 0, 2, 0);

  // subj = ToDyn(arr)
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 3, 0, 0, 0);
  // kind = kindof(subj)
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 4, 3, 0, 0);

  // arm: kind==array(7) and when(len==2 and arr[0]==5) => result=1 else result=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 5, 4, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 5, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 14); // default

  // as_arr = FromDyn_Ptr(subj)
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_PTR, 6, 3, 0, 0);
  // len = ARRAY_LEN(as_arr)
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_LEN, 7, 6, 0, 0);
  // cond = (len == 2)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 5, 7, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 5, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 8); // default

  // v0 = arr[0]; cond = (v0 == 5)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_GET, 8, 6, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 5);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 5, 8, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 5, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 2); // default

  // body: result=1; ret
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 9, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 9, 0, 0, 0);

  // default:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 9, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 9, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 1, "array len+index+when arm returns 1");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_switch_kind_dispatch_list(void) {
  const char* t = "exec_switch_kind_dispatch_list";
  uint8_t buf[2048];
  size_t off = 0;

  // types: I32(0), Bool(1), List<I32>(2), Dynamic(3)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_LIST, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs:
  // r0:List<I32>, r1:I32 tmp, r2:Dynamic subj, r3:I32 kind, r4:I32 out
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 12); off += 4; // ninsns
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // list [1]
  wr_insn(buf, &off, (uint8_t)JOP_LIST_NIL, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_LIST_CONS, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF, 3, 2, 0, 0);

  // switch on kind:
  // - list(6) => return 42
  // - default => return 0
  // table_end is pc after 2 case entries.
  wr_insn(buf, &off, (uint8_t)JOP_SWITCH_KIND, 3, 2, 0, 2);   // default delta -> default body
  wr_insn(buf, &off, (uint8_t)JOP_CASE_KIND, 6, 0, 0, 0);     // list => list body (delta 0)
  wr_insn(buf, &off, (uint8_t)JOP_CASE_KIND, 7, 0, 0, 2);     // array => default (delta 2)

  // list body:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 42);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  // default:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 42, "switch_kind dispatches list to 42");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_i8_i16_to_dyn_roundtrip(void) {
  const char* t = "exec_i8_i16_to_dyn_roundtrip";
  uint8_t buf[768];
  size_t off = 0;

  // types: I8(0), I16(1), I32(2), Dynamic(3)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I8, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I16, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs:
  // r0:I8
  // r1:Dynamic
  // r2:I32  (unboxed i8 via FromDyn_I32)
  // r3:I16
  // r4:Dynamic
  // r5:I32  (unboxed i16 via FromDyn_I32)
  // r6:I32  (out)
  wr_u32(buf + off, 7); off += 4;
  wr_u32(buf + off, 10); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4; // r0 -> I8
  wr_u32(buf + off, 3); off += 4; // r1 -> Dynamic
  wr_u32(buf + off, 2); off += 4; // r2 -> I32
  wr_u32(buf + off, 1); off += 4; // r3 -> I16
  wr_u32(buf + off, 3); off += 4; // r4 -> Dynamic
  wr_u32(buf + off, 2); off += 4; // r5 -> I32
  wr_u32(buf + off, 2); off += 4; // r6 -> I32

  // r0 = (i8)-5; r1 = ToDyn(r0); r2 = FromDyn_I32(r1)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, (uint32_t)(int32_t)-5);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I32, 2, 1, 0, 0);

  // r3 = (i16)1234; r4 = ToDyn(r3); r5 = FromDyn_I32(r4)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 1234);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I32, 5, 4, 0, 0);

  // out = r2 * 1000 + r5  => (-5)*1000 + 1234 = -3766
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 6, 0, 0, 1000);
  wr_insn(buf, &off, (uint8_t)JOP_MUL_I32, 6, 2, 6, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 6, 6, 5, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 6, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, -3766, "i8/i16 values roundtrip through ToDyn/FromDyn_I32");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_unary_neg_i32_and_not_bool(void) {
  const char* t = "exec_unary_neg_i32_and_not_bool";
  uint8_t buf[768];
  size_t off = 0;

  // types: I32 (0), Bool (1)
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs: r0:I32, r1:I32, r2:Bool, r3:Bool, r4:I32(out)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 10); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0 = 7; r1 = -r0  => -7
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_NEG_I32, 1, 0, 0, 0);

  // r2 = true; r3 = !r2 => false
  wr_insn(buf, &off, (uint8_t)JOP_CONST_BOOL, 2, 0, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_NOT_BOOL, 3, 2, 0, 0);

  // If !true unexpectedly produced true, branch to alternate return.
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 3, 0, 0, 3); // -> alt

  // default: out = r1 * 10 => (-7)*10 = -70
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 10);
  wr_insn(buf, &off, (uint8_t)JOP_MUL_I32, 4, 1, 4, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  // alt:
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, (uint32_t)(int32_t)-71);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "[LOADERR] %s: err=%d msg=%s off=%zu\n", t, (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, -70, "neg_i32 and not_bool work");

  jelly_vm_shutdown(&vm);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_f64_dyn_roundtrip(void) {
  const char* t = "exec_f64_dyn_roundtrip";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), F64 (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I32, r1:F64, r2:Dynamic, r3:F64, r4:I32(out)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 6); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=7; r1=f64_from_i32(r0); r2=ToDyn(r1); r3=FromDyn_F64(r2); r4=i32_from_f64(r3); ret r4
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_F64_FROM_I32, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_F64, 3, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 7, "f64 dyn roundtrip keeps value");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_f32_dyn_and_call_return(void) {
  const char* t = "exec_f32_dyn_and_call_return";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: F32 (0), Dynamic (1), I32 (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_F32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // --- func0 (entry): r0:F32, r1:Dynamic; call func1 -> r0; to_dyn r1=r0; ret r1
  wr_u32(buf + off, 2); off += 4;   // nregs
  wr_u32(buf + off, 3); off += 4;   // ninsns
  wr_u32(buf + off, 0); off += 4;   // r0 f32
  wr_u32(buf + off, 1); off += 4;   // r1 dyn
  wr_insn(buf, &off, (uint8_t)JOP_CALL, 0, 0, 0, 1);      // r0 = call func1()
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);    // r1 = box(r0)
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);       // ret dyn

  // --- func1: r0:F32; const_f32 1.5f; ret r0
  wr_u32(buf + off, 1); off += 4;   // nregs
  wr_u32(buf + off, 2); off += 4;   // ninsns
  wr_u32(buf + off, 0); off += 4;   // r0 f32
  {
    float f = 1.5f;
    uint32_t bits = 0;
    memcpy(&bits, &f, sizeof(bits));
    wr_insn(buf, &off, (uint8_t)JOP_CONST_F32, 0, 0, 0, bits);
  }
  wr_insn(buf, &off, (uint8_t)JOP_RET, 0, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_true(t, jelly_is_box_f32(v), "returns boxed f32");
  float got = jelly_as_box_f32(v);
  assert_true(t, got == 1.5f, "f32 value preserved");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_i8_const_to_dyn(void) {
  const char* t = "exec_i8_const_to_dyn";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I8 (0), Dynamic (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I8, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I8, r1:Dynamic
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 3); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 123);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 123, "const_i32 allowed for i8 regs (stored in i32 slot)");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_f32_arith_and_convert(void) {
  const char* t = "exec_f32_arith_and_convert";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), F32 (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_F32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs: r0:I32, r1:F32, r2:F32, r3:F32, r4:Bool, r5:I32(out)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 7); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=2; r1=f32_from_i32(r0); r2=r1+r1; r3=r2*r1; r4=(r3==r3); r5=i32_from_f32(r3); ret r5
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_F32_FROM_I32, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_F32, 2, 1, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_MUL_F32, 3, 2, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_F32, 4, 3, 3, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F32, 5, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 8, "f32 arith works and converts back (2+2)*2 == 8");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_f64_arith_and_convert(void) {
  const char* t = "exec_f64_arith_and_convert";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), F64 (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs: r0:I32, r1:F64, r2:F64, r3:F64, r4:Bool, r5:I32(out)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 7); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=2; r1=f64_from_i32(r0); r2=r1+r1; r3=r2*r1; r4=(r3==r3); r5=i32_from_f64(r3); ret r5
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 2);
  wr_insn(buf, &off, (uint8_t)JOP_F64_FROM_I32, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_F64, 2, 1, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_MUL_F64, 3, 2, 1, 0);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_F64, 4, 3, 3, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 5, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 8, "f64 arith works and converts back (2+2)*2 == 8");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_i64_f64_explicit_conversions(void) {
  const char* t = "exec_i64_f64_explicit_conversions";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), I64 (1), F64 (2), Dynamic (3)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I32, r1:I64, r2:F64, r3:I64, r4:I32(out)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 6); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=-5; r1=sext_i64(r0); r2=f64_from_i64(r1); r3=i64_from_f64(r2); r4=trunc_i32(r3); ret r4
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, (uint32_t)(int32_t)-5);
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_F64_FROM_I64, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I64_FROM_F64, 3, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, -5, "explicit i64<->f64 conversions roundtrip for small integers");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_i64_arith_and_eq_wrap(void) {
  const char* t = "exec_i64_arith_and_eq_wrap";
  uint8_t buf[1024];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONST64); off += 4; // features (for const_i64 pool)
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry
  wr_u32(buf + off, 2); off += 4; // nconst_i64
  wr_u32(buf + off, 0); off += 4; // nconst_f64

  // types: I64 (0), I32 (1), Bool (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // const_i64 pool: [1, -1]
  {
    int64_t v = 1;
    uint64_t bits = 0;
    memcpy(&bits, &v, sizeof(bits));
    wr_u64(buf + off, bits); off += 8;
  }
  {
    int64_t v = -1;
    uint64_t bits = 0;
    memcpy(&bits, &v, sizeof(bits));
    wr_u64(buf + off, bits); off += 8;
  }

  // regs:
  // r0:I64 = 1
  // r1:I64 = -1
  // r2:I64 = r0 + r1  (wrap -> 0)
  // r3:Bool = (r2 == r2) (true)
  // r4:I32 = bool as i32 via CONST+JMP (return 1)
  wr_u32(buf + off, 5); off += 4;  // nregs
  wr_u32(buf + off, 9); off += 4;  // ninsns
  wr_u32(buf + off, 0); off += 4;  // r0 I64
  wr_u32(buf + off, 0); off += 4;  // r1 I64
  wr_u32(buf + off, 0); off += 4;  // r2 I64
  wr_u32(buf + off, 2); off += 4;  // r3 Bool
  wr_u32(buf + off, 1); off += 4;  // r4 I32

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I64, 0, 0, 0, 0);           // r0 = 1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I64, 1, 0, 0, 1);           // r1 = -1
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I64, 2, 0, 1, 0);             // r2 = r0 + r1 (wrap)
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I64, 3, 2, 2, 0);              // r3 = (r2 == r2)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 0);           // r4 = 0 (else)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 3, 0, 0, 1);              // if true, jump to THEN
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 1);                 // skip THEN
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 1);           // THEN: r4 = 1
  wr_insn(buf, &off, (uint8_t)JOP_RET, 4, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 1, "i64 arithmetic + eq works (and wraparound is defined)");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_trap_f64_to_i32_overflow(void) {
  const char* t = "exec_trap_f64_to_i32_overflow";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONST64); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4; // nconst_i64
  wr_u32(buf + off, 1); off += 4; // nconst_f64

  // types: F64 (0), I32 (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // const_f64 pool: [1e300] (definitely out of i32 range)
  {
    double v = 1e300;
    uint64_t bits = 0;
    memcpy(&bits, &v, sizeof(bits));
    wr_u64(buf + off, bits); off += 8;
  }

  // regs: r0:F64, r1:I32(out)
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 3); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_F64, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = 0;
  jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &out);
  assert_true(t, st == JELLY_EXEC_TRAP, "conversion overflow traps");
  assert_true(t, jelly_vm_last_trap_code(&vm) == JELLY_TRAP_TYPE_MISMATCH, "trap code is TYPE_MISMATCH");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_const_i64_pool(void) {
  const char* t = "exec_const_i64_pool";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONST64); off += 4; // features
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry
  wr_u32(buf + off, 1); off += 4; // nconst_i64
  wr_u32(buf + off, 0); off += 4; // nconst_f64

  // types: I32 (0), I64 (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // atoms none

  // const_i64 pool: [-5]
  {
    int64_t v = (int64_t)-5;
    uint64_t bits = 0;
    memcpy(&bits, &v, sizeof(bits));
    wr_u64(buf + off, bits); off += 8;
  }

  // function0: r0:I64, r1:Dynamic, r2:I64, r3:I32(out)
  wr_u32(buf + off, 4); off += 4; // nregs
  wr_u32(buf + off, 5); off += 4; // ninsns
  wr_u32(buf + off, 1); off += 4; // r0 I64
  wr_u32(buf + off, 2); off += 4; // r1 Dynamic
  wr_u32(buf + off, 1); off += 4; // r2 I64
  wr_u32(buf + off, 0); off += 4; // r3 I32

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I64, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I64, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 3, 2, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 3, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, out, -5, "const_i64 pool loads and executes");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_const_f64_pool(void) {
  const char* t = "exec_const_f64_pool";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONST64); off += 4; // features
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry
  wr_u32(buf + off, 0); off += 4; // nconst_i64
  wr_u32(buf + off, 1); off += 4; // nconst_f64

  // types: F64 (0), I32 (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);

  // atoms none

  // const_f64 pool: [7.0]
  {
    double v = 7.0;
    uint64_t bits = 0;
    memcpy(&bits, &v, sizeof(bits));
    wr_u64(buf + off, bits); off += 8;
  }

  // function0: r0:F64, r1:I32(out)
  wr_u32(buf + off, 2); off += 4; // nregs
  wr_u32(buf + off, 3); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4; // r0 F64
  wr_u32(buf + off, 1); off += 4; // r1 I32

  wr_insn(buf, &off, (uint8_t)JOP_CONST_F64, 0, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value out = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, out, 7, "const_f64 pool loads and executes");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_i64_spill_roundtrip(void) {
  const char* t = "exec_i64_spill_roundtrip";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), I64 (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I32, r1:I64, r2:Dynamic, r3:Dynamic, r4:I64, r5:I32(out)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 8); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=-5; r1=sext_i64(r0); r2=ToDyn(r1); spill_push r2; spill_pop r3; r4=FromDyn_I64(r3); r5=trunc_i32(r4); ret r5
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, (uint32_t)(int32_t)-5);
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_PUSH, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_POP, 3, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I64, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 5, 4, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, -5, "i64 spill roundtrip keeps value");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_f64_spill_roundtrip(void) {
  const char* t = "exec_f64_spill_roundtrip";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), F64 (1), Dynamic (2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);

  // regs: r0:I32, r1:F64, r2:Dynamic, r3:Dynamic, r4:F64, r5:I32(out)
  wr_u32(buf + off, 6); off += 4;
  wr_u32(buf + off, 8); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=7; r1=f64_from_i32(r0); r2=ToDyn(r1); spill_push r2; spill_pop r3; r4=FromDyn_F64(r3); r5=i32_from_f64(r4); ret r5
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);
  wr_insn(buf, &off, (uint8_t)JOP_F64_FROM_I32, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN, 2, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_PUSH, 2, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SPILL_POP, 3, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_F64, 4, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 5, 4, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 5, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 7, "f64 spill roundtrip keeps value");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_wideslot_layout_i64(void) {
  const char* t = "exec_wideslot_layout_i64";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), I64 (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_I64, 0);

  // regs: r0:I32, r1:I64, r2:I32, r3:I64, r4:I32, r5:I32, r6:I32, r7:I32, r8:I32, r9:I32(out)
  // (Deliberately interleaves 4-byte and 8-byte slots to ensure layout is correct.)
  wr_u32(buf + off, 10); off += 4;
  wr_u32(buf + off, 12); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4; // r0 I32
  wr_u32(buf + off, 1); off += 4; // r1 I64
  wr_u32(buf + off, 0); off += 4; // r2 I32
  wr_u32(buf + off, 1); off += 4; // r3 I64
  wr_u32(buf + off, 0); off += 4; // r4 I32
  wr_u32(buf + off, 0); off += 4; // r5 I32
  wr_u32(buf + off, 0); off += 4; // r6 I32
  wr_u32(buf + off, 0); off += 4; // r7 I32
  wr_u32(buf + off, 0); off += 4; // r8 I32
  wr_u32(buf + off, 0); off += 4; // r9 I32

  // r0=111; r2=222; r5=333; r1=sext_i64(r5); r6=444; r3=sext_i64(r6);
  // r4=trunc_i32(r1); r7=trunc_i32(r3); r8=r0+r2; r8=r8+r4; r9=r8+r7; ret r9
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 111);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 222);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 5, 0, 0, 333);
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 1, 5, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 6, 0, 0, 444);
  wr_insn(buf, &off, (uint8_t)JOP_SEXT_I64, 3, 6, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 4, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_I64, 7, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 8, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 8, 8, 4, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 9, 8, 7, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 9, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 1110, "i64 wide-slots don't clobber neighboring i32 slots");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_wideslot_layout_f64(void) {
  const char* t = "exec_wideslot_layout_f64";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), F64 (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_F64, 0);

  // regs: r0:I32, r1:F64, r2:I32, r3:F64, r4:I32, r5:I32, r6:I32(out), r7:I32, r8:I32
  wr_u32(buf + off, 9); off += 4;
  wr_u32(buf + off, 12); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4; // r0 I32
  wr_u32(buf + off, 1); off += 4; // r1 F64
  wr_u32(buf + off, 0); off += 4; // r2 I32
  wr_u32(buf + off, 1); off += 4; // r3 F64
  wr_u32(buf + off, 0); off += 4; // r4 I32
  wr_u32(buf + off, 0); off += 4; // r5 I32
  wr_u32(buf + off, 0); off += 4; // r6 I32
  wr_u32(buf + off, 0); off += 4; // r7 I32
  wr_u32(buf + off, 0); off += 4; // r8 I32

  // r0=10; r2=20; r7=100; r1=f64_from_i32(r7); r8=200; r3=f64_from_i32(r8);
  // r4=i32_from_f64(r1); r5=i32_from_f64(r3); r6=r0+r2; r6=r6+r4; r6=r6+r5; ret r6
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 10);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 20);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 7, 0, 0, 100);
  wr_insn(buf, &off, (uint8_t)JOP_F64_FROM_I32, 1, 7, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 8, 0, 0, 200);
  wr_insn(buf, &off, (uint8_t)JOP_F64_FROM_I32, 3, 8, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 4, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_I32_FROM_F64, 5, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 6, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 6, 6, 4, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 6, 6, 5, 0);
  wr_insn(buf, &off, (uint8_t)JOP_RET, 6, 0, 0, 0);

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 330, "f64 wide-slots don't clobber neighboring i32 slots");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_if_else_eq_i32(void) {
  const char* t = "exec_if_else_eq_i32";
  uint8_t buf[768];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Bool (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs: r0:I32, r1:I32, r2:Bool, r3:I32(out)
  wr_u32(buf + off, 4); off += 4;
  wr_u32(buf + off, 8); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // r0=7; r1=7; r2 = (r0==r1); if r2 jump +2 to THEN; ELSE: r3=1; jmp +1 to END; THEN: r3=2; ret r3
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 7);       // 0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 7);       // 1
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 2, 0, 1, 0);          // 2
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 2, 0, 0, 2);          // 3 -> pc += 2 to THEN
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 1);       // 4 ELSE
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, 1);             // 5 -> pc += 1 to END
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 2);       // 6 THEN
  wr_insn(buf, &off, (uint8_t)JOP_RET, 3, 0, 0, 0);             // 7 END

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 2, "then branch taken");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

static int run_exec_loop_countdown_i32(void) {
  const char* t = "exec_loop_countdown_i32";
  uint8_t buf[1024];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // types: I32 (0), Bool (1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // regs: r0:i32(i), r1:i32(sum), r2:i32(one), r3:i32(zero), r4:bool(cond)
  wr_u32(buf + off, 5); off += 4;
  wr_u32(buf + off, 10); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  // i=3; sum=0; one=1; zero=0;
  // loop:
  //   sum = sum + i
  //   i = i - one
  //   cond = (i == zero)
  //   if cond jump +2 to end
  //   jmp -5 to loop
  // end: ret sum   (expect 3+2+1 = 6)
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, 3);  // 0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 0);  // 1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1);  // 2
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 0);  // 3
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 0, 0);    // 4 loop: sum += i
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32, 0, 0, 2, 0);    // 5 i -= 1
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32, 4, 0, 3, 0);     // 6 cond = (i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF, 4, 0, 0, 1);     // 7 if cond goto end (pc += 1)
  wr_insn(buf, &off, (uint8_t)JOP_JMP, 0, 0, 0, (uint32_t)(int32_t)-5); // 8 goto loop
  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0);        // 9 end

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "module loads");

  jelly_vm vm = {0};
  jelly_value v = jelly_vm_exec(&vm, &m);
  assert_i32_eq(t, v, 6, "sum countdown loop");

  free(vm.spill);
  jelly_bc_free(&m);
  return failures == 0 ? 0 : 1;
}

int main(int argc, char** argv) {
  if(argc < 2) {
    fprintf(stderr, "usage: %s <testname>\n", argv[0]);
    return 2;
  }
  const char* name = argv[1];
  if(strcmp(name, "exec_add_i32") == 0) return run_exec_add_i32();
  if(strcmp(name, "exec_const_bytes_len_and_get_u8") == 0) return run_exec_const_bytes_len_and_get_u8();
  if(strcmp(name, "exec_bytes_concat2_and_get_u8") == 0) return run_exec_bytes_concat2_and_get_u8();
  if(strcmp(name, "exec_bytes_concat_many_and_len") == 0) return run_exec_bytes_concat_many_and_len();
  if(strcmp(name, "exec_spill_roundtrip") == 0) return run_exec_spill_roundtrip();
  if(strcmp(name, "exec_list_head_tail") == 0) return run_exec_list_head_tail();
  if(strcmp(name, "exec_list_is_nil") == 0) return run_exec_list_is_nil();
  if(strcmp(name, "exec_array_set_get_len") == 0) return run_exec_array_set_get_len();
  if(strcmp(name, "exec_array_len") == 0) return run_exec_array_len();
  if(strcmp(name, "exec_bytes_set_get_len") == 0) return run_exec_bytes_set_get_len();
  if(strcmp(name, "exec_obj_get_atom") == 0) return run_exec_obj_get_atom();
  if(strcmp(name, "exec_obj_has_atom") == 0) return run_exec_obj_has_atom();
  if(strcmp(name, "exec_call_direct_i32") == 0) return run_exec_call_direct_i32();
  if(strcmp(name, "exec_call_indirect_i32") == 0) return run_exec_call_indirect_i32();
  if(strcmp(name, "exec_closure_capture_and_this") == 0) return run_exec_closure_capture_and_this();
  if(strcmp(name, "exec_closure_capture_i64_aggressive_gc") == 0) return run_exec_closure_capture_i64_aggressive_gc();
  if(strcmp(name, "gc_cycle_reclaim_object") == 0) return run_gc_cycle_reclaim_object();
  if(strcmp(name, "gc_abstract_finalizer_called_once") == 0) return run_gc_abstract_finalizer_called_once();
  if(strcmp(name, "gc_mixed_container_graph_reclaim") == 0) return run_gc_mixed_container_graph_reclaim();
  if(strcmp(name, "gc_object_rehash_under_aggressive_gc") == 0) return run_gc_object_rehash_under_aggressive_gc();
  if(strcmp(name, "exec_spill_roots_survive_aggressive_gc") == 0) return run_exec_spill_roots_survive_aggressive_gc();
  if(strcmp(name, "exec_i64_dyn_roundtrip") == 0) return run_exec_i64_dyn_roundtrip();
  if(strcmp(name, "exec_from_dyn_bool") == 0) return run_exec_from_dyn_bool();
  if(strcmp(name, "exec_from_dyn_atom") == 0) return run_exec_from_dyn_atom();
  if(strcmp(name, "exec_from_dyn_ptr_bytes") == 0) return run_exec_from_dyn_ptr_bytes();
  if(strcmp(name, "exec_trap_bytes_get_oob") == 0) return run_exec_trap_bytes_get_oob();
  if(strcmp(name, "exec_trap_from_dyn_bool_mismatch") == 0) return run_exec_trap_from_dyn_bool_mismatch();
  if(strcmp(name, "exec_try_catch_catches_trap_bytes_oob") == 0) return run_exec_try_catch_catches_trap_bytes_oob();
  if(strcmp(name, "exec_try_catch_catches_throw_payload") == 0) return run_exec_try_catch_catches_throw_payload();
  if(strcmp(name, "exec_kindof_i32_and_null") == 0) return run_exec_kindof_i32_and_null();
  if(strcmp(name, "exec_match_when_and_fallthrough_i32") == 0) return run_exec_match_when_and_fallthrough_i32();
  if(strcmp(name, "exec_match_object_prop_and_when") == 0) return run_exec_match_object_prop_and_when();
  if(strcmp(name, "exec_match_list_head_and_when") == 0) return run_exec_match_list_head_and_when();
  if(strcmp(name, "exec_match_array_len_index_and_when") == 0) return run_exec_match_array_len_index_and_when();
  if(strcmp(name, "exec_switch_kind_dispatch_list") == 0) return run_exec_switch_kind_dispatch_list();
  if(strcmp(name, "exec_i8_i16_to_dyn_roundtrip") == 0) return run_exec_i8_i16_to_dyn_roundtrip();
  if(strcmp(name, "exec_unary_neg_i32_and_not_bool") == 0) return run_exec_unary_neg_i32_and_not_bool();
  if(strcmp(name, "exec_f64_dyn_roundtrip") == 0) return run_exec_f64_dyn_roundtrip();
  if(strcmp(name, "exec_f32_dyn_and_call_return") == 0) return run_exec_f32_dyn_and_call_return();
  if(strcmp(name, "exec_i8_const_to_dyn") == 0) return run_exec_i8_const_to_dyn();
  if(strcmp(name, "exec_f32_arith_and_convert") == 0) return run_exec_f32_arith_and_convert();
  if(strcmp(name, "exec_f64_arith_and_convert") == 0) return run_exec_f64_arith_and_convert();
  if(strcmp(name, "exec_i64_f64_explicit_conversions") == 0) return run_exec_i64_f64_explicit_conversions();
  if(strcmp(name, "exec_i64_arith_and_eq_wrap") == 0) return run_exec_i64_arith_and_eq_wrap();
  if(strcmp(name, "exec_trap_f64_to_i32_overflow") == 0) return run_exec_trap_f64_to_i32_overflow();
  if(strcmp(name, "exec_const_i64_pool") == 0) return run_exec_const_i64_pool();
  if(strcmp(name, "exec_const_f64_pool") == 0) return run_exec_const_f64_pool();
  if(strcmp(name, "exec_i64_spill_roundtrip") == 0) return run_exec_i64_spill_roundtrip();
  if(strcmp(name, "exec_f64_spill_roundtrip") == 0) return run_exec_f64_spill_roundtrip();
  if(strcmp(name, "exec_wideslot_layout_i64") == 0) return run_exec_wideslot_layout_i64();
  if(strcmp(name, "exec_wideslot_layout_f64") == 0) return run_exec_wideslot_layout_f64();
  if(strcmp(name, "exec_if_else_eq_i32") == 0) return run_exec_if_else_eq_i32();
  if(strcmp(name, "exec_loop_countdown_i32") == 0) return run_exec_loop_countdown_i32();
  fprintf(stderr, "unknown test: %s\n", name);
  return 2;
}

