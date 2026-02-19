#include <jelly/bytecode.h>
#include <jelly/loader.h>
#include <jelly/vm.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static void wr_u32(uint8_t* p, uint32_t v) {
  p[0] = (uint8_t)(v & 0xffu);
  p[1] = (uint8_t)((v >> 8) & 0xffu);
  p[2] = (uint8_t)((v >> 16) & 0xffu);
  p[3] = (uint8_t)((v >> 24) & 0xffu);
}

static void wr_type(uint8_t* buf, size_t* off, uint8_t kind_u8, uint32_t p0) {
  buf[(*off)++] = kind_u8;
  buf[(*off)++] = 0;
  buf[(*off)++] = 0;
  buf[(*off)++] = 0;
  wr_u32(buf + *off, p0); *off += 4;
  wr_u32(buf + *off, 0);  *off += 4;
}

static void wr_insn(uint8_t* buf, size_t* off, uint8_t op, uint8_t a, uint8_t b, uint8_t c, uint32_t imm) {
  buf[(*off)++] = op;
  buf[(*off)++] = a;
  buf[(*off)++] = b;
  buf[(*off)++] = c;
  wr_u32(buf + *off, imm); *off += 4;
}

static uint64_t now_ns(void) {
#if defined(CLOCK_MONOTONIC)
  struct timespec ts;
  if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
    return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
  }
#endif
  // Fallback: CPU time.
  return (uint64_t)clock() * (1000000000ull / (uint64_t)CLOCKS_PER_SEC);
}

static jelly_bc_result make_loop_module(uint32_t n, jelly_bc_module* out) {
  // Bytecode (single function, entry=0):
  // r0=i (I32)
  // r1=sum (I32)
  // r2=one (I32)
  // r3=zero (I32)
  // r4=cond (Bool)
  //
  // i=n; sum=0; one=1; zero=0;
  // loop:
  //   sum = sum + i
  //   i = i - one
  //   cond = (i == zero)
  //   if cond jump +1 to end
  //   jmp -5 to loop
  // end:
  //   ret sum
  //
  // NOTE: uses the same instruction pattern as the VM exec test.

  uint8_t buf[1024];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32(0), Bool(1)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  // function0
  wr_u32(buf + off, 5); off += 4;  // nregs
  wr_u32(buf + off, 10); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;  // r0 I32
  wr_u32(buf + off, 0); off += 4;  // r1 I32
  wr_u32(buf + off, 0); off += 4;  // r2 I32
  wr_u32(buf + off, 0); off += 4;  // r3 I32
  wr_u32(buf + off, 1); off += 4;  // r4 Bool

  // insns
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, n);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32,   1, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,   0, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,    4, 0, 3, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,    4, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP,       0, 0, 0, (uint32_t)(int32_t)-5);
  wr_insn(buf, &off, (uint8_t)JOP_RET,       1, 0, 0, 0);

  return jelly_bc_read(buf, off, out);
}

static jelly_bc_result make_todyn_fromdyn_loop_module(uint32_t n, jelly_bc_module* out) {
  // r0:i32(i), r1:dynamic(tmp), r2:i32(one), r3:i32(sum), r4:i32(zero), r5:bool(cond)
  //
  // i=n; one=1; sum=0; zero=0;
  // loop:
  //   dyn = ToDyn(i)
  //   i2 = FromDyn_I32(dyn)
  //   sum = sum + i2
  //   i = i - one
  //   cond = (i == zero)
  //   if cond jump +1 to end
  //   jmp -7 to loop
  // end: ret sum
  uint8_t buf[1536];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32(0), Dynamic(1), Bool(2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);

  wr_u32(buf + off, 6); off += 4;  // nregs
  wr_u32(buf + off, 13); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;  // r0 I32
  wr_u32(buf + off, 1); off += 4;  // r1 Dynamic
  wr_u32(buf + off, 0); off += 4;  // r2 I32
  wr_u32(buf + off, 0); off += 4;  // r3 I32
  wr_u32(buf + off, 0); off += 4;  // r4 I32
  wr_u32(buf + off, 2); off += 4;  // r5 Bool

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, n);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 4, 0, 0, 0);

  // loop starts at pc=4
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN,       1, 0, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_FROM_DYN_I32, 0, 1, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32,      3, 3, 0, 0);
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,      0, 0, 2, 0);
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,       5, 0, 4, 0);
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,       5, 0, 0, 1);
  wr_insn(buf, &off, (uint8_t)JOP_JMP,          0, 0, 0, (uint32_t)(int32_t)-7);
  wr_insn(buf, &off, (uint8_t)JOP_RET,          3, 0, 0, 0);

  // Padding: keep ninsns consistent with above comment (13)
  // Two NOPs to reduce sensitivity if we later tweak loop body.
  wr_insn(buf, &off, (uint8_t)JOP_NOP,  0, 0, 0, 0);

  return jelly_bc_read(buf, off, out);
}

static jelly_bc_result make_array_i32_module(uint32_t n, jelly_bc_module* out) {
  // Writes arr[idx]=idx, then reads back and sums idx.
  // Expected: sum_{k=0..n-1} k = n*(n-1)/2
  uint8_t buf[2048];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32(0), Bool(1), Array<I32>(2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_ARRAY, 0); // p0=elem type_id (I32)

  // regs: r0:i, r1:sum, r2:one, r3:zero, r4:idx, r5:arr, r6:tmp, r7:cond
  wr_u32(buf + off, 8); off += 4;
  wr_u32(buf + off, 21); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4; // r0 I32
  wr_u32(buf + off, 0); off += 4; // r1 I32
  wr_u32(buf + off, 0); off += 4; // r2 I32
  wr_u32(buf + off, 0); off += 4; // r3 I32
  wr_u32(buf + off, 0); off += 4; // r4 I32
  wr_u32(buf + off, 2); off += 4; // r5 Array<I32>
  wr_u32(buf + off, 0); off += 4; // r6 I32
  wr_u32(buf + off, 1); off += 4; // r7 Bool

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, n); // 0 i=n
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 0); // 1 sum=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1); // 2 one=1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 0); // 3 zero=0
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_NEW, 5, 0, 0, 0); // 4 arr=new(len=i)

  // fill loop at 5
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,   4, 0, 2, 0); // 5 idx=i-1
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_SET, 4, 5, 4, 0); // 6 arr[idx]=idx
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,   0, 0, 2, 0); // 7 i--
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,    7, 0, 3, 0); // 8 cond=(i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,    7, 0, 0, 1); // 9 -> after_fill
  wr_insn(buf, &off, (uint8_t)JOP_JMP,       0, 0, 0, (uint32_t)(int32_t)-6); // 10 back to 5

  // after_fill
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, n); // 11 i=n
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 0); // 12 sum=0

  // sum loop at 13
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,   4, 0, 2, 0); // 13 idx=i-1
  wr_insn(buf, &off, (uint8_t)JOP_ARRAY_GET, 6, 5, 4, 0); // 14 tmp=arr[idx]
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32,   1, 1, 6, 0); // 15 sum+=tmp
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,   0, 0, 2, 0); // 16 i--
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,    7, 0, 3, 0); // 17 cond=(i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,    7, 0, 0, 1); // 18 -> end
  wr_insn(buf, &off, (uint8_t)JOP_JMP,       0, 0, 0, (uint32_t)(int32_t)-7); // 19 back to 13

  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0); // 20
  return jelly_bc_read(buf, off, out);
}

static jelly_bc_result make_bytes_u8_module(uint32_t n_in, jelly_bc_module* out, uint32_t* used_n) {
  // Clamp length so bytes_set_u8 value (idx) stays <=255.
  uint32_t n = n_in > 255u ? 255u : n_in;
  if(used_n) *used_n = n;

  uint8_t buf[2048];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 3); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32(0), Bool(1), Bytes(2)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);

  // regs: r0:i, r1:sum, r2:one, r3:zero, r4:idx, r5:bytes, r6:tmp, r7:cond
  wr_u32(buf + off, 8); off += 4;
  wr_u32(buf + off, 21); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // bytes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, n); // 0 i=n
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 0); // 1 sum=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 2, 0, 0, 1); // 2 one=1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 3, 0, 0, 0); // 3 zero=0
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW, 5, 0, 0, 0); // 4 bytes=new(len=i)

  // fill loop at 5
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,      4, 0, 2, 0); // 5 idx=i-1
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_SET_U8, 4, 5, 4, 0); // 6 bytes[idx]=idx
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,      0, 0, 2, 0); // 7 i--
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,       7, 0, 3, 0); // 8 cond=(i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,       7, 0, 0, 1); // 9 -> after_fill
  wr_insn(buf, &off, (uint8_t)JOP_JMP,          0, 0, 0, (uint32_t)(int32_t)-6); // 10 back to 5

  // after_fill
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 0, 0, 0, n); // 11 i=n
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 1, 0, 0, 0); // 12 sum=0

  // sum loop at 13
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,      4, 0, 2, 0); // 13 idx=i-1
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_GET_U8, 6, 5, 4, 0); // 14 tmp=bytes[idx]
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32,      1, 1, 6, 0); // 15 sum+=tmp
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32,      0, 0, 2, 0); // 16 i--
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,       7, 0, 3, 0); // 17 cond=(i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,       7, 0, 0, 1); // 18 -> end
  wr_insn(buf, &off, (uint8_t)JOP_JMP,          0, 0, 0, (uint32_t)(int32_t)-7); // 19 back to 13

  wr_insn(buf, &off, (uint8_t)JOP_RET, 1, 0, 0, 0); // 20
  return jelly_bc_read(buf, off, out);
}

static jelly_bc_result make_kindof_switch_module(uint32_t n, jelly_bc_module* out) {
  // Alternates between Dynamic(i32) and Dynamic(bytes) using a toggled bool.
  // switch_kind dispatch adds:
  //  - +1 for i32
  //  - +2 for bytes
  // (default would be +3, but shouldn't trigger)
  uint8_t buf[2048];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 4); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // types: I32(0), Bool(1), Dynamic(2), Bytes(3)
  wr_type(buf, &off, (uint8_t)JELLY_T_I32, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BOOL, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_DYNAMIC, 0);
  wr_type(buf, &off, (uint8_t)JELLY_T_BYTES, 0);

  // regs:
  // r0 i:I32
  // r1 sum:I32
  // r2 one:I32
  // r3 zero:I32
  // r4 cond:Bool
  // r5 toggle:Bool
  // r6 bytes:Bytes
  // r7 dyn:Dynamic
  // r8 kind:I32
  // r9 two:I32
  // r10 three:I32
  wr_u32(buf + off, 11); off += 4;
  wr_u32(buf + off, 27); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32,  0, 0, 0, n); // 0 i=n
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32,  1, 0, 0, 0); // 1 sum=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32,  2, 0, 0, 1); // 2 one=1
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32,  3, 0, 0, 0); // 3 zero=0
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32,  9, 0, 0, 2); // 4 two=2
  wr_insn(buf, &off, (uint8_t)JOP_CONST_I32, 10, 0, 0, 3); // 5 three=3
  wr_insn(buf, &off, (uint8_t)JOP_CONST_BOOL, 5, 0, 0, 0); // 6 toggle=false (uses c=0)
  wr_insn(buf, &off, (uint8_t)JOP_BYTES_NEW,  6, 2, 0, 0); // 7 bytes=new(len=1)

  // loop at 8
  wr_insn(buf, &off, (uint8_t)JOP_NOT_BOOL,  5, 5, 0, 0); // 8 toggle = !toggle
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,    5, 0, 0, 2); // 9 if toggle jump to i32 select (12)
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN,    7, 6, 0, 0); // 10 dyn=ToDyn(bytes)
  wr_insn(buf, &off, (uint8_t)JOP_JMP,       0, 0, 0, 1); // 11 jump to after_select (13)
  wr_insn(buf, &off, (uint8_t)JOP_TO_DYN,    7, 0, 0, 0); // 12 dyn=ToDyn(i)
  wr_insn(buf, &off, (uint8_t)JOP_KINDOF,    8, 7, 0, 0); // 13 kind = kindof(dyn)

  // switch_kind at 14: table_end = 17
  wr_insn(buf, &off, (uint8_t)JOP_SWITCH_KIND, 8, 2, 0, 4); // 14 default -> +4 => pc 21
  wr_insn(buf, &off, (uint8_t)JOP_CASE_KIND,   2, 0, 0, 0); // 15 kind 2 (i32) -> +0 => pc 17
  wr_insn(buf, &off, (uint8_t)JOP_CASE_KIND,   4, 0, 0, 2); // 16 kind 4 (bytes) -> +2 => pc 19

  // case i32 at 17
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 2, 0); // 17 sum += 1
  wr_insn(buf, &off, (uint8_t)JOP_JMP,     0, 0, 0, 3); // 18 -> after_switch (22)

  // case bytes at 19
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 9, 0); // 19 sum += 2
  wr_insn(buf, &off, (uint8_t)JOP_JMP,     0, 0, 0, 1); // 20 -> after_switch (22)

  // default at 21
  wr_insn(buf, &off, (uint8_t)JOP_ADD_I32, 1, 1, 10, 0); // 21 sum += 3

  // after_switch at 22
  wr_insn(buf, &off, (uint8_t)JOP_SUB_I32, 0, 0, 2, 0); // 22 i--
  wr_insn(buf, &off, (uint8_t)JOP_EQ_I32,  4, 0, 3, 0); // 23 cond=(i==0)
  wr_insn(buf, &off, (uint8_t)JOP_JMP_IF,  4, 0, 0, 1); // 24 -> end (26)
  wr_insn(buf, &off, (uint8_t)JOP_JMP,     0, 0, 0, (uint32_t)(int32_t)-18); // 25 back to loop (8)
  wr_insn(buf, &off, (uint8_t)JOP_RET,     1, 0, 0, 0); // 26

  return jelly_bc_read(buf, off, out);
}

int main(int argc, char** argv) {
  uint32_t n = 50000;
  uint32_t runs = 200;
  const char* which = "loop_i32";

  for(int i = 1; i < argc; i++) {
    if(strcmp(argv[i], "--n") == 0 && i + 1 < argc) {
      n = (uint32_t)strtoul(argv[++i], NULL, 10);
    } else if(strcmp(argv[i], "--runs") == 0 && i + 1 < argc) {
      runs = (uint32_t)strtoul(argv[++i], NULL, 10);
    } else if(strcmp(argv[i], "--bench") == 0 && i + 1 < argc) {
      which = argv[++i];
    } else if(strcmp(argv[i], "--help") == 0) {
      printf("usage: %s [--bench NAME] [--n N] [--runs RUNS]\n", argv[0]);
      printf("  benches:\n");
      printf("    loop_i32\n");
      printf("    todyn_i32\n");
      printf("    array_i32\n");
      printf("    bytes_u8\n");
      printf("    kindof_switch\n");
      return 0;
    } else {
      fprintf(stderr, "unknown arg: %s\n", argv[i]);
      return 2;
    }
  }

  jelly_bc_module m;
  jelly_bc_result r;
  int64_t expected = 0;
  uint32_t used_n = n;
  if(strcmp(which, "loop_i32") == 0) {
    r = make_loop_module(n, &m);
    expected = (int64_t)n * (int64_t)(n + 1u) / 2;
  } else if(strcmp(which, "todyn_i32") == 0) {
    r = make_todyn_fromdyn_loop_module(n, &m);
    expected = (int64_t)n * (int64_t)(n + 1u) / 2;
  } else if(strcmp(which, "array_i32") == 0) {
    r = make_array_i32_module(n, &m);
    expected = (int64_t)n * (int64_t)(n - 1u) / 2;
  } else if(strcmp(which, "bytes_u8") == 0) {
    r = make_bytes_u8_module(n, &m, &used_n);
    expected = (int64_t)used_n * (int64_t)(used_n - 1u) / 2;
  } else if(strcmp(which, "kindof_switch") == 0) {
    r = make_kindof_switch_module(n, &m);
    int64_t ci32 = (int64_t)((n + 1u) / 2u);
    int64_t cbytes = (int64_t)(n / 2u);
    expected = ci32 * 1 + cbytes * 2;
  } else {
    fprintf(stderr, "unknown bench: %s\n", which);
    return 2;
  }
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "failed to build bench module: %s\n", r.msg ? r.msg : "(no msg)");
    return 1;
  }

  // Warmup
  {
    jelly_vm vm = {0};
    (void)jelly_vm_exec(&vm, &m);
    free(vm.spill);
  }

  uint64_t t0 = now_ns();
  int64_t last = 0;
  for(uint32_t i = 0; i < runs; i++) {
    jelly_vm vm = {0};
    jelly_value out = jelly_vm_exec(&vm, &m);
    if(jelly_is_i32(out)) last = (int64_t)jelly_as_i32(out);
    free(vm.spill);
  }
  uint64_t t1 = now_ns();

  if(last != expected) {
    fprintf(stderr, "bad result: got %lld expected %lld (bench=%s n=%u used_n=%u)\n",
            (long long)last, (long long)expected, which, n, used_n);
    jelly_bc_free(&m);
    return 1;
  }

  double total_ms = (double)(t1 - t0) / 1000000.0;
  double per_run_us = (double)(t1 - t0) / 1000.0 / (double)runs;
  printf("jelly_bench %s n=%u runs=%u: total=%.2fms (%.2fus/run) result=%lld\n",
         which, n, runs, total_ms, per_run_us, (long long)last);

  jelly_bc_free(&m);
  return 0;
}

