#include <jelly/internal.h>

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

static void assert_u32_eq(const char* test, uint32_t got, uint32_t want, const char* msg) {
  if(got != want) {
    char buf[256];
    snprintf(buf, sizeof(buf), "%s (got=%u want=%u)", msg, got, want);
    failf(test, buf);
  }
}

static void assert_str_eq(const char* test, const char* got, const char* want, const char* msg) {
  if(!got || !want || strcmp(got, want) != 0) {
    char buf[256];
    snprintf(buf, sizeof(buf), "%s (got='%s' want='%s')", msg, got ? got : "(null)", want ? want : "(null)");
    failf(test, buf);
  }
}

static void assert_bytes_eq(const char* test, const uint8_t* got, const uint8_t* want, size_t n, const char* msg) {
  if(!got || !want || memcmp(got, want, n) != 0) {
    failf(test, msg);
  }
}

static void wr_u32(uint8_t* p, uint32_t v) {
  p[0] = (uint8_t)(v & 0xffu);
  p[1] = (uint8_t)((v >> 8) & 0xffu);
  p[2] = (uint8_t)((v >> 16) & 0xffu);
  p[3] = (uint8_t)((v >> 24) & 0xffu);
}

static int run_loader_minimal_ok(void) {
  const char* t = "loader_minimal_ok";
  // Build a minimal module:
  // - version=1
  // - types: [I32, Dynamic]
  // - sigs:  none
  // - atoms: none
  // - funcs: 1 function, entry=0
  // - function: nregs=1 (I32), ninsns=1: RET r0
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4; // version
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // type 0: I32
  buf[off++] = (uint8_t)JELLY_T_I32; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  // type 1: Dynamic
  buf[off++] = (uint8_t)JELLY_T_DYNAMIC; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // function 0
  wr_u32(buf + off, 1); off += 4; // nregs
  wr_u32(buf + off, 1); off += 4; // ninsns
  wr_u32(buf + off, 0); off += 4; // reg0 type = I32 (type_id 0)
  // insn: op=RET(1) a=0 b=0 c=0 imm=0
  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0;
  buf[off++] = 0;
  buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "load should succeed");
  assert_u32_eq(t, m->ntypes, 2, "ntypes=2");
  assert_u32_eq(t, m->nfuncs, 1, "nfuncs=1");
  assert_u32_eq(t, m->entry, 0, "entry=0");
  assert_u32_eq(t, m->funcs[0].nregs, 1, "nregs=1");
  assert_u32_eq(t, m->funcs[0].ninsns, 1, "ninsns=1");
  jelly_bc_free(m);
  return failures == 0 ? 0 : 1;
}

static int run_loader_bad_magic(void) {
  const char* t = "loader_bad_magic";
  uint8_t buf[8] = {0};
  wr_u32(buf, 0xDEADBEEFu);
  wr_u32(buf + 4, 1);
  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, sizeof(buf), &m);
  assert_true(t, r.err == JELLY_BC_BAD_MAGIC, "bad magic detected");
  return failures == 0 ? 0 : 1;
}

static int run_loader_nregs_too_big(void) {
  const char* t = "loader_nregs_too_big";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // type 0: I32
  buf[off++] = (uint8_t)JELLY_T_I32; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // function 0 with nregs=300
  wr_u32(buf + off, 300); off += 4;
  wr_u32(buf + off, 0); off += 4; // ninsns

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "nregs>256 rejected");
  return failures == 0 ? 0 : 1;
}

static int run_loader_bad_insn_type_mismatch(void) {
  const char* t = "loader_bad_insn_type_mismatch";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // type0: I32, type1: Bool
  buf[off++] = (uint8_t)JELLY_T_I32; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;
  buf[off++] = (uint8_t)JELLY_T_BOOL; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // function: r0:Bool, r1:I32, r2:I32  ; insn: ADD_I32 r2 = r0 + r1  (bad: r0 is bool)
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4; // r0 -> Bool
  wr_u32(buf + off, 0); off += 4; // r1 -> I32
  wr_u32(buf + off, 0); off += 4; // r2 -> I32
  buf[off++] = (uint8_t)JOP_ADD_I32;
  buf[off++] = 2; // a
  buf[off++] = 0; // b
  buf[off++] = 1; // c
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "type mismatch rejected");
  return failures == 0 ? 0 : 1;
}

static int run_loader_atoms_roundtrip(void) {
  const char* t = "loader_atoms_roundtrip";
  uint8_t buf[512];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4; // version
  wr_u32(buf + off, 0); off += 4; // features
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 2); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry

  // type0: I32
  buf[off++] = (uint8_t)JELLY_T_I32; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // atoms:
  // atom0 = "a"
  wr_u32(buf + off, 1); off += 4;
  buf[off++] = (uint8_t)'a';
  // atom1 = "hello"
  wr_u32(buf + off, 5); off += 4;
  memcpy(buf + off, "hello", 5); off += 5;

  // function0: nregs=1 (I32), ninsns=1: RET r0
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "load should succeed");
  assert_u32_eq(t, m->natoms, 2, "natoms=2");
  assert_true(t, m->atoms != NULL, "atoms ptr array stored");
  assert_str_eq(t, m->atoms[0], "a", "atom0 matches");
  assert_str_eq(t, m->atoms[1], "hello", "atom1 matches");
  jelly_bc_free(m);
  return failures == 0 ? 0 : 1;
}

static int run_loader_const_bytes_roundtrip(void) {
  const char* t = "loader_const_bytes_roundtrip";
  uint8_t buf[512];
  size_t off = 0;

  // header
  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, (uint32_t)JELLY_BC_FEAT_CONSTBYTES); off += 4; // features
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4; // nsigs
  wr_u32(buf + off, 0); off += 4; // natoms
  wr_u32(buf + off, 1); off += 4; // nfuncs
  wr_u32(buf + off, 0); off += 4; // entry
  wr_u32(buf + off, 1); off += 4; // nconst_bytes

  // type0: bytes
  buf[off++] = (uint8_t)JELLY_T_BYTES; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // const_bytes[0] = [0x41, 0x00, 0xff]
  const uint8_t want[3] = { 0x41u, 0x00u, 0xffu };
  wr_u32(buf + off, 3); off += 4;
  memcpy(buf + off, want, sizeof(want)); off += sizeof(want);

  // function0: nregs=1 (bytes), ninsns=2: CONST_BYTES r0, idx=0; RET r0
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;
  buf[off++] = (uint8_t)JOP_CONST_BYTES;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;
  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_OK, "load should succeed");
  assert_u32_eq(t, m->nconst_bytes, 1, "nconst_bytes=1");
  assert_u32_eq(t, m->const_bytes_len[0], 3, "const_bytes_len[0]=3");
  assert_bytes_eq(t, m->const_bytes_data + m->const_bytes_off[0], want, sizeof(want), "const bytes payload matches");
  jelly_bc_free(m);
  return failures == 0 ? 0 : 1;
}

static int run_loader_const_bool_bad_imm(void) {
  const char* t = "loader_const_bool_bad_imm";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // type0: Bool
  buf[off++] = (uint8_t)JELLY_T_BOOL; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // function: r0:Bool ; insn: CONST_BOOL with imm=2 (stored in c)
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  buf[off++] = (uint8_t)JOP_CONST_BOOL;
  buf[off++] = 0; // a
  buf[off++] = 0; // b
  buf[off++] = 2; // c (bad)
  wr_u32(buf + off, 0); off += 4;

  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "bad bool imm rejected");
  return failures == 0 ? 0 : 1;
}

static int run_loader_const_i8_oob(void) {
  const char* t = "loader_const_i8_oob";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // type0: I8
  buf[off++] = (uint8_t)JELLY_T_I8; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // function: r0:I8 ; insn: CONST_I32 r0, imm=200 (oob for i8)
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  buf[off++] = (uint8_t)JOP_CONST_I32;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 200); off += 4;

  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "i8 range enforced on const_i32");
  return failures == 0 ? 0 : 1;
}

static int run_loader_const_i16_oob(void) {
  const char* t = "loader_const_i16_oob";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // type0: I16
  buf[off++] = (uint8_t)JELLY_T_I16; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // function: r0:I16 ; insn: CONST_I32 r0, imm=40000 (oob for i16)
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  buf[off++] = (uint8_t)JOP_CONST_I32;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 40000); off += 4;

  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "i16 range enforced on const_i32");
  return failures == 0 ? 0 : 1;
}

static int run_loader_atom_id_oob(void) {
  const char* t = "loader_atom_id_oob";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 2); off += 4; // ntypes: Atom, Object
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // natoms = 1
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // type0: Atom, type1: Object
  buf[off++] = (uint8_t)JELLY_T_ATOM; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;
  buf[off++] = (uint8_t)JELLY_T_OBJECT; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // atoms: one empty atom (id 0)
  wr_u32(buf + off, 0); off += 4;

  // function: r0:Object, r1:Atom ; insn: CONST_ATOM r1, imm=123 (oob)
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 3); off += 4;
  wr_u32(buf + off, 1); off += 4; // r0 -> Object
  wr_u32(buf + off, 0); off += 4; // r1 -> Atom

  buf[off++] = (uint8_t)JOP_OBJ_NEW;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0; wr_u32(buf + off, 0); off += 4;
  buf[off++] = (uint8_t)JOP_CONST_ATOM;
  buf[off++] = 1; buf[off++] = 0; buf[off++] = 0; wr_u32(buf + off, 123); off += 4;
  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 1; buf[off++] = 0; buf[off++] = 0; wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "atom id bounds enforced");
  return failures == 0 ? 0 : 1;
}

static int run_loader_jmp_target_oob(void) {
  const char* t = "loader_jmp_target_oob";
  uint8_t buf[256];
  size_t off = 0;

  wr_u32(buf + off, JELLY_BC_MAGIC); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4; // ntypes
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 0); off += 4;
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 0); off += 4;

  // type0: I32
  buf[off++] = (uint8_t)JELLY_T_I32; buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4; wr_u32(buf + off, 0); off += 4;

  // function: r0:I32 ; insn0: JMP +1000 (oob)
  wr_u32(buf + off, 1); off += 4;
  wr_u32(buf + off, 2); off += 4;
  wr_u32(buf + off, 0); off += 4;

  buf[off++] = (uint8_t)JOP_JMP;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, (uint32_t)1000); off += 4;

  buf[off++] = (uint8_t)JOP_RET;
  buf[off++] = 0; buf[off++] = 0; buf[off++] = 0;
  wr_u32(buf + off, 0); off += 4;

  jelly_bc_module* m = NULL;
  jelly_bc_result r = jelly_bc_read(buf, off, &m);
  assert_true(t, r.err == JELLY_BC_BAD_FORMAT, "jmp target bounds enforced");
  return failures == 0 ? 0 : 1;
}

int main(int argc, char** argv) {
  if(argc < 2) {
    fprintf(stderr, "usage: %s <testname>\n", argv[0]);
    return 2;
  }
  const char* name = argv[1];
  if(strcmp(name, "loader_minimal_ok") == 0) return run_loader_minimal_ok();
  if(strcmp(name, "loader_bad_magic") == 0) return run_loader_bad_magic();
  if(strcmp(name, "loader_nregs_too_big") == 0) return run_loader_nregs_too_big();
  if(strcmp(name, "loader_bad_insn_type_mismatch") == 0) return run_loader_bad_insn_type_mismatch();
  if(strcmp(name, "loader_atoms_roundtrip") == 0) return run_loader_atoms_roundtrip();
  if(strcmp(name, "loader_const_bytes_roundtrip") == 0) return run_loader_const_bytes_roundtrip();
  if(strcmp(name, "loader_const_bool_bad_imm") == 0) return run_loader_const_bool_bad_imm();
  if(strcmp(name, "loader_const_i8_oob") == 0) return run_loader_const_i8_oob();
  if(strcmp(name, "loader_const_i16_oob") == 0) return run_loader_const_i16_oob();
  if(strcmp(name, "loader_atom_id_oob") == 0) return run_loader_atom_id_oob();
  if(strcmp(name, "loader_jmp_target_oob") == 0) return run_loader_jmp_target_oob();
  fprintf(stderr, "unknown test: %s\n", name);
  return 2;
}

