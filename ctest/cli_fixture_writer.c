#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void wr_u8(uint8_t* p, uint8_t v) { p[0] = v; }
static void wr_u16(uint8_t* p, uint16_t v) {
  p[0] = (uint8_t)(v & 0xffu);
  p[1] = (uint8_t)((v >> 8) & 0xffu);
}
static void wr_u32(uint8_t* p, uint32_t v) {
  p[0] = (uint8_t)(v & 0xffu);
  p[1] = (uint8_t)((v >> 8) & 0xffu);
  p[2] = (uint8_t)((v >> 16) & 0xffu);
  p[3] = (uint8_t)((v >> 24) & 0xffu);
}

int main(int argc, char** argv) {
  if(argc != 2 || !argv[1] || argv[1][0] == '\0') {
    fprintf(stderr, "usage: cli_fixture_writer <out.jlyb>\n");
    return 2;
  }

  // Minimal module:
  // - one type: I32
  // - one function (entry=0): r0:I32 = 7; ret r0
  //
  // Total size = 32(header) + 12(types) + 28(function) = 72 bytes.
  uint8_t buf[72];
  memset(buf, 0, sizeof(buf));

  size_t off = 0;
  wr_u32(buf + off, 0x4A4C5942u); off += 4; // magic 'JLYB'
  wr_u32(buf + off, 1); off += 4;          // version
  wr_u32(buf + off, 0); off += 4;          // features
  wr_u32(buf + off, 1); off += 4;          // ntypes
  wr_u32(buf + off, 0); off += 4;          // nsigs
  wr_u32(buf + off, 0); off += 4;          // natoms
  wr_u32(buf + off, 1); off += 4;          // nfuncs
  wr_u32(buf + off, 0); off += 4;          // entry

  // type[0] = I32
  wr_u8(buf + off, 3); off += 1;   // kind = JELLY_T_I32
  wr_u8(buf + off, 0); off += 1;   // pad
  wr_u16(buf + off, 0); off += 2;  // pad
  wr_u32(buf + off, 0); off += 4;  // p0
  wr_u32(buf + off, 0); off += 4;  // p1

  // function[0]
  wr_u32(buf + off, 1); off += 4;  // nregs
  wr_u32(buf + off, 2); off += 4;  // ninsns
  wr_u32(buf + off, 0); off += 4;  // reg_type[0] = type_id 0 (I32)

  // insn 0: CONST_I32 r0, imm=7
  wr_u8(buf + off + 0, 10); // op = JOP_CONST_I32
  wr_u8(buf + off + 1, 0);  // a = dst r0
  wr_u8(buf + off + 2, 0);  // b
  wr_u8(buf + off + 3, 0);  // c
  wr_u32(buf + off + 4, 7); // imm
  off += 8;

  // insn 1: RET r0
  wr_u8(buf + off + 0, 1);  // op = JOP_RET
  wr_u8(buf + off + 1, 0);  // a = r0
  wr_u8(buf + off + 2, 0);  // b
  wr_u8(buf + off + 3, 0);  // c
  wr_u32(buf + off + 4, 0); // imm
  off += 8;

  if(off != sizeof(buf)) {
    fprintf(stderr, "internal error: wrote %zu bytes (expected %zu)\n", off, sizeof(buf));
    return 1;
  }

  FILE* f = fopen(argv[1], "wb");
  if(!f) {
    fprintf(stderr, "failed to open for write: %s\n", argv[1]);
    return 1;
  }
  size_t n = fwrite(buf, 1, sizeof(buf), f);
  if(n != sizeof(buf)) {
    fprintf(stderr, "short write: %zu/%zu\n", n, sizeof(buf));
    fclose(f);
    return 1;
  }
  if(fclose(f) != 0) {
    fprintf(stderr, "failed to close: %s\n", argv[1]);
    return 1;
  }
  return 0;
}

