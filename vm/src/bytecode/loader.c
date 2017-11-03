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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// --- little-endian readers ----------------------------------------------------

typedef struct {
  const uint8_t* p;
  size_t n;
  size_t off;
} rd;

static jelly_bc_result ok(void) {
  jelly_bc_result r = { JELLY_BC_OK, "ok", 0 };
  return r;
}

static jelly_bc_result err(jelly_bc_error e, const char* msg, size_t off) {
  jelly_bc_result r = { e, msg, off };
  return r;
}

static int rd_need(rd* r, size_t k) {
  return r->off + k <= r->n;
}

static jelly_bc_result rd_u8(rd* r, uint8_t* out) {
  if(!rd_need(r, 1)) return err(JELLY_BC_EOF, "unexpected EOF", r->off);
  *out = r->p[r->off++];
  return ok();
}

static jelly_bc_result rd_u16(rd* r, uint16_t* out) {
  if(!rd_need(r, 2)) return err(JELLY_BC_EOF, "unexpected EOF", r->off);
  uint16_t v = (uint16_t)(((uint16_t)r->p[r->off]) | (uint16_t)((uint16_t)r->p[r->off + 1] << 8));
  r->off += 2;
  *out = v;
  return ok();
}

static jelly_bc_result rd_u32(rd* r, uint32_t* out) {
  if(!rd_need(r, 4)) return err(JELLY_BC_EOF, "unexpected EOF", r->off);
  uint32_t v = (uint32_t)r->p[r->off]
    | ((uint32_t)r->p[r->off + 1] << 8)
    | ((uint32_t)r->p[r->off + 2] << 16)
    | ((uint32_t)r->p[r->off + 3] << 24);
  r->off += 4;
  *out = v;
  return ok();
}

// --- free helpers -------------------------------------------------------------

void jelly_bc_free(jelly_bc_module* m) {
  if(!m) return;

  if(m->sigs) {
    for(uint32_t i = 0; i < m->nsigs; i++) {
      free((void*)m->sigs[i].args);
    }
  }

  if(m->funcs) {
    for(uint32_t i = 0; i < m->nfuncs; i++) {
      free((void*)m->funcs[i].reg_types);
      free((void*)m->funcs[i].insns);
    }
  }

  free((void*)m->types);
  free((void*)m->sigs);
  free((void*)m->atoms_data);
  free((void*)m->atoms);
  free((void*)m->const_i64);
  free((void*)m->const_f64);
  free((void*)m->const_bytes_len);
  free((void*)m->const_bytes_off);
  free((void*)m->const_bytes_data);
  free((void*)m->funcs);

  free(m);
}

// --- validation helpers -------------------------------------------------------

static jelly_bc_result validate_type_entry(const jelly_type_entry* t, uint32_t ntypes, uint32_t nsigs) {
  switch(t->kind) {
    case JELLY_T_I8:
    case JELLY_T_I16:
    case JELLY_T_I32:
    case JELLY_T_I64:
    case JELLY_T_F16:
    case JELLY_T_F32:
    case JELLY_T_F64:
    case JELLY_T_BOOL:
    case JELLY_T_ATOM:
    case JELLY_T_BYTES:
    case JELLY_T_DYNAMIC:
    case JELLY_T_OBJECT:
      return ok();
    case JELLY_T_LIST:
    case JELLY_T_ARRAY:
      if(t->as.unary.elem >= ntypes) return err(JELLY_BC_BAD_FORMAT, "bad elem type id", 0);
      return ok();
    case JELLY_T_FUNCTION:
      if(t->as.fun.sig_id >= nsigs) return err(JELLY_BC_BAD_FORMAT, "bad signature id", 0);
      return ok();
    case JELLY_T_ABSTRACT:
      return ok();
    default:
      return err(JELLY_BC_BAD_FORMAT, "unknown type kind", 0);
  }
}

// --- on-disk format -----------------------------------------------------------
//
// All integers are little-endian.
//
// header:
//   u32 magic        'JLYB'
//   u32 version      (currently 1)
//   u32 features
//   u32 ntypes
//   u32 nsigs
//   u32 natoms
//   u32 nfuncs
//   u32 entry
//   if(features & CONST64):
//     u32 nconst_i64
//     u32 nconst_f64
//   if(features & CONSTBYTES):
//     u32 nconst_bytes
//
// type table: ntypes * (u8 kind, u8 pad, u16 pad, u32 p0, u32 p1)
//   - LIST/ARRAY: p0=elem type_id
//   - FUNCTION:   p0=sig_id
//   - ABSTRACT:   p0=abstract_id
//   - others: p0/p1 ignored (0)
//
// sig table:
//   u32 ret_type
//   u16 nargs
//   u16 pad
//   u32 arg_type[nargs]
//
// atoms (MVP): natoms * (u32 utf8_len, u8 bytes[utf8_len])
//
// const pools (if features & CONST64), loaded before function table:
//   i64 pool: nconst_i64 * i64
//   f64 pool: nconst_f64 * f64
//
// bytes const pool (if features & CONSTBYTES), loaded before function table:
//   bytes pool: nconst_bytes * (u32 len, u8 bytes[len])
//
// functions:
//   u32 nregs   (<=256)
//   u32 ninsns
//   u32 reg_type[nregs]
//   insn[ninsns] where insn is:
//     u8 op, u8 a, u8 b, u8 c, u32 imm   (8 bytes)
//

jelly_bc_result jelly_bc_read(const uint8_t* data, size_t size, jelly_bc_module** out) {
  if(!out) return err(JELLY_BC_BAD_FORMAT, "out is NULL", 0);
  *out = (jelly_bc_module*)calloc(1, sizeof(jelly_bc_module));
  if(!*out) return err(JELLY_BC_OUT_OF_MEMORY, "oom module", 0);

  jelly_bc_module* m = *out;
  rd r = { data, size, 0 };
  uint32_t magic = 0;
  jelly_bc_result rr = rd_u32(&r, &magic);
  if(rr.err) return rr;
  if(magic != JELLY_BC_MAGIC) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_MAGIC, "bad magic", r.off - 4); }

  uint32_t version = 0;
  rr = rd_u32(&r, &version);
  if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  if(version != 1) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_UNSUPPORTED_VERSION, "unsupported version", r.off - 4); }

  m->version = version;
  rr = rd_u32(&r, &m->features); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }

  rr = rd_u32(&r, &m->ntypes); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  rr = rd_u32(&r, &m->nsigs);  if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  rr = rd_u32(&r, &m->natoms); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  rr = rd_u32(&r, &m->nfuncs); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  rr = rd_u32(&r, &m->entry);  if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  if(m->entry >= m->nfuncs) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_FORMAT, "entry out of range", r.off - 4); }

  if(m->features & (uint32_t)JELLY_BC_FEAT_CONST64) {
    rr = rd_u32(&r, &m->nconst_i64); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
    rr = rd_u32(&r, &m->nconst_f64); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  }
  if(m->features & (uint32_t)JELLY_BC_FEAT_CONSTBYTES) {
    rr = rd_u32(&r, &m->nconst_bytes); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
  }

  // --- types
  if(m->ntypes > 0) {
    jelly_type_entry* types = (jelly_type_entry*)calloc(m->ntypes, sizeof(jelly_type_entry));
    if(!types) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom types", r.off); }
    m->types = types;

    for(uint32_t i = 0; i < m->ntypes; i++) {
      uint8_t kind_u8 = 0;
      uint8_t pad1 = 0;
      uint16_t pad2 = 0;
      uint32_t p0 = 0, p1 = 0;
      rr = rd_u8(&r, &kind_u8); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      rr = rd_u8(&r, &pad1); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      rr = rd_u16(&r, &pad2); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      rr = rd_u32(&r, &p0); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      rr = rd_u32(&r, &p1); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      (void)pad1; (void)pad2; (void)p1;

      types[i].kind = (jelly_type_kind)kind_u8;
      switch(types[i].kind) {
        case JELLY_T_LIST:
        case JELLY_T_ARRAY:
          types[i].as.unary.elem = (jelly_type_id)p0;
          break;
        case JELLY_T_FUNCTION:
          types[i].as.fun.sig_id = p0;
          break;
        case JELLY_T_ABSTRACT:
          types[i].as.abs.abstract_id = p0;
          break;
        default:
          break;
      }

      jelly_bc_result vr = validate_type_entry(&types[i], m->ntypes, m->nsigs);
      if(vr.err) { jelly_bc_free(m); *out = NULL; vr.offset = r.off; return vr; }
    }
  }

  // --- signatures
  if(m->nsigs > 0) {
    jelly_fun_sig* sigs = (jelly_fun_sig*)calloc(m->nsigs, sizeof(jelly_fun_sig));
    if(!sigs) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom sigs", r.off); }
    m->sigs = sigs;

    for(uint32_t i = 0; i < m->nsigs; i++) {
      uint32_t ret = 0;
      uint16_t nargs = 0;
      uint16_t pad = 0;
      rr = rd_u32(&r, &ret); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      rr = rd_u16(&r, &nargs); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      rr = rd_u16(&r, &pad); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      (void)pad;

      if(ret >= m->ntypes) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_FORMAT, "sig ret type out of range", r.off); }

      jelly_type_id* args = NULL;
      if(nargs > 0) {
        args = (jelly_type_id*)malloc(sizeof(jelly_type_id) * (size_t)nargs);
        if(!args) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom sig args", r.off); }
        for(uint16_t j = 0; j < nargs; j++) {
          uint32_t a = 0;
          rr = rd_u32(&r, &a); if(rr.err) { free(args); jelly_bc_free(m); *out = NULL; return rr; }
          if(a >= m->ntypes) { free(args); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_FORMAT, "sig arg type out of range", r.off - 4); }
          args[j] = (jelly_type_id)a;
        }
      }

      sigs[i].ret = (jelly_type_id)ret;
      sigs[i].nargs = nargs;
      sigs[i].args = args;
    }
  }

  // --- atoms: store as UTF-8 NUL-terminated strings (owned by module)
  if(m->natoms > 0) {
    const size_t atoms_start = r.off;
    size_t total = 0;

    uint32_t* lens = (uint32_t*)malloc(sizeof(uint32_t) * (size_t)m->natoms);
    if(!lens) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom atom lens", r.off); }

    for(uint32_t i = 0; i < m->natoms; i++) {
      uint32_t len = 0;
      rr = rd_u32(&r, &len); if(rr.err) { free(lens); jelly_bc_free(m); *out = NULL; return rr; }
      lens[i] = len;
      if(len > 0) {
        if(!rd_need(&r, len)) { free(lens); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_EOF, "unexpected EOF in atom bytes", r.off); }
        r.off += len;
      }
      total += (size_t)len + 1u; // include NUL
    }

    const char** atoms = (const char**)calloc(m->natoms, sizeof(const char*));
    if(!atoms) { free(lens); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom atoms", atoms_start); }
    char* atom_data = (char*)malloc(total);
    if(!atom_data) { free(lens); free(atoms); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom atoms_data", atoms_start); }

    // second pass: fill
    r.off = atoms_start;
    char* w = atom_data;
    for(uint32_t i = 0; i < m->natoms; i++) {
      uint32_t len = 0;
      rr = rd_u32(&r, &len); if(rr.err) { free(lens); free(atoms); free(atom_data); jelly_bc_free(m); *out = NULL; return rr; }
      if(len != lens[i]) { free(lens); free(atoms); free(atom_data); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_FORMAT, "atom length mismatch", r.off - 4); }

      atoms[i] = w;
      if(len > 0) {
        if(!rd_need(&r, len)) { free(lens); free(atoms); free(atom_data); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_EOF, "unexpected EOF in atom bytes", r.off); }
        memcpy(w, r.p + r.off, len);
        r.off += len;
        w += len;
      }
      *w++ = 0;
    }

    free(lens);
    m->atoms = atoms;
    m->atoms_data = atom_data;
  }

  // --- const pools
  if(m->features & (uint32_t)JELLY_BC_FEAT_CONST64) {
    if(m->nconst_i64 > 0) {
      int64_t* p = (int64_t*)malloc(sizeof(int64_t) * (size_t)m->nconst_i64);
      if(!p) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom const_i64", r.off); }
      m->const_i64 = p;
      for(uint32_t i = 0; i < m->nconst_i64; i++) {
        uint32_t lo = 0, hi = 0;
        rr = rd_u32(&r, &lo); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
        rr = rd_u32(&r, &hi); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
        uint64_t u = (uint64_t)lo | ((uint64_t)hi << 32);
        memcpy(&p[i], &u, sizeof(u));
      }
    }
    if(m->nconst_f64 > 0) {
      double* p = (double*)malloc(sizeof(double) * (size_t)m->nconst_f64);
      if(!p) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom const_f64", r.off); }
      m->const_f64 = p;
      for(uint32_t i = 0; i < m->nconst_f64; i++) {
        uint32_t lo = 0, hi = 0;
        rr = rd_u32(&r, &lo); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
        rr = rd_u32(&r, &hi); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
        uint64_t u = (uint64_t)lo | ((uint64_t)hi << 32);
        memcpy(&p[i], &u, sizeof(u));
      }
    }
  }

  // --- bytes const pool
  if(m->features & (uint32_t)JELLY_BC_FEAT_CONSTBYTES) {
    const uint32_t n = m->nconst_bytes;
    if(n > 0) {
      uint32_t* lens = (uint32_t*)malloc(sizeof(uint32_t) * (size_t)n);
      uint32_t* offs = (uint32_t*)malloc(sizeof(uint32_t) * (size_t)n);
      if(!lens || !offs) {
        free(lens);
        free(offs);
        jelly_bc_free(m); *out = NULL;
        return err(JELLY_BC_OUT_OF_MEMORY, "oom const_bytes index", r.off);
      }

      size_t pool_start = r.off;
      uint32_t total = 0;

      // First pass: read lengths and skip payloads.
      for(uint32_t i = 0; i < n; i++) {
        uint32_t len = 0;
        rr = rd_u32(&r, &len);
        if(rr.err) { free(lens); free(offs); jelly_bc_free(m); *out = NULL; return rr; }
        lens[i] = len;
        offs[i] = total;
        total += len;
        if(len > 0) {
          if(!rd_need(&r, (size_t)len)) {
            free(lens); free(offs);
            jelly_bc_free(m); *out = NULL;
            return err(JELLY_BC_EOF, "unexpected EOF in const_bytes payload", r.off);
          }
          r.off += (size_t)len;
        }
      }

      uint8_t* blob = NULL;
      if(total > 0) {
        blob = (uint8_t*)malloc((size_t)total);
        if(!blob) {
          free(lens); free(offs);
          jelly_bc_free(m); *out = NULL;
          return err(JELLY_BC_OUT_OF_MEMORY, "oom const_bytes data", r.off);
        }
      }

      // Second pass: copy payloads.
      r.off = pool_start;
      for(uint32_t i = 0; i < n; i++) {
        uint32_t len = 0;
        rr = rd_u32(&r, &len);
        if(rr.err) { free(lens); free(offs); free(blob); jelly_bc_free(m); *out = NULL; return rr; }
        if(len != lens[i]) {
          free(lens); free(offs); free(blob);
          jelly_bc_free(m); *out = NULL;
          return err(JELLY_BC_BAD_FORMAT, "const_bytes length mismatch", r.off - 4);
        }
        if(len > 0) {
          if(!rd_need(&r, (size_t)len)) {
            free(lens); free(offs); free(blob);
            jelly_bc_free(m); *out = NULL;
            return err(JELLY_BC_EOF, "unexpected EOF in const_bytes payload", r.off);
          }
          memcpy(blob + offs[i], r.p + r.off, (size_t)len);
          r.off += (size_t)len;
        }
      }

      m->const_bytes_len = lens;
      m->const_bytes_off = offs;
      m->const_bytes_data = blob;
    }
  }

  // --- functions
  if(m->nfuncs > 0) {
    jelly_bc_function* funcs = (jelly_bc_function*)calloc(m->nfuncs, sizeof(jelly_bc_function));
    if(!funcs) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom funcs", r.off); }
    m->funcs = funcs;

    for(uint32_t i = 0; i < m->nfuncs; i++) {
      uint32_t nregs = 0, ninsns = 0, cap_start = 0;
      rr = rd_u32(&r, &nregs); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      if(m->features & (uint32_t)JELLY_BC_FEAT_CAP_START) {
        rr = rd_u32(&r, &cap_start); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      }
      rr = rd_u32(&r, &ninsns); if(rr.err) { jelly_bc_free(m); *out = NULL; return rr; }
      if(nregs > 256) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_FORMAT, "nregs > 256", r.off - 8); }

      jelly_type_id* reg_types = NULL;
      if(nregs > 0) {
        reg_types = (jelly_type_id*)malloc(sizeof(jelly_type_id) * (size_t)nregs);
        if(!reg_types) { jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom reg_types", r.off); }
        for(uint32_t j = 0; j < nregs; j++) {
          uint32_t tid = 0;
          rr = rd_u32(&r, &tid); if(rr.err) { free(reg_types); jelly_bc_free(m); *out = NULL; return rr; }
          if(tid >= m->ntypes) { free(reg_types); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_BAD_FORMAT, "reg type out of range", r.off - 4); }
          reg_types[j] = (jelly_type_id)tid;
        }
      }

      jelly_insn* insns = NULL;
      if(ninsns > 0) {
        insns = (jelly_insn*)malloc(sizeof(jelly_insn) * (size_t)ninsns);
        if(!insns) { free(reg_types); jelly_bc_free(m); *out = NULL; return err(JELLY_BC_OUT_OF_MEMORY, "oom insns", r.off); }

        for(uint32_t j = 0; j < ninsns; j++) {
          uint8_t op=0,a=0,b=0,c=0;
          uint32_t imm=0;
          rr = rd_u8(&r, &op); if(rr.err) { free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; return rr; }
          rr = rd_u8(&r, &a);  if(rr.err) { free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; return rr; }
          rr = rd_u8(&r, &b);  if(rr.err) { free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; return rr; }
          rr = rd_u8(&r, &c);  if(rr.err) { free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; return rr; }
          rr = rd_u32(&r, &imm); if(rr.err) { free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; return rr; }

          insns[j].op = op;
          insns[j].a = a;
          insns[j].b = b;
          insns[j].c = c;
          insns[j].imm = imm;

          jelly_bc_result vi = jelly_bc_validate_insn(m, reg_types, &insns[j], nregs, j, ninsns, m->nfuncs);
          if(vi.err) {
            free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; vi.offset = r.off; return vi;
          }
        }

        jelly_bc_result vr = jelly_bc_validate_function_semantics(m, reg_types, insns, nregs, ninsns);
        if(vr.err) { free(reg_types); free(insns); jelly_bc_free(m); *out = NULL; vr.offset = r.off; return vr; }
      }

      funcs[i].nregs = nregs;
      funcs[i].cap_start = cap_start;
      funcs[i].reg_types = reg_types;
      if(getenv("JELLY_TRACE_CLOSURE") && (cap_start > 0 || (m->features & (uint32_t)JELLY_BC_FEAT_CAP_START))) {
        fprintf(stderr, "[JELLY_TRACE] loader: func[%u] nregs=%u cap_start=%u feat_cap_start=%u\n",
                (unsigned)i, (unsigned)nregs, (unsigned)cap_start,
                (unsigned)((m->features & (uint32_t)JELLY_BC_FEAT_CAP_START) ? 1 : 0));
      }
      funcs[i].ninsns = ninsns;
      funcs[i].insns = insns;
    }
  }

  // no trailing bytes requirement (allow concatenation/embedding later)
  return ok();
}

