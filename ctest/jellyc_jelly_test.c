#include <jelly/loader.h>
#include <jelly/vm.h>
#include <jelly/bytes.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint8_t* read_file(const char* path, size_t* out_size) {
  if(out_size) *out_size = 0;
  FILE* f = fopen(path, "rb");
  if(!f) return NULL;
  if(fseek(f, 0, SEEK_END) != 0) { fclose(f); return NULL; }
  long sz = ftell(f);
  if(sz < 0) { fclose(f); return NULL; }
  if(fseek(f, 0, SEEK_SET) != 0) { fclose(f); return NULL; }
  uint8_t* data = (uint8_t*)malloc((size_t)sz);
  if(!data) { fclose(f); return NULL; }
  size_t got = fread(data, 1, (size_t)sz, f);
  fclose(f);
  if(got != (size_t)sz) { free(data); return NULL; }
  if(out_size) *out_size = (size_t)sz;
  return data;
}

static int run_and_expect_ok_bytes(const char* out_path) {
  size_t size = 0;
  uint8_t* data = read_file(out_path, &size);
  if(!data) {
    fprintf(stderr, "failed to read output bytecode: %s\n", out_path);
    return 2;
  }

  jelly_bc_module m;
  jelly_bc_result r = jelly_bc_read(data, size, &m);
  free(data);
  if(r.err != JELLY_BC_OK) {
    fprintf(stderr, "bytecode load failed: err=%d msg=%s off=%zu\n",
            (int)r.err, r.msg ? r.msg : "(null)", r.offset);
    return 1;
  }

  jelly_vm vm;
  jelly_vm_init(&vm);
  jelly_value outv = jelly_make_null();
  jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &outv);
  if(st != JELLY_EXEC_OK) {
    fprintf(stderr, "vm trapped: code=%d msg=%s\n",
            (int)jelly_vm_last_trap_code(&vm),
            jelly_vm_last_trap_msg(&vm) ? jelly_vm_last_trap_msg(&vm) : "(null)");
    jelly_bc_free(&m);
    jelly_vm_shutdown(&vm);
    return 1;
  }

  if(!jelly_is_ptr(outv) || jelly_obj_kind_of(outv) != (uint32_t)JELLY_OBJ_BYTES) {
    fprintf(stderr, "expected bytes return value\n");
    jelly_bc_free(&m);
    jelly_vm_shutdown(&vm);
    return 1;
  }
  jelly_bytes* b = (jelly_bytes*)jelly_as_ptr(outv);
  const char* want = "ok";
  if(b->length != 2 || memcmp(b->data, want, 2) != 0) {
    fprintf(stderr, "expected bytes \"ok\" (len=%u)\n", b->length);
    jelly_bc_free(&m);
    jelly_vm_shutdown(&vm);
    return 1;
  }

  jelly_bc_free(&m);
  jelly_vm_shutdown(&vm);
  return 0;
}

int main(int argc, char** argv) {
  if(argc != 5) {
    fprintf(stderr, "usage: %s <jellyc_bin> <backend ast|ir> <input.jelly> <out.jlyb>\n",
            argv[0] ? argv[0] : "jellyc_jelly_test");
    return 2;
  }
  const char* jellyc = argv[1];
  const char* backend = argv[2];
  const char* in_path = argv[3];
  const char* out_path = argv[4];

  if(strcmp(backend, "ast") != 0 && strcmp(backend, "ir") != 0) {
    fprintf(stderr, "bad backend: %s\n", backend);
    return 2;
  }

  char cmd[4096];
  int n = snprintf(cmd, sizeof(cmd), "\"%s\" \"%s\" --backend %s --out \"%s\"",
                   jellyc, in_path, backend, out_path);
  if(n <= 0 || (size_t)n >= sizeof(cmd)) {
    fprintf(stderr, "command too long\n");
    return 2;
  }

  int rc = system(cmd);
  if(rc != 0) {
    fprintf(stderr, "jellyc returned non-zero\n");
    return 1;
  }

  return run_and_expect_ok_bytes(out_path);
}

