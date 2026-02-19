#ifndef JELLY_LOADER_H
#define JELLY_LOADER_H

/*
  Bytecode loader/validator.

  Contract:
  - parses little-endian `JLYB` modules into an owned `jelly_bc_module`
  - validates bounds, indices, and basic structural invariants
  - does not execute; interpreter/JIT are separate
*/

#include <stddef.h>
#include <stdint.h>

#include "bytecode.h"

typedef enum jelly_bc_error {
  JELLY_BC_OK = 0,
  JELLY_BC_EOF,
  JELLY_BC_BAD_MAGIC,
  JELLY_BC_UNSUPPORTED_VERSION,
  JELLY_BC_BAD_FORMAT,
  JELLY_BC_OUT_OF_MEMORY,
} jelly_bc_error;

typedef struct jelly_bc_result {
  jelly_bc_error err;
  const char* msg; // static string
  size_t offset;   // best-effort byte offset of failure
} jelly_bc_result;

// Parse a bytecode module from memory.
// On success, returns {OK,...} and writes *out (caller must jelly_bc_free()).
jelly_bc_result jelly_bc_read(const uint8_t* data, size_t size, jelly_bc_module* out);

// Free allocations owned by a module produced by jelly_bc_read().
void jelly_bc_free(jelly_bc_module* m);

#endif /* JELLY_LOADER_H */

