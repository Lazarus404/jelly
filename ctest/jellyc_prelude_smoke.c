#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
  if(argc != 3) {
    fprintf(stderr, "usage: %s <jellyc_bin> <out.jlyb>\n", argv[0] ? argv[0] : "jellyc_prelude_smoke");
    return 2;
  }
  const char* jellyc = argv[1];
  const char* out = argv[2];

  // Minimal: run `jellyc prelude --out <file>`.
  // We keep this as a separate process so CI doesn't need Rust integration in CMake.
  char cmd[1024];
  int n = snprintf(cmd, sizeof(cmd), "\"%s\" prelude --out \"%s\"", jellyc, out);
  if(n <= 0 || (size_t)n >= sizeof(cmd)) {
    fprintf(stderr, "command too long\n");
    return 2;
  }
  int rc = system(cmd);
  if(rc != 0) {
    fprintf(stderr, "jellyc returned non-zero: %d\n", rc);
    return 1;
  }
  return 0;
}

