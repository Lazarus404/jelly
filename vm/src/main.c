#include <jelly.h>
#include <jelly/loader.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint8_t* read_file(const char* path, size_t* out_size) {
	if(out_size) *out_size = 0;
	FILE* f = fopen(path, "rb");
	if(!f) return NULL;

	if(fseek(f, 0, SEEK_END) != 0) {
		fclose(f);
		return NULL;
	}
	long sz = ftell(f);
	if(sz < 0) {
		fclose(f);
		return NULL;
	}
	if(fseek(f, 0, SEEK_SET) != 0) {
		fclose(f);
		return NULL;
	}

	uint8_t* data = (uint8_t*)malloc((size_t)sz);
	if(!data) {
		fclose(f);
		return NULL;
	}
	size_t got = fread(data, 1, (size_t)sz, f);
	fclose(f);
	if(got != (size_t)sz) {
		free(data);
		return NULL;
	}
	if(out_size) *out_size = (size_t)sz;
	return data;
}

static void usage(const char* argv0) {
	fprintf(stderr, "usage: %s <module.jlyb>\n", argv0 ? argv0 : "jellyvm");
}

int main(int argc, char** argv) {
	if(argc < 2) {
		usage(argv[0]);
		return 2;
	}

	const char* path = argv[1];
	size_t size = 0;
	uint8_t* data = read_file(path, &size);
	if(!data) {
		fprintf(stderr, "error: failed to read '%s': %s\n", path, strerror(errno));
		return 2;
	}

	jelly_bc_module m;
	jelly_bc_result r = jelly_bc_read(data, size, &m);
	free(data);
	if(r.err != JELLY_BC_OK) {
		fprintf(stderr, "error: bytecode load failed: err=%d msg=%s offset=%zu\n",
		        (int)r.err,
		        r.msg ? r.msg : "(null)",
		        r.offset);
		return 2;
	}

	jelly_vm vm;
	jelly_vm_init(&vm);

	jelly_value out = jelly_make_null();
	jelly_exec_status st = jelly_vm_exec_status(&vm, &m, &out);
	if(st == JELLY_EXEC_TRAP) {
		fprintf(stderr, "trap: code=%d msg=%s\n",
		        (int)jelly_vm_last_trap_code(&vm),
		        jelly_vm_last_trap_msg(&vm) ? jelly_vm_last_trap_msg(&vm) : "(null)");
		jelly_bc_free(&m);
		jelly_vm_shutdown(&vm);
		return 1;
	}

	// For now, print the boxed return value in a minimal way.
	// (If it is not representable as i32/bool/atom/null, print a placeholder.)
	if(jelly_is_null(out)) {
		puts("null");
	} else if(jelly_is_bool(out)) {
		puts(jelly_as_bool(out) ? "true" : "false");
	} else if(jelly_is_i32(out)) {
		printf("%d\n", jelly_as_i32(out));
	} else if(jelly_is_atom(out)) {
		printf("atom(%u)\n", jelly_as_atom(out));
	} else if(jelly_is_ptr(out)) {
		printf("ptr(" JELLY_PTR_FMT ")\n", (uintptr_t)jelly_as_ptr(out));
	} else {
		puts("<value>");
	}

	jelly_bc_free(&m);
	jelly_vm_shutdown(&vm);
	return 0;
}

