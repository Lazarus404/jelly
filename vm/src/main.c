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

#include <jelly.h>
#include <jelly/internal/vm_internal.h>

#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(__APPLE__) || defined(__linux__) || defined(__FreeBSD__)
#  define JELLY_HAVE_CLOCK_GETTIME 1
#endif

#ifdef JELLY_HAVE_CLOCK_GETTIME
static double now_ms(void) {
	struct timespec ts;
	if(clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return 0.0;
	return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
}
#else
static double now_ms(void) {
	return 0.0;
}
#endif

static uint64_t parse_u64_env(const char* key, uint64_t def) {
	const char* s = getenv(key);
	if(!s || !*s) return def;
	char* end = NULL;
	unsigned long long v = strtoull(s, &end, 10);
	if(end == s) return def;
	return (uint64_t)v;
}

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
	const uint8_t profile = (getenv("JELLY_PROFILE") != NULL);
	double t0 = 0.0, t_read = 0.0, t_bc = 0.0, t_vm_create = 0.0, t_exec = 0.0;

	if(profile) t0 = now_ms();
	size_t size = 0;
	uint8_t* data = read_file(path, &size);
	if(!data) {
		fprintf(stderr, "error: failed to read '%s': %s\n", path, strerror(errno));
		return 2;
	}
	if(profile) t_read = now_ms() - t0;

	if(profile) t0 = now_ms();
	jelly_bc_module* m = NULL;
	jelly_bc_result r = jelly_bc_read(data, size, &m);
	free(data);
	if(profile) t_bc = now_ms() - t0;
	if(r.err != JELLY_BC_OK) {
		fprintf(stderr, "error: bytecode load failed: err=%d msg=%s offset=%zu\n",
		        (int)r.err,
		        r.msg ? r.msg : "(null)",
		        r.offset);
		return 2;
	}

	if(profile) t0 = now_ms();
	jelly_vm* vm = jelly_vm_create();
	if(profile) t_vm_create = now_ms() - t0;
	if (!vm) {
		fprintf(stderr, "error: failed to create VM\n");
		jelly_bc_free(m);
		return 2;
	}

	/* Safety defaults (override via env):
	   - JELLY_FUEL: instruction budget (0 disables)
	   - JELLY_MAX_BYTES: max Bytes length (0 disables)
	   - JELLY_MAX_ARRAY: max Array length (0 disables) */
	jelly_vm_set_fuel(vm, parse_u64_env("JELLY_FUEL", 200000000ull));
	{
		uint64_t v = parse_u64_env("JELLY_MAX_BYTES", (uint64_t)(64u * 1024u * 1024u));
		if(v > 0xffffffffull) v = 0xffffffffull;
		jelly_vm_set_max_bytes_len(vm, (uint32_t)v);
	}
	{
		uint64_t v = parse_u64_env("JELLY_MAX_ARRAY", (uint64_t)(8u * 1024u * 1024u));
		if(v > 0xffffffffull) v = 0xffffffffull;
		jelly_vm_set_max_array_len(vm, (uint32_t)v);
	}

	if(profile) t0 = now_ms();
	jelly_value out = jelly_make_null();
	jelly_exec_status st = jelly_vm_exec_status(vm, m, &out);
	if(profile) t_exec = now_ms() - t0;
	if(profile) {
		fprintf(stderr, "JELLY_PROFILE read_file=%.2f bc_read=%.2f vm_create=%.2f exec=%.2f (ms)\n",
		        t_read, t_bc, t_vm_create, t_exec);
	}

	if(st == JELLY_EXEC_TRAP) {
		fprintf(stderr, "trap: code=%d msg=%s\n",
		        (int)jelly_vm_last_trap_code(vm),
		        jelly_vm_last_trap_msg(vm) ? jelly_vm_last_trap_msg(vm) : "(null)");
		jelly_bc_free(m);
		jelly_vm_destroy(vm);
		return 1;
	}

	// For now, print the boxed return value in a minimal way.
	if(jelly_is_null(out)) {
		puts("null");
	} else if(jelly_is_bool(out)) {
		puts(jelly_as_bool(out) ? "true" : "false");
	} else if(jelly_is_i32(out)) {
		printf("%d\n", jelly_as_i32(out));
	} else if(jelly_is_atom(out)) {
		printf("atom(%u)\n", jelly_as_atom(out));
	} else if(jelly_is_box_i64(out)) {
		printf("%" PRId64 "\n", jelly_as_box_i64(out));
	} else if(jelly_is_box_f64(out)) {
		printf("%g\n", jelly_as_box_f64(out));
	} else if(jelly_is_box_f32(out)) {
		printf("%g\n", (double)jelly_as_box_f32(out));
	} else if(jelly_is_box_f16(out)) {
		printf("%g\n", (double)vm_f16_bits_to_f32(jelly_as_box_f16(out)));
	} else if(jelly_is_ptr(out)) {
		const jelly_obj_header* h = jelly_obj_header_of(out);
		if(h && h->kind == (uint32_t)JELLY_OBJ_BYTES) {
			const jelly_bytes* b = (const jelly_bytes*)jelly_as_ptr(out);
			fwrite(b->data, 1, b->length, stdout);
			fputc('\n', stdout);
		} else {
			printf("ptr(" JELLY_PTR_FMT ")\n", (uintptr_t)jelly_as_ptr(out));
		}
	} else {
		puts("<value>");
	}

	jelly_bc_free(m);
	jelly_vm_destroy(vm);
	return 0;
}

