#include <jelly.h>

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

static void assert_int_eq(const char* test, int got, int want, const char* msg) {
	if(got != want) {
		char buf[256];
		snprintf(buf, sizeof(buf), "%s (got=%d want=%d)", msg, got, want);
		failf(test, buf);
	}
}

static void assert_ptrdiff_eq(const char* test, const uchar* got, const uchar* want, const char* msg) {
	if(got != want) failf(test, msg);
}

static void assert_str_eq(const char* test, const char* got, const char* want, const char* msg) {
	if(strcmp(got, want) != 0) {
		char buf[256];
		snprintf(buf, sizeof(buf), "%s (got='%s' want='%s')", msg, got, want);
		failf(test, buf);
	}
}

static int run_utod_empty(void) {
	const char* t = "utod_empty";
	const uchar s[] = USTR("");
	uchar* end = NULL;
	double v = utod(s, &end);
	assert_true(t, v == 0.0, "empty parses as 0.0");
	assert_ptrdiff_eq(t, end, s, "end points to start");
	return failures == 0 ? 0 : 1;
}

static int run_utod_suffix(void) {
	const char* t = "utod_suffix";
	const uchar s[] = USTR("1.23abc");
	uchar* end = NULL;
	double v = utod(s, &end);
	assert_true(t, v > 1.229 && v < 1.231, "parses 1.23");
	assert_ptrdiff_eq(t, end, s + 4, "end points after numeric prefix");
	return failures == 0 ? 0 : 1;
}

static int run_utod_cap30(void) {
	const char* t = "utod_cap30";
	// 40 digits; parser only copies 30 chars max
	uchar s[64];
	for(int i = 0; i < 40; i++) s[i] = (uchar)('0' + (i % 10));
	s[40] = 0;
	uchar* end = NULL;
	(void)utod(s, &end);
	assert_ptrdiff_eq(t, end, s + 30, "end caps at 30 chars");
	return failures == 0 ? 0 : 1;
}

static int run_utoi_empty(void) {
	const char* t = "utoi_empty";
	const uchar s[] = USTR("");
	uchar* end = NULL;
	int v = utoi(s, &end);
	assert_int_eq(t, v, 0, "empty parses as 0");
	assert_ptrdiff_eq(t, end, s, "end points to start");
	return failures == 0 ? 0 : 1;
}

static int run_utoi_sign(void) {
	const char* t = "utoi_sign";
	const uchar s[] = USTR("-12x");
	uchar* end = NULL;
	int v = utoi(s, &end);
	assert_int_eq(t, v, -12, "parses -12");
	assert_ptrdiff_eq(t, end, s + 3, "end points after -12");
	return failures == 0 ? 0 : 1;
}

static int run_utoi_cap16(void) {
	const char* t = "utoi_cap16";
	// 30 digits; parser only copies 16 chars max (including optional sign).
	uchar s[64];
	for(int i = 0; i < 30; i++) s[i] = (uchar)('1');
	s[30] = 0;
	uchar* end = NULL;
	(void)utoi(s, &end);
	assert_ptrdiff_eq(t, end, s + 16, "end caps at 16 chars");
	return failures == 0 ? 0 : 1;
}

static int run_ustrlen_basic(void) {
	const char* t = "ustrlen_basic";
	const uchar s[] = USTR("abc");
	assert_int_eq(t, ustrlen(s), 3, "length is 3");
	return failures == 0 ? 0 : 1;
}

static int run_ustrdup_basic(void) {
	const char* t = "ustrdup_basic";
	const uchar s[] = USTR("abc");
	uchar* d = ustrdup(s);
	assert_true(t, d != NULL, "allocated");
	assert_int_eq(t, ustrlen(d), 3, "dup length 3");
	assert_true(t, ucmp(s, d) == 0, "dup equals original");
	d[0] = USTR("z")[0];
	assert_true(t, ucmp(s, d) != 0, "mutating dup doesn't mutate original");
	free(d);
	return failures == 0 ? 0 : 1;
}

static int run_ucmp_basic(void) {
	const char* t = "ucmp_basic";
	const uchar a[] = USTR("a");
	const uchar b[] = USTR("b");
	const uchar a2[] = USTR("a");
	assert_true(t, ucmp(a, b) < 0, "a < b");
	assert_true(t, ucmp(b, a) > 0, "b > a");
	assert_int_eq(t, ucmp(a, a2), 0, "a == a");
	return failures == 0 ? 0 : 1;
}

static int run_utostr_ascii(void) {
	const char* t = "utostr_ascii";
	const uchar s[] = USTR("hello");
	char out[32];
	int n = utostr(out, (int)sizeof(out), s);
	assert_int_eq(t, n, 5, "utf8 byte length 5");
	assert_str_eq(t, out, "hello", "content matches");
	return failures == 0 ? 0 : 1;
}

static int run_utostr_two_byte(void) {
	const char* t = "utostr_two_byte";
	// U+00A2 '¢' -> C2 A2
	const uchar s[] = { (uchar)0x00A2, 0 };
	char out[8];
	int n = utostr(out, (int)sizeof(out), s);
	assert_int_eq(t, n, 2, "utf8 byte length 2");
	assert_true(t, (unsigned char)out[0] == 0xC2 && (unsigned char)out[1] == 0xA2, "bytes are C2 A2");
	return failures == 0 ? 0 : 1;
}

static int run_utostr_surrogate_pair(void) {
	const char* t = "utostr_surrogate_pair";
	// U+1F600 😀 as UTF-16 surrogate pair: D83D DE00 -> F0 9F 98 80
	const uchar s[] = { (uchar)0xD83D, (uchar)0xDE00, 0 };
	char out[16];
	int n = utostr(out, (int)sizeof(out), s);
	assert_int_eq(t, n, 4, "utf8 byte length 4");
	assert_true(t,
		(unsigned char)out[0] == 0xF0 &&
		(unsigned char)out[1] == 0x9F &&
		(unsigned char)out[2] == 0x98 &&
		(unsigned char)out[3] == 0x80,
		"bytes are F0 9F 98 80");
	return failures == 0 ? 0 : 1;
}

static int run_utostr_out_size_zero(void) {
	const char* t = "utostr_out_size_zero";
	const uchar s[] = USTR("x");
	char out[1];
	int n = utostr(out, 0, s);
	assert_int_eq(t, n, 0, "returns 0");
	return failures == 0 ? 0 : 1;
}

static int run_utostr_truncates(void) {
	const char* t = "utostr_truncates";
	const uchar s[] = USTR("hello");
	char out[4]; // can hold "hel" + nul
	int n = utostr(out, (int)sizeof(out), s);
	assert_int_eq(t, n, 3, "writes 3 bytes");
	assert_str_eq(t, out, "hel", "truncated content");
	return failures == 0 ? 0 : 1;
}

static int run_uvszprintf_basic(void) {
	const char* t = "uvszprintf_basic";
	uchar out[64];
	int n = usprintf(out, 64, USTR("x=%d y=%ld"), 12, (int64)34);
	assert_true(t, n > 0, "produced output");
	char buf[64];
	utostr(buf, (int)sizeof(buf), out);
	assert_str_eq(t, buf, "x=12 y=34", "formatted correctly");
	return failures == 0 ? 0 : 1;
}

static int run_uvszprintf_percent_s(void) {
	const char* t = "uvszprintf_percent_s";
	uchar out[64];
	uchar arg[] = USTR("ok");
	int n = usprintf(out, 64, USTR("-%s-"), arg);
	assert_int_eq(t, n, 4, "length is 4");
	char buf[64];
	utostr(buf, (int)sizeof(buf), out);
	assert_str_eq(t, buf, "-ok-", "formatted %s");
	return failures == 0 ? 0 : 1;
}

static int run_uvszprintf_unsupported_precision(void) {
	// This should abort (CTest marks WILL_FAIL).
	uchar out[32];
	uchar arg[] = USTR("x");
	(void)usprintf(out, 32, USTR("%.2s"), arg);
	return 0;
}

static int run_uvszprintf_unsupported_spec(void) {
	// This should abort (CTest marks WILL_FAIL).
	uchar out[32];
	(void)usprintf(out, 32, USTR("%q"), 1);
	return 0;
}

int main(int argc, char** argv) {
	if(argc < 2) {
		fprintf(stderr, "usage: %s <testname>\n", argv[0]);
		return 2;
	}

	const char* name = argv[1];
	if(strcmp(name, "utod_empty") == 0) return run_utod_empty();
	if(strcmp(name, "utod_suffix") == 0) return run_utod_suffix();
	if(strcmp(name, "utod_cap30") == 0) return run_utod_cap30();

	if(strcmp(name, "utoi_empty") == 0) return run_utoi_empty();
	if(strcmp(name, "utoi_sign") == 0) return run_utoi_sign();
	if(strcmp(name, "utoi_cap16") == 0) return run_utoi_cap16();

	if(strcmp(name, "ustrlen_basic") == 0) return run_ustrlen_basic();
	if(strcmp(name, "ustrdup_basic") == 0) return run_ustrdup_basic();
	if(strcmp(name, "ucmp_basic") == 0) return run_ucmp_basic();

	if(strcmp(name, "utostr_ascii") == 0) return run_utostr_ascii();
	if(strcmp(name, "utostr_two_byte") == 0) return run_utostr_two_byte();
	if(strcmp(name, "utostr_surrogate_pair") == 0) return run_utostr_surrogate_pair();
	if(strcmp(name, "utostr_out_size_zero") == 0) return run_utostr_out_size_zero();
	if(strcmp(name, "utostr_truncates") == 0) return run_utostr_truncates();

	if(strcmp(name, "uvszprintf_basic") == 0) return run_uvszprintf_basic();
	if(strcmp(name, "uvszprintf_percent_s") == 0) return run_uvszprintf_percent_s();
	if(strcmp(name, "uvszprintf_unsupported_precision") == 0) return run_uvszprintf_unsupported_precision();
	if(strcmp(name, "uvszprintf_unsupported_spec") == 0) return run_uvszprintf_unsupported_spec();

	fprintf(stderr, "unknown test: %s\n", name);
	return 2;
}

