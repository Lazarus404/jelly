#ifndef JELLY_H
#define JELLY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <inttypes.h>
#include <stdarg.h>

// `char16_t`/`char32_t` are normally provided by `<uchar.h>` in C11, but some
// libcs/SDKs (notably some Apple SDK configurations) don't ship that header.
// We provide a lightweight fallback so the VM can still build as C11.
#if !defined(__cplusplus)
#  if defined(__CHAR16_TYPE__) && defined(__CHAR32_TYPE__)
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
#  else
typedef uint16_t char16_t;
typedef uint32_t char32_t;
#  endif
#endif

// Bump as the VM API stabilizes.
#define JELLY_VERSION 0x011000u

// Pointer/word facts (portable, standard C).
#define JELLY_WSIZE ((uint32_t)sizeof(void*))
#define JELLY_IS_64 ((UINTPTR_MAX) > 0xffffffffu)

// printf-format helper for pointers-as-integers (via uintptr_t).
#define JELLY_PTR_FMT "%" PRIxPTR

// Thread-local storage (C11).
#define JELLY_THREAD_LOCAL _Thread_local

// Public API annotation (kept intentionally simple/portable for now).
#ifndef JELLY_API
#define JELLY_API extern
#endif

// Common integral aliases used throughout the VM.
typedef intptr_t int_val;
typedef int64_t int64;
typedef uint64_t uint64;

typedef char16_t uchar;
#define USTR(str) u##str

// VM core headers (typed VM + boxed values).
#include <jelly/value.h>
#include <jelly/type.h>
#include <jelly/bytecode.h>
#include <jelly/vm.h>
#include <jelly/list.h>
#include <jelly/array.h>
#include <jelly/bytes.h>
#include <jelly/object.h>
#include <jelly/function.h>
#include <jelly/abstract.h>
#include <jelly/box.h>
#include <jelly/gc.h>

// --- UTF-16 helpers (implemented in `src/types/strutil.c`) --------------------
JELLY_API double utod(const uchar* str, uchar** end);
JELLY_API int utoi(const uchar* str, uchar** end);
JELLY_API int ustrlen(const uchar* str);
JELLY_API uchar* ustrdup(const uchar* str);
JELLY_API int ucmp(const uchar* a, const uchar* b);
JELLY_API int utostr(char* out, int out_size, const uchar* str);
JELLY_API int usprintf(uchar* out, int out_size, const uchar* fmt, ...);
JELLY_API int uvszprintf(uchar* out, int out_size, const uchar* fmt, va_list arglist);
JELLY_API void uprintf(const uchar* fmt, const uchar* str);

// --- Debug/attributes (strict C11) --------------------------------------------
#ifdef JELLY_DEBUG
#define jelly_debug_break() abort()
#else
#define jelly_debug_break() ((void)0)
#endif

#define JELLY_NO_RETURN(decl) noreturn decl
#define JELLY_UNREACHABLE() abort()

// --- Planning scratchpad (non-compiling reference) ----------------------------
#if 0
/* Legacy planning reference (Hashlink-derived runtime surface) */
// ... keep notes here ...
#endif

#endif /* JELLY_H */

