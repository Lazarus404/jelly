# Standalone Makefile for the Jelly VM (jellyvm + libjellyvm.a).
#
# Goals:
# - Work with older GCC toolchains (pre-2018) by defaulting to C99
# - Avoid CMake as a build dependency
# - Keep flags conservative (no -Werror, no LTO/sanitizers by default)
#
# Usage:
#   make              # build build/bin/jellyvm and build/lib/libjellyvm.a
#   make clean
#   make DEBUG=1      # -O0 -g
#   make CC=gcc-4.9   # pick older compiler
#   make COMPUTED_GOTO=1
#

CC      ?= gcc
AR      ?= ar
RANLIB  ?= ranlib

BUILD   ?= build
OBJDIR  := $(BUILD)/obj
BINDIR  := $(BUILD)/bin
LIBDIR  := $(BUILD)/lib

# The VM headers use C11 features (e.g. `u"..."` via USTR()).
# GCC 5+ (2015) and newer should be fine; override if needed.
CSTD    ?= c11

DEBUG   ?= 0
WARN    ?= 1

COMPUTED_GOTO   ?= 0
REFERENCE_INTERP ?= 0

VERSION_MAJOR ?= 0
VERSION_MINOR ?= 1
VERSION_PATCH ?= 0

CPPFLAGS += -I src/include
CPPFLAGS += -DJELLYVM_VERSION_MAJOR=$(VERSION_MAJOR)
CPPFLAGS += -DJELLYVM_VERSION_MINOR=$(VERSION_MINOR)
CPPFLAGS += -DJELLYVM_VERSION_PATCH=$(VERSION_PATCH)

ifeq ($(COMPUTED_GOTO),1)
CPPFLAGS += -DJELLYVM_USE_COMPUTED_GOTO=1
endif
ifeq ($(REFERENCE_INTERP),1)
CPPFLAGS += -DJELLYVM_REFERENCE_INTERP=1
endif

CFLAGS  += -std=$(CSTD)

ifeq ($(WARN),1)
CFLAGS  += -Wall -Wextra
endif

ifeq ($(DEBUG),1)
CFLAGS  += -O0 -g
CPPFLAGS+= -DJELLYVM_DEBUG=1
else
CFLAGS  += -O2
CPPFLAGS+= -DNDEBUG
endif

# Dependency generation (supported by GCC/Clang for many years).
DEPFLAGS = -MMD -MP

LDLIBS  += -lm

LIB_SRCS = \
  src/vm.c \
  src/gc.c \
  src/bytecode/loader.c \
  src/bytecode/check.c \
  src/vm/interp.c \
  src/vm/reg.c \
  src/vm/spill.c \
  src/vm/box.c \
  src/vm/call.c \
  src/vm/frame.c \
  src/vm/exc.c \
  src/vm/ops/dispatch.c \
  src/vm/ops/exec_loop.c \
  src/vm/ops/ops_control.c \
  src/vm/ops/ops_const.c \
  src/vm/ops/ops_call.c \
  src/vm/ops/ops_builtins.c \
  src/vm/ops/ops_arith.c \
  src/vm/ops/ops_box.c \
  src/vm/ops/ops_list.c \
  src/vm/ops/ops_array.c \
  src/vm/ops/ops_bytes.c \
  src/vm/ops/ops_obj.c \
  src/types/bytes.c \
  src/types/box.c \
  src/types/function.c \
  src/types/abstract.c \
  src/types/object.c \
  src/types/array.c \
  src/types/list.c \
  src/types/strutil.c

CLI_SRCS = src/main.c

LIB_OBJS = $(addprefix $(OBJDIR)/,$(LIB_SRCS:.c=.o))
CLI_OBJS = $(addprefix $(OBJDIR)/,$(CLI_SRCS:.c=.o))

DEPS = $(LIB_OBJS:.o=.d) $(CLI_OBJS:.o=.d)

.PHONY: all clean help

all: $(BINDIR)/jellyvm $(LIBDIR)/libjellyvm.a

help:
	@printf "%s\n" "Targets:" \
	  "  all (default)   Build jellyvm + libjellyvm.a into $(BUILD)/" \
	  "  clean           Remove $(BUILD)/" \
	  "" \
	  "Variables:" \
	  "  CC=<cc>         (default: gcc)" \
	  "  DEBUG=1         Debug build (-O0 -g)" \
	  "  CSTD=c99|c11    C language standard (default: c99)" \
	  "  COMPUTED_GOTO=1 Enable computed-goto dispatch (GCC/Clang)" \
	  "  REFERENCE_INTERP=1 Use reference interpreter path"

$(LIBDIR)/libjellyvm.a: $(LIB_OBJS)
	@mkdir -p "$(LIBDIR)"
	$(AR) rcs "$@" $^
	$(RANLIB) "$@"

$(BINDIR)/jellyvm: $(CLI_OBJS) $(LIBDIR)/libjellyvm.a
	@mkdir -p "$(BINDIR)"
	$(CC) $(LDFLAGS) -o "$@" $(CLI_OBJS) $(LIBDIR)/libjellyvm.a $(LDLIBS)

$(OBJDIR)/%.o: %.c
	@mkdir -p "$(dir $@)"
	$(CC) $(CPPFLAGS) $(CFLAGS) $(DEPFLAGS) -c "$<" -o "$@"

-include $(DEPS)

clean:
	rm -rf "$(BUILD)"

