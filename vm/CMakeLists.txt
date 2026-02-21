cmake_minimum_required(VERSION 3.16)

# Allow building `vm/` standalone or via top-level `./CMakeLists.txt`.
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_LIST_DIR)
  project(jellyvm
    VERSION 0.1.0
    LANGUAGES C
  )
endif()

# --- Basics ------------------------------------------------------------------
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(JELLYVM_WARNINGS_AS_ERRORS "Treat warnings as errors" OFF)
option(JELLYVM_ENABLE_ASAN "Enable AddressSanitizer (clang/gcc)" OFF)
option(JELLYVM_ENABLE_UBSAN "Enable UndefinedBehaviorSanitizer (clang/gcc)" OFF)
option(JELLYVM_ENABLE_LTO "Enable link-time optimization if supported" OFF)
option(JELLYVM_BUILD_TESTS "Build tests" ON)
option(JELLYVM_USE_COMPUTED_GOTO "Use computed goto for opcode dispatch (GCC/Clang; ~10-20% faster)" OFF)
option(JELLYVM_REFERENCE_INTERP "Use reference interpreter path (no hot-op inlining)" OFF)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(CompilerWarnings)
include(Sanitizers)

# Put all binaries in one place (nice for local dev).
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")

# --- Targets -----------------------------------------------------------------
add_library(jellyvm
  src/vm.c
  src/gc.c
  src/bytecode/loader.c
  src/bytecode/check.c
  src/vm/interp.c
  src/vm/reg.c
  src/vm/spill.c
  src/vm/box.c
  src/vm/call.c
  src/vm/frame.c
  src/vm/exc.c
  src/vm/ops/dispatch.c
  src/vm/ops/exec_loop.c
  src/vm/ops/ops_control.c
  src/vm/ops/ops_const.c
  src/vm/ops/ops_call.c
  src/vm/ops/ops_builtins.c
  src/vm/ops/ops_arith.c
  src/vm/ops/ops_box.c
  src/vm/ops/ops_list.c
  src/vm/ops/ops_array.c
  src/vm/ops/ops_bytes.c
  src/vm/ops/ops_obj.c
  src/types/bytes.c
  src/types/box.c
  src/types/function.c
  src/types/abstract.c
  src/types/object.c
  src/types/array.c
  src/types/list.c
  src/types/strutil.c
)

target_include_directories(jellyvm
  PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/src/include>"
    "$<INSTALL_INTERFACE:include>"
)

target_compile_definitions(jellyvm
  PUBLIC
    JELLYVM_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
    JELLYVM_VERSION_MINOR=${PROJECT_VERSION_MINOR}
    JELLYVM_VERSION_PATCH=${PROJECT_VERSION_PATCH}
)

if(JELLYVM_USE_COMPUTED_GOTO)
  if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_definitions(jellyvm PRIVATE JELLYVM_USE_COMPUTED_GOTO=1)
  else()
    message(WARNING "JELLYVM_USE_COMPUTED_GOTO requires GCC or Clang; ignoring")
  endif()
endif()

if(JELLYVM_REFERENCE_INTERP)
  target_compile_definitions(jellyvm PRIVATE JELLYVM_REFERENCE_INTERP=1)
endif()

jellyvm_enable_warnings(jellyvm)
jellyvm_enable_sanitizers(jellyvm)

if(JELLYVM_ENABLE_LTO)
  include(CheckIPOSupported)
  check_ipo_supported(RESULT _ipo_ok OUTPUT _ipo_err)
  if(_ipo_ok)
    set_property(TARGET jellyvm PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
  else()
    message(WARNING "IPO/LTO not supported: ${_ipo_err}")
  endif()
endif()

add_executable(jellyvm_cli
  src/main.c
)
set_target_properties(jellyvm_cli PROPERTIES OUTPUT_NAME "jellyvm")
target_link_libraries(jellyvm_cli PRIVATE jellyvm)
jellyvm_enable_warnings(jellyvm_cli)
jellyvm_enable_sanitizers(jellyvm_cli)

install(FILES src/include/jelly.h DESTINATION include)
