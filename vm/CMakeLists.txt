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
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>"
    "$<INSTALL_INTERFACE:include>"
)

target_compile_definitions(jellyvm
  PUBLIC
    JELLYVM_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
    JELLYVM_VERSION_MINOR=${PROJECT_VERSION_MINOR}
    JELLYVM_VERSION_PATCH=${PROJECT_VERSION_PATCH}
)

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

# --- Tests -------------------------------------------------------------------
if(JELLYVM_BUILD_TESTS)
  include(CTest)
  if(BUILD_TESTING)
    # Tests live at repo-root `./ctest` by convention.
    add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/../ctest" "${CMAKE_BINARY_DIR}/ctest")
  endif()
endif()

