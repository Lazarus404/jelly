cmake_minimum_required(VERSION 3.16)

project(jelly
  VERSION 0.1.0
  LANGUAGES C
)

# Ensure tests are discoverable from the top-level build directory.
include(CTest)

# Put all binaries in one place (nice for local dev).
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")

# The focused VM lives in ./vm. Keeping this top-level CMake entrypoint makes
# `cmake -S . -B build` work, while still allowing `vm/` to be built standalone.
add_subdirectory(vm)

option(JELLYVM_BUILD_BENCH "Build microbench harness" OFF)
if(JELLYVM_BUILD_BENCH)
  add_subdirectory(bench)
endif()

