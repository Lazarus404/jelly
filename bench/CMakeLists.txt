add_executable(jelly_bench
  jelly_bench.c
)

target_link_libraries(jelly_bench PRIVATE jellyvm)

