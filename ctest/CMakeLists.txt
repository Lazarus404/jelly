# Tests have TIMEOUT set to fail fast on hangs. jellyc tests use RUN_SERIAL to avoid
# parallel spawns (compiler+VM) which can hang. To run without jellyc tests:
#   ctest --test-dir build -E "jellyc|ast_|ir_"
# If tests still hang, try: ctest --test-dir build -j1
add_executable(strutil_tests
  strutil_tests.c
)

target_link_libraries(strutil_tests PRIVATE jellyvm)

add_executable(bytecode_loader_tests
  bytecode_loader_tests.c
)
target_link_libraries(bytecode_loader_tests PRIVATE jellyvm)

add_executable(vm_exec_tests
  vm_exec_tests.c
)
target_link_libraries(vm_exec_tests PRIVATE jellyvm)

add_executable(cli_fixture_writer
  cli_fixture_writer.c
)
target_include_directories(cli_fixture_writer PRIVATE
  "${CMAKE_CURRENT_LIST_DIR}/../vm/src/include"
)

add_executable(jellyc_prelude_smoke
  jellyc_prelude_smoke.c
)

add_executable(jellyc_jelly_test
  jellyc_jelly_test.c
)
target_link_libraries(jellyc_jelly_test PRIVATE jellyvm)

add_test(NAME strutil_utod_empty COMMAND strutil_tests utod_empty)
add_test(NAME strutil_utod_suffix COMMAND strutil_tests utod_suffix)
add_test(NAME strutil_utod_cap30 COMMAND strutil_tests utod_cap30)

add_test(NAME strutil_utoi_empty COMMAND strutil_tests utoi_empty)
add_test(NAME strutil_utoi_sign COMMAND strutil_tests utoi_sign)
add_test(NAME strutil_utoi_cap16 COMMAND strutil_tests utoi_cap16)

add_test(NAME strutil_ustrlen_basic COMMAND strutil_tests ustrlen_basic)
add_test(NAME strutil_ustrdup_basic COMMAND strutil_tests ustrdup_basic)
add_test(NAME strutil_ucmp_basic COMMAND strutil_tests ucmp_basic)

add_test(NAME strutil_utostr_ascii COMMAND strutil_tests utostr_ascii)
add_test(NAME strutil_utostr_two_byte COMMAND strutil_tests utostr_two_byte)
add_test(NAME strutil_utostr_surrogate_pair COMMAND strutil_tests utostr_surrogate_pair)
add_test(NAME strutil_utostr_out_size_zero COMMAND strutil_tests utostr_out_size_zero)
add_test(NAME strutil_utostr_truncates COMMAND strutil_tests utostr_truncates)

add_test(NAME strutil_uvszprintf_basic COMMAND strutil_tests uvszprintf_basic)
add_test(NAME strutil_uvszprintf_percent_s COMMAND strutil_tests uvszprintf_percent_s)

# Expected-failure tests (should abort / non-zero exit)
add_test(NAME strutil_uvszprintf_unsupported_precision COMMAND strutil_tests uvszprintf_unsupported_precision)
set_tests_properties(strutil_uvszprintf_unsupported_precision PROPERTIES WILL_FAIL TRUE)

add_test(NAME strutil_uvszprintf_unsupported_spec COMMAND strutil_tests uvszprintf_unsupported_spec)
set_tests_properties(strutil_uvszprintf_unsupported_spec PROPERTIES WILL_FAIL TRUE)
set_tests_properties(strutil_utod_empty strutil_utod_suffix strutil_utod_cap30 strutil_utoi_empty
  strutil_utoi_sign strutil_utoi_cap16 strutil_ustrlen_basic strutil_ustrdup_basic strutil_ucmp_basic
  strutil_utostr_ascii strutil_utostr_two_byte strutil_utostr_surrogate_pair strutil_utostr_out_size_zero
  strutil_utostr_truncates strutil_uvszprintf_basic strutil_uvszprintf_percent_s
  strutil_uvszprintf_unsupported_precision strutil_uvszprintf_unsupported_spec
  PROPERTIES TIMEOUT 10)

add_test(NAME bc_loader_minimal_ok COMMAND bytecode_loader_tests loader_minimal_ok)
add_test(NAME bc_loader_bad_magic COMMAND bytecode_loader_tests loader_bad_magic)
add_test(NAME bc_loader_nregs_too_big COMMAND bytecode_loader_tests loader_nregs_too_big)
add_test(NAME bc_loader_bad_insn_type_mismatch COMMAND bytecode_loader_tests loader_bad_insn_type_mismatch)
add_test(NAME bc_loader_atoms_roundtrip COMMAND bytecode_loader_tests loader_atoms_roundtrip)
add_test(NAME bc_loader_const_bytes_roundtrip COMMAND bytecode_loader_tests loader_const_bytes_roundtrip)
add_test(NAME bc_loader_const_bool_bad_imm COMMAND bytecode_loader_tests loader_const_bool_bad_imm)
add_test(NAME bc_loader_const_i8_oob COMMAND bytecode_loader_tests loader_const_i8_oob)
add_test(NAME bc_loader_const_i16_oob COMMAND bytecode_loader_tests loader_const_i16_oob)
add_test(NAME bc_loader_atom_id_oob COMMAND bytecode_loader_tests loader_atom_id_oob)
add_test(NAME bc_loader_jmp_target_oob COMMAND bytecode_loader_tests loader_jmp_target_oob)
set_tests_properties(bc_loader_minimal_ok bc_loader_bad_magic bc_loader_nregs_too_big
  bc_loader_bad_insn_type_mismatch bc_loader_atoms_roundtrip bc_loader_const_bytes_roundtrip
  bc_loader_const_bool_bad_imm bc_loader_const_i8_oob bc_loader_const_i16_oob
  bc_loader_atom_id_oob bc_loader_jmp_target_oob PROPERTIES TIMEOUT 10)

# Timeout for VM/loader tests (recursion, GC, etc. can hang on bugs)
set(_vm_tests
  vm_exec_add_i32 vm_exec_i32_imm_arith vm_exec_i32_imm_cmp vm_exec_const_bytes_len_and_get_u8 vm_exec_bytes_concat2_and_get_u8
  vm_exec_bytes_concat_many_and_len vm_exec_spill_roundtrip vm_exec_list_head_tail
  vm_exec_list_is_nil vm_exec_array_set_get_len vm_exec_array_len vm_exec_bytes_set_get_len
  vm_exec_obj_get_atom vm_exec_obj_has_atom vm_exec_call_direct_i32 vm_exec_call_indirect_i32
  vm_exec_closure_capture_and_this vm_exec_closure_capture_i64_aggressive_gc
  gc_cycle_reclaim_object gc_abstract_finalizer_called_once gc_mixed_container_graph_reclaim
  gc_object_rehash_under_aggressive_gc exec_spill_roots_survive_aggressive_gc
  vm_exec_i64_dyn_roundtrip vm_exec_from_dyn_bool vm_exec_from_dyn_atom vm_exec_from_dyn_ptr_bytes
  vm_exec_trap_bytes_get_oob vm_exec_trap_from_dyn_bool_mismatch
  vm_exec_try_catch_catches_trap_bytes_oob vm_exec_try_catch_catches_throw_payload
  vm_exec_kindof_i32_and_null vm_exec_match_when_and_fallthrough_i32 vm_exec_match_object_prop_and_when
  vm_exec_match_list_head_and_when vm_exec_match_array_len_index_and_when vm_exec_switch_kind_dispatch_list
  vm_exec_i8_i16_to_dyn_roundtrip vm_exec_i8_i16_arith_cmp_relaxed vm_exec_unary_neg_i32_and_not_bool vm_exec_f64_dyn_roundtrip
  vm_exec_f32_dyn_and_call_return vm_exec_const_i8_imm vm_exec_const_f16_to_dyn vm_exec_f16_i32_convert vm_exec_f16_arith vm_exec_from_dyn_i8 vm_exec_from_dyn_i16 vm_exec_from_dyn_f32 vm_exec_sext_i16 vm_exec_trunc_i8 vm_exec_trunc_i16 vm_exec_i8_const_to_dyn vm_exec_f32_arith_and_convert
  vm_exec_f64_arith_and_convert vm_exec_i64_f64_explicit_conversions vm_exec_i64_arith_and_eq_wrap
  vm_exec_trap_f64_to_i32_overflow vm_exec_const_i64_pool vm_exec_const_f64_pool
  vm_exec_i64_spill_roundtrip vm_exec_f64_spill_roundtrip vm_exec_wideslot_layout_i64
  vm_exec_wideslot_layout_f64 vm_exec_if_else_eq_i32 vm_exec_loop_countdown_i32
)
add_test(NAME vm_exec_add_i32 COMMAND vm_exec_tests exec_add_i32)
add_test(NAME vm_exec_i32_imm_arith COMMAND vm_exec_tests exec_i32_imm_arith)
add_test(NAME vm_exec_i32_imm_cmp COMMAND vm_exec_tests exec_i32_imm_cmp)
add_test(NAME vm_exec_const_bytes_len_and_get_u8 COMMAND vm_exec_tests exec_const_bytes_len_and_get_u8)
add_test(NAME vm_exec_bytes_concat2_and_get_u8 COMMAND vm_exec_tests exec_bytes_concat2_and_get_u8)
add_test(NAME vm_exec_bytes_concat_many_and_len COMMAND vm_exec_tests exec_bytes_concat_many_and_len)
add_test(NAME vm_exec_spill_roundtrip COMMAND vm_exec_tests exec_spill_roundtrip)
add_test(NAME vm_exec_list_head_tail COMMAND vm_exec_tests exec_list_head_tail)
add_test(NAME vm_exec_list_is_nil COMMAND vm_exec_tests exec_list_is_nil)
add_test(NAME vm_exec_array_set_get_len COMMAND vm_exec_tests exec_array_set_get_len)
add_test(NAME vm_exec_array_len COMMAND vm_exec_tests exec_array_len)
add_test(NAME vm_exec_bytes_set_get_len COMMAND vm_exec_tests exec_bytes_set_get_len)
add_test(NAME vm_exec_obj_get_atom COMMAND vm_exec_tests exec_obj_get_atom)
add_test(NAME vm_exec_obj_has_atom COMMAND vm_exec_tests exec_obj_has_atom)
add_test(NAME vm_exec_call_direct_i32 COMMAND vm_exec_tests exec_call_direct_i32)
add_test(NAME vm_exec_call_indirect_i32 COMMAND vm_exec_tests exec_call_indirect_i32)
add_test(NAME vm_exec_closure_capture_and_this COMMAND vm_exec_tests exec_closure_capture_and_this)
add_test(NAME vm_exec_closure_capture_i64_aggressive_gc COMMAND vm_exec_tests exec_closure_capture_i64_aggressive_gc)
add_test(NAME gc_cycle_reclaim_object COMMAND vm_exec_tests gc_cycle_reclaim_object)
add_test(NAME gc_abstract_finalizer_called_once COMMAND vm_exec_tests gc_abstract_finalizer_called_once)
add_test(NAME gc_mixed_container_graph_reclaim COMMAND vm_exec_tests gc_mixed_container_graph_reclaim)
add_test(NAME gc_object_rehash_under_aggressive_gc COMMAND vm_exec_tests gc_object_rehash_under_aggressive_gc)
add_test(NAME exec_spill_roots_survive_aggressive_gc COMMAND vm_exec_tests exec_spill_roots_survive_aggressive_gc)
add_test(NAME vm_exec_i64_dyn_roundtrip COMMAND vm_exec_tests exec_i64_dyn_roundtrip)
add_test(NAME vm_exec_from_dyn_bool COMMAND vm_exec_tests exec_from_dyn_bool)
add_test(NAME vm_exec_from_dyn_atom COMMAND vm_exec_tests exec_from_dyn_atom)
add_test(NAME vm_exec_from_dyn_ptr_bytes COMMAND vm_exec_tests exec_from_dyn_ptr_bytes)
add_test(NAME vm_exec_trap_bytes_get_oob COMMAND vm_exec_tests exec_trap_bytes_get_oob)
add_test(NAME vm_exec_trap_from_dyn_bool_mismatch COMMAND vm_exec_tests exec_trap_from_dyn_bool_mismatch)
add_test(NAME vm_exec_try_catch_catches_trap_bytes_oob COMMAND vm_exec_tests exec_try_catch_catches_trap_bytes_oob)
add_test(NAME vm_exec_try_catch_catches_throw_payload COMMAND vm_exec_tests exec_try_catch_catches_throw_payload)
add_test(NAME vm_exec_kindof_i32_and_null COMMAND vm_exec_tests exec_kindof_i32_and_null)
add_test(NAME vm_exec_match_when_and_fallthrough_i32 COMMAND vm_exec_tests exec_match_when_and_fallthrough_i32)
add_test(NAME vm_exec_match_object_prop_and_when COMMAND vm_exec_tests exec_match_object_prop_and_when)
add_test(NAME vm_exec_match_list_head_and_when COMMAND vm_exec_tests exec_match_list_head_and_when)
add_test(NAME vm_exec_match_array_len_index_and_when COMMAND vm_exec_tests exec_match_array_len_index_and_when)
add_test(NAME vm_exec_switch_kind_dispatch_list COMMAND vm_exec_tests exec_switch_kind_dispatch_list)
add_test(NAME vm_exec_i8_i16_to_dyn_roundtrip COMMAND vm_exec_tests exec_i8_i16_to_dyn_roundtrip)
add_test(NAME vm_exec_i8_i16_arith_cmp_relaxed COMMAND vm_exec_tests exec_i8_i16_arith_cmp_relaxed)
add_test(NAME vm_exec_unary_neg_i32_and_not_bool COMMAND vm_exec_tests exec_unary_neg_i32_and_not_bool)
add_test(NAME vm_exec_f64_dyn_roundtrip COMMAND vm_exec_tests exec_f64_dyn_roundtrip)
add_test(NAME vm_exec_f32_dyn_and_call_return COMMAND vm_exec_tests exec_f32_dyn_and_call_return)
add_test(NAME vm_exec_const_i8_imm COMMAND vm_exec_tests exec_const_i8_imm)
add_test(NAME vm_exec_const_f16_to_dyn COMMAND vm_exec_tests exec_const_f16_to_dyn)
add_test(NAME vm_exec_f16_i32_convert COMMAND vm_exec_tests exec_f16_i32_convert)
add_test(NAME vm_exec_f16_arith COMMAND vm_exec_tests exec_f16_arith)
add_test(NAME vm_exec_from_dyn_i8 COMMAND vm_exec_tests exec_from_dyn_i8)
add_test(NAME vm_exec_from_dyn_i16 COMMAND vm_exec_tests exec_from_dyn_i16)
add_test(NAME vm_exec_from_dyn_f32 COMMAND vm_exec_tests exec_from_dyn_f32)
add_test(NAME vm_exec_sext_i16 COMMAND vm_exec_tests exec_sext_i16)
add_test(NAME vm_exec_trunc_i8 COMMAND vm_exec_tests exec_trunc_i8)
add_test(NAME vm_exec_trunc_i16 COMMAND vm_exec_tests exec_trunc_i16)
add_test(NAME vm_exec_i8_const_to_dyn COMMAND vm_exec_tests exec_i8_const_to_dyn)
add_test(NAME vm_exec_f32_arith_and_convert COMMAND vm_exec_tests exec_f32_arith_and_convert)
add_test(NAME vm_exec_f64_arith_and_convert COMMAND vm_exec_tests exec_f64_arith_and_convert)
add_test(NAME vm_exec_i64_f64_explicit_conversions COMMAND vm_exec_tests exec_i64_f64_explicit_conversions)
add_test(NAME vm_exec_i64_arith_and_eq_wrap COMMAND vm_exec_tests exec_i64_arith_and_eq_wrap)
add_test(NAME vm_exec_trap_f64_to_i32_overflow COMMAND vm_exec_tests exec_trap_f64_to_i32_overflow)
add_test(NAME vm_exec_const_i64_pool COMMAND vm_exec_tests exec_const_i64_pool)
add_test(NAME vm_exec_const_f64_pool COMMAND vm_exec_tests exec_const_f64_pool)
add_test(NAME vm_exec_i64_spill_roundtrip COMMAND vm_exec_tests exec_i64_spill_roundtrip)
add_test(NAME vm_exec_f64_spill_roundtrip COMMAND vm_exec_tests exec_f64_spill_roundtrip)
add_test(NAME vm_exec_wideslot_layout_i64 COMMAND vm_exec_tests exec_wideslot_layout_i64)
add_test(NAME vm_exec_wideslot_layout_f64 COMMAND vm_exec_tests exec_wideslot_layout_f64)
add_test(NAME vm_exec_if_else_eq_i32 COMMAND vm_exec_tests exec_if_else_eq_i32)
add_test(NAME vm_exec_loop_countdown_i32 COMMAND vm_exec_tests exec_loop_countdown_i32)
set_tests_properties(${_vm_tests} PROPERTIES TIMEOUT 30)

set(_cli_fixture_path "${CMAKE_CURRENT_BINARY_DIR}/cli_minimal.jlyb")
add_test(NAME cli_write_minimal COMMAND cli_fixture_writer "${_cli_fixture_path}")
add_test(NAME cli_smoke_minimal COMMAND $<TARGET_FILE:jellyvm_cli> "${_cli_fixture_path}")
set_tests_properties(cli_write_minimal PROPERTIES FIXTURES_SETUP cli_fixture TIMEOUT 30)
set_tests_properties(cli_smoke_minimal PROPERTIES FIXTURES_REQUIRED cli_fixture TIMEOUT 30)

# jellyc prelude smoke (optional; requires a built jellyc binary)
# NOTE: This file is added as a subdir from `vm/`, so `CMAKE_SOURCE_DIR` may be `.../vm`.
# Anchor from this file's directory to reach repo-root `./jellyc/...` reliably.
set(_JELLYC_BIN_DEFAULT "${CMAKE_CURRENT_LIST_DIR}/../jellyc/target/debug/jellyc")
set(JELLYC_BIN "${_JELLYC_BIN_DEFAULT}" CACHE STRING "Path to jellyc binary for CTest integration")
# If the cached value points nowhere (e.g. cache created from a different source dir),
# snap back to the default.
if(NOT EXISTS "${JELLYC_BIN}" AND EXISTS "${_JELLYC_BIN_DEFAULT}")
  set(JELLYC_BIN "${_JELLYC_BIN_DEFAULT}" CACHE STRING "Path to jellyc binary for CTest integration" FORCE)
endif()
if(EXISTS "${JELLYC_BIN}")
  # Keep the Rust compiler binary up to date for CTests that invoke it.
  # Use CTest fixtures so filtered runs still build `jellyc` reliably.
  add_test(
    NAME jellyc_build
    COMMAND cargo build --manifest-path "${CMAKE_CURRENT_LIST_DIR}/../jellyc/Cargo.toml"
  )
  set_tests_properties(jellyc_build PROPERTIES FIXTURES_SETUP jellyc_bin TIMEOUT 120 RUN_SERIAL TRUE)

  set(_jellyc_prelude_path "${CMAKE_CURRENT_BINARY_DIR}/jellyc_prelude.jlyb")
  add_test(NAME jellyc_prelude_write COMMAND jellyc_prelude_smoke "${JELLYC_BIN}" "${_jellyc_prelude_path}")
  add_test(NAME jellyc_prelude_run COMMAND $<TARGET_FILE:jellyvm_cli> "${_jellyc_prelude_path}")
  set_tests_properties(jellyc_prelude_run PROPERTIES DEPENDS "jellyc_build;jellyc_prelude_write")
  set_tests_properties(jellyc_prelude_write jellyc_prelude_run PROPERTIES TIMEOUT 60 RUN_SERIAL TRUE)

  # Serialize jellyc tests (spawn compiler + VM; parallel runs can hang).
  function(add_jellyc_ok_test test_name backend jelly_file)
    set(_out_path "${CMAKE_CURRENT_BINARY_DIR}/${test_name}_out.jlyb")
    add_test(
      NAME ${test_name}
      COMMAND $<TARGET_FILE:jellyc_jelly_test>
        "${JELLYC_BIN}"
        "${backend}"
        "${CMAKE_CURRENT_LIST_DIR}/${jelly_file}"
        "${_out_path}"
    )
    set_tests_properties(${test_name} PROPERTIES FIXTURES_REQUIRED jellyc_bin TIMEOUT 60 RUN_SERIAL TRUE)
  endfunction()

  function(add_jellyc_fail_test test_name backend jelly_file)
    add_jellyc_ok_test(${test_name} ${backend} ${jelly_file})
    set_tests_properties(${test_name} PROPERTIES WILL_FAIL TRUE)
  endfunction()

  add_jellyc_ok_test(ast_bytes_hello ast bytes/hello.jelly)
  add_jellyc_ok_test(ir_bytes_hello ir bytes/hello.jelly)
  add_jellyc_ok_test(ir_fn_infer_ackerman ir fn/infer_ackerman.jelly)
endif()

