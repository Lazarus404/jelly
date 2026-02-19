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

add_test(NAME vm_exec_add_i32 COMMAND vm_exec_tests exec_add_i32)
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
add_test(NAME vm_exec_unary_neg_i32_and_not_bool COMMAND vm_exec_tests exec_unary_neg_i32_and_not_bool)
add_test(NAME vm_exec_f64_dyn_roundtrip COMMAND vm_exec_tests exec_f64_dyn_roundtrip)
add_test(NAME vm_exec_f32_dyn_and_call_return COMMAND vm_exec_tests exec_f32_dyn_and_call_return)
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

set(_cli_fixture_path "${CMAKE_CURRENT_BINARY_DIR}/cli_minimal.jlyb")
add_test(NAME cli_write_minimal COMMAND cli_fixture_writer "${_cli_fixture_path}")
add_test(NAME cli_smoke_minimal COMMAND $<TARGET_FILE:jellyvm_cli> "${_cli_fixture_path}")
set_tests_properties(cli_smoke_minimal PROPERTIES DEPENDS cli_write_minimal)

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
  set_tests_properties(jellyc_build PROPERTIES FIXTURES_SETUP jellyc_bin)

  set(_jellyc_prelude_path "${CMAKE_CURRENT_BINARY_DIR}/jellyc_prelude.jlyb")
  add_test(NAME jellyc_prelude_write COMMAND jellyc_prelude_smoke "${JELLYC_BIN}" "${_jellyc_prelude_path}")
  add_test(NAME jellyc_prelude_run COMMAND $<TARGET_FILE:jellyvm_cli> "${_jellyc_prelude_path}")
  set_tests_properties(jellyc_prelude_run PROPERTIES DEPENDS "jellyc_build;jellyc_prelude_write")

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
    set_tests_properties(${test_name} PROPERTIES FIXTURES_REQUIRED jellyc_bin)
  endfunction()

  add_jellyc_ok_test(ast_bytes_hello ast bytes/hello.jelly)
  add_jellyc_ok_test(ir_bytes_hello ir bytes/hello.jelly)

  add_jellyc_ok_test(ast_call_basic ast call/basic.jelly)
  add_jellyc_ok_test(ir_call_basic ir call/basic.jelly)

  add_jellyc_ok_test(ast_call_in_while ast call/in_while.jelly)
  add_jellyc_ok_test(ir_call_in_while ir call/in_while.jelly)

  add_jellyc_ok_test(ast_call_callee_expr ast call/callee_expr.jelly)
  add_jellyc_ok_test(ir_call_callee_expr ir call/callee_expr.jelly)

  add_jellyc_ok_test(ast_call_large_arity ast call/large_arity.jelly)
  add_jellyc_ok_test(ir_call_large_arity ir call/large_arity.jelly)

  add_jellyc_ok_test(ast_if_basic ast if/basic.jelly)
  add_jellyc_ok_test(ast_if_join ast if/join.jelly)

  add_jellyc_ok_test(ast_bool_basic ast bool/basic.jelly)
  add_jellyc_ok_test(ast_i32_cond ast i32/cond.jelly)
  add_jellyc_ok_test(ast_i32_unary_neg ast i32/unary_neg.jelly)
  add_jellyc_ok_test(ast_i32_lt ast i32/lt.jelly)
  add_jellyc_ok_test(ast_i32_relops ast i32/relops.jelly)
  add_jellyc_ok_test(ast_bool_and_or ast bool/and_or.jelly)

  add_jellyc_ok_test(ast_bool_shortcircuit_trap ast bool/shortcircuit_trap.jelly)
  add_jellyc_ok_test(ir_bool_shortcircuit_trap ir bool/shortcircuit_trap.jelly)

  add_jellyc_ok_test(ast_while_countdown ast while/countdown.jelly)
  add_jellyc_ok_test(ast_while_break_continue ast while/break_continue.jelly)
  add_jellyc_ok_test(ast_block_expr ast block/expr.jelly)

  add_jellyc_ok_test(ast_try_catch_basic ast try-catch/basic.jelly)
  add_jellyc_ok_test(ir_try_catch_basic ir try-catch/basic.jelly)

  add_jellyc_ok_test(ast_array_bytes_ops ast array/bytes_ops.jelly)
  add_jellyc_ok_test(ir_array_bytes_ops ir array/bytes_ops.jelly)

  add_jellyc_ok_test(ast_bytes_concat_many ast bytes/concat_many.jelly)
  add_jellyc_ok_test(ast_bytes_slice ast bytes/slice.jelly)
  add_jellyc_ok_test(ast_bytes_eq ast bytes/eq.jelly)
  add_jellyc_ok_test(ast_bytes_interp_basic ast bytes/interp_basic.jelly)
  add_jellyc_ok_test(ast_bytes_interp_many ast bytes/interp_many.jelly)
  add_jellyc_ok_test(ast_bytes_interp_block ast bytes/interp_block.jelly)
  add_jellyc_ok_test(ast_bytes_unicode_escape ast bytes/unicode_escape.jelly)
  add_jellyc_ok_test(ast_syntax_haxe ast syntax/haxe.jelly)

  add_jellyc_ok_test(ast_tuple_basic ast tuple/basic.jelly)
  add_jellyc_ok_test(ir_tuple_basic ir tuple/basic.jelly)
  add_jellyc_ok_test(ir_tuple_eq ir tuple/eq.jelly)

  add_jellyc_ok_test(ir_function_body_locals ir function/body_locals.jelly)
  add_jellyc_ok_test(ir_function_return_void ir function/return_void.jelly)
  add_jellyc_ok_test(ir_function_tail_expr ir function/tail_expr.jelly)
  add_jellyc_ok_test(ir_function_closure_capture ir function/closure_capture.jelly)

  add_jellyc_ok_test(ir_recursion_basic ir recursion/basic.jelly)
  add_jellyc_ok_test(ir_recursion_tail ir recursion/tail.jelly)

  add_jellyc_ok_test(ir_object_literal ir object/literal.jelly)
  add_jellyc_ok_test(ast_object_get ast object/get.jelly)
  add_jellyc_ok_test(ir_object_get ir object/get.jelly)
  add_jellyc_ok_test(ast_object_assign ast object/assign.jelly)
  add_jellyc_ok_test(ir_object_assign ir object/assign.jelly)
  add_jellyc_ok_test(ir_object_across_fn ir object/across_fn.jelly)
  add_jellyc_ok_test(ir_object_method_call_this ir object/method_call_this.jelly)
  add_jellyc_ok_test(ast_object_prototype_delegation ast object/prototype_delegation.jelly)
  add_jellyc_ok_test(ir_object_prototype_delegation ir object/prototype_delegation.jelly)
  add_jellyc_ok_test(ast_object_prototype_sugar_basic ast object/prototype_sugar_basic.jelly)
  add_jellyc_ok_test(ast_object_prototype_sugar_template ast object/prototype_sugar_template.jelly)
  add_jellyc_ok_test(ast_object_dynamic_access ast object/dynamic_access.jelly)
  add_jellyc_ok_test(ir_object_dynamic_access ir object/dynamic_access.jelly)
  add_jellyc_ok_test(ir_object_new_calls_init ir object/new_calls_init.jelly)
  add_jellyc_ok_test(ir_object_new_calls_init_args ir object/new_calls_init_args.jelly)
  add_jellyc_ok_test(ir_object_new_template_proto_init ir object/new_template_proto_init.jelly)
  add_jellyc_ok_test(ir_object_new_template_proto_infer ir object/new_template_proto_infer.jelly)
  add_jellyc_ok_test(ast_object_new_template_proto_infer_var ast object/new_template_proto_infer_var.jelly)
  add_jellyc_ok_test(ir_object_new_template_proto_infer_var ir object/new_template_proto_infer_var.jelly)
  add_jellyc_ok_test(ast_object_template_runtime_metadata ast object/template_runtime_metadata.jelly)
  add_jellyc_ok_test(ir_object_template_runtime_metadata ir object/template_runtime_metadata.jelly)
  add_jellyc_ok_test(ast_object_prototype_as_type ast object/prototype_as_type.jelly)
  add_jellyc_ok_test(ir_object_prototype_as_type ir object/prototype_as_type.jelly)
  add_jellyc_ok_test(ir_object_prototype_syntax_basic ir object/prototype_syntax_basic.jelly)
  add_jellyc_ok_test(ir_object_prototype_syntax_template ir object/prototype_syntax_template.jelly)
  add_jellyc_ok_test(ir_object_template_ops ir object/template_ops.jelly)

  add_jellyc_ok_test(ir_match_basic ir match/basic.jelly)
  add_jellyc_ok_test(ir_match_fallthrough_when ir match/fallthrough_when.jelly)
  add_jellyc_ok_test(ir_match_object_pin ir match/object_pin.jelly)
  add_jellyc_ok_test(ir_match_tuple_basic ir match/tuple_basic.jelly)

  add_jellyc_ok_test(ir_module_basic ir module/main.jelly)
  add_jellyc_ok_test(ir_module_named_imports ir module/named_imports.jelly)
  add_jellyc_ok_test(ir_match_object_own_only_proto ir match/object_own_only_proto.jelly)
  add_jellyc_ok_test(ir_match_array_head_tail ir match/array_head_tail.jelly)
  add_jellyc_ok_test(ir_match_array_prefix_rest ir match/array_prefix_rest.jelly)
  add_jellyc_ok_test(ir_match_list_head_tail ir match/list_head_tail.jelly)
  add_jellyc_ok_test(ir_match_dynamic_scalar ir match/dynamic_scalar.jelly)
  add_jellyc_ok_test(ir_bytes_interp_order ir bytes/interp_order.jelly)
  add_jellyc_ok_test(ir_bytes_slice ir bytes/slice.jelly)
  add_jellyc_ok_test(ir_bytes_eq ir bytes/eq.jelly)
endif()

