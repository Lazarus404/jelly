#ifndef JELLY_BYTECODE_CHECK_H
#define JELLY_BYTECODE_CHECK_H

#include <jelly/loader.h>

// Instruction validation that depends on opcode semantics.
jelly_bc_result jelly_bc_validate_insn(const jelly_bc_module* m,
                                      const jelly_type_id* reg_types,
                                      const jelly_insn* ins,
                                      uint32_t nregs,
                                      uint32_t pc,
                                      uint32_t ninsns,
                                      uint32_t nfuncs);

// Post-parse validation that needs visibility of the full instruction stream
// (e.g. jump tables / multi-insn encoding constraints).
jelly_bc_result jelly_bc_validate_function_semantics(const jelly_bc_module* m,
                                                    const jelly_type_id* reg_types,
                                                    const jelly_insn* insns,
                                                    uint32_t nregs,
                                                    uint32_t ninsns);

#endif /* JELLY_BYTECODE_CHECK_H */

