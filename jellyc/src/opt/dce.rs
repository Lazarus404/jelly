/**
 * Copyright 2022 - Jahred Love
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this
 * list of conditions and the following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may
 * be used to endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

// Dead code elimination: remove instructions whose result is never used.

use crate::ir::{IrFunction, IrModule, IrOp, IrTerminator, VRegId};
use std::collections::HashSet;

/// Collect all vregs that are used (as operands or in terminators).
fn used_vregs(func: &IrFunction) -> HashSet<VRegId> {
    let mut used = HashSet::new();

    for block in &func.blocks {
        for ins in &block.insns {
            for v in ins.op.uses() {
                used.insert(v);
            }
        }
        match &block.term {
            IrTerminator::Ret { value } => {
                used.insert(*value);
            }
            IrTerminator::JmpIf { cond, .. } => {
                used.insert(*cond);
            }
            IrTerminator::SwitchKind { kind, .. } => {
                used.insert(*kind);
            }
            IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
        }
    }

    used
}

/// True if the instruction has side effects and must never be removed.
/// Includes ops that can trap (control flow) even when their result is unused.
fn has_side_effects(op: &IrOp) -> bool {
    matches!(
        op,
        IrOp::Call { .. }
            | IrOp::Throw { .. }
            | IrOp::Try { .. }
            | IrOp::EndTry
            | IrOp::BytesSetU8 { .. }
            | IrOp::BytesGetU8 { .. }
            | IrOp::BytesLen { .. }
            | IrOp::BytesConcat2 { .. }
            | IrOp::BytesConcatMany { .. }
            | IrOp::ArraySet { .. }
            | IrOp::ArrayGet { .. }
            | IrOp::ArrayLen { .. }
            | IrOp::ListHead { .. }
            | IrOp::ListTail { .. }
            | IrOp::ListIsNil { .. }
            | IrOp::ObjSetAtom { .. }
            | IrOp::ObjGet { .. }
            | IrOp::ObjHasAtom { .. }
            | IrOp::ObjGetAtom { .. }
            | IrOp::FromDynI8 { .. }
            | IrOp::FromDynI16 { .. }
            | IrOp::FromDynI32 { .. }
            | IrOp::FromDynI64 { .. }
            | IrOp::FromDynF16 { .. }
            | IrOp::FromDynF32 { .. }
            | IrOp::FromDynF64 { .. }
            | IrOp::FromDynBool { .. }
            | IrOp::FromDynPtr { .. }
    )
}

/// Remove instructions whose defined vreg is never used.
/// Never removes instructions with side effects (Call, Throw, stores, etc.).
/// Returns true if any change was made.
pub fn dead_code_elimination(m: &mut IrModule) -> bool {
    let mut changed = false;

    for func in &mut m.funcs {
        let used = used_vregs(func);

        for block in &mut func.blocks {
            block.insns.retain(|ins| {
                if has_side_effects(&ins.op) {
                    return true; // never remove
                }
                if let Some(dst) = ins.op.def() {
                    if !used.contains(&dst) {
                        changed = true;
                        return false;
                    }
                }
                true
            });
        }
    }

    changed
}
