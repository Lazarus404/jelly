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

use crate::ir::{IrOp, IrTerminator, VRegId};
use std::collections::HashMap;

pub(super) fn resolve(v: VRegId, alias: &mut HashMap<VRegId, VRegId>) -> VRegId {
    let mut cur = v;
    // Follow at most a bounded number of steps to avoid accidental cycles
    // (shouldn't happen in SSA-ish IR, but be defensive).
    for _ in 0..64 {
        let next = match alias.get(&cur).copied() {
            Some(n) => n,
            None => break,
        };
        if next == cur {
            break;
        }
        cur = next;
    }
    // Compress one step.
    if cur != v {
        alias.insert(v, cur);
    }
    cur
}

pub(super) fn rewrite_term_uses(t: &mut IrTerminator, alias: &mut HashMap<VRegId, VRegId>) -> bool {
    match t {
        IrTerminator::Ret { value } => {
            let v2 = resolve(*value, alias);
            if v2 != *value {
                *value = v2;
                return true;
            }
            false
        }
        IrTerminator::JmpIf { cond, .. } => {
            let v2 = resolve(*cond, alias);
            if v2 != *cond {
                *cond = v2;
                return true;
            }
            false
        }
        IrTerminator::SwitchKind { kind, .. } => {
            let v2 = resolve(*kind, alias);
            if v2 != *kind {
                *kind = v2;
                return true;
            }
            false
        }
        IrTerminator::Jmp { .. } | IrTerminator::TailCall { .. } | IrTerminator::Unreachable => false,
    }
}

pub(super) fn rewrite_op_uses(op: &mut IrOp, alias: &mut HashMap<VRegId, VRegId>) -> bool {
    use IrOp::*;
    let mut changed = false;
    match op {
        Mov { src, .. } => {
            let v2 = resolve(*src, alias);
            if v2 != *src {
                *src = v2;
                changed = true;
            }
        }
        AddI32 { a, b, .. }
        | SubI32 { a, b, .. }
        | MulI32 { a, b, .. }
        | EqI32 { a, b, .. }
        | LtI32 { a, b, .. }
        | AddI64 { a, b, .. }
        | SubI64 { a, b, .. }
        | MulI64 { a, b, .. }
        | ModI32S { a, b, .. }
        | ModI64S { a, b, .. }
        | ShlI32 { a, b, .. }
        | ShlI64 { a, b, .. }
        | ShrI32S { a, b, .. }
        | ShrI64S { a, b, .. }
        | EqI64 { a, b, .. }
        | LtI64 { a, b, .. }
        | AddF16 { a, b, .. }
        | SubF16 { a, b, .. }
        | MulF16 { a, b, .. }
        | AddF32 { a, b, .. }
        | SubF32 { a, b, .. }
        | MulF32 { a, b, .. }
        | DivF32 { a, b, .. }
        | EqF32 { a, b, .. }
        | LtF32 { a, b, .. }
        | AddF64 { a, b, .. }
        | SubF64 { a, b, .. }
        | MulF64 { a, b, .. }
        | DivF64 { a, b, .. }
        | EqF64 { a, b, .. }
        | LtF64 { a, b, .. }
        | Physeq { a, b, .. }
        | BytesConcat2 { a, b, .. }
        | ListCons {
            head: a, tail: b, ..
        } => {
            let a2 = resolve(*a, alias);
            let b2 = resolve(*b, alias);
            if a2 != *a {
                *a = a2;
                changed = true;
            }
            if b2 != *b {
                *b = b2;
                changed = true;
            }
        }
        NegI32 { src, .. }
        | NegI64 { src, .. }
        | NegF32 { src, .. }
        | NegF64 { src, .. }
        | NotBool { src, .. }
        | Assert { cond: src }
        | Kindof { src, .. }
        | SextI64 { src, .. }
        | SextI16 { src, .. }
        | TruncI8 { src, .. }
        | TruncI16 { src, .. }
        | F16FromF32 { src, .. }
        | F32FromF16 { src, .. }
        | F32FromI32 { src, .. }
        | F64FromI32 { src, .. }
        | F64FromI64 { src, .. }
        | F64FromF32 { src, .. }
        | F32FromF64 { src, .. }
        | F32FromI64 { src, .. }
        | I64FromF32 { src, .. }
        | I32FromI64 { src, .. }
        | I32FromF64 { src, .. }
        | I64FromF64 { src, .. }
        | I32FromF32 { src, .. }
        | F16FromI32 { src, .. }
        | I32FromF16 { src, .. }
        | BytesNew { len: src, .. }
        | BytesLen { bytes: src, .. }
        | BytesConcatMany { parts: src, .. }
        | ListHead { list: src, .. }
        | ListTail { list: src, .. }
        | ListIsNil { list: src, .. }
        | ArrayNew { len: src, .. }
        | ArrayLen { arr: src, .. }
        | ToDyn { src, .. }
        | FromDynI8 { src, .. }
        | FromDynI16 { src, .. }
        | FromDynI32 { src, .. }
        | FromDynI64 { src, .. }
        | FromDynF16 { src, .. }
        | FromDynF32 { src, .. }
        | FromDynF64 { src, .. }
        | FromDynBool { src, .. }
        | FromDynPtr { src, .. }
        | Throw { payload: src }
        | ObjHasAtom { obj: src, .. }
        | ObjGetAtom { obj: src, .. } => {
            let v2 = resolve(*src, alias);
            if v2 != *src {
                *src = v2;
                changed = true;
            }
        }
        BytesGetU8 { bytes, index, .. }
        | ArrayGet {
            arr: bytes, index, ..
        } => {
            let b2 = resolve(*bytes, alias);
            let i2 = resolve(*index, alias);
            if b2 != *bytes {
                *bytes = b2;
                changed = true;
            }
            if i2 != *index {
                *index = i2;
                changed = true;
            }
        }
        BytesSetU8 {
            bytes,
            index,
            value,
        }
        | ArraySet {
            arr: bytes,
            index,
            value,
        } => {
            let b2 = resolve(*bytes, alias);
            let i2 = resolve(*index, alias);
            let v2 = resolve(*value, alias);
            if b2 != *bytes {
                *bytes = b2;
                changed = true;
            }
            if i2 != *index {
                *index = i2;
                changed = true;
            }
            if v2 != *value {
                *value = v2;
                changed = true;
            }
        }
        ObjSetAtom { obj, value, .. } => {
            let o2 = resolve(*obj, alias);
            let v2 = resolve(*value, alias);
            if o2 != *obj {
                *obj = o2;
                changed = true;
            }
            if v2 != *value {
                *value = v2;
                changed = true;
            }
        }
        ObjGet { obj, atom, .. } => {
            let o2 = resolve(*obj, alias);
            let a2 = resolve(*atom, alias);
            if o2 != *obj {
                *obj = o2;
                changed = true;
            }
            if a2 != *atom {
                *atom = a2;
                changed = true;
            }
        }
        ObjSet { obj, atom, value } => {
            let o2 = resolve(*obj, alias);
            let a2 = resolve(*atom, alias);
            let v2 = resolve(*value, alias);
            if o2 != *obj {
                *obj = o2;
                changed = true;
            }
            if a2 != *atom {
                *atom = a2;
                changed = true;
            }
            if v2 != *value {
                *value = v2;
                changed = true;
            }
        }
        BindThis { func, this, .. } => {
            let f2 = resolve(*func, alias);
            let t2 = resolve(*this, alias);
            if f2 != *func {
                *func = f2;
                changed = true;
            }
            if t2 != *this {
                *this = t2;
                changed = true;
            }
        }
        // NOTE: Do not rewrite `arg_base` (implicit window); do rewrite callee.
        Call { callee, .. } => {
            let c2 = resolve(*callee, alias);
            if c2 != *callee {
                *callee = c2;
                changed = true;
            }
        }
        // NOTE: Do not rewrite `cap_base` (implicit window).
        Closure { .. } => {}
        Phi { incomings, .. } => {
            for (_, v) in incomings.iter_mut() {
                let v2 = resolve(*v, alias);
                if v2 != *v {
                    *v = v2;
                    changed = true;
                }
            }
        }
        ConstI32 { .. }
        | ConstI8Imm { .. }
        | ConstF16 { .. }
        | ConstI64 { .. }
        | ConstF32 { .. }
        | ConstF64 { .. }
        | ConstBool { .. }
        | ConstNull { .. }
        | ConstBytes { .. }
        | ConstAtom { .. }
        | ConstFun { .. }
        | ListNil { .. }
        | ObjNew { .. }
        | Try { .. }
        | EndTry => {}
    }
    changed
}
