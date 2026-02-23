/*
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

use super::super::VRegId;
use super::IrOp;

impl IrOp {
    /// VRegIds used as operands (not defined).
    pub fn uses(&self) -> Vec<VRegId> {
        use IrOp::*;
        match self {
            Mov { src, .. } => vec![*src],
            AddI32 { a, b, .. }
            | SubI32 { a, b, .. }
            | MulI32 { a, b, .. }
            | EqI32 { a, b, .. }
            | LtI32 { a, b, .. }
            | AddI64 { a, b, .. }
            | SubI64 { a, b, .. }
            | MulI64 { a, b, .. }
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
            | ModI32S { a, b, .. }
            | ModI64S { a, b, .. }
            | ShlI32 { a, b, .. }
            | ShlI64 { a, b, .. }
            | ShrI32S { a, b, .. }
            | ShrI64S { a, b, .. } => vec![*a, *b],
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
            | I32FromF16 { src, .. } => vec![*src],
            BytesNew { len, .. } => vec![*len],
            BytesLen { bytes, .. } => vec![*bytes],
            BytesGetU8 { bytes, index, .. } => vec![*bytes, *index],
            BytesSetU8 {
                bytes,
                index,
                value,
            } => vec![*bytes, *index, *value],
            BytesConcat2 { a, b, .. } => vec![*a, *b],
            BytesConcatMany { parts, .. } => vec![*parts],
            ListCons { head, tail, .. } => vec![*head, *tail],
            ListHead { list, .. } | ListTail { list, .. } | ListIsNil { list, .. } => vec![*list],
            ArrayNew { len, .. } => vec![*len],
            ArrayLen { arr, .. } => vec![*arr],
            ArrayGet { arr, index, .. } => vec![*arr, *index],
            ArraySet { arr, index, value } => vec![*arr, *index, *value],
            ObjHasAtom { obj, .. } | ObjGetAtom { obj, .. } => vec![*obj],
            ObjSetAtom { obj, value, .. } => vec![*obj, *value],
            ObjGet { obj, atom, .. } => vec![*obj, *atom],
            ObjSet { obj, atom, value } => vec![*obj, *atom, *value],
            ToDyn { src, .. }
            | FromDynI8 { src, .. }
            | FromDynI16 { src, .. }
            | FromDynI32 { src, .. }
            | FromDynI64 { src, .. }
            | FromDynF16 { src, .. }
            | FromDynF32 { src, .. }
            | FromDynF64 { src, .. }
            | FromDynBool { src, .. }
            | FromDynPtr { src, .. } => vec![*src],
            Throw { payload } => vec![*payload],
            Call {
                callee,
                arg_base,
                nargs,
                ..
            } => {
                let mut v = vec![*callee];
                for i in 0..(*nargs as u32) {
                    v.push(VRegId(arg_base.0 + i));
                }
                v
            }
            Closure {
                cap_base, ncaps, ..
            } => (0..(*ncaps as u32))
                .map(|i| VRegId(cap_base.0 + i))
                .collect(),
            BindThis { func, this, .. } => vec![*func, *this],
            Phi { incomings, .. } => incomings.iter().map(|(_, v)| *v).collect(),
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
            | EndTry => vec![],
        }
    }

    /// VRegId defined by this op, if any.
    pub fn def(&self) -> Option<VRegId> {
        use IrOp::*;
        match self {
            Try { catch_dst, .. } => Some(*catch_dst),
            ConstI32 { dst, .. }
            | ConstI8Imm { dst, .. }
            | ConstF16 { dst, .. }
            | ConstI64 { dst, .. }
            | ConstF32 { dst, .. }
            | ConstF64 { dst, .. }
            | ConstBool { dst, .. }
            | ConstNull { dst }
            | ConstBytes { dst, .. }
            | ConstAtom { dst, .. }
            | ConstFun { dst, .. }
            | Mov { dst, .. }
            | AddI32 { dst, .. }
            | SubI32 { dst, .. }
            | MulI32 { dst, .. }
            | NegI32 { dst, .. }
            | EqI32 { dst, .. }
            | LtI32 { dst, .. }
            | AddI64 { dst, .. }
            | SubI64 { dst, .. }
            | MulI64 { dst, .. }
            | ModI32S { dst, .. }
            | ModI64S { dst, .. }
            | ShlI32 { dst, .. }
            | ShlI64 { dst, .. }
            | ShrI32S { dst, .. }
            | ShrI64S { dst, .. }
            | NegI64 { dst, .. }
            | EqI64 { dst, .. }
            | LtI64 { dst, .. }
            | AddF16 { dst, .. }
            | SubF16 { dst, .. }
            | MulF16 { dst, .. }
            | AddF32 { dst, .. }
            | SubF32 { dst, .. }
            | MulF32 { dst, .. }
            | DivF32 { dst, .. }
            | NegF32 { dst, .. }
            | EqF32 { dst, .. }
            | LtF32 { dst, .. }
            | AddF64 { dst, .. }
            | SubF64 { dst, .. }
            | MulF64 { dst, .. }
            | DivF64 { dst, .. }
            | NegF64 { dst, .. }
            | EqF64 { dst, .. }
            | LtF64 { dst, .. }
            | Physeq { dst, .. }
            | NotBool { dst, .. }
            | SextI64 { dst, .. }
            | SextI16 { dst, .. }
            | TruncI8 { dst, .. }
            | TruncI16 { dst, .. }
            | F16FromF32 { dst, .. }
            | F32FromF16 { dst, .. }
            | F32FromI32 { dst, .. }
            | F64FromI32 { dst, .. }
            | F64FromI64 { dst, .. }
            | F64FromF32 { dst, .. }
            | F32FromF64 { dst, .. }
            | F32FromI64 { dst, .. }
            | I64FromF32 { dst, .. }
            | I32FromI64 { dst, .. }
            | I32FromF64 { dst, .. }
            | I64FromF64 { dst, .. }
            | I32FromF32 { dst, .. }
            | F16FromI32 { dst, .. }
            | I32FromF16 { dst, .. }
            | Kindof { dst, .. }
            | BytesNew { dst, .. }
            | BytesLen { dst, .. }
            | BytesGetU8 { dst, .. }
            | BytesConcat2 { dst, .. }
            | BytesConcatMany { dst, .. }
            | ListNil { dst }
            | ListCons { dst, .. }
            | ListHead { dst, .. }
            | ListTail { dst, .. }
            | ListIsNil { dst, .. }
            | ArrayNew { dst, .. }
            | ArrayLen { dst, .. }
            | ArrayGet { dst, .. }
            | ObjNew { dst }
            | ObjHasAtom { dst, .. }
            | ObjGetAtom { dst, .. }
            | ObjGet { dst, .. }
            | ToDyn { dst, .. }
            | FromDynI8 { dst, .. }
            | FromDynI16 { dst, .. }
            | FromDynI32 { dst, .. }
            | FromDynI64 { dst, .. }
            | FromDynF16 { dst, .. }
            | FromDynF32 { dst, .. }
            | FromDynF64 { dst, .. }
            | FromDynBool { dst, .. }
            | FromDynPtr { dst, .. }
            | Call { dst, .. }
            | Closure { dst, .. }
            | BindThis { dst, .. }
            | Phi { dst, .. } => Some(*dst),
            BytesSetU8 { .. }
            | ArraySet { .. }
            | ObjSetAtom { .. }
            | ObjSet { .. }
            | Throw { .. }
            | EndTry
            | Assert { .. } => None,
        }
    }
}
