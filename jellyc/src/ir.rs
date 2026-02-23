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

use crate::ast::Span;
use crate::jlyb::{FunSig, TypeEntry};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VRegId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

pub type TypeId = u32;

#[derive(Clone, Debug)]
pub struct IrModule {
    pub types: Vec<TypeEntry>,
    pub sigs: Vec<FunSig>,
    pub const_i64: Vec<i64>,
    pub const_f64: Vec<f64>,
    pub const_bytes: Vec<Vec<u8>>,
    pub atoms: Vec<Vec<u8>>,
    pub funcs: Vec<IrFunction>,
    pub entry: usize,
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub name: Option<String>,
    pub param_count: u8,
    /// Captured values live in `Dynamic` regs at the *end* of the callee frame.
    /// The vregs listed here correspond 1:1 with the closure capture order.
    pub cap_vregs: Vec<VRegId>,
    pub entry: BlockId,
    pub blocks: Vec<IrBlock>,
    pub vreg_types: Vec<TypeId>,
}

#[derive(Clone, Debug)]
pub struct IrBlock {
    pub label: Option<String>,
    pub insns: Vec<IrInsn>,
    pub term: IrTerminator,
}

#[derive(Clone, Debug)]
pub struct IrInsn {
    pub span: Span,
    pub op: IrOp,
}

#[derive(Clone, Debug)]
pub enum IrOp {
    // Constants / moves
    ConstI32 { dst: VRegId, imm: i32 },
    ConstI8Imm { dst: VRegId, imm: u8 },
    ConstF16 { dst: VRegId, bits: u16 },
    ConstI64 { dst: VRegId, pool_index: u32 },
    ConstBool { dst: VRegId, imm: bool },
    ConstNull { dst: VRegId },
    ConstBytes { dst: VRegId, pool_index: u32 },
    ConstF32 { dst: VRegId, bits: u32 },
    ConstF64 { dst: VRegId, pool_index: u32 },
    ConstAtom { dst: VRegId, atom_id: u32 },
    ConstFun { dst: VRegId, func_index: u32 },
    Mov { dst: VRegId, src: VRegId },

    // Arithmetic / compare (typed)
    AddI32 { dst: VRegId, a: VRegId, b: VRegId },
    SubI32 { dst: VRegId, a: VRegId, b: VRegId },
    MulI32 { dst: VRegId, a: VRegId, b: VRegId },
    NegI32 { dst: VRegId, src: VRegId },
    EqI32 { dst: VRegId, a: VRegId, b: VRegId },
    LtI32 { dst: VRegId, a: VRegId, b: VRegId },
    AddI64 { dst: VRegId, a: VRegId, b: VRegId },
    SubI64 { dst: VRegId, a: VRegId, b: VRegId },
    MulI64 { dst: VRegId, a: VRegId, b: VRegId },
    NegI64 { dst: VRegId, src: VRegId },
    EqI64 { dst: VRegId, a: VRegId, b: VRegId },
    LtI64 { dst: VRegId, a: VRegId, b: VRegId },
    AddF16 { dst: VRegId, a: VRegId, b: VRegId },
    SubF16 { dst: VRegId, a: VRegId, b: VRegId },
    MulF16 { dst: VRegId, a: VRegId, b: VRegId },
    AddF32 { dst: VRegId, a: VRegId, b: VRegId },
    SubF32 { dst: VRegId, a: VRegId, b: VRegId },
    MulF32 { dst: VRegId, a: VRegId, b: VRegId },
    DivF32 { dst: VRegId, a: VRegId, b: VRegId },
    NegF32 { dst: VRegId, src: VRegId },
    EqF32 { dst: VRegId, a: VRegId, b: VRegId },
    LtF32 { dst: VRegId, a: VRegId, b: VRegId },
    AddF64 { dst: VRegId, a: VRegId, b: VRegId },
    SubF64 { dst: VRegId, a: VRegId, b: VRegId },
    MulF64 { dst: VRegId, a: VRegId, b: VRegId },
    DivF64 { dst: VRegId, a: VRegId, b: VRegId },
    NegF64 { dst: VRegId, src: VRegId },
    EqF64 { dst: VRegId, a: VRegId, b: VRegId },
    LtF64 { dst: VRegId, a: VRegId, b: VRegId },
    Physeq { dst: VRegId, a: VRegId, b: VRegId }, // physical equality for non-i32 primitives/pointers
    NotBool { dst: VRegId, src: VRegId },
    Assert { cond: VRegId },

    // Numeric conversions (explicit)
    SextI64 { dst: VRegId, src: VRegId },     // i32 -> i64
    SextI16 { dst: VRegId, src: VRegId },     // i8 -> i16
    TruncI8 { dst: VRegId, src: VRegId },     // i16/i32 -> i8
    TruncI16 { dst: VRegId, src: VRegId },    // i32 -> i16
    F16FromF32 { dst: VRegId, src: VRegId },  // f32 -> f16
    F32FromF16 { dst: VRegId, src: VRegId },  // f16 -> f32
    F32FromI32 { dst: VRegId, src: VRegId },  // i32 -> f32
    F64FromI32 { dst: VRegId, src: VRegId },  // i32 -> f64
    F64FromI64 { dst: VRegId, src: VRegId },  // i64 -> f64
    F64FromF32 { dst: VRegId, src: VRegId },  // f32 -> f64
    F32FromF64 { dst: VRegId, src: VRegId },  // f64 -> f32
    F32FromI64 { dst: VRegId, src: VRegId },  // i64 -> f32
    I64FromF32 { dst: VRegId, src: VRegId },  // f32 -> i64 (checked)
    I32FromI64 { dst: VRegId, src: VRegId },  // i64 -> i32 (truncate)
    I32FromF64 { dst: VRegId, src: VRegId },  // f64 -> i32 (checked)
    I64FromF64 { dst: VRegId, src: VRegId },  // f64 -> i64 (checked)
    I32FromF32 { dst: VRegId, src: VRegId },  // f32 -> i32 (checked)
    F16FromI32 { dst: VRegId, src: VRegId },  // i32 -> f16
    I32FromF16 { dst: VRegId, src: VRegId },  // f16 -> i32 (checked)

    // Dynamic introspection
    Kindof { dst: VRegId, src: VRegId }, // dst:I32 = kindof(src:Dynamic)

    // Bytes
    BytesNew { dst: VRegId, len: VRegId },
    BytesLen { dst: VRegId, bytes: VRegId },
    BytesGetU8 { dst: VRegId, bytes: VRegId, index: VRegId },
    BytesSetU8 { bytes: VRegId, index: VRegId, value: VRegId },
    BytesConcat2 { dst: VRegId, a: VRegId, b: VRegId },
    BytesConcatMany { dst: VRegId, parts: VRegId }, // parts: Array<bytes>

    // Lists (immutable cons list)
    ListNil { dst: VRegId },
    ListCons { dst: VRegId, head: VRegId, tail: VRegId },
    ListHead { dst: VRegId, list: VRegId },
    ListTail { dst: VRegId, list: VRegId },
    ListIsNil { dst: VRegId, list: VRegId },

    // Arrays
    ArrayNew { dst: VRegId, len: VRegId },
    ArrayLen { dst: VRegId, arr: VRegId },
    ArrayGet { dst: VRegId, arr: VRegId, index: VRegId },
    ArraySet { arr: VRegId, index: VRegId, value: VRegId },

    // Objects
    ObjNew { dst: VRegId },
    ObjHasAtom { dst: VRegId, obj: VRegId, atom_id: u32 },
    ObjGetAtom { dst: VRegId, obj: VRegId, atom_id: u32 },
    ObjSetAtom { obj: VRegId, atom_id: u32, value: VRegId },
    ObjGet { dst: VRegId, obj: VRegId, atom: VRegId },
    ObjSet { obj: VRegId, atom: VRegId, value: VRegId },

    // Dynamic boundaries (explicit)
    ToDyn { dst: VRegId, src: VRegId },
    FromDynI8 { dst: VRegId, src: VRegId },
    FromDynI16 { dst: VRegId, src: VRegId },
    FromDynI32 { dst: VRegId, src: VRegId },
    FromDynI64 { dst: VRegId, src: VRegId },
    FromDynF16 { dst: VRegId, src: VRegId },
    FromDynF32 { dst: VRegId, src: VRegId },
    FromDynF64 { dst: VRegId, src: VRegId },
    FromDynBool { dst: VRegId, src: VRegId },
    FromDynPtr { dst: VRegId, src: VRegId },

    // Exceptions (VM-level)
    /// Exception handler.
    ///
    /// `trap_only=true` means this handler only catches VM traps
    /// (ie trap_code != THROWN). `trap_only=false` catches everything
    /// (including user `throw` and `System.assert` failures).
    Try { catch_dst: VRegId, catch_block: BlockId, trap_only: bool },
    EndTry,
    Throw { payload: VRegId },

    // Calls
    Call {
        dst: VRegId,
        callee: VRegId,
        sig_id: u32,
        arg_base: VRegId, // args live in vregs [arg_base .. arg_base+nargs)
        nargs: u8,
    },
    /// Create a closure for `func_index` with `ncaps` captures read from
    /// `[cap_base .. cap_base+ncaps)`.
    Closure {
        dst: VRegId,
        func_index: u32,
        cap_sig_id: u32,
        cap_base: VRegId,
        ncaps: u8,
    },
    /// Bind `this` to a function value (changes its function type).
    BindThis { dst: VRegId, func: VRegId, this: VRegId },

    // SSA join (must be eliminated before emission)
    Phi { dst: VRegId, incomings: Vec<(BlockId, VRegId)> },
}

#[derive(Clone, Debug)]
pub enum IrTerminator {
    Jmp { target: BlockId },
    JmpIf { cond: VRegId, then_tgt: BlockId, else_tgt: BlockId },
    SwitchKind {
        kind: VRegId,              // I32 kind code
        cases: Vec<(u8, BlockId)>, // kind_u8 -> target block
        default: BlockId,
    },
    Ret { value: VRegId },
    Unreachable,
}

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
            | Physeq { a, b, .. } => vec![*a, *b],
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
            BytesSetU8 { bytes, index, value } => vec![*bytes, *index, *value],
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
            ToDyn { src, .. } | FromDynI8 { src, .. } | FromDynI16 { src, .. } | FromDynI32 { src, .. }
            | FromDynI64 { src, .. } | FromDynF16 { src, .. } | FromDynF32 { src, .. } | FromDynF64 { src, .. }
            | FromDynBool { src, .. } | FromDynPtr { src, .. } => vec![*src],
            Throw { payload } => vec![*payload],
            Call { callee, arg_base, nargs, .. } => {
                let mut v = vec![*callee];
                for i in 0..(*nargs as u32) {
                    v.push(VRegId(arg_base.0 + i));
                }
                v
            }
            Closure { cap_base, ncaps, .. } => {
                (0..(*ncaps as u32)).map(|i| VRegId(cap_base.0 + i)).collect()
            }
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
            BytesSetU8 { .. } | ArraySet { .. } | ObjSetAtom { .. } | ObjSet { .. }
            | Throw { .. } | EndTry | Assert { .. } => None,
        }
    }
}

#[derive(Debug)]
pub struct IrBuilder {
    pub func: IrFunction,
    cur: BlockId,
}

impl IrBuilder {
    pub fn new(name: Option<String>) -> Self {
        let entry = BlockId(0);
        Self {
            func: IrFunction {
                name,
                param_count: 0,
                cap_vregs: Vec::new(),
                entry,
                blocks: vec![IrBlock {
                    label: Some("entry".to_string()),
                    insns: Vec::new(),
                    term: IrTerminator::Unreachable,
                }],
                vreg_types: Vec::new(),
            },
            cur: entry,
        }
    }

    pub fn new_block(&mut self, label: Option<String>) -> BlockId {
        let id = BlockId(self.func.blocks.len() as u32);
        self.func.blocks.push(IrBlock {
            label,
            insns: Vec::new(),
            term: IrTerminator::Unreachable,
        });
        id
    }

    pub fn set_block(&mut self, b: BlockId) {
        self.cur = b;
    }

    pub fn cur_block(&self) -> BlockId {
        self.cur
    }

    pub fn is_open(&self) -> bool {
        matches!(
            self.func.blocks[self.cur.0 as usize].term,
            IrTerminator::Unreachable
        )
    }

    pub fn new_vreg(&mut self, tid: TypeId) -> VRegId {
        let id = VRegId(self.func.vreg_types.len() as u32);
        self.func.vreg_types.push(tid);
        id
    }

    pub fn emit(&mut self, span: Span, op: IrOp) {
        let b = &mut self.func.blocks[self.cur.0 as usize];
        b.insns.push(IrInsn { span, op });
    }

    pub fn term(&mut self, term: IrTerminator) {
        let b = &mut self.func.blocks[self.cur.0 as usize];
        b.term = term;
    }
}

