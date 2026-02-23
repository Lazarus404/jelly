use crate::ir::{IrOp, IrTerminator, VRegId};

pub(super) fn rename_use(v: VRegId, is_multi: &[bool], stacks: &[Vec<VRegId>]) -> VRegId {
    let i = v.0 as usize;
    if i < is_multi.len() && is_multi[i] {
        stacks[i].last().copied().unwrap_or(v)
    } else {
        v
    }
}

pub(super) fn map_op_uses(op: &mut IrOp, mut f: impl FnMut(VRegId) -> VRegId) {
    use IrOp::*;
    match op {
        Mov { src, .. } => *src = f(*src),
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
        | Physeq { a, b, .. } => {
            *a = f(*a);
            *b = f(*b);
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
        | ToDyn { src, .. }
        | FromDynI8 { src, .. }
        | FromDynI16 { src, .. }
        | FromDynI32 { src, .. }
        | FromDynI64 { src, .. }
        | FromDynF16 { src, .. }
        | FromDynF32 { src, .. }
        | FromDynF64 { src, .. }
        | FromDynBool { src, .. }
        | FromDynPtr { src, .. } => *src = f(*src),
        BytesNew { len, .. } | ArrayNew { len, .. } => *len = f(*len),
        BytesLen { bytes, .. } => *bytes = f(*bytes),
        BytesGetU8 { bytes, index, .. } => {
            *bytes = f(*bytes);
            *index = f(*index);
        }
        BytesSetU8 {
            bytes,
            index,
            value,
        } => {
            *bytes = f(*bytes);
            *index = f(*index);
            *value = f(*value);
        }
        BytesConcat2 { a, b, .. } => {
            *a = f(*a);
            *b = f(*b);
        }
        BytesConcatMany { parts, .. } => *parts = f(*parts),
        ListCons { head, tail, .. } => {
            *head = f(*head);
            *tail = f(*tail);
        }
        ListHead { list, .. } | ListTail { list, .. } | ListIsNil { list, .. } => *list = f(*list),
        ArrayLen { arr, .. } => *arr = f(*arr),
        ArrayGet { arr, index, .. } => {
            *arr = f(*arr);
            *index = f(*index);
        }
        ArraySet { arr, index, value } => {
            *arr = f(*arr);
            *index = f(*index);
            *value = f(*value);
        }
        ObjHasAtom { obj, .. } | ObjGetAtom { obj, .. } => *obj = f(*obj),
        ObjSetAtom { obj, value, .. } => {
            *obj = f(*obj);
            *value = f(*value);
        }
        ObjGet { obj, atom, .. } => {
            *obj = f(*obj);
            *atom = f(*atom);
        }
        ObjSet { obj, atom, value } => {
            *obj = f(*obj);
            *atom = f(*atom);
            *value = f(*value);
        }
        Throw { payload } => *payload = f(*payload),
        // NOTE: do not rewrite arg_base/cap_base (implicit windows); rewrite callee / captured values are implicit.
        Call { callee, .. } => *callee = f(*callee),
        Closure { .. } => {}
        BindThis { func, this, .. } => {
            *func = f(*func);
            *this = f(*this);
        }
        Phi { incomings, .. } => {
            for (_, v) in incomings.iter_mut() {
                *v = f(*v);
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
}

pub(super) fn set_op_def(op: &mut IrOp, new_dst: VRegId) {
    use IrOp::*;
    match op {
        Try { catch_dst, .. } => *catch_dst = new_dst,
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
        | NotBool { dst, .. }
        | Physeq { dst, .. }
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
        | Closure { dst, .. }
        | BindThis { dst, .. }
        | Call { dst, .. }
        | Phi { dst, .. } => *dst = new_dst,
        BytesSetU8 { .. }
        | ArraySet { .. }
        | ObjSetAtom { .. }
        | ObjSet { .. }
        | Throw { .. }
        | Assert { .. }
        | EndTry => {}
    }
}

pub(super) fn map_term_uses(term: &mut IrTerminator, mut f: impl FnMut(VRegId) -> VRegId) {
    match term {
        IrTerminator::Jmp { .. } | IrTerminator::Unreachable => {}
        IrTerminator::JmpIf { cond, .. } => *cond = f(*cond),
        IrTerminator::SwitchKind { kind, .. } => *kind = f(*kind),
        IrTerminator::Ret { value } => *value = f(*value),
        IrTerminator::TailCall { callee, .. } => *callee = f(*callee),
    }
}
