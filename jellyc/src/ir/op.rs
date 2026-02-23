use super::ids::{BlockId, VRegId};

#[path = "op/analysis.rs"]
mod analysis;

#[derive(Clone, Debug)]
#[rustfmt::skip]
pub enum IrOp {
    // Constants / moves
    ConstI32   { dst: VRegId, imm: i32 },
    ConstI8Imm { dst: VRegId, imm: u8 },
    ConstF16   { dst: VRegId, bits: u16 },
    ConstI64   { dst: VRegId, pool_index: u32 },
    ConstBool  { dst: VRegId, imm: bool },
    ConstNull  { dst: VRegId },
    ConstBytes { dst: VRegId, pool_index: u32 },
    ConstF32   { dst: VRegId, bits: u32 },
    ConstF64   { dst: VRegId, pool_index: u32 },
    ConstAtom  { dst: VRegId, atom_id: u32 },
    ConstFun   { dst: VRegId, func_index: u32 },
    Mov        { dst: VRegId, src: VRegId },

    // Arithmetic / compare (typed)
    AddI32 { dst: VRegId, a: VRegId, b: VRegId },
    SubI32 { dst: VRegId, a: VRegId, b: VRegId },
    MulI32 { dst: VRegId, a: VRegId, b: VRegId },
    NegI32 { dst: VRegId, src: VRegId },
    EqI32  { dst: VRegId, a: VRegId, b: VRegId },
    LtI32  { dst: VRegId, a: VRegId, b: VRegId },

    AddI64 { dst: VRegId, a: VRegId, b: VRegId },
    SubI64 { dst: VRegId, a: VRegId, b: VRegId },
    MulI64 { dst: VRegId, a: VRegId, b: VRegId },
    NegI64 { dst: VRegId, src: VRegId },
    EqI64  { dst: VRegId, a: VRegId, b: VRegId },
    LtI64  { dst: VRegId, a: VRegId, b: VRegId },

    AddF16 { dst: VRegId, a: VRegId, b: VRegId },
    SubF16 { dst: VRegId, a: VRegId, b: VRegId },
    MulF16 { dst: VRegId, a: VRegId, b: VRegId },

    AddF32 { dst: VRegId, a: VRegId, b: VRegId },
    SubF32 { dst: VRegId, a: VRegId, b: VRegId },
    MulF32 { dst: VRegId, a: VRegId, b: VRegId },
    DivF32 { dst: VRegId, a: VRegId, b: VRegId },
    NegF32 { dst: VRegId, src: VRegId },
    EqF32  { dst: VRegId, a: VRegId, b: VRegId },
    LtF32  { dst: VRegId, a: VRegId, b: VRegId },

    AddF64 { dst: VRegId, a: VRegId, b: VRegId },
    SubF64 { dst: VRegId, a: VRegId, b: VRegId },
    MulF64 { dst: VRegId, a: VRegId, b: VRegId },
    DivF64 { dst: VRegId, a: VRegId, b: VRegId },
    NegF64 { dst: VRegId, src: VRegId },
    EqF64  { dst: VRegId, a: VRegId, b: VRegId },
    LtF64  { dst: VRegId, a: VRegId, b: VRegId },

    Physeq  { dst: VRegId, a: VRegId, b: VRegId }, // physical equality for non-i32 primitives/pointers
    NotBool { dst: VRegId, src: VRegId },
    Assert  { cond: VRegId },

    // Numeric conversions (explicit)
    SextI64    { dst: VRegId, src: VRegId }, // i32 -> i64
    SextI16    { dst: VRegId, src: VRegId }, // i8 -> i16
    TruncI8    { dst: VRegId, src: VRegId }, // i16/i32 -> i8
    TruncI16   { dst: VRegId, src: VRegId }, // i32 -> i16
    F16FromF32 { dst: VRegId, src: VRegId }, // f32 -> f16
    F32FromF16 { dst: VRegId, src: VRegId }, // f16 -> f32
    F32FromI32 { dst: VRegId, src: VRegId }, // i32 -> f32
    F64FromI32 { dst: VRegId, src: VRegId }, // i32 -> f64
    F64FromI64 { dst: VRegId, src: VRegId }, // i64 -> f64
    F64FromF32 { dst: VRegId, src: VRegId }, // f32 -> f64
    F32FromF64 { dst: VRegId, src: VRegId }, // f64 -> f32
    F32FromI64 { dst: VRegId, src: VRegId }, // i64 -> f32
    I64FromF32 { dst: VRegId, src: VRegId }, // f32 -> i64 (checked)
    I32FromI64 { dst: VRegId, src: VRegId }, // i64 -> i32 (truncate)
    I32FromF64 { dst: VRegId, src: VRegId }, // f64 -> i32 (checked)
    I64FromF64 { dst: VRegId, src: VRegId }, // f64 -> i64 (checked)
    I32FromF32 { dst: VRegId, src: VRegId }, // f32 -> i32 (checked)
    F16FromI32 { dst: VRegId, src: VRegId }, // i32 -> f16
    I32FromF16 { dst: VRegId, src: VRegId }, // f16 -> i32 (checked)

    // Dynamic introspection
    Kindof { dst: VRegId, src: VRegId }, // dst:I32 = kindof(src:Dynamic)

    // Bytes
    BytesNew        { dst: VRegId, len: VRegId },
    BytesLen        { dst: VRegId, bytes: VRegId },
    BytesGetU8      { dst: VRegId, bytes: VRegId, index: VRegId },
    BytesSetU8      { bytes: VRegId, index: VRegId, value: VRegId },
    BytesConcat2    { dst: VRegId, a: VRegId, b: VRegId },
    BytesConcatMany { dst: VRegId, parts: VRegId }, // parts: Array<bytes>

    // Lists (immutable cons list)
    ListNil   { dst: VRegId },
    ListCons  { dst: VRegId, head: VRegId, tail: VRegId },
    ListHead  { dst: VRegId, list: VRegId },
    ListTail  { dst: VRegId, list: VRegId },
    ListIsNil { dst: VRegId, list: VRegId },

    // Arrays
    ArrayNew { dst: VRegId, len: VRegId },
    ArrayLen { dst: VRegId, arr: VRegId },
    ArrayGet { dst: VRegId, arr: VRegId, index: VRegId },
    ArraySet { arr: VRegId, index: VRegId, value: VRegId },

    // Objects
    ObjNew     { dst: VRegId },
    ObjHasAtom { dst: VRegId, obj: VRegId, atom_id: u32 },
    ObjGetAtom { dst: VRegId, obj: VRegId, atom_id: u32 },
    ObjSetAtom { obj: VRegId, atom_id: u32, value: VRegId },
    ObjGet     { dst: VRegId, obj: VRegId, atom: VRegId },
    ObjSet     { obj: VRegId, atom: VRegId, value: VRegId },

    // Dynamic boundaries (explicit)
    ToDyn       { dst: VRegId, src: VRegId },
    FromDynI8   { dst: VRegId, src: VRegId },
    FromDynI16  { dst: VRegId, src: VRegId },
    FromDynI32  { dst: VRegId, src: VRegId },
    FromDynI64  { dst: VRegId, src: VRegId },
    FromDynF16  { dst: VRegId, src: VRegId },
    FromDynF32  { dst: VRegId, src: VRegId },
    FromDynF64  { dst: VRegId, src: VRegId },
    FromDynBool { dst: VRegId, src: VRegId },
    FromDynPtr  { dst: VRegId, src: VRegId },

    // Try/catch
    Try { catch_dst: VRegId, catch_block: BlockId, trap_only: bool },
    EndTry,
    Throw { payload: VRegId },

    // Calls
    Call { dst: VRegId, callee: VRegId, sig_id: u32, arg_base: VRegId, nargs: u8 }, // args live in vregs [arg_base .. arg_base+nargs)
    /// Create a closure for `func_index` with `ncaps` captures read from
    /// `[cap_base .. cap_base+ncaps)`.
    Closure { dst: VRegId, func_index: u32, cap_sig_id: u32, cap_base: VRegId, ncaps: u8 },
    /// Bind `this` to a function value (changes its function type).
    BindThis { dst: VRegId, func: VRegId, this: VRegId },

    // SSA join (must be eliminated before emission)
    Phi { dst: VRegId, incomings: Vec<(BlockId, VRegId)> },
}
