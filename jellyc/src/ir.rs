use crate::ast::Span;
use crate::jlyb::{FunSig, TypeEntry};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VRegId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub u32);

pub type TypeId = u32;

#[derive(Clone)]
pub struct IrModule {
    pub types: Vec<TypeEntry>,
    pub sigs: Vec<FunSig>,
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
    ConstBool { dst: VRegId, imm: bool },
    ConstNull { dst: VRegId },
    ConstBytes { dst: VRegId, pool_index: u32 },
    ConstAtom { dst: VRegId, atom_id: u32 },
    ConstFun { dst: VRegId, func_index: u32 },
    Mov { dst: VRegId, src: VRegId },

    // Arithmetic / compare (typed)
    AddI32 { dst: VRegId, a: VRegId, b: VRegId },
    SubI32 { dst: VRegId, a: VRegId, b: VRegId },
    NegI32 { dst: VRegId, src: VRegId },
    EqI32 { dst: VRegId, a: VRegId, b: VRegId },
    LtI32 { dst: VRegId, a: VRegId, b: VRegId },
    Physeq { dst: VRegId, a: VRegId, b: VRegId }, // physical equality for non-i32 primitives/pointers
    NotBool { dst: VRegId, src: VRegId },

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
    FromDynI32 { dst: VRegId, src: VRegId },
    FromDynBool { dst: VRegId, src: VRegId },
    FromDynPtr { dst: VRegId, src: VRegId },

    // Exceptions (VM-level)
    Try { catch_dst: VRegId, catch_block: BlockId },
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

