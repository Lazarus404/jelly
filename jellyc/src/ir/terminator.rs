use super::ids::{BlockId, VRegId};

#[derive(Clone, Debug)]
pub enum IrTerminator {
    Jmp {
        target: BlockId,
    },
    JmpIf {
        cond: VRegId,
        then_tgt: BlockId,
        else_tgt: BlockId,
    },
    SwitchKind {
        kind: VRegId,              // I32 kind code
        cases: Vec<(u8, BlockId)>, // kind_u8 -> target block
        default: BlockId,
    },
    Ret {
        value: VRegId,
    },
    /// Tail call: replace current frame with callee. Same encoding as Call.
    TailCall {
        callee: VRegId,
        sig_id: u32,
        arg_base: VRegId,
        nargs: u8,
    },
    Unreachable,
}
