use super::*;
use crate::regalloc::VReg;

#[test]
fn insert_spill_push_pop_around_def_and_use() {
    // v0 = ConstI32 123; v1 = ToDyn v0; SpillPush v1; SpillPop v1; Ret v1
    // Simulate: v0 in r0, v1 spilled (slot 0), reload into r1
    let vinsns = vec![
        VInsn {
            op: Op::ConstI32 as u8,
            a: 0,
            b: 0,
            c: 0,
            imm: 123,
        },
        VInsn {
            op: Op::ToDyn as u8,
            a: 1,
            b: 0,
            c: 0,
            imm: 0,
        },
        VInsn {
            op: Op::Ret as u8,
            a: 1,
            b: 0,
            c: 0,
            imm: 0,
        },
    ];
    let infos = vec![
        InstrInfo {
            uses: vec![],
            defs: vec![VReg(0)],
        },
        InstrInfo {
            uses: vec![VReg(0)],
            defs: vec![VReg(1)],
        },
        InstrInfo {
            uses: vec![VReg(1)],
            defs: vec![],
        },
    ];
    let vreg_to_reg = vec![0, 1]; // v0->r0, v1->r1 (spill reload)
    let vreg_to_spill = vec![None, Some(0)]; // v1 spilled
    let last_def_pc = vec![0, 1]; // v0 def at 0, v1 def at 1

    let out = insert_spill_ops(
        &vinsns,
        &infos,
        &vreg_to_reg,
        &vreg_to_spill,
        [1u8, 2u8, 3u8],
        &last_def_pc,
        None,
    );

    // Expect: ConstI32; ToDyn; SpillPush r1; SpillPop r1; Ret r1
    assert!(out.len() >= 5);
    assert_eq!(out[0].op, Op::ConstI32 as u8);
    assert_eq!(out[1].op, Op::ToDyn as u8);
    assert_eq!(out[2].op, Op::SpillPush as u8);
    assert_eq!(out[2].a, 1);
    assert_eq!(out[3].op, Op::SpillPop as u8);
    assert_eq!(out[3].a, 1);
    assert_eq!(out[4].op, Op::Ret as u8);
    assert_eq!(out[4].a, 1);
}

#[test]
fn spill_preserves_const_bool_c_field() {
    // v0 = ConstBool true; (spilled) → expect ConstBool.c == 1 before SpillPush.
    let vinsns = vec![VInsn {
        op: Op::ConstBool as u8,
        a: 0,
        b: 0,
        c: 1,
        imm: 0,
    }];
    let infos = vec![InstrInfo {
        uses: vec![],
        defs: vec![VReg(0)],
    }];
    let vreg_to_reg = vec![5]; // v0 assigned to spill reload reg r5
    let vreg_to_spill = vec![Some(0)];
    let last_def_pc = vec![0];

    let out = insert_spill_ops(
        &vinsns,
        &infos,
        &vreg_to_reg,
        &vreg_to_spill,
        [5u8, 6u8, 7u8],
        &last_def_pc,
        None,
    );
    assert!(out.len() >= 2);
    assert_eq!(out[0].op, Op::ConstBool as u8);
    assert_eq!(out[0].a, 5);
    assert_eq!(out[0].c, 1);
    assert_eq!(out[1].op, Op::SpillPush as u8);
    assert_eq!(out[1].a, 5);
}

#[test]
fn spill_preserves_const_i8_imm_c_field() {
    // v0 = ConstI8Imm 7; (spilled) → expect ConstI8Imm.c == 7 before SpillPush.
    let vinsns = vec![VInsn {
        op: Op::ConstI8Imm as u8,
        a: 0,
        b: 0,
        c: 7,
        imm: 0,
    }];
    let infos = vec![InstrInfo {
        uses: vec![],
        defs: vec![VReg(0)],
    }];
    let vreg_to_reg = vec![9]; // v0 assigned to spill reload reg r9
    let vreg_to_spill = vec![Some(0)];
    let last_def_pc = vec![0];

    let out = insert_spill_ops(
        &vinsns,
        &infos,
        &vreg_to_reg,
        &vreg_to_spill,
        [9u8, 10u8, 11u8],
        &last_def_pc,
        None,
    );
    assert!(out.len() >= 2);
    assert_eq!(out[0].op, Op::ConstI8Imm as u8);
    assert_eq!(out[0].a, 9);
    assert_eq!(out[0].c, 7);
    assert_eq!(out[1].op, Op::SpillPush as u8);
    assert_eq!(out[1].a, 9);
}

#[test]
fn preserves_case_kind_fields_even_with_spill_inserter() {
    let vinsns = vec![VInsn {
        op: Op::CaseKind as u8,
        a: 11,
        b: 0,
        c: 0,
        imm: 123,
    }];
    let infos = vec![InstrInfo {
        uses: vec![],
        defs: vec![],
    }];
    let vreg_to_reg = vec![0u8; 1];
    let vreg_to_spill = vec![None; 1];
    let last_def_pc = vec![0u32; 1];

    let out = insert_spill_ops(
        &vinsns,
        &infos,
        &vreg_to_reg,
        &vreg_to_spill,
        [1u8, 2u8, 3u8],
        &last_def_pc,
        None,
    );
    assert_eq!(out.len(), 1);
    assert_eq!(out[0].op, Op::CaseKind as u8);
    assert_eq!(out[0].a, 11);
    assert_eq!(out[0].imm, 123);
}

#[test]
fn preserves_call_and_closure_immediates() {
    let vinsns = vec![
        VInsn {
            op: Op::Call as u8,
            a: 0,    // dst vreg
            b: 1,    // arg_base vreg
            c: 3,    // nargs (immediate)
            imm: 42, // func index
        },
        VInsn {
            op: Op::Closure as u8,
            a: 0,    // dst vreg
            b: 1,    // cap_base vreg
            c: 7,    // ncaps (immediate)
            imm: 99, // func index
        },
    ];
    let infos = vec![
        InstrInfo {
            uses: vec![],
            defs: vec![],
        },
        InstrInfo {
            uses: vec![],
            defs: vec![],
        },
    ];
    let vreg_to_reg = vec![10u8, 20u8];
    let vreg_to_spill = vec![None, None];
    let last_def_pc = vec![0u32, 0u32];

    let out = insert_spill_ops(
        &vinsns,
        &infos,
        &vreg_to_reg,
        &vreg_to_spill,
        [1u8, 2u8, 3u8],
        &last_def_pc,
        None,
    );
    assert_eq!(out.len(), 2);

    assert_eq!(out[0].op, Op::Call as u8);
    assert_eq!(out[0].a, 10);
    assert_eq!(out[0].b, 20);
    assert_eq!(out[0].c, 3);
    assert_eq!(out[0].imm, 42);

    assert_eq!(out[1].op, Op::Closure as u8);
    assert_eq!(out[1].a, 10);
    assert_eq!(out[1].b, 20);
    assert_eq!(out[1].c, 7);
    assert_eq!(out[1].imm, 99);
}

#[test]
fn supports_three_spilled_operands_in_one_insn() {
    // ObjSet uses 3 operands (value, obj, atom). If all are spilled, we must be able to
    // pop three values into three distinct reload regs and map the insn to those regs.
    let vinsns = vec![VInsn {
        op: Op::ObjSet as u8,
        a: 0,
        b: 1,
        c: 2,
        imm: 0,
    }];
    let infos = vec![InstrInfo {
        uses: vec![VReg(0), VReg(1), VReg(2)],
        defs: vec![],
    }];
    let vreg_to_reg = vec![99u8, 99u8, 99u8]; // placeholder; spilled mapping overrides
    let vreg_to_spill = vec![Some(0), Some(1), Some(2)];
    let last_def_pc = vec![0u32, 1u32, 2u32];

    let out = insert_spill_ops(
        &vinsns,
        &infos,
        &vreg_to_reg,
        &vreg_to_spill,
        [10u8, 11u8, 12u8],
        &last_def_pc,
        None,
    );

    assert!(out.len() >= 4);
    assert_eq!(out[0].op, Op::SpillPop as u8);
    assert_eq!(out[1].op, Op::SpillPop as u8);
    assert_eq!(out[2].op, Op::SpillPop as u8);
    // v2 has the latest def → popped first into r10; then v1→r11; then v0→r12.
    assert_eq!(out[0].a, 10);
    assert_eq!(out[1].a, 11);
    assert_eq!(out[2].a, 12);

    // Mapped instruction must use the three reload regs.
    assert_eq!(out[3].op, Op::ObjSet as u8);
    // ObjSet encodes value in `a` (v0), obj in `b` (v1), atom in `c` (v2).
    assert_eq!(out[3].a, 12);
    assert_eq!(out[3].b, 11);
    assert_eq!(out[3].c, 10);
}
