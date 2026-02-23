use super::*;

#[test]
fn folds_const_i32_lt_to_imm_until_overwrite() {
    // Pattern like fib:
    //   r2 = 2
    //   r1 = (r0 < r2)
    //   ...
    //   r2 = <something else>   // overwrite -> later uses of r2 should not block folding
    let insns = vec![
        Insn {
            op: OP_CONST_I32,
            a: 2,
            b: 0,
            c: 0,
            imm: 2,
        },
        Insn {
            op: OP_LT_I32,
            a: 1,
            b: 0,
            c: 2,
            imm: 0,
        },
        Insn {
            op: OP_JMPIF,
            a: 1,
            b: 0,
            c: 0,
            imm: 1,
        },
        Insn {
            op: OP_JMP,
            a: 0,
            b: 0,
            c: 0,
            imm: 1,
        },
        Insn {
            op: OP_RET,
            a: 0,
            b: 0,
            c: 0,
            imm: 0,
        },
        // overwrite r2
        Insn {
            op: OP_CONST_I32,
            a: 2,
            b: 0,
            c: 0,
            imm: 123,
        },
        // later "use" of r2 after overwrite should not block folding
        Insn {
            op: OP_ADD_I32,
            a: 4,
            b: 2,
            c: 0,
            imm: 0,
        },
    ];
    let out = peephole(&insns);
    assert!(out.len() < insns.len());
    assert_eq!(out[0].op, OP_LT_I32_IMM);
    assert_eq!(out[0].a, 1);
    assert_eq!(out[0].b, 0);
    assert_eq!(out[0].c, 2);
}
