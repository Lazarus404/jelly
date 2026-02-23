use super::*;

fn features_word(bytes: &[u8]) -> u32 {
    // header: magic(u32), version(u32), features(u32)
    let w = &bytes[8..12];
    u32::from_le_bytes([w[0], w[1], w[2], w[3]])
}

#[test]
fn cap_start_feature_set_for_closure_with_captures_even_when_cap_start_is_zero() {
    let m = Module {
        types: vec![],
        sigs: vec![],
        atoms: vec![],
        const_i64: vec![],
        const_f64: vec![],
        const_bytes: vec![],
        funcs: vec![Function {
            reg_types: vec![],
            cap_start: 0,
            insns: vec![Insn {
                op: Op::Closure as u8,
                a: 0,
                b: 0,
                c: 1, // ncaps > 0
                imm: 0,
            }],
        }],
        entry: 0,
        prelude_count: 0,
        used_prelude: vec![],
    };

    let mut out: Vec<u8> = Vec::new();
    m.write_to(&mut out).unwrap();
    let features = features_word(&out);
    assert_ne!(
        features & (1u32 << 2),
        0,
        "expected CAP_START feature bit set"
    );
}

#[test]
fn cap_start_feature_unset_when_no_captures_and_all_cap_start_zero() {
    let m = Module {
        types: vec![],
        sigs: vec![],
        atoms: vec![],
        const_i64: vec![],
        const_f64: vec![],
        const_bytes: vec![],
        funcs: vec![Function {
            reg_types: vec![],
            cap_start: 0,
            insns: vec![Insn {
                op: Op::Nop as u8,
                a: 0,
                b: 0,
                c: 0,
                imm: 0,
            }],
        }],
        entry: 0,
        prelude_count: 0,
        used_prelude: vec![],
    };

    let mut out: Vec<u8> = Vec::new();
    m.write_to(&mut out).unwrap();
    let features = features_word(&out);
    assert_eq!(
        features & (1u32 << 2),
        0,
        "expected CAP_START feature bit unset"
    );
}
