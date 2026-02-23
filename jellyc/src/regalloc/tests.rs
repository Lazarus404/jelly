use super::*;

fn ins(uses: &[u32], defs: &[u32]) -> InstrInfo {
    InstrInfo {
        uses: uses.iter().map(|&x| VReg(x)).collect(),
        defs: defs.iter().map(|&x| VReg(x)).collect(),
    }
}

#[test]
fn alloc_reuses_registers() {
    let instrs = vec![
        ins(&[], &[0]),
        ins(&[0], &[]),
        ins(&[], &[1]),
        ins(&[1], &[]),
    ];
    let spillable = vec![false, false];
    let a = lsra_allocate(1, 2, &instrs, &spillable, SpillPolicy::Forbid, None).unwrap();
    assert_eq!(a.used_pregs, 1);
    assert_eq!(a.vreg_to_preg[0], Some(PReg(0)));
    assert_eq!(a.vreg_to_preg[1], Some(PReg(0)));
}

#[test]
fn detects_multiple_defs() {
    let instrs = vec![ins(&[], &[0]), ins(&[], &[0])];
    let spillable = vec![false];
    let e = lsra_allocate(2, 1, &instrs, &spillable, SpillPolicy::Forbid, None).unwrap_err();
    assert!(matches!(e, AllocError::MultipleDefs { .. }));
}

#[test]
fn allows_multiple_defs_when_opted_in() {
    let instrs = vec![
        ins(&[], &[0]),
        ins(&[], &[]),
        ins(&[], &[0]),
        ins(&[0], &[]),
    ];
    let spillable = vec![false];
    let allow = vec![true];
    let a = lsra_allocate(2, 1, &instrs, &spillable, SpillPolicy::Forbid, Some(&allow)).unwrap();
    assert_eq!(a.used_pregs, 1);
    assert_eq!(a.vreg_to_preg[0], Some(PReg(0)));
}

#[test]
fn spills_when_allowed() {
    let instrs = vec![ins(&[], &[0]), ins(&[], &[1]), ins(&[0, 1], &[])];
    let spillable = vec![true, true];
    let a = lsra_allocate(1, 2, &instrs, &spillable, SpillPolicy::Allow, None).unwrap();
    assert_eq!(a.used_pregs, 1);
    assert_eq!(a.used_spill_slots, 1);
    assert!(a.vreg_to_preg.iter().flatten().count() == 1);
    assert!(a.vreg_to_spill.iter().flatten().count() == 1);
}

#[test]
fn forbid_spill_errors() {
    let instrs = vec![ins(&[], &[0]), ins(&[], &[1]), ins(&[0, 1], &[])];
    let spillable = vec![true, true];
    let e = lsra_allocate(1, 2, &instrs, &spillable, SpillPolicy::Forbid, None).unwrap_err();
    assert!(matches!(e, AllocError::OutOfPhysicalRegsNoSpill));
}

#[test]
fn parallel_copy_swap_uses_temp() {
    let a = PReg(0);
    let b = PReg(1);
    let t = PReg(2);
    let seq = schedule_parallel_moves(&[(a, b), (b, a)], t);
    let mut regs = [10i32, 20i32, 30i32];
    for (d, s) in seq {
        regs[d.0 as usize] = regs[s.0 as usize];
    }
    assert_eq!(regs[a.0 as usize], 20);
    assert_eq!(regs[b.0 as usize], 10);
}

#[test]
fn parallel_copy_three_cycle() {
    let a = PReg(0);
    let b = PReg(1);
    let c = PReg(2);
    let t = PReg(3);
    let seq = schedule_parallel_moves(&[(a, b), (b, c), (c, a)], t);
    let mut regs = [10i32, 20i32, 30i32, 40i32];
    for (d, s) in seq {
        regs[d.0 as usize] = regs[s.0 as usize];
    }
    assert_eq!(regs[a.0 as usize], 20);
    assert_eq!(regs[b.0 as usize], 30);
    assert_eq!(regs[c.0 as usize], 10);
}
