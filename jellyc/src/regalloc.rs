#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VReg(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PReg(pub u16);

#[derive(Clone, Debug)]
pub struct InstrInfo {
    pub uses: Vec<VReg>,
    pub defs: Vec<VReg>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpillPolicy {
    Forbid,
    Allow,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AllocError {
    TooManyPhysicalRegs,
    VRegOutOfRange { vreg: VReg, vregs: u32 },
    MultipleDefs { vreg: VReg },
    UseBeforeDef { vreg: VReg },
    OutOfPhysicalRegsNoSpill,
    IntervalInternalBug,
}

/// Schedule a set of moves that are intended to happen "in parallel" (phi-elim copies).
///
/// Each move is a pair (dst <- src). The returned sequence can be executed in order
/// without clobbering sources, using `temp` to break cycles.
///
/// The caller must ensure `temp` is not used as a destination of any move.
pub fn schedule_parallel_moves(moves: &[(PReg, PReg)], temp: PReg) -> Vec<(PReg, PReg)> {
    // Filter no-ops and build a dst->src map. If multiple moves target same dst,
    // last one wins (caller should avoid this; it's not meaningful for parallel copy).
    use std::collections::{HashMap, HashSet};

    let mut map: HashMap<PReg, PReg> = HashMap::new();
    for &(d, s) in moves {
        if d != s {
            map.insert(d, s);
        }
    }

    let mut out: Vec<(PReg, PReg)> = Vec::new();

    // Helper: compute current destination set.
    let mut dsts: HashSet<PReg> = map.keys().copied().collect();

    // Emit acyclic moves first: any move whose src is not a dst of another move.
    loop {
        let mut progressed = false;

        // Find any dst with a "ready" src.
        let ready: Option<(PReg, PReg)> = map
            .iter()
            .find_map(|(&d, &s)| if !dsts.contains(&s) { Some((d, s)) } else { None });

        if let Some((d, s)) = ready {
            out.push((d, s));
            map.remove(&d);
            progressed = true;
        }

        if !progressed {
            break;
        }

        dsts = map.keys().copied().collect();
        // (srcs not needed here)
    }

    // Remaining moves, if any, are cycles. Break each cycle using temp.
    while !map.is_empty() {
        dsts = map.keys().copied().collect();

        // Pick an arbitrary cycle start dst.
        let start = *dsts.iter().next().expect("non-empty");
        assert!(start != temp);

        // Save the old value in start.
        out.push((temp, start));

        // Rotate the cycle: repeatedly assign dst <- src, walking dst = src.
        let mut d = start;
        loop {
            let s = *map.get(&d).expect("cycle must have mapping");
            map.remove(&d);
            if s == start {
                // Close the cycle: d gets temp.
                out.push((d, temp));
                break;
            } else {
                out.push((d, s));
                d = s;
            }
        }

        // After removing a cycle, there may now be newly acyclic moves; drain them.
        dsts = map.keys().copied().collect();
        loop {
            let ready: Option<(PReg, PReg)> = map
                .iter()
                .find_map(|(&d, &s)| if !dsts.contains(&s) { Some((d, s)) } else { None });
            if let Some((d, s)) = ready {
                out.push((d, s));
                map.remove(&d);
                dsts = map.keys().copied().collect();
            } else {
                break;
            }
        }
    }

    out
}

#[derive(Clone, Debug)]
pub struct Allocation {
    /// For each vreg: assigned physical register (if not spilled).
    pub vreg_to_preg: Vec<Option<PReg>>,
    /// For each vreg: spill slot index (if spilled).
    pub vreg_to_spill: Vec<Option<u32>>,
    pub used_pregs: u16,
    pub used_spill_slots: u32,
}

#[derive(Clone, Copy, Debug)]
struct Interval {
    v: VReg,
    start: u32,
    end: u32, // inclusive
}

pub fn lsra_allocate(
    num_pregs: u16,
    num_vregs: u32,
    instrs: &[InstrInfo],
    spillable: &[bool],
    spill_policy: SpillPolicy,
    allow_multi_def: Option<&[bool]>,
) -> Result<Allocation, AllocError> {
    if num_pregs == 0 || num_pregs > 256 {
        return Err(AllocError::TooManyPhysicalRegs);
    }
    if spillable.len() != num_vregs as usize {
        return Err(AllocError::IntervalInternalBug);
    }

    let intervals = build_intervals(num_vregs, instrs, allow_multi_def)?;
    let mut ivs = intervals;
    ivs.sort_by_key(|iv| (iv.start, iv.end, iv.v.0));

    let mut free: Vec<PReg> = (0..num_pregs).rev().map(|r| PReg(r)).collect();
    let mut active: Vec<(Interval, PReg)> = Vec::new();

    let mut vreg_to_preg: Vec<Option<PReg>> = vec![None; num_vregs as usize];
    let mut vreg_to_spill: Vec<Option<u32>> = vec![None; num_vregs as usize];
    let mut spill_slots: u32 = 0;

    for iv in ivs.iter().copied() {
        expire_old(&mut active, &mut free, iv.start);

        if let Some(p) = free.pop() {
            vreg_to_preg[iv.v.0 as usize] = Some(p);
            active.push((iv, p));
            active.sort_by_key(|(a, _)| (a.end, a.start, a.v.0));
            continue;
        }

        // Need to spill someone (if allowed).
        if spill_policy == SpillPolicy::Forbid {
            return Err(AllocError::OutOfPhysicalRegsNoSpill);
        }

        // Pick interval with farthest end to spill.
        let (spill_i, (spill_iv, spill_p)) = active
            .iter()
            .enumerate()
            .max_by_key(|(_, (a, _))| a.end)
            .ok_or(AllocError::IntervalInternalBug)?
            .to_owned();

        if spill_iv.end > iv.end {
            // Spill the active one, give its preg to the new interval.
            if !spillable[spill_iv.v.0 as usize] {
                return Err(AllocError::OutOfPhysicalRegsNoSpill);
            }
            vreg_to_preg[spill_iv.v.0 as usize] = None;
            vreg_to_spill[spill_iv.v.0 as usize] = Some(spill_slots);
            spill_slots += 1;

            vreg_to_preg[iv.v.0 as usize] = Some(*spill_p);
            active[spill_i] = (iv, *spill_p);
            active.sort_by_key(|(a, _)| (a.end, a.start, a.v.0));
        } else {
            // Spill the new interval.
            if !spillable[iv.v.0 as usize] {
                return Err(AllocError::OutOfPhysicalRegsNoSpill);
            }
            vreg_to_spill[iv.v.0 as usize] = Some(spill_slots);
            spill_slots += 1;
        }
    }

    let used_pregs = vreg_to_preg.iter().flatten().map(|p| p.0).max().map(|x| x + 1).unwrap_or(0);
    Ok(Allocation {
        vreg_to_preg,
        vreg_to_spill,
        used_pregs,
        used_spill_slots: spill_slots,
    })
}

fn expire_old(active: &mut Vec<(Interval, PReg)>, free: &mut Vec<PReg>, start: u32) {
    // Intervals are inclusive, so expire those ending before start.
    let mut i = 0;
    while i < active.len() {
        if active[i].0.end < start {
            let (_, p) = active.remove(i);
            free.push(p);
        } else {
            i += 1;
        }
    }
    // Keep free deterministic: stack behavior is fine but sort for stability.
    // Keep it so `pop()` returns the lowest-numbered register.
    free.sort_by_key(|p| std::cmp::Reverse(p.0));
}

fn build_intervals(
    num_vregs: u32,
    instrs: &[InstrInfo],
    allow_multi_def: Option<&[bool]>,
) -> Result<Vec<Interval>, AllocError> {
    if let Some(a) = allow_multi_def {
        if a.len() != num_vregs as usize {
            return Err(AllocError::IntervalInternalBug);
        }
    }
    let mut def_pos: Vec<Option<u32>> = vec![None; num_vregs as usize];
    let mut end_pos: Vec<Option<u32>> = vec![None; num_vregs as usize];

    for (pc, ins) in instrs.iter().enumerate() {
        let pos = pc as u32;
        for &d in &ins.defs {
            if d.0 >= num_vregs {
                return Err(AllocError::VRegOutOfRange { vreg: d, vregs: num_vregs });
            }
            if def_pos[d.0 as usize].is_some() {
                let allow = allow_multi_def.map(|a| a[d.0 as usize]).unwrap_or(false);
                if !allow {
                    return Err(AllocError::MultipleDefs { vreg: d });
                }
            }
            if def_pos[d.0 as usize].is_none() {
                def_pos[d.0 as usize] = Some(pos);
            }
            // A def is also a live position (even if never used).
            end_pos[d.0 as usize] = Some(end_pos[d.0 as usize].unwrap_or(pos).max(pos));
        }
        for &u in &ins.uses {
            if u.0 >= num_vregs {
                return Err(AllocError::VRegOutOfRange { vreg: u, vregs: num_vregs });
            }
            let dpos = def_pos[u.0 as usize].ok_or(AllocError::UseBeforeDef { vreg: u })?;
            let _ = dpos;
            let cur = end_pos[u.0 as usize].unwrap_or(pos);
            end_pos[u.0 as usize] = Some(cur.max(pos));
        }
    }

    let mut out = Vec::new();
    for v in 0..num_vregs {
        if let Some(s) = def_pos[v as usize] {
            let e = end_pos[v as usize].ok_or(AllocError::IntervalInternalBug)?;
            out.push(Interval { v: VReg(v), start: s, end: e });
        }
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ins(uses: &[u32], defs: &[u32]) -> InstrInfo {
        InstrInfo {
            uses: uses.iter().map(|&x| VReg(x)).collect(),
            defs: defs.iter().map(|&x| VReg(x)).collect(),
        }
    }

    #[test]
    fn alloc_reuses_registers() {
        // v0 defined, used, then dead; v1 defined after, can reuse same preg.
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
        // v0 is assigned in two mutually-exclusive branches; allow multi-def for it.
        let instrs = vec![
            ins(&[], &[0]),      // v0 = ...
            ins(&[], &[]),       // jmp ...
            ins(&[], &[0]),      // v0 = ... (other branch)
            ins(&[0], &[]),      // use v0 at join
        ];
        let spillable = vec![false];
        let allow = vec![true];
        let a = lsra_allocate(2, 1, &instrs, &spillable, SpillPolicy::Forbid, Some(&allow)).unwrap();
        assert_eq!(a.used_pregs, 1);
        assert_eq!(a.vreg_to_preg[0], Some(PReg(0)));
    }

    #[test]
    fn spills_when_allowed() {
        let instrs = vec![
            ins(&[], &[0]),
            ins(&[], &[1]),
            ins(&[0, 1], &[]),
        ];
        let spillable = vec![true, true];
        let a = lsra_allocate(1, 2, &instrs, &spillable, SpillPolicy::Allow, None).unwrap();
        // One in reg, one spilled.
        assert_eq!(a.used_pregs, 1);
        assert_eq!(a.used_spill_slots, 1);
        assert!(a.vreg_to_preg.iter().flatten().count() == 1);
        assert!(a.vreg_to_spill.iter().flatten().count() == 1);
    }

    #[test]
    fn forbid_spill_errors() {
        let instrs = vec![
            ins(&[], &[0]),
            ins(&[], &[1]),
            ins(&[0, 1], &[]),
        ];
        let spillable = vec![true, true];
        let e = lsra_allocate(1, 2, &instrs, &spillable, SpillPolicy::Forbid, None).unwrap_err();
        assert!(matches!(e, AllocError::OutOfPhysicalRegsNoSpill));
    }

    #[test]
    fn parallel_copy_swap_uses_temp() {
        // Parallel: a=b, b=a
        let a = PReg(0);
        let b = PReg(1);
        let t = PReg(2);
        let seq = schedule_parallel_moves(&[(a, b), (b, a)], t);
        // Validate semantics (not a specific schedule).
        let mut regs = [10i32, 20i32, 30i32];
        for (d, s) in seq {
            regs[d.0 as usize] = regs[s.0 as usize];
        }
        // Parallel result: a=old b, b=old a (temp can be clobbered).
        assert_eq!(regs[a.0 as usize], 20);
        assert_eq!(regs[b.0 as usize], 10);
    }

    #[test]
    fn parallel_copy_three_cycle() {
        // Parallel: a=b, b=c, c=a
        let a = PReg(0);
        let b = PReg(1);
        let c = PReg(2);
        let t = PReg(3);
        let seq = schedule_parallel_moves(&[(a, b), (b, c), (c, a)], t);
        // Validate semantics (not a specific schedule).
        let mut regs = [10i32, 20i32, 30i32, 40i32];
        for (d, s) in seq {
            regs[d.0 as usize] = regs[s.0 as usize];
        }
        // Parallel result: a=old b, b=old c, c=old a (temp can be clobbered).
        assert_eq!(regs[a.0 as usize], 20);
        assert_eq!(regs[b.0 as usize], 30);
        assert_eq!(regs[c.0 as usize], 10);
    }
}

