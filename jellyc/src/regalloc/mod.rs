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

 mod live;
pub mod spill;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VReg(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    UsedButNeverDefined { vreg: VReg },
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
    use std::collections::{BTreeMap, BTreeSet};

    // Use deterministic structures: the result should not depend on hash iteration order.
    let mut map: BTreeMap<PReg, PReg> = BTreeMap::new();
    for &(d, s) in moves {
        if d != s {
            map.insert(d, s);
        }
    }

    let mut out: Vec<(PReg, PReg)> = Vec::new();

    while !map.is_empty() {
        // Drain all moves whose sources won't be clobbered by remaining destinations.
        loop {
            let dsts: BTreeSet<PReg> = map.keys().copied().collect();
            let ready = map.iter().find_map(|(&d, &s)| if !dsts.contains(&s) { Some((d, s)) } else { None });
            if let Some((d, s)) = ready {
                out.push((d, s));
                map.remove(&d);
            } else {
                break;
            }
        }

        if map.is_empty() {
            break;
        }

        // Break a cycle deterministically (pick smallest dst).
        let start = *map.keys().next().expect("non-empty");
        assert!(start != temp);

        out.push((temp, start));

        let mut d = start;
        loop {
            let s = *map.get(&d).expect("cycle must have mapping");
            map.remove(&d);
            if s == start {
                out.push((d, temp));
                break;
            } else {
                out.push((d, s));
                d = s;
            }
        }
    }

    out
}

#[derive(Clone, Debug)]
pub struct Allocation {
    pub vreg_to_preg: Vec<Option<PReg>>,
    pub vreg_to_spill: Vec<Option<u32>>,
    pub used_pregs: u16,
    pub used_spill_slots: u32,
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

    let intervals = live::build_intervals(num_vregs, instrs, allow_multi_def)?;
    let mut ivs = intervals;
    ivs.sort_by_key(|iv| (iv.start, iv.end, iv.vreg.0));

    let mut free: Vec<PReg> = (0..num_pregs).rev().map(PReg).collect();
    let mut active: Vec<(live::LiveInterval, PReg)> = Vec::new();

    let mut vreg_to_preg: Vec<Option<PReg>> = vec![None; num_vregs as usize];
    let mut vreg_to_spill: Vec<Option<u32>> = vec![None; num_vregs as usize];
    let mut spill_slots: u32 = 0;

    for iv in ivs.iter().copied() {
        expire_old(&mut active, &mut free, iv.start);

        if let Some(p) = free.pop() {
            vreg_to_preg[iv.vreg.0 as usize] = Some(p);
            active.push((iv, p));
            active.sort_by_key(|(a, _)| (a.end, a.start, a.vreg.0));
            continue;
        }

        if spill_policy == SpillPolicy::Forbid {
            return Err(AllocError::OutOfPhysicalRegsNoSpill);
        }

        let (spill_i, (spill_iv, spill_p)) = active
            .iter()
            .enumerate()
            .max_by_key(|(_, (a, _))| a.end)
            .ok_or(AllocError::IntervalInternalBug)?
            .to_owned();

        if spill_iv.end > iv.end {
            if !spillable[spill_iv.vreg.0 as usize] {
                return Err(AllocError::OutOfPhysicalRegsNoSpill);
            }
            vreg_to_preg[spill_iv.vreg.0 as usize] = None;
            vreg_to_spill[spill_iv.vreg.0 as usize] = Some(spill_slots);
            spill_slots += 1;

            vreg_to_preg[iv.vreg.0 as usize] = Some(*spill_p);
            active[spill_i] = (iv, *spill_p);
            active.sort_by_key(|(a, _)| (a.end, a.start, a.vreg.0));
        } else {
            if !spillable[iv.vreg.0 as usize] {
                return Err(AllocError::OutOfPhysicalRegsNoSpill);
            }
            vreg_to_spill[iv.vreg.0 as usize] = Some(spill_slots);
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

fn expire_old(active: &mut Vec<(live::LiveInterval, PReg)>, free: &mut Vec<PReg>, start: u32) {
    let mut i = 0;
    while i < active.len() {
        if active[i].0.end < start {
            let (_, p) = active.remove(i);
            free.push(p);
        } else {
            i += 1;
        }
    }
    free.sort_by_key(|p| std::cmp::Reverse(p.0));
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
        let instrs = vec![
            ins(&[], &[0]),
            ins(&[], &[1]),
            ins(&[0, 1], &[]),
        ];
        let spillable = vec![true, true];
        let a = lsra_allocate(1, 2, &instrs, &spillable, SpillPolicy::Allow, None).unwrap();
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
}
