/*
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

use super::{live, AllocError, Allocation, InstrInfo, PReg, SpillPolicy, VReg};

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

    let used_pregs = vreg_to_preg
        .iter()
        .flatten()
        .map(|p| p.0)
        .max()
        .map(|x| x + 1)
        .unwrap_or(0);
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

#[allow(dead_code)]
fn _keep_imports_used() {
    // Ensure `VReg` stays referenced from this file (helps when refactoring).
    let _ = VReg(0);
}
