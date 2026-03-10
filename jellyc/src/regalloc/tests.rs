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
