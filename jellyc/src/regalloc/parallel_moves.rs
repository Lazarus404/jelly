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

use super::PReg;

// Schedule a set of moves that are intended to happen "in parallel" (phi-elim copies).
//
// Each move is a pair (dst <- src). The returned sequence can be executed in order
// without clobbering sources, using `temp` to break cycles.
//
// The caller must ensure `temp` is not used as a destination of any move.
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
            let ready = map.iter().find_map(|(&d, &s)| {
                if !dsts.contains(&s) {
                    Some((d, s))
                } else {
                    None
                }
            });
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
