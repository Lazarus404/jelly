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

use std::collections::HashMap;

use super::Module;

const MOD_ABI_MAGIC: &[u8] = b"JLYMODABI1\0";

#[derive(Clone, Debug)]
pub struct ModuleAbi {
    pub exports: HashMap<String, u32>,
    pub imports: Vec<String>,
}

pub fn extract_module_abi(m: &Module) -> Option<ModuleAbi> {
    fn rd_u32le(b: &[u8], i: &mut usize) -> Option<u32> {
        if *i + 4 > b.len() {
            return None;
        }
        let v = u32::from_le_bytes([b[*i], b[*i + 1], b[*i + 2], b[*i + 3]]);
        *i += 4;
        Some(v)
    }

    for blob in &m.const_bytes {
        if !blob.starts_with(MOD_ABI_MAGIC) {
            continue;
        }
        let mut i = MOD_ABI_MAGIC.len();
        let n = match rd_u32le(blob, &mut i) {
            Some(x) => x as usize,
            None => continue,
        };
        let mut exports: HashMap<String, u32> = HashMap::new();
        for _ in 0..n {
            let len = match rd_u32le(blob, &mut i) {
                Some(x) => x as usize,
                None => return None,
            };
            if i + len > blob.len() {
                return None;
            }
            let name_bytes = &blob[i..i + len];
            i += len;
            let tid = match rd_u32le(blob, &mut i) {
                Some(x) => x,
                None => return None,
            };
            if let Ok(s) = std::str::from_utf8(name_bytes) {
                exports.insert(s.to_string(), tid);
            }
        }

        let nimports = match rd_u32le(blob, &mut i) {
            Some(x) => x as usize,
            None => return None,
        };
        let mut imports: Vec<String> = Vec::with_capacity(nimports);
        for _ in 0..nimports {
            let len = match rd_u32le(blob, &mut i) {
                Some(x) => x as usize,
                None => return None,
            };
            if i + len > blob.len() {
                return None;
            }
            let b = &blob[i..i + len];
            i += len;
            let s = std::str::from_utf8(b).ok()?.to_string();
            imports.push(s);
        }

        return Some(ModuleAbi { exports, imports });
    }
    None
}
