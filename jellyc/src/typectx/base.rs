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
use std::collections::HashMap;

use crate::jlyb::{TypeEntry, TypeKind};

use super::TypeCtx;

// Base program-module type IDs (type table indices).
// Must match `TypeCtx::new_program_base()` and is shared across compiler pipelines.
pub const T_BOOL: u32 = 0;
pub const T_ATOM: u32 = 1;
pub const T_I8: u32 = 2;
pub const T_I16: u32 = 3;
pub const T_I32: u32 = 4;
pub const T_I64: u32 = 5;
pub const T_F16: u32 = 6;
pub const T_F32: u32 = 7;
pub const T_F64: u32 = 8;
pub const T_BYTES: u32 = 9;
pub const T_DYNAMIC: u32 = 10;
pub const T_OBJECT: u32 = 11;
pub const T_ARRAY_I32: u32 = 12;
pub const T_ARRAY_BYTES: u32 = 13;
pub const T_LIST_I32: u32 = 14;
pub const T_LIST_BYTES: u32 = 15;

impl TypeCtx {
    pub fn new_program_base() -> Self {
        // Must match the program-module base table in `jlyb.rs`.
        let types = vec![
            TypeEntry {
                kind: TypeKind::Bool,
                p0: 0,
            }, // 0
            TypeEntry {
                kind: TypeKind::Atom,
                p0: 0,
            }, // 1
            TypeEntry {
                kind: TypeKind::I8,
                p0: 0,
            }, // 2
            TypeEntry {
                kind: TypeKind::I16,
                p0: 0,
            }, // 3
            TypeEntry {
                kind: TypeKind::I32,
                p0: 0,
            }, // 4
            TypeEntry {
                kind: TypeKind::I64,
                p0: 0,
            }, // 5
            TypeEntry {
                kind: TypeKind::F16,
                p0: 0,
            }, // 6
            TypeEntry {
                kind: TypeKind::F32,
                p0: 0,
            }, // 7
            TypeEntry {
                kind: TypeKind::F64,
                p0: 0,
            }, // 8
            TypeEntry {
                kind: TypeKind::Bytes,
                p0: 0,
            }, // 9
            TypeEntry {
                kind: TypeKind::Dynamic,
                p0: 0,
            }, // 10
            TypeEntry {
                kind: TypeKind::Object,
                p0: 0,
            }, // 11
            TypeEntry {
                kind: TypeKind::Array,
                p0: T_I32,
            }, // 12: Array<i32>
            TypeEntry {
                kind: TypeKind::Array,
                p0: T_BYTES,
            }, // 13: Array<bytes>
            TypeEntry {
                kind: TypeKind::List,
                p0: T_I32,
            }, // 14: List<i32>
            TypeEntry {
                kind: TypeKind::List,
                p0: T_BYTES,
            }, // 15: List<bytes>
        ];
        Self {
            types,
            sigs: Vec::new(),
            sig_key_to_id: HashMap::new(),
            fun_sig_to_type: HashMap::new(),
            tuple_sig_to_type: HashMap::new(),
            nominal_obj_key_to_tid: HashMap::new(),
        }
    }
}
