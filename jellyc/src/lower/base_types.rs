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

use crate::ir::TypeId;

// Base program-module type IDs. Must match `TypeCtx::new_program_base()`.
pub(crate) const T_BOOL: TypeId = crate::typectx::T_BOOL; // 0
pub(crate) const T_ATOM: TypeId = crate::typectx::T_ATOM; // 1
pub(crate) const T_I8: TypeId = crate::typectx::T_I8; // 2
pub(crate) const T_I16: TypeId = crate::typectx::T_I16; // 3
pub(crate) const T_I32: TypeId = crate::typectx::T_I32; // 4
pub(crate) const T_I64: TypeId = crate::typectx::T_I64; // 5
pub(crate) const T_F16: TypeId = crate::typectx::T_F16; // 6
pub(crate) const T_F32: TypeId = crate::typectx::T_F32; // 7
pub(crate) const T_F64: TypeId = crate::typectx::T_F64; // 8
pub(crate) const T_BYTES: TypeId = crate::typectx::T_BYTES; // 9
pub(crate) const T_DYNAMIC: TypeId = crate::typectx::T_DYNAMIC; // 10
pub(crate) const T_OBJECT: TypeId = crate::typectx::T_OBJECT; // 11
pub(crate) const T_ARRAY_I32: TypeId = crate::typectx::T_ARRAY_I32; // 12
pub(crate) const T_ARRAY_BYTES: TypeId = crate::typectx::T_ARRAY_BYTES; // 13
pub(crate) const T_LIST_I32: TypeId = crate::typectx::T_LIST_I32; // 14
pub(crate) const T_LIST_BYTES: TypeId = crate::typectx::T_LIST_BYTES; // 15
