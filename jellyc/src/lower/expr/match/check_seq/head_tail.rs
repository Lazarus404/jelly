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

use crate::ast::{Pattern, Span};
use crate::error::{CompileError, ErrorKind};
use crate::ir::{BlockId, IrBuilder, TypeId, VRegId};
use crate::lower::LowerCtx;

use super::super::util::{elem_tid_for_array, elem_tid_for_list};

mod array;
mod list;

pub(super) fn lower_check_array_head_tail(
    i: usize,
    pat_span: Span,
    head: &Pattern,
    ctx: &mut LowerCtx,
    b: &mut IrBuilder,
    v_subj: VRegId,
    t_subj: TypeId,
    bind_b: BlockId,
    next_check: BlockId,
) -> Result<(), CompileError> {
    let elem_tid = elem_tid_for_array(t_subj)
        .or_else(|| elem_tid_for_list(t_subj))
        .ok_or_else(|| {
            CompileError::new(
            ErrorKind::Type,
            pat_span,
            "array/list pattern requires Array<i32>/Array<bytes> or List<i32>/List<bytes> subject",
        )
        })?;

    if elem_tid_for_list(t_subj).is_some() {
        list::lower_check_array_head_tail_list(
            i, pat_span, head, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    } else {
        array::lower_check_array_head_tail_array(
            i, pat_span, head, ctx, b, v_subj, t_subj, elem_tid, bind_b, next_check,
        )
    }
}
