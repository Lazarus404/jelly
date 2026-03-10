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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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
use crate::ast::{Expr, MatchArm, Pattern, Program, Stmt, Ty};

mod walk;

pub use walk::{
    walk_expr, walk_expr_mut, walk_match_arm, walk_match_arm_mut, walk_pattern, walk_pattern_mut,
    walk_program, walk_program_mut, walk_stmt, walk_stmt_mut, walk_ty, walk_ty_mut,
};

/// A fallible, read-only visitor over the AST.
pub trait Visitor {
    type Err;

    fn visit_program(&mut self, p: &Program) -> Result<(), Self::Err> {
        walk_program(self, p)
    }
    fn visit_stmt(&mut self, s: &Stmt) -> Result<(), Self::Err> {
        walk_stmt(self, s)
    }
    fn visit_expr(&mut self, e: &Expr) -> Result<(), Self::Err> {
        walk_expr(self, e)
    }
    fn visit_match_arm(&mut self, a: &MatchArm) -> Result<(), Self::Err> {
        walk_match_arm(self, a)
    }
    fn visit_pattern(&mut self, p: &Pattern) -> Result<(), Self::Err> {
        walk_pattern(self, p)
    }
    fn visit_ty(&mut self, t: &Ty) -> Result<(), Self::Err> {
        walk_ty(self, t)
    }
}

/// A fallible, mutable visitor over the AST.
pub trait VisitorMut {
    type Err;

    fn visit_program(&mut self, p: &mut Program) -> Result<(), Self::Err> {
        walk_program_mut(self, p)
    }
    fn visit_stmt(&mut self, s: &mut Stmt) -> Result<(), Self::Err> {
        walk_stmt_mut(self, s)
    }
    fn visit_expr(&mut self, e: &mut Expr) -> Result<(), Self::Err> {
        walk_expr_mut(self, e)
    }
    fn visit_match_arm(&mut self, a: &mut MatchArm) -> Result<(), Self::Err> {
        walk_match_arm_mut(self, a)
    }
    fn visit_pattern(&mut self, p: &mut Pattern) -> Result<(), Self::Err> {
        walk_pattern_mut(self, p)
    }
    fn visit_ty(&mut self, t: &mut Ty) -> Result<(), Self::Err> {
        walk_ty_mut(self, t)
    }
}
