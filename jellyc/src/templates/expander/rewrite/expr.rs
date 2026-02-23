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

use crate::ast::{Expr, ExprKind};
use crate::error::{CompileError, ErrorKind};

use super::super::Expander;

impl Expander {
    /// Rewrite an expression.
    /// This is used to rewrite an expression to a concrete specialized expression.
    pub(in crate::templates) fn rewrite_expr(&mut self, e: &Expr) -> Result<Expr, CompileError> {
        match &e.node {
            ExprKind::TypeApp { base, type_args } => {
                let base_name = match &base.node {
                    ExprKind::Var(n) => n,
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "type application is only supported on a template name (e.g. Name<I32>)",
                        ));
                    }
                };
                let nm = self.instantiate(base_name, type_args, e.span)?;
                Ok(Expr::new(ExprKind::Var(nm), e.span))
            }
            ExprKind::Var(n) if self.templates.contains_key(n) => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                format!(
                    "templated value '{}' must be specialized: use '{}<...>'",
                    n, n
                ),
            )),
            _ => {
                let node = match &e.node {
                    ExprKind::BytesLit(b) => ExprKind::BytesLit(b.clone()),
                    ExprKind::BoolLit(x) => ExprKind::BoolLit(*x),
                    ExprKind::I32Lit(x) => ExprKind::I32Lit(*x),
                    ExprKind::I8Lit(x) => ExprKind::I8Lit(*x),
                    ExprKind::I16Lit(x) => ExprKind::I16Lit(*x),
                    ExprKind::I64Lit(x) => ExprKind::I64Lit(*x),
                    ExprKind::F16Lit(x) => ExprKind::F16Lit(*x),
                    ExprKind::F64Lit(x) => ExprKind::F64Lit(*x),
                    ExprKind::AtomLit(n) => ExprKind::AtomLit(n.clone()),
                    ExprKind::Null => ExprKind::Null,
                    ExprKind::Var(n) => ExprKind::Var(n.clone()),
                    ExprKind::Member { base, name } => ExprKind::Member {
                        base: Box::new(self.rewrite_expr(base)?),
                        name: name.clone(),
                    },
                    ExprKind::Call {
                        callee,
                        type_args,
                        args,
                    } => {
                        // Templated function call: `f<T>(...)`
                        if !type_args.is_empty() {
                            if let ExprKind::Var(base_name) = &callee.node {
                                if self.templates.contains_key(base_name) {
                                    let nm = self.instantiate(base_name, type_args, e.span)?;
                                    ExprKind::Call {
                                        callee: Box::new(Expr::new(ExprKind::Var(nm), callee.span)),
                                        type_args: Vec::new(),
                                        args: args
                                            .iter()
                                            .map(|a| self.rewrite_expr(a))
                                            .collect::<Result<_, _>>()?,
                                    }
                                } else {
                                    ExprKind::Call {
                                        callee: Box::new(self.rewrite_expr(callee)?),
                                        type_args: type_args.clone(),
                                        args: args
                                            .iter()
                                            .map(|a| self.rewrite_expr(a))
                                            .collect::<Result<_, _>>()?,
                                    }
                                }
                            } else {
                                ExprKind::Call {
                                    callee: Box::new(self.rewrite_expr(callee)?),
                                    type_args: type_args.clone(),
                                    args: args
                                        .iter()
                                        .map(|a| self.rewrite_expr(a))
                                        .collect::<Result<_, _>>()?,
                                }
                            }
                        } else {
                            ExprKind::Call {
                                callee: Box::new(self.rewrite_expr(callee)?),
                                type_args: Vec::new(),
                                args: args
                                    .iter()
                                    .map(|a| self.rewrite_expr(a))
                                    .collect::<Result<_, _>>()?,
                            }
                        }
                    }
                    ExprKind::TypeApp { .. } => unreachable!("handled above"),
                    ExprKind::ArrayLit(elems) => ExprKind::ArrayLit(
                        elems
                            .iter()
                            .map(|a| self.rewrite_expr(a))
                            .collect::<Result<_, _>>()?,
                    ),
                    ExprKind::TupleLit(elems) => ExprKind::TupleLit(
                        elems
                            .iter()
                            .map(|a| self.rewrite_expr(a))
                            .collect::<Result<_, _>>()?,
                    ),
                    ExprKind::ObjLit(fields) => ExprKind::ObjLit(
                        fields
                            .iter()
                            .map(|(k, v)| Ok((k.clone(), self.rewrite_expr(v)?)))
                            .collect::<Result<_, CompileError>>()?,
                    ),
                    ExprKind::Index { base, index } => ExprKind::Index {
                        base: Box::new(self.rewrite_expr(base)?),
                        index: Box::new(self.rewrite_expr(index)?),
                    },
                    ExprKind::IndexAssign { base, index, expr } => ExprKind::IndexAssign {
                        base: Box::new(self.rewrite_expr(base)?),
                        index: Box::new(self.rewrite_expr(index)?),
                        expr: Box::new(self.rewrite_expr(expr)?),
                    },
                    ExprKind::Fn { params, body, tail } => ExprKind::Fn {
                        params: params.clone(),
                        body: body
                            .iter()
                            .map(|s| self.rewrite_stmt(s))
                            .collect::<Result<_, _>>()?,
                        tail: match tail {
                            Some(t) => Some(Box::new(self.rewrite_expr(t)?)),
                            None => None,
                        },
                    },
                    ExprKind::Truthy(a) => ExprKind::Truthy(Box::new(self.rewrite_expr(a)?)),
                    ExprKind::Not(a) => ExprKind::Not(Box::new(self.rewrite_expr(a)?)),
                    ExprKind::Neg(a) => ExprKind::Neg(Box::new(self.rewrite_expr(a)?)),
                    ExprKind::Add(a, b) => ExprKind::Add(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Sub(a, b) => ExprKind::Sub(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Mul(a, b) => ExprKind::Mul(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Div(a, b) => ExprKind::Div(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Mod(a, b) => ExprKind::Mod(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Shl(a, b) => ExprKind::Shl(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Shr(a, b) => ExprKind::Shr(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Eq(a, b) => ExprKind::Eq(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Ne(a, b) => ExprKind::Ne(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Lt(a, b) => ExprKind::Lt(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Le(a, b) => ExprKind::Le(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Gt(a, b) => ExprKind::Gt(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Ge(a, b) => ExprKind::Ge(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::And(a, b) => ExprKind::And(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::Or(a, b) => ExprKind::Or(
                        Box::new(self.rewrite_expr(a)?),
                        Box::new(self.rewrite_expr(b)?),
                    ),
                    ExprKind::If {
                        cond,
                        then_br,
                        else_br,
                    } => ExprKind::If {
                        cond: Box::new(self.rewrite_expr(cond)?),
                        then_br: Box::new(self.rewrite_expr(then_br)?),
                        else_br: Box::new(self.rewrite_expr(else_br)?),
                    },
                    ExprKind::Block { stmts, expr } => ExprKind::Block {
                        stmts: stmts
                            .iter()
                            .map(|s| self.rewrite_stmt(s))
                            .collect::<Result<_, _>>()?,
                        expr: Box::new(self.rewrite_expr(expr)?),
                    },
                    ExprKind::Assign { name, expr } => ExprKind::Assign {
                        name: name.clone(),
                        expr: Box::new(self.rewrite_expr(expr)?),
                    },
                    ExprKind::MemberAssign { base, name, expr } => ExprKind::MemberAssign {
                        base: Box::new(self.rewrite_expr(base)?),
                        name: name.clone(),
                        expr: Box::new(self.rewrite_expr(expr)?),
                    },
                    ExprKind::Let {
                        is_const,
                        name,
                        type_params,
                        ty,
                        expr: init,
                    } => ExprKind::Let {
                        is_const: *is_const,
                        name: name.clone(),
                        type_params: type_params.clone(),
                        ty: ty.clone(),
                        expr: Box::new(self.rewrite_expr(init)?),
                    },
                    ExprKind::Try {
                        body,
                        catch_name,
                        catch_body,
                    } => ExprKind::Try {
                        body: Box::new(self.rewrite_expr(body)?),
                        catch_name: catch_name.clone(),
                        catch_body: Box::new(self.rewrite_expr(catch_body)?),
                    },
                    ExprKind::Match { subject, arms } => ExprKind::Match {
                        subject: Box::new(self.rewrite_expr(subject)?),
                        arms: arms
                            .iter()
                            .map(|a| self.rewrite_arm(a))
                            .collect::<Result<_, _>>()?,
                    },
                    ExprKind::With {
                        clauses,
                        body,
                        else_arms,
                    } => ExprKind::With {
                        clauses: clauses
                            .iter()
                            .map(|(pat, expr)| Ok((pat.clone(), self.rewrite_expr(expr)?)))
                            .collect::<Result<_, _>>()?,
                        body: Box::new(self.rewrite_expr(body)?),
                        else_arms: else_arms
                            .as_ref()
                            .map(|arms| {
                                arms
                                    .iter()
                                    .map(|a| self.rewrite_arm(a))
                                    .collect::<Result<_, _>>()
                            })
                            .transpose()?,
                    },
                    ExprKind::New { proto, args } => ExprKind::New {
                        proto: {
                            // Template inference (MVP): allow `new Proto(args...)` where `Proto`
                            // is a templated `let Proto<T,...> = ...;`, and infer `<...>` from
                            // literal argument types.
                            //
                            // Rule: template arity must equal argument count, and each type arg
                            // must be inferrable (currently: I32/Bool/Bytes literals only).
                            if let ExprKind::Var(base_name) = &proto.node {
                                if let Some(def) = self.templates.get(base_name) {
                                    if def.type_params.len() != args.len() {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            e.span,
                                            format!(
                                                "cannot infer template args for 'new {}(...)': template expects {} type args, call provides {} value args",
                                                base_name,
                                                def.type_params.len(),
                                                args.len()
                                            ),
                                        ));
                                    }
                                    let mut targs = Vec::with_capacity(args.len());
                                    for a in args {
                                        let Some(t) = self.infer_ty_from_expr(a) else {
                                            return Err(CompileError::new(
                                                ErrorKind::Type,
                                                a.span,
                                                format!(
                                                    "cannot infer template type for 'new {}(...)' from this argument; use 'new {}<...>(...)'",
                                                    base_name, base_name
                                                ),
                                            ));
                                        };
                                        targs.push(t);
                                    }
                                    let nm = self.instantiate(base_name, &targs, e.span)?;
                                    Box::new(Expr::new(ExprKind::Var(nm), proto.span))
                                } else {
                                    Box::new(self.rewrite_expr(proto)?)
                                }
                            } else {
                                Box::new(self.rewrite_expr(proto)?)
                            }
                        },
                        args: args
                            .iter()
                            .map(|a| self.rewrite_expr(a))
                            .collect::<Result<_, _>>()?,
                    },
                };
                Ok(Expr::new(node, e.span))
            }
        }
    }
}
