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

// AST backend: compile typed AST directly to bytecode (single-file, no IR).

use std::collections::HashMap;

fn f32_to_f16_bits(f: f32) -> u16 {
    let u32_bits = f.to_bits();
    let sign = (u32_bits >> 16) & 0x8000;
    let exp = (u32_bits >> 23) & 0xFF;
    let mant = u32_bits & 0x7FFFFF;
    if exp == 0xFF {
        return (sign | 0x7C00 | if mant != 0 { 0x200 } else { 0 }) as u16;
    }
    if exp == 0 && mant == 0 {
        return sign as u16;
    }
    let exp16 = (exp as i32) - 127 + 15;
    if exp16 >= 31 {
        return (sign | 0x7C00) as u16;
    }
    if exp16 <= 0 {
        return sign as u16;
    }
    (sign | ((exp16 as u32) << 10) | (mant >> 13)) as u16
}

use crate::ast::{Expr, ExprKind, Program, Span, Stmt, StmtKind};
use crate::error::{CompileError, ErrorKind};
use crate::regalloc::{self, InstrInfo, SpillPolicy, VReg};
use crate::typectx::{
    TypeCtx, T_ARRAY_BYTES, T_ARRAY_I32, T_ATOM, T_BOOL, T_BYTES, T_DYNAMIC, T_F16, T_F32, T_F64,
    T_I16, T_I32, T_I64, T_I8, T_OBJECT,
};

use super::prelude::prelude_funcs_for_program;
use super::{
    ATOM_INIT, ATOM___PROTO__, Function, Insn, Module, Op, PRELUDE_BYTES_EQ, PRELUDE_BYTES_SLICE,
    PRELUDE_FUN_COUNT, TypeKind,
};

pub fn build_program_module(p: &Program) -> Result<Module, CompileError> {
    let mut type_ctx = TypeCtx::new_program_base();
    let mut nested_funcs: Vec<Function> = Vec::new();

    #[derive(Clone, Copy)]
    struct VTmp {
        v: u32,
    }
    #[derive(Clone, Copy)]
    enum Opnd {
        V(u32),
        Z,
    }
    #[derive(Clone, Copy)]
    struct VInsn {
        op: Op,
        a: VTmp,
        b: Opnd,
        c: Opnd,
        imm: u32,
    }

    struct CallSite {
        pc: u32,       // VInsn index
        sig_id: u32,   // function signature ID
        args: Vec<u32> // vregs
    }

    let mut env: HashMap<String, VTmp> = HashMap::new();

    struct CompileCtx<'a> {
        // module tables
        type_ctx: &'a mut TypeCtx,
        nested_funcs: &'a mut Vec<Function>,

        // codegen state
        const_bytes: &'a mut Vec<Vec<u8>>,
        const_i64: &'a mut Vec<i64>,
        const_f64: &'a mut Vec<f64>,
        vtypes: &'a mut Vec<u32>,
        allow_multi_def: &'a mut Vec<bool>,
        next_v: &'a mut u32,
        vinsns: &'a mut Vec<VInsn>,
        infos: &'a mut Vec<InstrInfo>,
        call_sites: &'a mut Vec<CallSite>,

        // aux state
        loop_stack: &'a mut Vec<LoopCtx>,
        atoms: &'a mut Vec<Vec<u8>>,
        atom_ids: &'a mut HashMap<String, u32>,
    }

    impl<'a> CompileCtx<'a> {
        fn emit(&mut self, vi: VInsn, uses: Vec<u32>, defs: Vec<u32>) {
            self.vinsns.push(vi);
            self.infos.push(InstrInfo {
                uses: uses.into_iter().map(VReg).collect(),
                defs: defs.into_iter().map(VReg).collect(),
            });
        }

        fn new_v(&mut self, tid: u32) -> VTmp {
            let v = *self.next_v;
            *self.next_v += 1;
            if v as usize == self.vtypes.len() {
                self.vtypes.push(tid);
                self.allow_multi_def.push(false);
            } else {
                self.vtypes[v as usize] = tid;
            }
            VTmp { v }
        }
    }

    fn compile_expr(
        e: &Expr,
        env: &HashMap<String, VTmp>,
        ctx: &mut CompileCtx,
    ) -> Result<VTmp, CompileError> {
        compile_expr_expect(e, None, env, ctx)
    }

    fn compile_expr_expect(
        e: &Expr,
        expect: Option<u32>,
        env: &HashMap<String, VTmp>,
        ctx: &mut CompileCtx,
    ) -> Result<VTmp, CompileError> {
        fn compile_member_get(
            span: Span,
            base: &Expr,
            name: &str,
            expect_tid: u32,
            env: &HashMap<String, VTmp>,
            ctx: &mut CompileCtx,
        ) -> Result<VTmp, CompileError> {
            // Namespaces must be called (handled via ExprKind::Call builtin logic).
            if let ExprKind::Var(ns) = &base.node {
                if matches!(
                    ns.as_str(),
                    "System" | "Bytes" | "Integer" | "Float" | "Atom" | "Object" | "Array" | "List"
                ) {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        span,
                        "namespace members must be called (e.g. Bytes.len(x))",
                    ));
                }
            }

            let v_obj = compile_expr(base, env, ctx)?;
            let base_tid = ctx.vtypes[v_obj.v as usize];
            let te = ctx
                .type_ctx
                .types
                .get(base_tid as usize)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad member base type id"))?;
            if te.kind != TypeKind::Object {
                return Err(CompileError::new(
                    ErrorKind::Type,
                    span,
                    "member access currently only supported for Object (obj.field)",
                ));
            }
            let atom_id = if let Some(&id) = ctx.atom_ids.get(name) {
                id
            } else {
                let id = ctx.atoms.len() as u32;
                ctx.atoms.push(name.as_bytes().to_vec());
                ctx.atom_ids.insert(name.to_string(), id);
                id
            };

            if ctx.type_ctx.is_tuple_type(base_tid) {
                // Tuple element access: `t.0`, `t.1`, ...
                let idx: usize = name
                    .parse()
                    .map_err(|_| CompileError::new(ErrorKind::Type, span, "tuple element access must be .<index>"))?;
                let elems = ctx
                    .type_ctx
                    .tuple_elems(base_tid)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, span, "bad tuple type"))?;
                if idx >= elems.len() {
                    return Err(CompileError::new(ErrorKind::Type, span, "tuple index out of range"));
                }
                let elem_tid = elems[idx];
                let v_elem = ctx.new_v(elem_tid);
                ctx.emit(
                    VInsn { op: Op::ObjGetAtom, a: v_elem, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                    vec![v_obj.v],
                    vec![v_elem.v],
                );
                if expect_tid == elem_tid {
                    return Ok(v_elem);
                }
                if expect_tid == T_DYNAMIC {
                    let vd = ctx.new_v(T_DYNAMIC);
                    ctx.emit(
                        VInsn { op: Op::ToDyn, a: vd, b: Opnd::V(v_elem.v), c: Opnd::Z, imm: 0 },
                        vec![v_elem.v],
                        vec![vd.v],
                    );
                    return Ok(vd);
                }
                return Err(CompileError::new(ErrorKind::Type, span, "tuple element type mismatch"));
            }

            let out = ctx.new_v(expect_tid);
            ctx.emit(
                VInsn { op: Op::ObjGetAtom, a: out, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                vec![v_obj.v],
                vec![out.v],
            );
            Ok(out)
        }

        fn compile_expr_expect(
            e: &Expr,
            expect_tid: u32,
            env: &HashMap<String, VTmp>,
            ctx: &mut CompileCtx,
        ) -> Result<VTmp, CompileError> {
            match &e.node {
                ExprKind::Member { base, name } => compile_member_get(e.span, base, name.as_str(), expect_tid, env, ctx),
                _ => {
                    let v = compile_expr(e, env, ctx)?;
                    if ctx.vtypes[v.v as usize] != expect_tid {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "type mismatch",
                        ));
                    }
                    Ok(v)
                }
            }
        }

        let is_numeric = |t: u32| matches!(t, T_I32 | T_I64 | T_F32 | T_F64);
        let numeric_rank = |t: u32| -> u8 {
            match t {
                T_I32 => 0,
                T_I64 => 1,
                T_F32 => 2,
                T_F64 => 3,
                _ => 255,
            }
        };
        let mut coerce_numeric = |span: Span, v: VTmp, from: u32, to: u32, ctx: &mut CompileCtx| -> Result<VTmp, CompileError> {
            if from == to {
                return Ok(v);
            }
            let out = ctx.new_v(to);
            let op = match (from, to) {
                (T_I32, T_I64) => Op::SextI64,
                (T_I32, T_F32) => Op::F32FromI32,
                (T_I32, T_F64) => Op::F64FromI32,
                (T_I64, T_F64) => Op::F64FromI64,
                (T_I64, T_F32) => Op::F32FromI64,
                (T_F32, T_F64) => Op::F64FromF32,
                (T_F64, T_F32) => Op::F32FromF64,
                _ => {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        span,
                        "unsupported numeric conversion",
                    ))
                }
            };
            ctx.emit(
                VInsn { op, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                vec![v.v],
                vec![out.v],
            );
            Ok(out)
        };

        fn compile_truthy(_span: Span, v: VTmp, tid: u32, ctx: &mut CompileCtx) -> Result<VTmp, CompileError> {
            match tid {
                T_BOOL => Ok(v),
                T_DYNAMIC => {
                    // Truthiness for Dynamic:
                    // - null => false
                    // - bool => itself
                    // - everything else => true
                    let v_kind = ctx.new_v(T_I32);
                    ctx.emit(
                        VInsn { op: Op::Kindof, a: v_kind, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                        vec![v.v],
                        vec![v_kind.v],
                    );

                    let out = ctx.new_v(T_BOOL);
                    ctx.allow_multi_def[out.v as usize] = true;

                    // is_null = (kind == 0)
                    let v0 = ctx.new_v(T_I32);
                    ctx.emit(
                        VInsn { op: Op::ConstI32, a: v0, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                        vec![],
                        vec![v0.v],
                    );
                    let v_is_null = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::EqI32, a: v_is_null, b: Opnd::V(v_kind.v), c: Opnd::V(v0.v), imm: 0 },
                        vec![v_kind.v, v0.v],
                        vec![v_is_null.v],
                    );

                    // if is_null jump to null block; fallthrough is not-null.
                    let jmp_if_null_pc = ctx.vinsns.len() as u32;
                    ctx.emit(
                        VInsn { op: Op::JmpIf, a: v_is_null, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                        vec![v_is_null.v],
                        vec![],
                    );

                    let v1 = ctx.new_v(T_I32);
                    ctx.emit(
                        VInsn { op: Op::ConstI32, a: v1, b: Opnd::Z, c: Opnd::Z, imm: 1 },
                        vec![],
                        vec![v1.v],
                    );
                    let v_is_bool = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::EqI32, a: v_is_bool, b: Opnd::V(v_kind.v), c: Opnd::V(v1.v), imm: 0 },
                        vec![v_kind.v, v1.v],
                        vec![v_is_bool.v],
                    );

                    // if is_bool jump to bool block; fallthrough is "other" (truthy).
                    let jmp_if_bool_pc = ctx.vinsns.len() as u32;
                    ctx.emit(
                        VInsn { op: Op::JmpIf, a: v_is_bool, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                        vec![v_is_bool.v],
                        vec![],
                    );

                    // other path: out=true; jmp join
                    let v_true = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::ConstBool, a: v_true, b: Opnd::Z, c: Opnd::Z, imm: 1 },
                        vec![],
                        vec![v_true.v],
                    );
                    ctx.emit(
                        VInsn { op: Op::Mov, a: out, b: Opnd::V(v_true.v), c: Opnd::Z, imm: 0 },
                        vec![v_true.v],
                        vec![out.v],
                    );
                    let jmp_to_join_from_other_pc = ctx.vinsns.len() as u32;
                    ctx.emit(VInsn { op: Op::Jmp, a: out, b: Opnd::Z, c: Opnd::Z, imm: 0 }, vec![], vec![]);

                    // bool path starts here (patched from second jmp_if)
                    let bool_pc = ctx.vinsns.len() as u32;
                    let v_unboxed = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::FromDynBool, a: v_unboxed, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                        vec![v.v],
                        vec![v_unboxed.v],
                    );
                    ctx.emit(
                        VInsn { op: Op::Mov, a: out, b: Opnd::V(v_unboxed.v), c: Opnd::Z, imm: 0 },
                        vec![v_unboxed.v],
                        vec![out.v],
                    );
                    let jmp_to_join_from_bool_pc = ctx.vinsns.len() as u32;
                    ctx.emit(VInsn { op: Op::Jmp, a: out, b: Opnd::Z, c: Opnd::Z, imm: 0 }, vec![], vec![]);

                    // null block (patched from first jmp_if): out=false; fallthrough to join
                    let null_pc = ctx.vinsns.len() as u32;
                    let v_false = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::ConstBool, a: v_false, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                        vec![],
                        vec![v_false.v],
                    );
                    ctx.emit(
                        VInsn { op: Op::Mov, a: out, b: Opnd::V(v_false.v), c: Opnd::Z, imm: 0 },
                        vec![v_false.v],
                        vec![out.v],
                    );
                    let join_pc = ctx.vinsns.len() as u32;

                    // Patch jumps.
                    let delta_to_null: i32 = (null_pc as i32) - ((jmp_if_null_pc + 1) as i32);
                    ctx.vinsns[jmp_if_null_pc as usize].imm = delta_to_null as u32;

                    let delta_to_bool: i32 = (bool_pc as i32) - ((jmp_if_bool_pc + 1) as i32);
                    ctx.vinsns[jmp_if_bool_pc as usize].imm = delta_to_bool as u32;

                    let delta_to_join_from_other: i32 = (join_pc as i32) - ((jmp_to_join_from_other_pc + 1) as i32);
                    ctx.vinsns[jmp_to_join_from_other_pc as usize].imm = delta_to_join_from_other as u32;
                    let delta_to_join_from_bool: i32 = (join_pc as i32) - ((jmp_to_join_from_bool_pc + 1) as i32);
                    ctx.vinsns[jmp_to_join_from_bool_pc as usize].imm = delta_to_join_from_bool as u32;

                    Ok(out)
                }
                _ => {
                    let out = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::ConstBool, a: out, b: Opnd::Z, c: Opnd::Z, imm: 1 },
                        vec![],
                        vec![out.v],
                    );
                    Ok(out)
                }
            }
        }

        match &e.node {
            ExprKind::BytesLit(b) => {
                let idx = ctx.const_bytes.len() as u32;
                ctx.const_bytes.push(b.clone());
                let v = ctx.new_v(T_BYTES);
                ctx.emit(VInsn { op: Op::ConstBytes, a: v, b: Opnd::Z, c: Opnd::Z, imm: idx }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::BoolLit(b) => {
                let v = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::ConstBool, a: v, b: Opnd::Z, c: Opnd::Z, imm: if *b { 1 } else { 0 } },
                    vec![],
                    vec![v.v],
                );
                Ok(v)
            }
            ExprKind::I32Lit(x) => {
                // Default integer literal type is I32 unless an explicit expected type narrows/widens it.
                if expect == Some(T_I8) && *x >= -128 && *x <= 127 {
                    let v = ctx.new_v(T_I8);
                    let imm = (*x as i16).to_le_bytes()[0];
                    ctx.emit(
                        VInsn { op: Op::ConstI8Imm, a: v, b: Opnd::Z, c: Opnd::Z, imm: imm as u32 },
                        vec![],
                        vec![v.v],
                    );
                    return Ok(v);
                }
                if expect == Some(T_I16) && *x >= -32768 && *x <= 32767 {
                    let v = ctx.new_v(T_I16);
                    ctx.emit(
                        VInsn { op: Op::ConstI32, a: v, b: Opnd::Z, c: Opnd::Z, imm: *x as u32 },
                        vec![],
                        vec![v.v],
                    );
                    return Ok(v);
                }
                if expect == Some(T_I64) {
                    let idx = ctx.const_i64.len() as u32;
                    ctx.const_i64.push(*x as i64);
                    let v = ctx.new_v(T_I64);
                    ctx.emit(
                        VInsn { op: Op::ConstI64, a: v, b: Opnd::Z, c: Opnd::Z, imm: idx },
                        vec![],
                        vec![v.v],
                    );
                    return Ok(v);
                }

                let v = ctx.new_v(T_I32);
                ctx.emit(
                    VInsn { op: Op::ConstI32, a: v, b: Opnd::Z, c: Opnd::Z, imm: *x as u32 },
                    vec![],
                    vec![v.v],
                );
                Ok(v)
            }
            ExprKind::I8Lit(x) => {
                let v = ctx.new_v(T_I8);
                let imm = (*x as i16).to_le_bytes()[0];
                ctx.emit(VInsn { op: Op::ConstI8Imm, a: v, b: Opnd::Z, c: Opnd::Z, imm: imm as u32 }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::I16Lit(x) => {
                let v = ctx.new_v(T_I16);
                ctx.emit(VInsn { op: Op::ConstI32, a: v, b: Opnd::Z, c: Opnd::Z, imm: *x as u32 }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::F16Lit(x) => {
                let v = ctx.new_v(T_F16);
                let bits = f32_to_f16_bits(*x);
                ctx.emit(VInsn { op: Op::ConstF16, a: v, b: Opnd::Z, c: Opnd::Z, imm: bits as u32 }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::I64Lit(x) => {
                let idx = ctx.const_i64.len() as u32;
                ctx.const_i64.push(*x);
                let v = ctx.new_v(T_I64);
                ctx.emit(
                    VInsn { op: Op::ConstI64, a: v, b: Opnd::Z, c: Opnd::Z, imm: idx },
                    vec![],
                    vec![v.v],
                );
                Ok(v)
            }
            ExprKind::F64Lit(x) => {
                // Default float literal type is F32 unless an explicit expected type is F64.
                if expect == Some(T_F64) {
                    let idx = ctx.const_f64.len() as u32;
                    ctx.const_f64.push(*x);
                    let v = ctx.new_v(T_F64);
                    ctx.emit(
                        VInsn { op: Op::ConstF64, a: v, b: Opnd::Z, c: Opnd::Z, imm: idx },
                        vec![],
                        vec![v.v],
                    );
                    Ok(v)
                } else {
                    let v = ctx.new_v(T_F32);
                    let bits = (*x as f32).to_bits();
                    ctx.emit(
                        VInsn { op: Op::ConstF32, a: v, b: Opnd::Z, c: Opnd::Z, imm: bits },
                        vec![],
                        vec![v.v],
                    );
                    Ok(v)
                }
            }
            ExprKind::AtomLit(name) => {
                let atom_id = if let Some(&id) = ctx.atom_ids.get(name.as_str()) {
                    id
                } else {
                    let id = ctx.atoms.len() as u32;
                    ctx.atoms.push(name.as_bytes().to_vec());
                    ctx.atom_ids.insert(name.to_string(), id);
                    id
                };
                let v = ctx.new_v(T_ATOM);
                ctx.emit(
                    VInsn { op: Op::ConstAtom, a: v, b: Opnd::Z, c: Opnd::Z, imm: atom_id },
                    vec![],
                    vec![v.v],
                );
                Ok(v)
            }
            ExprKind::Var(name) => env
                .get(name)
                .copied()
                .ok_or_else(|| CompileError::new(ErrorKind::Name, e.span, format!("unknown variable '{}'", name))),
            ExprKind::Member { base, name } => {
                // Default to dynamic unless an operator/builtin provides an expected type.
                compile_member_get(e.span, base, name.as_str(), T_DYNAMIC, env, ctx)
            }
            ExprKind::Call { callee, type_args, args } => {
                // Builtins are namespaced (eg `Bytes.get_u8`, `Array.new<I32>`).
                let builtin = match &callee.node {
                    ExprKind::Member { base, name } => match &base.node {
                        ExprKind::Var(ns) => Some((ns.clone(), name.clone())),
                        _ => None,
                    },
                    _ => None,
                };

                if let Some((ns, name)) = builtin {
                    let ns = ns.as_str();
                    let name = name.as_str();
                    // System.assert(cond: Bool) -> Bool (throws if cond is false)
                    if ns == "System" && name == "assert" {
                        if !type_args.is_empty() {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "assert does not take type arguments",
                            ));
                        }
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "assert expects 1 arg"));
                        }
                        let vcond = compile_expr_expect(&args[0], T_BOOL, env, ctx)?;
                        ctx.emit(
                            VInsn { op: Op::Assert, a: vcond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                            vec![vcond.v],
                            vec![],
                        );
                        return Ok(vcond);
                    }
                    // Bytes builtins
                    if ns == "Bytes" && name == "new" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.new expects 1 arg"));
                        }
                        let vlen = compile_expr_expect(&args[0], T_I32, env, ctx)?;
                        let out = ctx.new_v(T_BYTES);
                        ctx.emit(VInsn { op: Op::BytesNew, a: out, b: Opnd::V(vlen.v), c: Opnd::Z, imm: 0 }, vec![vlen.v], vec![out.v]);
                        return Ok(out);
                    } else if ns == "Bytes" && name == "get_u8" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.get_u8 expects 2 args"));
                        }
                        let vb = compile_expr_expect(&args[0], T_BYTES, env, ctx)?;
                        let vi = compile_expr_expect(&args[1], T_I32, env, ctx)?;
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::BytesGetU8, a: out, b: Opnd::V(vb.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![vb.v, vi.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Bytes" && name == "set_u8" {
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.set_u8 expects 3 args"));
                        }
                        let vb = compile_expr_expect(&args[0], T_BYTES, env, ctx)?;
                        let vi = compile_expr_expect(&args[1], T_I32, env, ctx)?;
                        let vv = compile_expr_expect(&args[2], T_I32, env, ctx)?;
                        ctx.emit(
                            VInsn { op: Op::BytesSetU8, a: vv, b: Opnd::V(vb.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![vv.v, vb.v, vi.v],
                            vec![],
                        );
                        // Treat as expression returning the mutated bytes object.
                        return Ok(vb);
                    } else if ns == "Bytes" && name == "len" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.len expects 1 arg"));
                        }
                        let vb = compile_expr_expect(&args[0], T_BYTES, env, ctx)?;
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::BytesLen, a: out, b: Opnd::V(vb.v), c: Opnd::Z, imm: 0 },
                            vec![vb.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Bytes" && name == "slice" {
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.slice expects 3 args"));
                        }
                        let vb = compile_expr_expect(&args[0], T_BYTES, env, ctx)?;
                        let v_start = compile_expr_expect(&args[1], T_I32, env, ctx)?;
                        let v_len = compile_expr_expect(&args[2], T_I32, env, ctx)?;

                        let sig_args = vec![T_BYTES, T_I32, T_I32]; // (bytes, i32, i32)
                        let fun_tid = ctx.type_ctx.intern_fun_type(T_BYTES, &sig_args);
                        let sig_id = ctx.type_ctx.intern_sig(T_BYTES, &sig_args);

                        let vcallee = ctx.new_v(fun_tid);
                        ctx.emit(
                            VInsn { op: Op::ConstFun, a: vcallee, b: Opnd::Z, c: Opnd::Z, imm: PRELUDE_BYTES_SLICE },
                            vec![],
                            vec![vcallee.v],
                        );

                        let out = ctx.new_v(T_BYTES);
                        let pc = ctx.vinsns.len() as u32;
                        ctx.call_sites.push(CallSite { pc, sig_id, args: vec![vb.v, v_start.v, v_len.v] });

                        ctx.emit(
                            VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: 3 },
                            vec![vcallee.v, vb.v, v_start.v, v_len.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Bytes" && name == "eq" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Bytes.eq expects 2 args"));
                        }
                        let va = compile_expr_expect(&args[0], T_BYTES, env, ctx)?;
                        let vb = compile_expr_expect(&args[1], T_BYTES, env, ctx)?;

                        let sig_args = vec![T_BYTES, T_BYTES]; // (bytes, bytes)
                        let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args); // -> bool
                        let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);

                        let vcallee = ctx.new_v(fun_tid);
                        ctx.emit(
                            VInsn { op: Op::ConstFun, a: vcallee, b: Opnd::Z, c: Opnd::Z, imm: PRELUDE_BYTES_EQ },
                            vec![],
                            vec![vcallee.v],
                        );

                        let out = ctx.new_v(T_BOOL);
                        let pc = ctx.vinsns.len() as u32;
                        ctx.call_sites.push(CallSite { pc, sig_id, args: vec![va.v, vb.v] });

                        ctx.emit(
                            VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: 2 },
                            vec![vcallee.v, va.v, vb.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Atom" && name == "intern" {
                        if !type_args.is_empty() {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Atom.intern does not take type args"));
                        }
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Atom.intern expects 1 arg"));
                        }
                        let b = match &args[0].node {
                            ExprKind::BytesLit(bb) => bb,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    args[0].span,
                                    "Atom.intern expects a bytes literal",
                                ))
                            }
                        };
                        let name = std::str::from_utf8(b).map_err(|_| {
                            CompileError::new(ErrorKind::Type, args[0].span, "Atom.intern expects UTF-8 bytes")
                        })?;
                        let atom_id = if let Some(&id) = ctx.atom_ids.get(name) {
                            id
                        } else {
                            let id = ctx.atoms.len() as u32;
                            ctx.atoms.push(name.as_bytes().to_vec());
                            ctx.atom_ids.insert(name.to_string(), id);
                            id
                        };
                        let out = ctx.new_v(T_ATOM);
                        ctx.emit(
                            VInsn { op: Op::ConstAtom, a: out, b: Opnd::Z, c: Opnd::Z, imm: atom_id },
                            vec![],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Object" && name == "get" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.get expects 2 args"));
                        }
                        let out_tid = if type_args.is_empty() {
                            T_DYNAMIC
                        } else {
                            if type_args.len() != 1 {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "Object.get<T>(obj, key): expects 1 type arg",
                                ));
                            }
                            ctx.type_ctx.resolve_ty(&type_args[0])?
                        };
                        let v_obj = compile_expr_expect(&args[0], T_OBJECT, env, ctx)?;
                        let v_key = compile_expr_expect(&args[1], T_ATOM, env, ctx)?;
                        let out = ctx.new_v(out_tid);
                        ctx.emit(
                            VInsn { op: Op::ObjGet, a: out, b: Opnd::V(v_obj.v), c: Opnd::V(v_key.v), imm: 0 },
                            vec![v_obj.v, v_key.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Object" && name == "set" {
                        if !type_args.is_empty() {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.set does not take type args"));
                        }
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Object.set expects 3 args"));
                        }
                        let v_obj = compile_expr_expect(&args[0], T_OBJECT, env, ctx)?;
                        let v_key = compile_expr_expect(&args[1], T_ATOM, env, ctx)?;
                        let v_val = compile_expr(&args[2], env, ctx)?;
                        ctx.emit(
                            VInsn { op: Op::ObjSet, a: v_val, b: Opnd::V(v_obj.v), c: Opnd::V(v_key.v), imm: 0 },
                            vec![v_val.v, v_obj.v, v_key.v],
                            vec![],
                        );
                        return Ok(v_obj);
                    } else if ns == "Array" && name == "new" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.new expects 1 arg"));
                        }
                        // Determine element type.
                        if type_args.len() != 1 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "Array.new<T>(len): missing type argument",
                            ));
                        }
                        let elem_tid = ctx.type_ctx.resolve_ty(&type_args[0])?;
                        let arr_tid = match elem_tid {
                            T_I32 => T_ARRAY_I32,
                            T_BYTES => T_ARRAY_BYTES,
                            _ => {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    e.span,
                                    "only Array<I32> and Array<Bytes> supported for now",
                                ))
                            }
                        };
                        let vlen = compile_expr_expect(&args[0], T_I32, env, ctx)?;
                        let out = ctx.new_v(arr_tid);
                        ctx.emit(
                            VInsn { op: Op::ArrayNew, a: out, b: Opnd::V(vlen.v), c: Opnd::Z, imm: 0 },
                            vec![vlen.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Array" && name == "len" {
                        if args.len() != 1 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.len expects 1 arg"));
                        }
                        let va = compile_expr(&args[0], env, ctx)?;
                        let at = ctx.vtypes[va.v as usize];
                        if at != T_ARRAY_I32 && at != T_ARRAY_BYTES {
                            return Err(CompileError::new(ErrorKind::Type, args[0].span, "Array.len(Array<T>)"));
                        }
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::ArrayLen, a: out, b: Opnd::V(va.v), c: Opnd::Z, imm: 0 },
                            vec![va.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Array" && name == "get" {
                        if args.len() != 2 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get expects 2 args"));
                        }
                        let va = compile_expr(&args[0], env, ctx)?;
                        let vi = compile_expr_expect(&args[1], T_I32, env, ctx)?;
                        let at = ctx.vtypes[va.v as usize];
                        if (at != T_ARRAY_I32 && at != T_ARRAY_BYTES) || ctx.vtypes[vi.v as usize] != T_I32 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.get(Array<T>, i32)"));
                        }
                        let out_tid = if at == T_ARRAY_I32 { T_I32 } else { T_BYTES };
                        let out = ctx.new_v(out_tid);
                        ctx.emit(
                            VInsn { op: Op::ArrayGet, a: out, b: Opnd::V(va.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![va.v, vi.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    } else if ns == "Array" && name == "set" {
                        if args.len() != 3 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set expects 3 args"));
                        }
                        let va = compile_expr(&args[0], env, ctx)?;
                        let vi = compile_expr_expect(&args[1], T_I32, env, ctx)?;
                        let at = ctx.vtypes[va.v as usize];
                        let want_vt = if at == T_ARRAY_I32 { T_I32 } else if at == T_ARRAY_BYTES { T_BYTES } else { u32::MAX };
                        if want_vt == u32::MAX || ctx.vtypes[vi.v as usize] != T_I32 {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "Array.set(Array<T>, i32, T)"));
                        }
                        let vv = compile_expr_expect(&args[2], want_vt, env, ctx)?;
                        // JOP_ARRAY_SET: rB[rC] = rA
                        ctx.emit(
                            VInsn { op: Op::ArraySet, a: vv, b: Opnd::V(va.v), c: Opnd::V(vi.v), imm: 0 },
                            vec![vv.v, va.v, vi.v],
                            vec![],
                        );
                        return Ok(va);
                    }
                }

                if !type_args.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "generic type arguments are only supported for builtins (for now)",
                    ));
                }

                let vcallee = compile_expr(callee, env, ctx)?;
                let callee_tid = ctx.vtypes[vcallee.v as usize];
                let te = ctx
                    .type_ctx
                    .types
                    .get(callee_tid as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad callee type id"))?;
                if te.kind != TypeKind::Function {
                    return Err(CompileError::new(ErrorKind::Type, callee.span, "call target is not a function"));
                }
                let sig_id = te.p0;
                let (sig_args, sig_ret) = {
                    let sig = ctx
                        .type_ctx
                        .sigs
                        .get(sig_id as usize)
                        .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad fun sig id"))?;
                    (sig.args.clone(), sig.ret_type)
                };
                if args.len() != sig_args.len() {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "call arity does not match function type"));
                }
                if sig_args.len() > 255 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "too many call arguments"));
                }
                let mut arg_vs: Vec<u32> = Vec::with_capacity(args.len());
                for (i, a) in args.iter().enumerate() {
                    let va = compile_expr(a, env, ctx)?;
                    if ctx.vtypes[va.v as usize] != sig_args[i] {
                        return Err(CompileError::new(ErrorKind::Type, a.span, "call argument type mismatch"));
                    }
                    arg_vs.push(va.v);
                }

                let out = ctx.new_v(sig_ret);
                let pc = ctx.vinsns.len() as u32;
                ctx.call_sites.push(CallSite { pc, sig_id, args: arg_vs.clone() });

                let mut uses: Vec<u32> = Vec::with_capacity(1 + arg_vs.len());
                uses.push(vcallee.v);
                uses.extend(arg_vs.iter().copied());

                ctx.emit(
                    VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: arg_vs.len() as u32 },
                    uses,
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Truthy(inner) => {
                let v = compile_expr(inner, env, ctx)?;
                let tid = ctx.vtypes[v.v as usize];
                compile_truthy(e.span, v, tid, ctx)
            }
            ExprKind::Not(inner) => {
                let v = compile_expr(inner, env, ctx)?;
                if ctx.vtypes[v.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'!' expects bool"));
                }
                let out = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                    vec![v.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Add(_a, _b) => {
                // Special-case bytes concatenation: flatten long chains into concat_many(Array<bytes>).
                fn collect_add_terms<'e>(e: &'e Expr, out: &mut Vec<&'e Expr>) {
                    match &e.node {
                        ExprKind::Add(x, y) => {
                            collect_add_terms(x, out);
                            collect_add_terms(y, out);
                        }
                        _ => out.push(e),
                    }
                }

                let mut terms: Vec<&Expr> = Vec::new();
                collect_add_terms(e, &mut terms);

                // If it's a simple binary add, keep existing path.
                if terms.len() == 2 {
                    let va = compile_expr(terms[0], env, ctx)?;
                    let vb = compile_expr(terms[1], env, ctx)?;
                    let mut ta = ctx.vtypes[va.v as usize];
                    let mut tb = ctx.vtypes[vb.v as usize];
                    let (va, vb) = if ta != tb {
                        match (&terms[0].node, &terms[1].node) {
                            (ExprKind::Member { .. }, _) => {
                                let va = compile_expr_expect(terms[0], tb, env, ctx)?;
                                ta = tb;
                                (va, vb)
                            }
                            (_, ExprKind::Member { .. }) => {
                                let vb = compile_expr_expect(terms[1], ta, env, ctx)?;
                                tb = ta;
                                (va, vb)
                            }
                            _ => (va, vb),
                        }
                    } else {
                        (va, vb)
                    };
                    if ta == T_BYTES && tb == T_BYTES {
                        let out = ctx.new_v(T_BYTES);
                        ctx.emit(
                            VInsn { op: Op::BytesConcat2, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    }
                    if is_numeric(ta) && is_numeric(tb) {
                        let out_t = if expect == Some(T_F64) {
                            T_F64
                        } else if expect == Some(T_F32) && (numeric_rank(ta) >= 2 || numeric_rank(tb) >= 2) {
                            T_F32
                        } else if numeric_rank(ta) >= numeric_rank(tb) {
                            ta
                        } else {
                            tb
                        };
                        let va2 = coerce_numeric(e.span, va, ta, out_t, ctx)?;
                        let vb2 = coerce_numeric(e.span, vb, tb, out_t, ctx)?;
                        let out = ctx.new_v(out_t);
                        let op = match out_t {
                            T_I32 => Op::AddI32,
                            T_I64 => Op::AddI64,
                            T_F32 => Op::AddF32,
                            T_F64 => Op::AddF64,
                            _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad numeric promotion")),
                        };
                        ctx.emit(
                            VInsn { op, a: out, b: Opnd::V(va2.v), c: Opnd::V(vb2.v), imm: 0 },
                            vec![va2.v, vb2.v],
                            vec![out.v],
                        );
                        return Ok(out);
                    }
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects bytes or numeric operands"));
                }

                // Otherwise compile left-to-right and decide based on term types.
                let mut compiled: Vec<Option<VTmp>> = vec![None; terms.len()];
                let mut deferred_members: Vec<usize> = Vec::new();
                let mut t0: Option<u32> = None;
                for (i, t) in terms.iter().enumerate() {
                    if matches!(t.node, ExprKind::Member { .. }) {
                        deferred_members.push(i);
                        continue;
                    }
                    let v = compile_expr(t, env, ctx)?;
                    let tt = ctx.vtypes[v.v as usize];
                    if let Some(t0v) = t0 {
                        if tt != t0v {
                            return Err(CompileError::new(ErrorKind::Type, e.span, "'+' expects operands of same type"));
                        }
                    } else {
                        t0 = Some(tt);
                    }
                    compiled[i] = Some(v);
                }
                let t0 = t0.ok_or_else(|| CompileError::new(ErrorKind::Type, e.span, "cannot infer '+' operand type"))?;
                for i in deferred_members {
                    let v = compile_expr_expect(terms[i], t0, env, ctx)?;
                    compiled[i] = Some(v);
                }
                let compiled: Vec<VTmp> = compiled.into_iter().map(|x| x.expect("compiled term")).collect();

                match t0 {
                    T_BYTES => {
                        // Build Array<bytes> and concat_many.
                        let n = compiled.len();
                        let v_n = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::ConstI32, a: v_n, b: Opnd::Z, c: Opnd::Z, imm: n as u32 },
                            vec![],
                            vec![v_n.v],
                        );
                        let v_arr = ctx.new_v(T_ARRAY_BYTES);
                        ctx.emit(
                            VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_n.v), c: Opnd::Z, imm: 0 },
                            vec![v_n.v],
                            vec![v_arr.v],
                        );
                        for (i, part) in compiled.iter().enumerate() {
                            let v_i = ctx.new_v(T_I32);
                            ctx.emit(
                                VInsn { op: Op::ConstI32, a: v_i, b: Opnd::Z, c: Opnd::Z, imm: i as u32 },
                                vec![],
                                vec![v_i.v],
                            );
                            ctx.emit(
                                VInsn { op: Op::ArraySet, a: *part, b: Opnd::V(v_arr.v), c: Opnd::V(v_i.v), imm: 0 },
                                vec![part.v, v_arr.v, v_i.v],
                                vec![],
                            );
                        }
                        let out = ctx.new_v(T_BYTES);
                        ctx.emit(
                            VInsn { op: Op::BytesConcatMany, a: out, b: Opnd::V(v_arr.v), c: Opnd::Z, imm: 0 },
                            vec![v_arr.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_I32 => {
                        // Fold i32 addition left-to-right (keeps semantics identical).
                        let mut acc = compiled[0];
                        for rhs in &compiled[1..] {
                            let out = ctx.new_v(T_I32);
                            ctx.emit(
                                VInsn { op: Op::AddI32, a: out, b: Opnd::V(acc.v), c: Opnd::V(rhs.v), imm: 0 },
                                vec![acc.v, rhs.v],
                                vec![out.v],
                            );
                            acc = out;
                        }
                        Ok(acc)
                    }
                    _ => Err(CompileError::new(ErrorKind::Type, e.span, "'+' not supported for this type yet")),
                }
            }
            ExprKind::Sub(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                let ta = ctx.vtypes[va.v as usize];
                let tb = ctx.vtypes[vb.v as usize];
                if !(is_numeric(ta) && is_numeric(tb)) {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'-' expects numeric operands"));
                }
                let out_t = if expect == Some(T_F64) {
                    T_F64
                } else if expect == Some(T_F32) && (numeric_rank(ta) >= 2 || numeric_rank(tb) >= 2) {
                    T_F32
                } else if numeric_rank(ta) >= numeric_rank(tb) {
                    ta
                } else {
                    tb
                };
                let va2 = coerce_numeric(e.span, va, ta, out_t, ctx)?;
                let vb2 = coerce_numeric(e.span, vb, tb, out_t, ctx)?;
                let out = ctx.new_v(out_t);
                let op = match out_t {
                    T_I32 => Op::SubI32,
                    T_I64 => Op::SubI64,
                    T_F32 => Op::SubF32,
                    T_F64 => Op::SubF64,
                    _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad numeric promotion")),
                };
                ctx.emit(
                    VInsn { op, a: out, b: Opnd::V(va2.v), c: Opnd::V(vb2.v), imm: 0 },
                    vec![va2.v, vb2.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Mul(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                let ta = ctx.vtypes[va.v as usize];
                let tb = ctx.vtypes[vb.v as usize];
                if !(is_numeric(ta) && is_numeric(tb)) {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'*' expects numeric operands"));
                }
                let out_t = if expect == Some(T_F64) {
                    T_F64
                } else if expect == Some(T_F32) && (numeric_rank(ta) >= 2 || numeric_rank(tb) >= 2) {
                    T_F32
                } else if numeric_rank(ta) >= numeric_rank(tb) {
                    ta
                } else {
                    tb
                };
                let va2 = coerce_numeric(e.span, va, ta, out_t, ctx)?;
                let vb2 = coerce_numeric(e.span, vb, tb, out_t, ctx)?;
                let out = ctx.new_v(out_t);
                let op = match out_t {
                    T_I32 => Op::MulI32,
                    T_I64 => Op::MulI64,
                    T_F32 => Op::MulF32,
                    T_F64 => Op::MulF64,
                    _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad numeric promotion")),
                };
                ctx.emit(
                    VInsn { op, a: out, b: Opnd::V(va2.v), c: Opnd::V(vb2.v), imm: 0 },
                    vec![va2.v, vb2.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Div(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                let ta = ctx.vtypes[va.v as usize];
                let tb = ctx.vtypes[vb.v as usize];
                if !(is_numeric(ta) && is_numeric(tb)) {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'/' expects numeric operands"));
                }
                let out_t = if expect == Some(T_F64) || ta == T_F64 || tb == T_F64 {
                    T_F64
                } else {
                    T_F32
                };
                let va2 = coerce_numeric(e.span, va, ta, out_t, ctx)?;
                let vb2 = coerce_numeric(e.span, vb, tb, out_t, ctx)?;
                let out = ctx.new_v(out_t);
                let op = if out_t == T_F64 { Op::DivF64 } else { Op::DivF32 };
                ctx.emit(
                    VInsn { op, a: out, b: Opnd::V(va2.v), c: Opnd::V(vb2.v), imm: 0 },
                    vec![va2.v, vb2.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Neg(inner) => {
                let v = compile_expr(inner, env, ctx)?;
                let t = ctx.vtypes[v.v as usize];
                if !is_numeric(t) {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "unary '-' expects numeric"));
                }
                let out = ctx.new_v(t);
                let op = match t {
                    T_I32 => Op::NegI32,
                    T_I64 => Op::NegI64,
                    T_F32 => Op::NegF32,
                    T_F64 => Op::NegF64,
                    _ => return Err(CompileError::new(ErrorKind::Internal, e.span, "bad neg type")),
                };
                ctx.emit(
                    VInsn { op, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                    vec![v.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Eq(a, b) => {
                // If one side is a member access, allow it to take its type from the other side.
                let (va, vb) = match (&a.node, &b.node) {
                    (ExprKind::Member { .. }, _) => {
                        let vb = compile_expr(b, env, ctx)?;
                        let tb = ctx.vtypes[vb.v as usize];
                        let va = compile_expr_expect(a, tb, env, ctx)?;
                        (va, vb)
                    }
                    (_, ExprKind::Member { .. }) => {
                        let va = compile_expr(a, env, ctx)?;
                        let ta = ctx.vtypes[va.v as usize];
                        let vb = compile_expr_expect(b, ta, env, ctx)?;
                        (va, vb)
                    }
                    _ => (compile_expr(a, env, ctx)?, compile_expr(b, env, ctx)?),
                };
                let ta0 = ctx.vtypes[va.v as usize];
                let tb0 = ctx.vtypes[vb.v as usize];
                let (va, vb, ta) = if ta0 != tb0 && is_numeric(ta0) && is_numeric(tb0) {
                    let out_t = if numeric_rank(ta0) >= numeric_rank(tb0) { ta0 } else { tb0 };
                    let va2 = coerce_numeric(e.span, va, ta0, out_t, ctx)?;
                    let vb2 = coerce_numeric(e.span, vb, tb0, out_t, ctx)?;
                    (va2, vb2, out_t)
                } else {
                    (va, vb, ta0)
                };
                let tb = ctx.vtypes[vb.v as usize];
                // Truthiness equality: when comparing against a boolean,
                // interpret the other side via truthy/falsey semantics.
                if ta == T_BOOL && tb != T_BOOL {
                    let vbt = compile_truthy(e.span, vb, tb, ctx)?;
                    let out = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::Physeq, a: out, b: Opnd::V(va.v), c: Opnd::V(vbt.v), imm: 0 },
                        vec![va.v, vbt.v],
                        vec![out.v],
                    );
                    return Ok(out);
                }
                if tb == T_BOOL && ta != T_BOOL {
                    let vat = compile_truthy(e.span, va, ta, ctx)?;
                    let out = ctx.new_v(T_BOOL);
                    ctx.emit(
                        VInsn { op: Op::Physeq, a: out, b: Opnd::V(vat.v), c: Opnd::V(vb.v), imm: 0 },
                        vec![vat.v, vb.v],
                        vec![out.v],
                    );
                    return Ok(out);
                }
                if ta != tb {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'==' expects operands of same type"));
                }
                let out = ctx.new_v(T_BOOL);
                match ta {
                    T_BYTES => {
                        // bytes equality is structural, not physical pointer equality
                        let sig_args = vec![T_BYTES, T_BYTES]; // (bytes, bytes)
                        let fun_tid = ctx.type_ctx.intern_fun_type(T_BOOL, &sig_args); // -> bool
                        let sig_id = ctx.type_ctx.intern_sig(T_BOOL, &sig_args);

                        let vcallee = ctx.new_v(fun_tid);
                        ctx.emit(
                            VInsn { op: Op::ConstFun, a: vcallee, b: Opnd::Z, c: Opnd::Z, imm: PRELUDE_BYTES_EQ },
                            vec![],
                            vec![vcallee.v],
                        );

                        let pc = ctx.vinsns.len() as u32;
                        ctx.call_sites.push(CallSite { pc, sig_id, args: vec![va.v, vb.v] });
                        ctx.emit(
                            VInsn { op: Op::CallR, a: out, b: Opnd::V(vcallee.v), c: Opnd::Z, imm: 2 },
                            vec![vcallee.v, va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_BOOL => {
                        ctx.emit(
                            VInsn { op: Op::Physeq, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_ATOM => {
                        ctx.emit(
                            VInsn { op: Op::Physeq, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_I32 => {
                        ctx.emit(
                            VInsn { op: Op::EqI32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_I64 => {
                        ctx.emit(
                            VInsn { op: Op::EqI64, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_F32 => {
                        ctx.emit(
                            VInsn { op: Op::EqF32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_F64 => {
                        ctx.emit(
                            VInsn { op: Op::EqF64, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                            vec![va.v, vb.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    _ => Err(CompileError::new(ErrorKind::Type, e.span, "'==' not supported for this type yet")),
                }
            }
            ExprKind::Ne(a, b) => {
                // a != b  ==  !(a == b)
                let t = compile_expr(
                    &crate::ast::Spanned::new(ExprKind::Eq(a.clone(), b.clone()), e.span),
                    env,
                    ctx,
                )?;
                let out = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(t.v), c: Opnd::Z, imm: 0 },
                    vec![t.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Lt(a, b) => {
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[va.v as usize] != T_I32 || ctx.vtypes[vb.v as usize] != T_I32 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'<' currently only supported for i32"));
                }
                let out = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: out, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                    vec![va.v, vb.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Gt(a, b) => {
                // a > b  ==  b < a
                let vb = compile_expr(b, env, ctx)?;
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != T_I32 || ctx.vtypes[vb.v as usize] != T_I32 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'>' currently only supported for i32"));
                }
                let out = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: out, b: Opnd::V(vb.v), c: Opnd::V(va.v), imm: 0 },
                    vec![vb.v, va.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Le(a, b) => {
                // a <= b  ==  !(b < a)
                let vb = compile_expr(b, env, ctx)?;
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != T_I32 || ctx.vtypes[vb.v as usize] != T_I32 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'<=' currently only supported for i32"));
                }
                let t = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: t, b: Opnd::V(vb.v), c: Opnd::V(va.v), imm: 0 },
                    vec![vb.v, va.v],
                    vec![t.v],
                );
                let out = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(t.v), c: Opnd::Z, imm: 0 },
                    vec![t.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::Ge(a, b) => {
                // a >= b  ==  !(a < b)
                let va = compile_expr(a, env, ctx)?;
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[va.v as usize] != T_I32 || ctx.vtypes[vb.v as usize] != T_I32 {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'>=' currently only supported for i32"));
                }
                let t = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::LtI32, a: t, b: Opnd::V(va.v), c: Opnd::V(vb.v), imm: 0 },
                    vec![va.v, vb.v],
                    vec![t.v],
                );
                let out = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: out, b: Opnd::V(t.v), c: Opnd::Z, imm: 0 },
                    vec![t.v],
                    vec![out.v],
                );
                Ok(out)
            }
            ExprKind::And(a, b) => {
                // Short-circuit:
                //   if(!a) res=false else res=b
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
                }

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![va.v],
                    vec![],
                );

                // false path
                let v_false = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::ConstBool, a: v_false, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_false.v],
                );
                let v_res = ctx.new_v(T_BOOL);
                ctx.allow_multi_def[v_res.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_false.v), c: Opnd::Z, imm: 0 },
                    vec![v_false.v],
                    vec![v_res.v],
                );
                let jmp_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                // rhs path
                let rhs_pc = ctx.vinsns.len() as u32;
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[vb.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'&&' expects bool operands"));
                }
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(vb.v), c: Opnd::Z, imm: 0 },
                    vec![vb.v],
                    vec![v_res.v],
                );
                let join_pc = ctx.vinsns.len() as u32;

                // Patch jumps.
                let delta_to_rhs: i32 = (rhs_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_rhs as u32;
                ctx.vinsns[jmp_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::Or(a, b) => {
                // Short-circuit:
                //   if(a) res=true else res=b
                let va = compile_expr(a, env, ctx)?;
                if ctx.vtypes[va.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
                }

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![va.v],
                    vec![],
                );

                // false path: compute b
                let vb = compile_expr(b, env, ctx)?;
                if ctx.vtypes[vb.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "'||' expects bool operands"));
                }
                let v_res = ctx.new_v(T_BOOL);
                ctx.allow_multi_def[v_res.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(vb.v), c: Opnd::Z, imm: 0 },
                    vec![vb.v],
                    vec![v_res.v],
                );
                let jmp_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: va, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                // true path
                let true_pc = ctx.vinsns.len() as u32;
                let v_true = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::ConstBool, a: v_true, b: Opnd::Z, c: Opnd::Z, imm: 1 },
                    vec![],
                    vec![v_true.v],
                );
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_true.v), c: Opnd::Z, imm: 0 },
                    vec![v_true.v],
                    vec![v_res.v],
                );
                let join_pc = ctx.vinsns.len() as u32;

                // Patch jumps.
                let delta_to_true: i32 = (true_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_true as u32;
                ctx.vinsns[jmp_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::If { cond, then_br, else_br } => {
                let v_cond = compile_expr(cond, env, ctx)?;
                if ctx.vtypes[v_cond.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, cond.span, "if condition must be bool"));
                }

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![v_cond.v],
                    vec![],
                );

                // else block
                let v_else = compile_expr(else_br, env, ctx)?;

                // then block (type must match else)
                let v_then_start_pc = ctx.vinsns.len() as u32; // after else code + mov + jmp we compute; placeholder for type checking
                let _ = v_then_start_pc;

                let v_res_tid = ctx.vtypes[v_else.v as usize];
                let v_res = ctx.new_v(v_res_tid);
                ctx.allow_multi_def[v_res.v as usize] = true; // join var multi-def
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_else.v), c: Opnd::Z, imm: 0 },
                    vec![v_else.v],
                    vec![v_res.v],
                );

                let jmp_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                // then label
                let then_pc = ctx.vinsns.len() as u32;
                let v_then = compile_expr(then_br, env, ctx)?;
                if ctx.vtypes[v_then.v as usize] != v_res_tid {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "if branches must return same type"));
                }
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_then.v), c: Opnd::Z, imm: 0 },
                    vec![v_then.v],
                    vec![v_res.v],
                );

                let join_pc = ctx.vinsns.len() as u32;

                // Patch jumps.
                let delta_to_then: i32 = (then_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_then as u32;
                ctx.vinsns[jmp_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::Block { stmts, expr } => {
                let mut scoped = env.clone();
                for s in stmts {
                    compile_stmt(s, &mut scoped, ctx)?;
                }
                compile_expr(expr, &scoped, ctx)
            }
            ExprKind::Try { body, catch_name, catch_body } => {
                let v_exc = ctx.new_v(T_DYNAMIC);
                let try_pc = ctx.vinsns.len() as u32;
                // b=0: catch everything (back-compat); b=1: traps only (do not catch thrown/assert)
                let trap_only = if catch_name.is_none() { Opnd::V(1) } else { Opnd::Z };
                ctx.emit(
                    VInsn { op: Op::Try, a: v_exc, b: trap_only, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_exc.v],
                );

                let v_body = compile_expr(body, env, ctx)?;
                let tid = ctx.vtypes[v_body.v as usize];
                let v_res = ctx.new_v(tid);
                ctx.allow_multi_def[v_res.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_body.v), c: Opnd::Z, imm: 0 },
                    vec![v_body.v],
                    vec![v_res.v],
                );

                ctx.emit(
                    VInsn { op: Op::EndTry, a: v_exc, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                let jmp_over_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_exc, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                let catch_pc = ctx.vinsns.len() as u32;
                let mut catch_env = env.clone();
                if let Some(n) = catch_name {
                    catch_env.insert(n.clone(), v_exc);
                }
                let v_catch = compile_expr(catch_body, &catch_env, ctx)?;
                if ctx.vtypes[v_catch.v as usize] != tid {
                    return Err(CompileError::new(ErrorKind::Type, e.span, "try and catch must return same type"));
                }
                ctx.emit(
                    VInsn { op: Op::Mov, a: v_res, b: Opnd::V(v_catch.v), c: Opnd::Z, imm: 0 },
                    vec![v_catch.v],
                    vec![v_res.v],
                );

                let join_pc = ctx.vinsns.len() as u32;
                let delta_to_catch: i32 = (catch_pc as i32) - ((try_pc + 1) as i32);
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_over_pc + 1) as i32);
                ctx.vinsns[try_pc as usize].imm = delta_to_catch as u32;
                ctx.vinsns[jmp_over_pc as usize].imm = delta_to_join as u32;

                Ok(v_res)
            }
            ExprKind::Null => {
                let v = ctx.new_v(T_DYNAMIC);
                ctx.emit(VInsn { op: Op::ConstNull, a: v, b: Opnd::Z, c: Opnd::Z, imm: 0 }, vec![], vec![v.v]);
                Ok(v)
            }
            ExprKind::ArrayLit(elems) => {
                if elems.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "empty array literal requires a type annotation (not implemented yet)",
                    ));
                }
                let mut vs: Vec<VTmp> = Vec::with_capacity(elems.len());
                for e in elems {
                    vs.push(compile_expr(e, env, ctx)?);
                }
                let t0 = ctx.vtypes[vs[0].v as usize];
                for v in &vs[1..] {
                    if ctx.vtypes[v.v as usize] != t0 {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "array literal elements must have same type",
                        ));
                    }
                }
                let arr_tid = match t0 {
                    T_I32 => T_ARRAY_I32,
                    T_BYTES => T_ARRAY_BYTES,
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            e.span,
                            "array literal only supports i32/bytes elements for now",
                        ))
                    }
                };

                let v_len = ctx.new_v(T_I32);
                ctx.emit(
                    VInsn { op: Op::ConstI32, a: v_len, b: Opnd::Z, c: Opnd::Z, imm: vs.len() as u32 },
                    vec![],
                    vec![v_len.v],
                );
                let v_arr = ctx.new_v(arr_tid);
                ctx.emit(
                    VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_len.v), c: Opnd::Z, imm: 0 },
                    vec![v_len.v],
                    vec![v_arr.v],
                );
                for (i, part) in vs.iter().enumerate() {
                    let v_i = ctx.new_v(T_I32);
                    ctx.emit(
                        VInsn { op: Op::ConstI32, a: v_i, b: Opnd::Z, c: Opnd::Z, imm: i as u32 },
                        vec![],
                        vec![v_i.v],
                    );
                    ctx.emit(
                        VInsn { op: Op::ArraySet, a: *part, b: Opnd::V(v_arr.v), c: Opnd::V(v_i.v), imm: 0 },
                        vec![part.v, v_arr.v, v_i.v],
                        vec![],
                    );
                }
                Ok(v_arr)
            }
            ExprKind::TupleLit(elems) => {
                let mut vs: Vec<VTmp> = Vec::with_capacity(elems.len());
                let mut elem_tids: Vec<u32> = Vec::with_capacity(elems.len());
                for e in elems {
                    let v = compile_expr(e, env, ctx)?;
                    elem_tids.push(ctx.vtypes[v.v as usize]);
                    vs.push(v);
                }

                let tup_tid = ctx.type_ctx.intern_tuple_type(&elem_tids);
                let v_tup = ctx.new_v(tup_tid);
                ctx.emit(
                    VInsn { op: Op::ObjNew, a: v_tup, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_tup.v],
                );

                for (i, v) in vs.iter().enumerate() {
                    let key = i.to_string();
                    let atom_id = if let Some(&id) = ctx.atom_ids.get(&key) {
                        id
                    } else {
                        let id = ctx.atoms.len() as u32;
                        ctx.atoms.push(key.as_bytes().to_vec());
                        ctx.atom_ids.insert(key, id);
                        id
                    };
                    ctx.emit(
                        VInsn { op: Op::ObjSetAtom, a: *v, b: Opnd::V(v_tup.v), c: Opnd::Z, imm: atom_id },
                        vec![v.v, v_tup.v],
                        vec![],
                    );
                }

                Ok(v_tup)
            }
            ExprKind::ObjLit(fields) => {
                let out_tid = match expect {
                    Some(et) => ctx
                        .type_ctx
                        .types
                        .get(et as usize)
                        .is_some_and(|te| te.kind == TypeKind::Object)
                        .then_some(et)
                        .unwrap_or(T_OBJECT),
                    None => T_OBJECT,
                };

                let v_obj = ctx.new_v(out_tid);
                ctx.emit(
                    VInsn { op: Op::ObjNew, a: v_obj, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_obj.v],
                );
                for (k, ve) in fields {
                    let v_val = compile_expr(ve, env, ctx)?;
                    let atom_id = if let Some(&id) = ctx.atom_ids.get(k) {
                        id
                    } else {
                        let id = ctx.atoms.len() as u32;
                        ctx.atoms.push(k.as_bytes().to_vec());
                        ctx.atom_ids.insert(k.clone(), id);
                        id
                    };
                    ctx.emit(
                        VInsn { op: Op::ObjSetAtom, a: v_val, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                        vec![v_val.v, v_obj.v],
                        vec![],
                    );
                }
                Ok(v_obj)
            }
            ExprKind::Index { base, index } => {
                let v_base = compile_expr(base, env, ctx)?;
                let v_idx = compile_expr(index, env, ctx)?;
                let v_idx = match ctx.vtypes[v_idx.v as usize] {
                    T_I32 => v_idx,
                    T_I8 | T_I16 => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::Mov, a: out, b: Opnd::V(v_idx.v), c: Opnd::Z, imm: 0 },
                            vec![v_idx.v],
                            vec![out.v],
                        );
                        out
                    }
                    T_I64 => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::I32FromI64, a: out, b: Opnd::V(v_idx.v), c: Opnd::Z, imm: 0 },
                            vec![v_idx.v],
                            vec![out.v],
                        );
                        out
                    }
                    T_DYNAMIC => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::FromDynI32, a: out, b: Opnd::V(v_idx.v), c: Opnd::Z, imm: 0 },
                            vec![v_idx.v],
                            vec![out.v],
                        );
                        out
                    }
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            index.span,
                            "index must be an integer",
                        ))
                    }
                };
                match ctx.vtypes[v_base.v as usize] {
                    T_ARRAY_I32 => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::ArrayGet, a: out, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_base.v, v_idx.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    T_BYTES => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::BytesGetU8, a: out, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_base.v, v_idx.v],
                            vec![out.v],
                        );
                        Ok(out)
                    }
                    _ => Err(CompileError::new(
                        ErrorKind::Type,
                        e.span,
                        "indexing only supported for Array<i32> and bytes for now",
                    )),
                }
            }
            ExprKind::New { proto, args } => {
                let v_proto = compile_expr(proto, env, ctx)?;
                let t_proto = ctx.vtypes[v_proto.v as usize];
                let te = ctx
                    .type_ctx
                    .types
                    .get(t_proto as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, proto.span, "bad prototype type id"))?;
                if te.kind != TypeKind::Object {
                    return Err(CompileError::new(ErrorKind::Type, proto.span, "new expects an Object prototype"));
                }

                let self_tid = match expect {
                    Some(et) if et == T_OBJECT => T_OBJECT,
                    Some(et) => {
                        let te = ctx
                            .type_ctx
                            .types
                            .get(et as usize)
                            .ok_or_else(|| CompileError::new(ErrorKind::Internal, e.span, "bad expected type id"))?;
                        if te.kind != TypeKind::Object {
                            t_proto
                        } else if et != t_proto {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                e.span,
                                "new: expected object type does not match prototype type",
                            ));
                        } else {
                            et
                        }
                    }
                    None => t_proto,
                };

                let v_self = ctx.new_v(self_tid);
                ctx.emit(
                    VInsn { op: Op::ObjNew, a: v_self, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![v_self.v],
                );

                // self.__proto__ = proto
                ctx.emit(
                    VInsn { op: Op::ObjSetAtom, a: v_proto, b: Opnd::V(v_self.v), c: Opnd::Z, imm: ATOM___PROTO__ },
                    vec![v_proto.v, v_self.v],
                    vec![],
                );

                // if proto has init, call init(self, ...args)
                let v_has = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::ObjHasAtom, a: v_has, b: Opnd::V(v_proto.v), c: Opnd::Z, imm: ATOM_INIT },
                    vec![v_proto.v],
                    vec![v_has.v],
                );
                let v_no = ctx.new_v(T_BOOL);
                ctx.emit(
                    VInsn { op: Op::NotBool, a: v_no, b: Opnd::V(v_has.v), c: Opnd::Z, imm: 0 },
                    vec![v_has.v],
                    vec![v_no.v],
                );

                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: v_no, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![v_no.v],
                    vec![],
                );

                // Build expected init function type: (Object, Dynamic, ...) -> Object.
                let mut sig_args: Vec<u32> = Vec::with_capacity(1 + args.len());
                sig_args.push(T_OBJECT);
                sig_args.extend(std::iter::repeat(T_DYNAMIC).take(args.len()));
                let fun_tid = ctx.type_ctx.intern_fun_type(T_OBJECT, &sig_args);
                let sig_id = ctx.type_ctx.intern_sig(T_OBJECT, &sig_args);

                let v_init = ctx.new_v(fun_tid);
                ctx.emit(
                    VInsn { op: Op::ObjGetAtom, a: v_init, b: Opnd::V(v_proto.v), c: Opnd::Z, imm: ATOM_INIT },
                    vec![v_proto.v],
                    vec![v_init.v],
                );

                // Build call args: (Object, Dynamic, ...) -> Object.
                // Marshal `self` into an Object-typed vreg when needed.
                let v_self_obj = if self_tid == T_OBJECT {
                    v_self
                } else {
                    let v = ctx.new_v(T_OBJECT);
                    ctx.emit(
                        VInsn { op: Op::Mov, a: v, b: Opnd::V(v_self.v), c: Opnd::Z, imm: 0 },
                        vec![v_self.v],
                        vec![v.v],
                    );
                    v
                };

                let mut arg_vs: Vec<u32> = Vec::with_capacity(1 + args.len());
                arg_vs.push(v_self_obj.v);
                for a in args {
                    let v = compile_expr(a, env, ctx)?;
                    let vd = if ctx.vtypes[v.v as usize] == T_DYNAMIC {
                        v
                    } else {
                        let out = ctx.new_v(T_DYNAMIC);
                        ctx.emit(
                            VInsn { op: Op::ToDyn, a: out, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 },
                            vec![v.v],
                            vec![out.v],
                        );
                        out
                    };
                    arg_vs.push(vd.v);
                }

                let v_tmp_ret = ctx.new_v(T_OBJECT);
                let pc = ctx.vinsns.len() as u32;
                ctx.call_sites.push(CallSite { pc, sig_id, args: arg_vs.clone() });

                let mut uses: Vec<u32> = Vec::with_capacity(1 + arg_vs.len());
                uses.push(v_init.v);
                uses.extend(arg_vs.iter().copied());
                ctx.emit(
                    VInsn { op: Op::CallR, a: v_tmp_ret, b: Opnd::V(v_init.v), c: Opnd::Z, imm: arg_vs.len() as u32 },
                    uses,
                    vec![v_tmp_ret.v],
                );

                // Patch jump over the init call when missing.
                let join_pc = ctx.vinsns.len() as u32;
                let delta_to_join: i32 = (join_pc as i32) - ((jmp_if_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_join as u32;

                Ok(v_self)
            }
            ExprKind::Match { .. } => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "match is not supported in the AST backend yet (use --backend ir)",
            )),
            ExprKind::TypeApp { .. } => Err(CompileError::new(
                ErrorKind::Type,
                e.span,
                "templates are not supported in the AST backend yet (use --backend ir)",
            )),
            ExprKind::Fn { .. } => Err(CompileError::new(ErrorKind::Type, e.span, "fn literal not supported here yet")),
        }
    }

    fn compile_stmt(s: &Stmt, env: &mut HashMap<String, VTmp>, ctx: &mut CompileCtx) -> Result<(), CompileError> {
        match &s.node {
            StmtKind::ImportModule { .. } | StmtKind::ImportFrom { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "modules are not supported in the AST backend yet (use --backend ir)",
            )),
            StmtKind::Prototype { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "prototype must be expanded before AST backend compilation",
            )),
            StmtKind::Let {
                exported,
                name,
                type_params,
                ty,
                expr,
            } => {
                if *exported {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "modules are not supported in the AST backend yet (use --backend ir)",
                    ));
                }
                if !type_params.is_empty() {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "templates are not supported in the AST backend yet (use --backend ir)",
                    ));
                }
                if env.contains_key(name) {
                    return Err(CompileError::new(
                        ErrorKind::Name,
                        s.span,
                        format!("duplicate let binding '{}'", name),
                    ));
                }
                match ty {
                    None => {
                        if matches!(expr.node, ExprKind::Fn { .. }) {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                expr.span,
                                "fn literal requires a type annotation on its let binding (function type inference is not implemented yet).\nhelp: `let f: (I32, I32) -> I32 = fn(x, y) { ... };` (required for recursion).",
                            ));
                        }
                        let v = compile_expr(expr, env, ctx)?;
                        env.insert(name.clone(), v);
                        Ok(())
                    }
                    Some(ty) => {
                        if let ExprKind::Fn { params, body, tail } = &expr.node {
                            let fun_tid = ctx.type_ctx.resolve_ty(ty)?;
                            let te = ctx
                                .type_ctx
                                .types
                                .get(fun_tid as usize)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, ty.span, "bad function type id"))?;
                            if te.kind != TypeKind::Function {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    ty.span,
                                    "fn literal requires a function type annotation",
                                ));
                            }

                            let sig_id = te.p0 as usize;
                            let sig = ctx
                                .type_ctx
                                .sigs
                                .get(sig_id)
                                .ok_or_else(|| CompileError::new(ErrorKind::Internal, ty.span, "bad fun sig id"))?
                                .clone();

                            if params.len() != sig.args.len() {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    expr.span,
                                    "function literal arity does not match annotated type",
                                ));
                            }
                            for (i, (_pn, pty)) in params.iter().enumerate() {
                                if let Some(pt) = pty {
                                    let ptid = ctx.type_ctx.resolve_ty(pt)?;
                                    if ptid != sig.args[i] {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            pt.span,
                                            "function parameter type does not match annotated type",
                                        ));
                                    }
                                }
                            }

                            // Build a very small typed function (no captures) and reference it via CONST_FUN.
                            let mut reg_types: Vec<u32> = sig.args.clone();
                            let mut insns: Vec<Insn> = Vec::new();
                            let mut param_regs: HashMap<String, u8> = HashMap::new();
                            for (i, (pn, _)) in params.iter().enumerate() {
                                param_regs.insert(pn.clone(), i as u8);
                            }

                            fn alloc_reg(reg_types: &mut Vec<u32>, tid: u32, span: Span) -> Result<u8, CompileError> {
                                if reg_types.len() >= 256 {
                                    return Err(CompileError::new(
                                        ErrorKind::Codegen,
                                        span,
                                        "register allocation exceeded 256 regs",
                                    ));
                                }
                                reg_types.push(tid);
                                Ok((reg_types.len() - 1) as u8)
                            }

                            fn compile_small_expr(
                                e: &Expr,
                                param_regs: &HashMap<String, u8>,
                                out_reg_types: &mut Vec<u32>,
                                insns: &mut Vec<Insn>,
                            ) -> Result<(u8, u32), CompileError> {
                                match &e.node {
                                    ExprKind::Var(n) => {
                                        let r = *param_regs.get(n).ok_or_else(|| {
                                            CompileError::new(ErrorKind::Name, e.span, format!("unknown variable '{}'", n))
                                        })?;
                                        Ok((r, out_reg_types[r as usize]))
                                    }
                                    ExprKind::I32Lit(x) => {
                                        let r = alloc_reg(out_reg_types, T_I32, e.span)?;
                                        insns.push(Insn { op: Op::ConstI32 as u8, a: r, b: 0, c: 0, imm: *x as u32 });
                                        Ok((r, T_I32))
                                    }
                                    ExprKind::Index { base, index } => {
                                        let (rb, tb) = compile_small_expr(base, param_regs, out_reg_types, insns)?;
                                        let (ri, ti) = compile_small_expr(index, param_regs, out_reg_types, insns)?;
                                        if ti != T_I32 {
                                            return Err(CompileError::new(ErrorKind::Type, index.span, "index must be i32"));
                                        }
                                        match tb {
                                            T_ARRAY_I32 => {
                                                let ro = alloc_reg(out_reg_types, T_I32, e.span)?;
                                                insns.push(Insn { op: Op::ArrayGet as u8, a: ro, b: rb, c: ri, imm: 0 });
                                                Ok((ro, T_I32))
                                            }
                                            T_BYTES => {
                                                let ro = alloc_reg(out_reg_types, T_I32, e.span)?;
                                                insns.push(Insn { op: Op::BytesGetU8 as u8, a: ro, b: rb, c: ri, imm: 0 });
                                                Ok((ro, T_I32))
                                            }
                                            _ => Err(CompileError::new(
                                                ErrorKind::Type,
                                                e.span,
                                                "indexing not supported for this type",
                                            )),
                                        }
                                    }
                                    _ => Err(CompileError::new(
                                        ErrorKind::Type,
                                        e.span,
                                        "unsupported expression in fn literal (MVP)",
                                    )),
                                }
                            }

                            // Choose return expression: first explicit return, else tail.
                            let mut ret_expr: Option<&Expr> = None;
                            for st in body {
                                match &st.node {
                                    StmtKind::Return { expr: Some(e) } => {
                                        if ret_expr.is_some() {
                                            return Err(CompileError::new(
                                                ErrorKind::Type,
                                                st.span,
                                                "multiple returns in fn literal not supported yet",
                                            ));
                                        }
                                        ret_expr = Some(e);
                                    }
                                    StmtKind::Return { expr: None } => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            st.span,
                                            "return; without value not supported",
                                        ))
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            st.span,
                                            "only 'return <expr>;' supported in fn literal body for now",
                                        ))
                                    }
                                }
                            }
                            let tail_expr = tail.as_deref();
                            let re = ret_expr
                                .or(tail_expr)
                                .ok_or_else(|| CompileError::new(ErrorKind::Type, expr.span, "fn literal has no return value"))?;

                            let (rr, rt) = compile_small_expr(re, &param_regs, &mut reg_types, &mut insns)?;
                            if rt != sig.ret_type {
                                return Err(CompileError::new(
                                    ErrorKind::Type,
                                    re.span,
                                    "fn literal return type does not match annotation",
                                ));
                            }
                            insns.push(Insn { op: Op::Ret as u8, a: rr, b: 0, c: 0, imm: 0 });

                            // Logical index: 0=native, 1..4=prelude, 5=init, 6+=nested
                            let fn_index = PRELUDE_FUN_COUNT + 2u32 + (ctx.nested_funcs.len() as u32);
                            ctx.nested_funcs.push(Function { reg_types, cap_start: 0, insns });

                            let v_fun = ctx.new_v(fun_tid);
                            ctx.emit(
                                VInsn { op: Op::ConstFun, a: v_fun, b: Opnd::Z, c: Opnd::Z, imm: fn_index },
                                vec![],
                                vec![v_fun.v],
                            );
                            env.insert(name.clone(), v_fun);
                            return Ok(());
                        }

                        let expected_tid = ctx.type_ctx.resolve_ty(ty)?;

                        // Empty array literals require an expected type. Allow them only in typed
                        // `let` initializers for now.
                        if let ExprKind::ArrayLit(elems) = &expr.node {
                            if elems.is_empty() {
                                match expected_tid {
                                    T_ARRAY_I32 | T_ARRAY_BYTES => {
                                        let v_len = ctx.new_v(T_I32);
                                        ctx.emit(
                                            VInsn { op: Op::ConstI32, a: v_len, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                                            vec![],
                                            vec![v_len.v],
                                        );
                                        let v_arr = ctx.new_v(expected_tid);
                                        ctx.emit(
                                            VInsn { op: Op::ArrayNew, a: v_arr, b: Opnd::V(v_len.v), c: Opnd::Z, imm: 0 },
                                            vec![v_len.v],
                                            vec![v_arr.v],
                                        );
                                        env.insert(name.clone(), v_arr);
                                        return Ok(());
                                    }
                                    _ => {
                                        return Err(CompileError::new(
                                            ErrorKind::Type,
                                            expr.span,
                                            "empty array literal requires an Array<i32> or Array<bytes> annotation",
                                        ))
                                    }
                                }
                            }
                        }

                        let v = compile_expr_expect(expr, Some(expected_tid), env, ctx)?;
                        if ctx.vtypes[v.v as usize] != expected_tid {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                s.span,
                                "let annotation does not match expression type",
                            ));
                        }
                        env.insert(name.clone(), v);
                        Ok(())
                    }
                }
            }
            StmtKind::Assign { name, expr } => {
                let dst = env
                    .get(name)
                    .copied()
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, format!("unknown variable '{}'", name)))?;
                let rhs = compile_expr(expr, env, ctx)?;
                let td = ctx.vtypes[dst.v as usize];
                let tr = ctx.vtypes[rhs.v as usize];
                if td != tr {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        format!("assignment to '{}' changes type", name),
                    ));
                }
                ctx.allow_multi_def[dst.v as usize] = true;
                ctx.emit(
                    VInsn { op: Op::Mov, a: dst, b: Opnd::V(rhs.v), c: Opnd::Z, imm: 0 },
                    vec![rhs.v],
                    vec![dst.v],
                );
                Ok(())
            }
            StmtKind::MemberAssign { base, name, expr } => {
                let v_obj = compile_expr(base, env, ctx)?;
                let base_tid = ctx.vtypes[v_obj.v as usize];
                let te = ctx
                    .type_ctx
                    .types
                    .get(base_tid as usize)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, base.span, "bad member assignment base type id"))?;
                if te.kind != TypeKind::Object {
                    return Err(CompileError::new(
                        ErrorKind::Type,
                        base.span,
                        "member assignment target must be Object",
                    ));
                }
                if ctx.type_ctx.is_tuple_type(base_tid) {
                    return Err(CompileError::new(ErrorKind::Type, base.span, "tuples are immutable"));
                }
                let v_val = compile_expr(expr, env, ctx)?;
                let atom_id = if let Some(&id) = ctx.atom_ids.get(name) {
                    id
                } else {
                    let id = ctx.atoms.len() as u32;
                    ctx.atoms.push(name.as_bytes().to_vec());
                    ctx.atom_ids.insert(name.clone(), id);
                    id
                };
                ctx.emit(
                    VInsn { op: Op::ObjSetAtom, a: v_val, b: Opnd::V(v_obj.v), c: Opnd::Z, imm: atom_id },
                    vec![v_val.v, v_obj.v],
                    vec![],
                );
                Ok(())
            }
            StmtKind::While { cond, body } => {
                let cond_pc = ctx.vinsns.len() as u32;
                let v_cond = compile_expr(cond, env, ctx)?;
                if ctx.vtypes[v_cond.v as usize] != T_BOOL {
                    return Err(CompileError::new(ErrorKind::Type, cond.span, "while condition must be bool"));
                }

                // if(cond) jump to body, else jump to exit.
                let jmp_if_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::JmpIf, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![v_cond.v],
                    vec![],
                );
                let jmp_exit_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );

                let body_pc = ctx.vinsns.len() as u32;
                ctx.loop_stack.push(LoopCtx { anchor: v_cond, breaks: Vec::new(), continues: Vec::new() });
                let mut scoped = env.clone();
                for bs in body {
                    compile_stmt(bs, &mut scoped, ctx)?;
                }
                let loop_ctx = ctx.loop_stack.pop().expect("loop ctx");

                let jmp_back_pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: v_cond, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                let exit_pc = ctx.vinsns.len() as u32;

                let delta_to_body: i32 = (body_pc as i32) - ((jmp_if_pc + 1) as i32);
                let delta_to_exit: i32 = (exit_pc as i32) - ((jmp_exit_pc + 1) as i32);
                let delta_to_cond: i32 = (cond_pc as i32) - ((jmp_back_pc + 1) as i32);
                ctx.vinsns[jmp_if_pc as usize].imm = delta_to_body as u32;
                ctx.vinsns[jmp_exit_pc as usize].imm = delta_to_exit as u32;
                ctx.vinsns[jmp_back_pc as usize].imm = delta_to_cond as u32;

                // Patch break/continue sites inside the body.
                for pc in loop_ctx.breaks {
                    let d: i32 = (exit_pc as i32) - ((pc + 1) as i32);
                    ctx.vinsns[pc as usize].imm = d as u32;
                }
                for pc in loop_ctx.continues {
                    let d: i32 = (cond_pc as i32) - ((pc + 1) as i32);
                    ctx.vinsns[pc as usize].imm = d as u32;
                }
                Ok(())
            }
            StmtKind::Break => {
                let anchor = ctx
                    .loop_stack
                    .last()
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "break used outside of while"))?
                    .anchor;
                let pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: anchor, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                ctx.loop_stack.last_mut().expect("loop ctx").breaks.push(pc);
                Ok(())
            }
            StmtKind::Continue => {
                let anchor = ctx
                    .loop_stack
                    .last()
                    .ok_or_else(|| CompileError::new(ErrorKind::Name, s.span, "continue used outside of while"))?
                    .anchor;
                let pc = ctx.vinsns.len() as u32;
                ctx.emit(
                    VInsn { op: Op::Jmp, a: anchor, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![],
                    vec![],
                );
                ctx.loop_stack.last_mut().expect("loop ctx").continues.push(pc);
                Ok(())
            }
            StmtKind::Throw { expr } => {
                let v = compile_expr(expr, env, ctx)?;
                let payload = if ctx.vtypes[v.v as usize] == T_DYNAMIC {
                    v
                } else {
                    let vd = ctx.new_v(T_DYNAMIC);
                    ctx.emit(VInsn { op: Op::ToDyn, a: vd, b: Opnd::V(v.v), c: Opnd::Z, imm: 0 }, vec![v.v], vec![vd.v]);
                    vd
                };
                ctx.emit(
                    VInsn { op: Op::Throw, a: payload, b: Opnd::Z, c: Opnd::Z, imm: 0 },
                    vec![payload.v],
                    vec![],
                );
                Ok(())
            }
            StmtKind::IndexAssign { base, index, expr } => {
                let v_base = compile_expr(base, env, ctx)?;
                let v_idx = compile_expr(index, env, ctx)?;
                let v_val = compile_expr(expr, env, ctx)?;
                let v_idx = match ctx.vtypes[v_idx.v as usize] {
                    T_I32 => v_idx,
                    T_I8 | T_I16 => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::Mov, a: out, b: Opnd::V(v_idx.v), c: Opnd::Z, imm: 0 },
                            vec![v_idx.v],
                            vec![out.v],
                        );
                        out
                    }
                    T_I64 => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::I32FromI64, a: out, b: Opnd::V(v_idx.v), c: Opnd::Z, imm: 0 },
                            vec![v_idx.v],
                            vec![out.v],
                        );
                        out
                    }
                    T_DYNAMIC => {
                        let out = ctx.new_v(T_I32);
                        ctx.emit(
                            VInsn { op: Op::FromDynI32, a: out, b: Opnd::V(v_idx.v), c: Opnd::Z, imm: 0 },
                            vec![v_idx.v],
                            vec![out.v],
                        );
                        out
                    }
                    _ => {
                        return Err(CompileError::new(
                            ErrorKind::Type,
                            index.span,
                            "index must be an integer",
                        ))
                    }
                };
                match ctx.vtypes[v_base.v as usize] {
                    T_ARRAY_I32 => {
                        if ctx.vtypes[v_val.v as usize] != T_I32 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                expr.span,
                                "Array<i32> index assignment requires i32 value",
                            ));
                        }
                        ctx.emit(
                            VInsn { op: Op::ArraySet, a: v_val, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_val.v, v_base.v, v_idx.v],
                            vec![],
                        );
                        Ok(())
                    }
                    T_BYTES => {
                        if ctx.vtypes[v_val.v as usize] != T_I32 {
                            return Err(CompileError::new(
                                ErrorKind::Type,
                                expr.span,
                                "bytes index assignment requires i32 value",
                            ));
                        }
                        ctx.emit(
                            VInsn { op: Op::BytesSetU8, a: v_val, b: Opnd::V(v_base.v), c: Opnd::V(v_idx.v), imm: 0 },
                            vec![v_val.v, v_base.v, v_idx.v],
                            vec![],
                        );
                        Ok(())
                    }
                    _ => Err(CompileError::new(
                        ErrorKind::Type,
                        s.span,
                        "index assignment only supported for Array<i32> and bytes",
                    )),
                }
            }
            StmtKind::Return { .. } => Err(CompileError::new(
                ErrorKind::Type,
                s.span,
                "return only allowed in fn bodies (not implemented yet)",
            )),
            StmtKind::Expr { expr } => {
                let _ = compile_expr(expr, env, ctx)?;
                Ok(())
            }
        }
    }

    struct LoopCtx {
        anchor: VTmp,
        breaks: Vec<u32>,
        continues: Vec<u32>,
    }

    let mut const_bytes: Vec<Vec<u8>> = Vec::new();
    let mut const_i64: Vec<i64> = Vec::new();
    let mut const_f64: Vec<f64> = Vec::new();
    let mut vtypes: Vec<u32> = Vec::new();
    let mut next_v: u32 = 0;
    let mut allow_multi_def: Vec<bool> = Vec::new();
    let mut call_sites: Vec<CallSite> = Vec::new();
    let mut vinsns: Vec<VInsn> = Vec::new();
    let mut infos: Vec<InstrInfo> = Vec::new();

    let mut loop_stack: Vec<LoopCtx> = Vec::new();
    // Reserve well-known atoms at stable ids (see `ATOM_*` constants).
    let mut atoms: Vec<Vec<u8>> = vec![b"__proto__".to_vec(), b"init".to_vec()];
    let mut atom_ids: HashMap<String, u32> = HashMap::from([
        ("__proto__".to_string(), ATOM___PROTO__),
        ("init".to_string(), ATOM_INIT),
    ]);

    let mut ctx = CompileCtx {
        type_ctx: &mut type_ctx,
        nested_funcs: &mut nested_funcs,
        const_bytes: &mut const_bytes,
        const_i64: &mut const_i64,
        const_f64: &mut const_f64,
        vtypes: &mut vtypes,
        allow_multi_def: &mut allow_multi_def,
        next_v: &mut next_v,
        vinsns: &mut vinsns,
        infos: &mut infos,
        call_sites: &mut call_sites,
        loop_stack: &mut loop_stack,
        atoms: &mut atoms,
        atom_ids: &mut atom_ids,
    };

    for s in &p.stmts {
        compile_stmt(s, &mut env, &mut ctx)?;
    }

    // final expr must be bytes for our MVP
    let outv = compile_expr(&p.expr, &env, &mut ctx)?;
    if ctx.vtypes[outv.v as usize] != T_BYTES {
        return Err(CompileError::new(
            ErrorKind::Type,
            p.expr.span,
            "program must evaluate to bytes",
        ));
    }
    ctx.emit(VInsn { op: Op::Ret, a: outv, b: Opnd::Z, c: Opnd::Z, imm: 0 }, vec![outv.v], vec![]);

    // End compilation borrows; emit phase reads the finalized vectors.
    drop(ctx);

    // Type-aware LSRA with opt-in multi-def per join dest.
    let num_vregs = next_v;
    let mut type_ids: Vec<u32> = vtypes.iter().copied().collect();
    type_ids.sort();
    type_ids.dedup();

    let mut vreg_to_reg: Vec<u8> = vec![0; num_vregs as usize];
    let mut reg_types: Vec<u32> = Vec::new();

    let mut base: u16 = 0;
    for tid in type_ids {
        let mut globals: Vec<u32> = Vec::new();
        for (gv, &t) in vtypes.iter().enumerate() {
            if t == tid {
                globals.push(gv as u32);
            }
        }
        let mut g2l: Vec<Option<u32>> = vec![None; num_vregs as usize];
        for (li, &gv) in globals.iter().enumerate() {
            g2l[gv as usize] = Some(li as u32);
        }

        let mut allow_local: Vec<bool> = vec![false; globals.len()];
        for (li, &gv) in globals.iter().enumerate() {
            allow_local[li] = allow_multi_def[gv as usize];
        }

        let mut cls_instrs: Vec<InstrInfo> = Vec::with_capacity(infos.len());
        for ins in &infos {
            let mut uses = Vec::new();
            let mut defs = Vec::new();
            for &u in &ins.uses {
                if let Some(li) = g2l[u.0 as usize] {
                    uses.push(VReg(li));
                }
            }
            for &d in &ins.defs {
                if let Some(li) = g2l[d.0 as usize] {
                    defs.push(VReg(li));
                }
            }
            cls_instrs.push(InstrInfo { uses, defs });
        }

        let spillable = vec![false; globals.len()];
        let alloc = regalloc::lsra_allocate(
            256,
            globals.len() as u32,
            &cls_instrs,
            &spillable,
            SpillPolicy::Forbid,
            Some(&allow_local),
        )
        .map_err(|e| CompileError::new(ErrorKind::Codegen, Span::point(0), format!("register allocation failed: {:?}", e)))?;

        let used = alloc.used_pregs;
        if base as u32 + used as u32 > 256 {
            return Err(CompileError::new(ErrorKind::Codegen, Span::point(0), "register allocation exceeded 256 regs"));
        }

        let need = base as usize + used as usize;
        if reg_types.len() < need {
            reg_types.resize(need, 0);
        }
        for i in 0..used {
            reg_types[base as usize + i as usize] = tid;
        }

        for (li, &gv) in globals.iter().enumerate() {
            let p = alloc
                .vreg_to_preg[li]
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "unexpected spill"))?;
            vreg_to_reg[gv as usize] = (base + p.0) as u8;
        }

        base += used;
    }

    // Emit final bytecode (CALLR expands to MOV* + CALLR, so we must re-patch jump deltas).
    let mut call_by_pc: Vec<Option<usize>> = vec![None; vinsns.len()];
    for (i, cs) in call_sites.iter().enumerate() {
        call_by_pc[cs.pc as usize] = Some(i);
    }

    let mut need_sigs: Vec<u32> = call_sites.iter().map(|cs| cs.sig_id).collect();
    need_sigs.sort();
    need_sigs.dedup();

    let mut arg_block_start: HashMap<u32, u8> = HashMap::new();
    for sig_id in need_sigs {
        let sig = type_ctx
            .sigs
            .get(sig_id as usize)
            .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "bad fun sig id"))?;
        if reg_types.len() + sig.args.len() > 256 {
            return Err(CompileError::new(ErrorKind::Codegen, Span::point(0), "register allocation exceeded 256 regs"));
        }
        let start = reg_types.len() as u8;
        for &tid in &sig.args {
            reg_types.push(tid);
        }
        arg_block_start.insert(sig_id, start);
    }

    let mut old_to_new: Vec<u32> = vec![0; vinsns.len() + 1];
    let mut npc: u32 = 0;
    for (opc, _vi) in vinsns.iter().enumerate() {
        old_to_new[opc] = npc;
        if let Some(ci) = call_by_pc[opc] {
            npc += 1 + (call_sites[ci].args.len() as u32);
        } else {
            npc += 1;
        }
    }
    old_to_new[vinsns.len()] = npc;

    let extra_movs: usize = call_sites.iter().map(|cs| cs.args.len()).sum();
    let mut insns: Vec<Insn> = Vec::with_capacity(vinsns.len() + extra_movs);

    for (opc, vi) in vinsns.iter().enumerate() {
        if let Some(ci) = call_by_pc[opc] {
            let cs = &call_sites[ci];
            let start = *arg_block_start
                .get(&cs.sig_id)
                .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "missing arg block"))?;

            for (i, &av) in cs.args.iter().enumerate() {
                let dst = start
                    .checked_add(i as u8)
                    .ok_or_else(|| CompileError::new(ErrorKind::Internal, Span::point(0), "arg block overflow"))?;
                let src = vreg_to_reg[av as usize];
                insns.push(Insn { op: Op::Mov as u8, a: dst, b: src, c: 0, imm: 0 });
            }

            let nargs = cs.args.len() as u8;
            let dst = vreg_to_reg[vi.a.v as usize];
            let callee = match vi.b {
                Opnd::V(x) => vreg_to_reg[x as usize],
                Opnd::Z => return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad call callee operand")),
            };
            insns.push(Insn { op: Op::CallR as u8, a: dst, b: callee, c: nargs, imm: start as u32 });
            continue;
        }

        match vi.op {
            Op::ConstBool => {
                insns.push(Insn {
                    op: Op::ConstBool as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: (vi.imm & 1) as u8,
                    imm: 0,
                });
            }
            Op::ConstI8Imm => {
                insns.push(Insn {
                    op: Op::ConstI8Imm as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: (vi.imm & 0xFF) as u8,
                    imm: 0,
                });
            }
            Op::JmpIf => {
                let old_target = (opc as i32 + 1) + (vi.imm as i32);
                if old_target < 0 || (old_target as usize) > vinsns.len() {
                    return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad jmp_if target"));
                }
                let new_pc = old_to_new[opc];
                let new_target = old_to_new[old_target as usize];
                let new_delta: i32 = (new_target as i32) - ((new_pc + 1) as i32);
                insns.push(Insn {
                    op: Op::JmpIf as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: 0,
                    c: 0,
                    imm: new_delta as u32,
                });
            }
            Op::Jmp => {
                let old_target = (opc as i32 + 1) + (vi.imm as i32);
                if old_target < 0 || (old_target as usize) > vinsns.len() {
                    return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad jmp target"));
                }
                let new_pc = old_to_new[opc];
                let new_target = old_to_new[old_target as usize];
                let new_delta: i32 = (new_target as i32) - ((new_pc + 1) as i32);
                insns.push(Insn { op: Op::Jmp as u8, a: 0, b: 0, c: 0, imm: new_delta as u32 });
            }
            Op::Try => {
                let old_target = (opc as i32 + 1) + (vi.imm as i32);
                if old_target < 0 || (old_target as usize) > vinsns.len() {
                    return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "bad try target"));
                }
                let new_pc = old_to_new[opc];
                let new_target = old_to_new[old_target as usize];
                let new_delta: i32 = (new_target as i32) - ((new_pc + 1) as i32);
                let trap_only = match vi.b {
                    Opnd::V(x) => x as u8,
                    Opnd::Z => 0,
                };
                insns.push(Insn {
                    op: Op::Try as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b: trap_only,
                    c: 0,
                    imm: new_delta as u32,
                });
            }
            Op::EndTry => {
                insns.push(Insn { op: Op::EndTry as u8, a: 0, b: 0, c: 0, imm: 0 });
            }
            Op::Throw => {
                insns.push(Insn { op: Op::Throw as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            Op::Ret => {
                insns.push(Insn { op: Op::Ret as u8, a: vreg_to_reg[vi.a.v as usize], b: 0, c: 0, imm: 0 });
            }
            Op::CallR => return Err(CompileError::new(ErrorKind::Internal, Span::point(0), "CALLR without callsite record")),
            _ => {
                let b = match vi.b { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                let c = match vi.c { Opnd::V(x) => vreg_to_reg[x as usize], Opnd::Z => 0 };
                insns.push(Insn {
                    op: vi.op as u8,
                    a: vreg_to_reg[vi.a.v as usize],
                    b,
                    c,
                    imm: vi.imm,
                });
            }
        }
    }

    let f0 = Function { reg_types, cap_start: 0, insns };
    let mut funcs: Vec<Function> = Vec::with_capacity((PRELUDE_FUN_COUNT as usize) + 1 + nested_funcs.len());
    // Inject prelude bytecode at module indices 0..PRELUDE_FUN_COUNT-1.
    funcs.extend(prelude_funcs_for_program());
    funcs.push(f0);
    funcs.extend(nested_funcs);
    Ok(Module {
        types: type_ctx.types,
        sigs: type_ctx.sigs,
        atoms,
        const_i64,
        const_f64,
        const_bytes,
        funcs,
        entry: PRELUDE_FUN_COUNT,
        prelude_count: PRELUDE_FUN_COUNT,
        used_prelude: vec![],
    })
}

