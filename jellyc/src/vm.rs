#![cfg(feature = "embed-vm")]
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

// FFI bindings to the jelly VM for in-process REPL execution.

use std::ffi::CStr;
use std::os::raw::{c_char, c_void};

// Opaque types
#[repr(C)]
pub struct JellyVm {
    _private: [u8; 0],
}

#[repr(C)]
pub struct JellyBcModule {
    _private: [u8; 0],
}

// jelly_value = uintptr_t
pub type JellyValue = usize;

// Tags (low 3 bits of jelly_value)
const JELLY_TAG_PTR: usize = 0x0;
const JELLY_TAG_I32: usize = 0x1;
const JELLY_TAG_ATOM: usize = 0x2;
const JELLY_TAG_BOOL: usize = 0x3;
const JELLY_TAG_NULL: usize = 0x4;

// jelly_obj_kind (must match jelly.h)
pub const JELLY_OBJ_BYTES: u32 = 1;
pub const JELLY_OBJ_BOX_I64: u32 = 7;
pub const JELLY_OBJ_BOX_F64: u32 = 8;
pub const JELLY_OBJ_BOX_F32: u32 = 9;
pub const JELLY_OBJ_BOX_F16: u32 = 10;

#[repr(C)]
pub struct JellyBcResult {
    pub err: u32, // jelly_bc_error
    pub msg: *const c_char,
    pub offset: usize,
}

#[repr(C)]
#[allow(dead_code)]
pub enum JellyExecStatus {
    Ok = 0,
    Trap = 1,
}

#[repr(C)]
#[allow(dead_code)]
pub enum JellyBcError {
    Ok = 0,
    Eof = 1,
    BadMagic = 2,
    UnsupportedVersion = 3,
    BadFormat = 4,
    OutOfMemory = 5,
}

extern "C" {
    pub fn jelly_bc_read(
        data: *const u8,
        size: usize,
        out: *mut *mut JellyBcModule,
    ) -> JellyBcResult;
    pub fn jelly_bc_free(m: *mut JellyBcModule);
    pub fn jelly_bc_get_entry(m: *const JellyBcModule) -> u32;

    pub fn jelly_vm_create() -> *mut JellyVm;
    pub fn jelly_vm_destroy(vm: *mut JellyVm);
    #[allow(dead_code)]
    pub fn jelly_gc_shutdown(vm: *mut JellyVm);
    pub fn jelly_gc_push_root(vm: *mut JellyVm, v: JellyValue);
    pub fn jelly_gc_pop_roots(vm: *mut JellyVm, n: u32);
    pub fn jelly_vm_invalidate_frame_cache(vm: *mut JellyVm);
    pub fn jelly_vm_exec_status(
        vm: *mut JellyVm,
        m: *const JellyBcModule,
        out: *mut JellyValue,
    ) -> JellyExecStatus;
    pub fn jelly_vm_exec_status_exports(
        vm: *mut JellyVm,
        m: *const JellyBcModule,
        entry_module_idx: u32,
        out: *mut JellyValue,
        out_exports: *mut JellyValue,
    ) -> JellyExecStatus;
    pub fn jelly_vm_exec_status_chunk(
        vm: *mut JellyVm,
        m: *const JellyBcModule,
        entry_func_index: u32,
        args: *const JellyValue,
        nargs: u32,
        entry_module_idx: u32,
        out: *mut JellyValue,
        out_exports: *mut JellyValue,
    ) -> JellyExecStatus;
    pub fn jelly_vm_last_trap_code(vm: *const JellyVm) -> u32;
    pub fn jelly_vm_last_trap_msg(vm: *const JellyVm) -> *const c_char;
    pub fn jelly_vm_set_fuel(vm: *mut JellyVm, fuel: u64);
    pub fn jelly_vm_set_max_bytes_len(vm: *mut JellyVm, max_len: u32);
    pub fn jelly_vm_set_max_array_len(vm: *mut JellyVm, max_len: u32);
}

// Value helpers (mirror jelly.h inline functions)
#[inline]
pub fn jelly_is_null(v: JellyValue) -> bool {
    (v & 0x7) == JELLY_TAG_NULL
}

#[inline]
pub fn jelly_is_bool(v: JellyValue) -> bool {
    (v & 0x7) == JELLY_TAG_BOOL
}

#[inline]
pub fn jelly_as_bool(v: JellyValue) -> bool {
    ((v >> 3) & 1) != 0
}

#[inline]
pub fn jelly_is_i32(v: JellyValue) -> bool {
    (v & 0x7) == JELLY_TAG_I32
}

#[inline]
pub fn jelly_as_i32(v: JellyValue) -> i32 {
    (v >> 3) as i32
}

#[inline]
pub fn jelly_is_atom(v: JellyValue) -> bool {
    (v & 0x7) == JELLY_TAG_ATOM
}

#[inline]
pub fn jelly_as_atom(v: JellyValue) -> u32 {
    (v >> 3) as u32
}

#[inline]
pub fn jelly_is_ptr(v: JellyValue) -> bool {
    (v & 0x7) == JELLY_TAG_PTR
}

#[inline]
pub fn jelly_as_ptr(v: JellyValue) -> *mut c_void {
    v as *mut c_void
}

/// F16 bits to f32 (IEEE 754 binary16). Matches vm_f16_bits_to_f32 in reg.c.
fn f16_bits_to_f32(bits: u16) -> f32 {
    if (bits & 0x7FFF) == 0 {
        return if (bits & 0x8000) != 0 { -0.0 } else { 0.0 };
    }
    let sign = ((bits as u32) & 0x8000) << 16;
    let mut exp = ((bits >> 10) & 0x1F) as i32;
    let mut mant = ((bits & 0x3FF) as u32) << 13;
    if exp == 0 {
        while (mant & 0x800000) == 0 {
            mant <<= 1;
            exp -= 1;
        }
        exp += 1;
    } else if exp == 31 {
        let inf = f32::INFINITY;
        let nan = f32::NAN;
        return if (bits & 0x8000) != 0 {
            -if mant != 0 { nan } else { inf }
        } else {
            if mant != 0 { nan } else { inf }
        };
    }
    exp += 127 - 15;
    let u32_bits = sign | ((exp as u32) << 23) | mant;
    f32::from_bits(u32_bits)
}

/// Format a jelly_value for display (matches jellyvm main.c behavior).
pub fn format_value(v: JellyValue) -> String {
    if jelly_is_null(v) {
        "null".to_string()
    } else if jelly_is_bool(v) {
        if jelly_as_bool(v) {
            "true".to_string()
        } else {
            "false".to_string()
        }
    } else if jelly_is_i32(v) {
        format!("{}", jelly_as_i32(v))
    } else if jelly_is_atom(v) {
        format!("atom({})", jelly_as_atom(v))
    } else if jelly_is_ptr(v) {
        let ptr = jelly_as_ptr(v);
        if ptr.is_null() {
            return "<null ptr>".to_string();
        }
        let kind = unsafe { *(ptr as *const u32) };
        match kind {
            JELLY_OBJ_BOX_I64 => {
                let val = unsafe { *((ptr as *const u8).add(8) as *const i64) };
                format!("{}", val)
            }
            JELLY_OBJ_BOX_F64 => {
                let val = unsafe { *((ptr as *const u8).add(8) as *const f64) };
                format!("{}", val)
            }
            JELLY_OBJ_BOX_F32 => {
                let val = unsafe { *((ptr as *const u8).add(8) as *const f32) };
                format!("{}", val)
            }
            JELLY_OBJ_BOX_F16 => {
                let bits = unsafe { *((ptr as *const u8).add(8) as *const u16) };
                format!("{}", f16_bits_to_f32(bits))
            }
            JELLY_OBJ_BYTES => {
                let len = unsafe { *((ptr as *const u8).add(8) as *const u32) };
                let data_ptr = unsafe { (ptr as *const u8).add(12) };
                match std::str::from_utf8(unsafe {
                    std::slice::from_raw_parts(data_ptr, len as usize)
                }) {
                    Ok(s) => s.to_string(),
                    Err(_) => format!("ptr({:p})\n", ptr),
                }
            }
            _ => format!("ptr({:p})\n", ptr),
        }
    } else {
        "<value>".to_string()
    }
}

/// Persistent VM for REPL: create once, reuse for many executions.
/// Phase 2: heap persists across runs; exports captured for future incremental exec.
pub struct ReplVm {
    vm: *mut JellyVm,
}

impl ReplVm {
    pub fn new() -> Result<Self, String> {
        let vm = unsafe { jelly_vm_create() };
        if vm.is_null() {
            return Err("failed to create VM".to_string());
        }
        unsafe {
            jelly_vm_set_fuel(vm, 200_000_000);
            jelly_vm_set_max_bytes_len(vm, 64 * 1024 * 1024);
            jelly_vm_set_max_array_len(vm, 8 * 1024 * 1024);
        }
        Ok(ReplVm { vm })
    }

    /// Execute bytecode. Phase 2: heap persists; optionally captures exports for next run.
    /// entry_module_idx: 0 for single module, 1 for linked [prior, new]. UINT32_MAX to skip capture.
    pub fn exec(
        &mut self,
        bytecode: &[u8],
        entry_module_idx: u32,
        out_exports: Option<&mut JellyValue>,
    ) -> Result<String, String> {
        let mut module: *mut JellyBcModule = std::ptr::null_mut();
        let result = unsafe { jelly_bc_read(bytecode.as_ptr(), bytecode.len(), &mut module) };

        if result.err != JellyBcError::Ok as u32 {
            let msg = if result.msg.is_null() {
                "unknown".to_string()
            } else {
                unsafe { CStr::from_ptr(result.msg).to_string_lossy().into_owned() }
            };
            return Err(format!(
                "bytecode load failed: err={} msg={} off={}",
                result.err, msg, result.offset
            ));
        }

        // Phase 2: skip jelly_gc_shutdown so heap persists across runs
        // (unsafe { jelly_gc_shutdown(self.vm) };)

        let mut out = 0usize;
        let mut exports = 0usize;
        let (capture_exports, exports_ptr) = match &out_exports {
            Some(_) if entry_module_idx != u32::MAX => (true, &mut exports as *mut JellyValue),
            _ => (false, std::ptr::null_mut()),
        };
        let status = unsafe {
            jelly_vm_exec_status_exports(
                self.vm,
                module,
                if capture_exports {
                    entry_module_idx
                } else {
                    u32::MAX
                },
                &mut out,
                exports_ptr,
            )
        };

        unsafe {
            jelly_vm_invalidate_frame_cache(self.vm);
            jelly_bc_free(module);
        }

        if let Some(dst) = out_exports {
            *dst = exports;
        }

        match status {
            JellyExecStatus::Ok => Ok(format_value(out)),
            JellyExecStatus::Trap => {
                let code = unsafe { jelly_vm_last_trap_code(self.vm) };
                let msg = unsafe { jelly_vm_last_trap_msg(self.vm) };
                let msg_str = if msg.is_null() {
                    "(null)".to_string()
                } else {
                    unsafe { CStr::from_ptr(msg).to_string_lossy().into_owned() }
                };
                Err(format!("trap: code={} msg={}", code, msg_str))
            }
        }
    }

    // Execute a chunk module with pre-bound session_exports.
    // Chunk entry takes 1 arg (session_exports). Returns output and optionally updated exports.
    pub fn exec_chunk(
        &mut self,
        bytecode: &[u8],
        session_exports: JellyValue,
        out_exports: Option<&mut JellyValue>,
    ) -> Result<String, String> {
        // Keep session_exports as GC root for entire chunk exec so exported heap objects
        // (e.g. object literals) are not collected before the chunk can use them.
        unsafe { jelly_gc_push_root(self.vm, session_exports) };
        let mut module: *mut JellyBcModule = std::ptr::null_mut();
        let result = unsafe { jelly_bc_read(bytecode.as_ptr(), bytecode.len(), &mut module) };

        if result.err != JellyBcError::Ok as u32 {
            unsafe { jelly_gc_pop_roots(self.vm, 1) };
            let msg = if result.msg.is_null() {
                "unknown".to_string()
            } else {
                unsafe { CStr::from_ptr(result.msg).to_string_lossy().into_owned() }
            };
            return Err(format!(
                "bytecode load failed: err={} msg={} off={}",
                result.err, msg, result.offset
            ));
        }

        let entry = unsafe { jelly_bc_get_entry(module) };
        let args = [session_exports];
        let mut out = 0usize;
        let mut exports = 0usize;
        let exports_ptr = match &out_exports {
            Some(_) => &mut exports as *mut JellyValue,
            None => std::ptr::null_mut(),
        };
        let status = unsafe {
            jelly_vm_exec_status_chunk(
                self.vm,
                module,
                entry,
                args.as_ptr(),
                1,
                0, // entry_module_idx: reg 0 holds session (prior exports) for next run
                &mut out,
                exports_ptr,
            )
        };
        unsafe { jelly_gc_pop_roots(self.vm, 1) };

        unsafe {
            jelly_vm_invalidate_frame_cache(self.vm);
            jelly_bc_free(module);
        }

        if let Some(dst) = out_exports {
            *dst = exports;
        }

        match status {
            JellyExecStatus::Ok => Ok(format_value(out)),
            JellyExecStatus::Trap => {
                let code = unsafe { jelly_vm_last_trap_code(self.vm) };
                let msg = unsafe { jelly_vm_last_trap_msg(self.vm) };
                let msg_str = if msg.is_null() {
                    "(null)".to_string()
                } else {
                    unsafe { CStr::from_ptr(msg).to_string_lossy().into_owned() }
                };
                Err(format!("trap: code={} msg={}", code, msg_str))
            }
        }
    }
}

impl Drop for ReplVm {
    fn drop(&mut self) {
        if !self.vm.is_null() {
            unsafe { jelly_vm_destroy(self.vm) };
            self.vm = std::ptr::null_mut();
        }
    }
}

/// Load bytecode from bytes and execute in a fresh VM. Returns (output_string, exit_code).
/// Prefer `ReplVm` for repeated execution (REPL loop).
#[allow(dead_code)]
/// exit_code: Some(123) if System.exit() was called, None on success, Some(1) on trap.
pub fn exec_bytecode(bytecode: &[u8]) -> Result<(String, Option<i32>), String> {
    let mut module: *mut JellyBcModule = std::ptr::null_mut();
    let result = unsafe { jelly_bc_read(bytecode.as_ptr(), bytecode.len(), &mut module) };

    if result.err != JellyBcError::Ok as u32 {
        let msg = if result.msg.is_null() {
            "unknown".to_string()
        } else {
            unsafe { CStr::from_ptr(result.msg).to_string_lossy().into_owned() }
        };
        return Err(format!(
            "bytecode load failed: err={} msg={} off={}",
            result.err, msg, result.offset
        ));
    }

    let vm = unsafe { jelly_vm_create() };
    if vm.is_null() {
        unsafe { jelly_bc_free(module) };
        return Err("failed to create VM".to_string());
    }

    // Set safety limits (match jellyvm main.c defaults)
    unsafe {
        jelly_vm_set_fuel(vm, 200_000_000);
        jelly_vm_set_max_bytes_len(vm, 64 * 1024 * 1024);
        jelly_vm_set_max_array_len(vm, 8 * 1024 * 1024);
    }

    let mut out = 0usize;
    let status = unsafe { jelly_vm_exec_status(vm, module, &mut out) };

    let _exit_code = match status {
        JellyExecStatus::Ok => {
            // Normal return. (System.exit would have terminated the process; REPL checks
            // for "exit()" input before running to avoid that.)
            None
        }
        JellyExecStatus::Trap => {
            let code = unsafe { jelly_vm_last_trap_code(vm) };
            let msg = unsafe {
                jelly_vm_last_trap_msg(vm)
            };
            let msg_str = if msg.is_null() {
                "(null)".to_string()
            } else {
                unsafe { CStr::from_ptr(msg).to_string_lossy().into_owned() }
            };
            return Err(format!("trap: code={} msg={}", code, msg_str));
        }
    };

    unsafe {
        jelly_bc_free(module);
        jelly_vm_destroy(vm);
    }

    let output = format_value(out);
    Ok((output, _exit_code))
}
