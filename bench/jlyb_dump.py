#!/usr/bin/env python3
from __future__ import annotations

import argparse
import collections
import struct
from dataclasses import dataclass
from pathlib import Path


MAGIC_JLYB = 0x4A4C5942
VERSION = 1


def rd_u8(buf: memoryview, off: int) -> tuple[int, int]:
    return buf[off], off + 1


def rd_u16le(buf: memoryview, off: int) -> tuple[int, int]:
    (v,) = struct.unpack_from("<H", buf, off)
    return v, off + 2


def rd_u32le(buf: memoryview, off: int) -> tuple[int, int]:
    (v,) = struct.unpack_from("<I", buf, off)
    return v, off + 4


def rd_i64le(buf: memoryview, off: int) -> tuple[int, int]:
    (v,) = struct.unpack_from("<q", buf, off)
    return v, off + 8


def rd_f64le(buf: memoryview, off: int) -> tuple[float, int]:
    (v,) = struct.unpack_from("<d", buf, off)
    return v, off + 8


@dataclass(frozen=True)
class Insn:
    op: int
    a: int
    b: int
    c: int
    imm: int


def op_name(op: int) -> str:
    # Keep this minimal; we mainly care about call/arith/control for perf.
    names = {
        0: "NOP",
        1: "RET",
        2: "MOV",
        3: "CALL",
        4: "CALLR",
        8: "CONST_I32",
        10: "CONST_BOOL",
        20: "JMP",
        21: "JMP_IF",
        26: "ADD_I32",
        27: "SUB_I32",
        30: "ADD_I32_IMM",
        31: "SUB_I32_IMM",
        53: "EQ_I32",
        54: "LT_I32",
        55: "EQ_I32_IMM",
        56: "LT_I32_IMM",
        60: "TO_DYN",
        63: "FROM_DYN_I32",
        115: "LT_I64",
        116: "LT_F32",
        117: "LT_F64",
    }
    return names.get(op, f"OP_{op}")


def parse_module(path: Path) -> tuple[int, list[list[Insn]]]:
    data = path.read_bytes()
    buf = memoryview(data)
    off = 0

    magic, off = rd_u32le(buf, off)
    if magic != MAGIC_JLYB:
        raise SystemExit(f"bad magic: 0x{magic:08x}")
    version, off = rd_u32le(buf, off)
    if version != VERSION:
        raise SystemExit(f"unsupported version: {version} (expected {VERSION})")
    features, off = rd_u32le(buf, off)
    ntypes, off = rd_u32le(buf, off)
    nsigs, off = rd_u32le(buf, off)
    natoms, off = rd_u32le(buf, off)
    nfuncs, off = rd_u32le(buf, off)
    entry, off = rd_u32le(buf, off)

    feat_const64 = (features & 1) != 0
    if feat_const64:
        nconst_i64, off = rd_u32le(buf, off)
        nconst_f64, off = rd_u32le(buf, off)
    else:
        nconst_i64 = 0
        nconst_f64 = 0

    feat_constbytes = (features & (1 << 1)) != 0
    if feat_constbytes:
        nconst_bytes, off = rd_u32le(buf, off)
    else:
        nconst_bytes = 0

    # types: 12 bytes each
    off += int(ntypes) * 12

    # sigs
    for _ in range(int(nsigs)):
        _ret, off = rd_u32le(buf, off)
        nargs, off = rd_u16le(buf, off)
        _pad, off = rd_u16le(buf, off)
        off += int(nargs) * 4

    # atoms
    for _ in range(int(natoms)):
        alen, off = rd_u32le(buf, off)
        off += int(alen)

    # const64
    for _ in range(int(nconst_i64)):
        _x, off = rd_i64le(buf, off)
    for _ in range(int(nconst_f64)):
        _x, off = rd_f64le(buf, off)

    # const bytes
    for _ in range(int(nconst_bytes)):
        blen, off = rd_u32le(buf, off)
        off += int(blen)

    funcs: list[list[Insn]] = []
    for _fi in range(int(nfuncs)):
        nregs, off = rd_u32le(buf, off)
        ninsns, off = rd_u32le(buf, off)
        off += int(nregs) * 4  # reg_types
        insns: list[Insn] = []
        for _ in range(int(ninsns)):
            op, off = rd_u8(buf, off)
            a, off = rd_u8(buf, off)
            b, off = rd_u8(buf, off)
            c, off = rd_u8(buf, off)
            imm, off = rd_u32le(buf, off)
            insns.append(Insn(op=op, a=a, b=b, c=c, imm=imm))
        funcs.append(insns)

    return entry, funcs


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("path", type=Path)
    ap.add_argument("--top", type=int, default=30, help="top N opcodes to show")
    ap.add_argument("--head", type=int, default=24, help="show first N insns per function")
    args = ap.parse_args()

    entry, funcs = parse_module(args.path)

    print(f"entry={entry} nfuncs={len(funcs)}")
    counts = collections.Counter()
    for fi, insns in enumerate(funcs):
        for ins in insns:
            counts[ins.op] += 1
        print(f"func[{fi}]: ninsns={len(insns)} calls={sum(1 for x in insns if x.op in (3,4))}")

    print("\nTop opcodes:")
    for op, n in counts.most_common(args.top):
        print(f"  {op:3d} {op_name(op):12s}  {n}")

    print("\nDisassembly (head):")
    for fi, insns in enumerate(funcs):
        if not insns:
            continue
        print(f"\nfunc[{fi}] (first {min(args.head, len(insns))}):")
        for pc, ins in enumerate(insns[: args.head]):
            print(f"  {pc:4d}  {op_name(ins.op):12s}  a={ins.a:3d} b={ins.b:3d} c={ins.c:3d} imm={ins.imm}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

