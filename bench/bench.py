import argparse
import os
import re
import shutil
import statistics
import subprocess
import sys
import time


ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BENCH_DIR = os.path.join(ROOT, "bench")
OUT_DIR = os.path.join(BENCH_DIR, "out")


def _first_existing(*paths: str) -> str | None:
    """Return the first path that exists, or None."""
    for p in paths:
        if p and os.path.exists(p):
            return p
    return None


# jellyvm: Prefer build/ (release.sh output) over in-place bin/.
# release.sh: build/bin/jellyvm
# In-place (cmake -S . -B .): bin/jellyvm
# Standalone vm build can have format mismatch with jellyc-linked bytecode.
JELLYVM_CANDIDATES = [
    os.path.join(ROOT, "build", "bin", "jellyvm"),
    os.path.join(ROOT, "build", "release", "bin", "jellyvm"),
    os.path.join(ROOT, "build", "debug", "bin", "jellyvm"),
    os.path.join(ROOT, "bin", "jellyvm"),
    os.path.join(ROOT, "vm", "build", "bin", "jellyvm"),
]
DEFAULT_JELLYVM = _first_existing(*JELLYVM_CANDIDATES) or JELLYVM_CANDIDATES[0]

# jellyc: cargo target or Makefile symlinks in build/<type>/bin
JELLYC_DEBUG_CANDIDATES = [
    os.path.join(ROOT, "jellyc", "target", "debug", "jellyc"),
    os.path.join(ROOT, "build", "debug", "bin", "jellyc"),
    os.path.join(ROOT, "build", "release", "bin", "jellyc"),
]
JELLYC_RELEASE_CANDIDATES = [
    os.path.join(ROOT, "jellyc", "target", "release", "jellyc"),
    os.path.join(ROOT, "build", "release", "bin", "jellyc"),
    os.path.join(ROOT, "build", "debug", "bin", "jellyc"),
]
DEFAULT_JELLYC = _first_existing(*JELLYC_DEBUG_CANDIDATES) or JELLYC_DEBUG_CANDIDATES[0]
DEFAULT_JELLYC_RELEASE = _first_existing(*JELLYC_RELEASE_CANDIDATES) or JELLYC_RELEASE_CANDIDATES[0]


def which_lua() -> list[str]:
    # Prefer plain Lua unless only LuaJIT exists.
    if shutil.which("lua"):
        return ["lua"]
    if shutil.which("luajit"):
        return ["luajit"]
    raise SystemExit("error: neither `lua` nor `luajit` found on PATH")


def run_checked(cmd: list[str], *, cwd: str | None = None) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, check=True, capture_output=True, text=True, cwd=cwd or ROOT)


def compile_jelly(jellyc: str, jellyvm: str, src: str, out: str) -> None:
    if not os.path.exists(jellyc):
        raise SystemExit(f"error: jellyc not found at {jellyc} (build it first)")
    if not os.path.exists(jellyvm):
        raise SystemExit(f"error: jellyvm not found at {jellyvm} (build_root missing?)")
    os.makedirs(os.path.dirname(out), exist_ok=True)
    run_checked([jellyc, src, "--out", out, "--backend", "ir"])


def time_many(cmd: list[str], runs: int, warmup: int) -> tuple[list[float], str]:
    last_out = ""
    for _ in range(warmup):
        p = subprocess.run(cmd, capture_output=True, text=True, cwd=ROOT)
        if p.returncode != 0:
            raise SystemExit(
                f"error: {cmd[0]!r} exited {p.returncode}\n"
                f"  stderr: {p.stderr.strip()!r}\n"
                f"  stdout: {p.stdout.strip()!r}"
            )
        last_out = p.stdout
    times: list[float] = []
    for _ in range(runs):
        t0 = time.perf_counter()
        p = subprocess.run(cmd, capture_output=True, text=True, cwd=ROOT)
        t1 = time.perf_counter()
        if p.returncode != 0:
            raise SystemExit(
                f"error: {cmd[0]!r} exited {p.returncode}\n"
                f"  stderr: {p.stderr.strip()!r}\n"
                f"  stdout: {p.stdout.strip()!r}"
            )
        last_out = p.stdout
        times.append(t1 - t0)
    return times, last_out


def fmt_ms(x: float) -> str:
    return f"{x * 1000:.2f}"


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--jellyvm", default=DEFAULT_JELLYVM, help="Path override (debug/conformance only)")
    ap.add_argument("--jellyc", default=DEFAULT_JELLYC, help="Path override (debug/conformance only)")
    ap.add_argument("--release", action="store_true", help="Use release-built jellyc (faster compile)")
    ap.add_argument("--runs", type=int, default=int(os.environ.get("RUNS", "10")))
    ap.add_argument("--warmup", type=int, default=int(os.environ.get("WARMUP", "3")))
    ap.add_argument("--filter", default="", help="Regex filter for benchmark name")
    ap.add_argument("--no-compile", action="store_true", help="Skip jellyc compilation step")
    args = ap.parse_args()

    jellyc = args.jellyc
    if args.release:
        jellyc = DEFAULT_JELLYC_RELEASE

    # Validate binaries before running; give clear instructions if missing.
    jellyvm = args.jellyvm
    if not os.path.exists(jellyvm):
        raise SystemExit(
            f"error: jellyvm not found at {jellyvm}\n"
            f"  Build with: cmake -S . -B build -DCMAKE_BUILD_TYPE=Release && cmake --build build\n"
            f"  Or: ./configure && make"
        )
    if not os.path.exists(jellyc):
        raise SystemExit(
            f"error: jellyc not found at {jellyc}\n"
            f"  Build with: cargo build -p jellyc (debug) or cargo build -p jellyc --release"
        )

    print(f"jellyvm: {jellyvm}")
    print(f"jellyc:  {jellyc}")
    print()
    sys.stdout.flush()

    lua = which_lua()

    benches: list[dict[str, str]] = [
        {"name": "startup", "jelly": "startup.jelly", "lua": "startup.lua"},
        {"name": "fib", "jelly": "fib.jelly", "lua": "fib.lua"},
        {"name": "ackerman", "jelly": "ackerman.jelly", "lua": "ackerman.lua"},
        {"name": "fannkuch", "jelly": "fannkuch.jelly", "lua": "fannkuch.lua"},
        {"name": "recursive", "jelly": "recursive.jelly", "lua": "recursive.lua"},
        {"name": "tail_sum", "jelly": "tail_sum.jelly", "lua": "tail_sum.lua"},
        {"name": "nsieve", "jelly": "nsieve.jelly", "lua": "nsieve.lua"},
        {"name": "binary_trees", "jelly": "binary_trees.jelly", "lua": "binary_trees.lua"},
        {"name": "fp", "jelly": "fp.jelly", "lua": "fp.lua"},
        {"name": "module", "jelly": "module.jelly", "lua": "module.lua"},
        {"name": "nbodies", "jelly": "nbodies.jelly", "lua": "nbodies.lua"},
    ]

    rx: re.Pattern[str] | None = None
    if args.filter:
        rx = re.compile(args.filter)
        benches = [b for b in benches if rx.search(b["name"])]
        if not benches:
            raise SystemExit(f"error: no benchmarks matched --filter {args.filter!r}")

    print(f"Running {len(benches)} benchmarks ({args.runs} runs, {args.warmup} warmup)...")
    print()

    rows: list[dict[str, object]] = []
    for b in benches:
        name = b["name"]
        jelly_src = os.path.join(BENCH_DIR, b["jelly"]) if b["jelly"] else ""
        lua_src = os.path.join(BENCH_DIR, b["lua"])
        jelly_out = os.path.join(OUT_DIR, f"{name}.jlyb")

        if jelly_src and not args.no_compile:
            compile_jelly(jellyc, args.jellyvm, jelly_src, jelly_out)

        jt: list[float] = []
        jout = ""
        if jelly_src:
            jt, jout = time_many([args.jellyvm, jelly_out], runs=args.runs, warmup=args.warmup)
        lt, lout = time_many(lua + [lua_src], runs=args.runs, warmup=args.warmup)

        # Basic correctness check: all our bench programs print exactly "ok".
        if jelly_src and jout.strip() != "ok":
            raise SystemExit(
                f"error: {name}: jelly output unexpected: {jout!r}\n"
                f"  cmd: {args.jellyvm!r} {jelly_out!r}\n"
                f"  cwd: {ROOT}"
            )
        if lout.strip() != "ok":
            raise SystemExit(f"error: {name}: lua output unexpected: {lout!r}")

        lmed = statistics.median(lt)
        lmean = statistics.fmean(lt)
        jmed = statistics.median(jt) if jt else float("nan")
        jmean = statistics.fmean(jt) if jt else float("nan")
        rows.append(
            {
                "name": name,
                "jmed": jmed,
                "lmed": lmed,
                "jmean": jmean,
                "lmean": lmean,
                "speedup": (lmed / jmed) if jmed > 0 else float("nan"),
            }
        )

    # Summary table
    print(f"Lua runner: {lua[0]}")
    print()
    print("| bench | jelly median (ms) | lua median (ms) | Lua/Jelly |")
    print("|---|---:|---:|---:|")
    for r in rows:
        jmed = float(r["jmed"])
        if jmed != jmed:  # NaN
            print(f"| {r['name']} | (unsupported) | {fmt_ms(r['lmed'])} |  |")
            continue
        print(
            f"| {r['name']} | {fmt_ms(r['jmed'])} | {fmt_ms(r['lmed'])} | {r['speedup']:.2f}x |"
        )

    # Totals (median-of-medians-ish): use sum of medians as a rough overall indicator.
    jtot = sum(float(r["jmed"]) for r in rows if float(r["jmed"]) == float(r["jmed"]))
    ltot = sum(float(r["lmed"]) for r in rows)
    if rows:
        ratio = (ltot / jtot) if jtot > 0 else float("nan")
        print("| **TOTAL (sum of medians)** | "
              f"**{fmt_ms(jtot)}** | **{fmt_ms(ltot)}** | **{ratio:.2f}x** |")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

