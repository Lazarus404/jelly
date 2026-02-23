#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"

echo "Building VM (release)..."
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build

echo "Building compiler (release)..."
cargo build --release --manifest-path jellyc/Cargo.toml

echo "Done. jellyvm: build/bin/jellyvm  jellyc: jellyc/target/release/jellyc"
