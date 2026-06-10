#!/usr/bin/env bash
# Build every component the Rust interop pipeline needs, in the order they depend on each other.
# Builds the five in-repo components: Frontend, Backend, ValeRuster, Coordinator, and Divination
# (Divination is now vendored at Divination/ in this repo; previously it lived in a separate repo
# and was opt-in via DIVINATION_DIR).
#
# Usage:
#   ./scripts/build-rust-interop.sh <BOOTSTRAPPING_VALEC_DIR>
#
# <BOOTSTRAPPING_VALEC_DIR> points at a pre-built Vale compiler used to bootstrap the Coordinator
# (which is itself written in Vale). It should contain Frontend.jar, backend, valec, stdlib/, builtins/.
#
# Optional environment variables:
#   DIVINATION_DIR      Override the Divination source location (default: $REPO_ROOT/Divination).
#                       Divination generates the Rust crate that wraps imported Rust types; the
#                       Backend invokes it at runtime.
#   LLVM_OUTER_DIR      Override LLVM install location (default: `brew --prefix llvm@16`).
#   SKIP_FRONTEND       If set (any value), skip rebuilding Frontend.jar. Useful for quick iteration
#                       when only the Backend or Coordinator changed.
#   SKIP_BACKEND        Same for Backend.
#   SKIP_VALERUSTER     Same for ValeRuster.
#   SKIP_COORDINATOR    Same for Coordinator.
#   SKIP_DIVINATION     Same for Divination.
#   SKIP_TESTERRUST     Same for TesterRust.

set -euo pipefail

BOOTSTRAPPING_VALEC_DIR="${1:-}"
if [ -z "$BOOTSTRAPPING_VALEC_DIR" ]; then
  echo "Usage: $0 <BOOTSTRAPPING_VALEC_DIR>" >&2
  echo "Example: $0 ~/BootstrappingValeCompiler" >&2
  exit 1
fi
if [ ! -f "$BOOTSTRAPPING_VALEC_DIR/valec" ] || [ ! -f "$BOOTSTRAPPING_VALEC_DIR/Frontend.jar" ]; then
  echo "ERROR: $BOOTSTRAPPING_VALEC_DIR doesn't look like a Vale compiler distribution" >&2
  echo "  (expected valec + Frontend.jar inside it)" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# LLVM_CMAKE_DIR: the Backend needs LLVM 16's CMake config to generate build files.
LLVM_OUTER_DIR="${LLVM_OUTER_DIR:-}"
if [ -z "$LLVM_OUTER_DIR" ]; then
  LLVM_PREFIX="$(brew --prefix llvm@16 2>/dev/null || true)"
  if [ -z "$LLVM_PREFIX" ] || [ ! -d "$LLVM_PREFIX" ]; then
    echo "ERROR: LLVM 16 not found via brew. Install with \`brew install llvm@16\` or set LLVM_OUTER_DIR." >&2
    exit 1
  fi
  LLVM_OUTER_DIR="$(greadlink -f "$LLVM_PREFIX")"
fi
LLVM_CMAKE_DIR="$LLVM_OUTER_DIR/lib/cmake/llvm"
if [ ! -d "$LLVM_CMAKE_DIR" ]; then
  echo "ERROR: $LLVM_CMAKE_DIR doesn't exist (LLVM install looks incomplete)" >&2
  exit 1
fi
echo "Using LLVM CMake dir: $LLVM_CMAKE_DIR"

# Frontend: Scala, built via sbt assembly. Tests are skipped here because some are pre-existing
# failures unrelated to the rust-interop pipeline (see the 16 failing tests in CompilerTests).
if [ -z "${SKIP_FRONTEND:-}" ]; then
  echo ""
  echo "=== Building Frontend ==="
  cd "$REPO_ROOT/Frontend"
  sbt 'set test in assembly := {}' clean assembly
fi

# Backend: C++, built via cmake. Depends on LLVM 16 being installed.
if [ -z "${SKIP_BACKEND:-}" ]; then
  echo ""
  echo "=== Building Backend ==="
  cd "$REPO_ROOT/Backend"
  cmake -B build -DLLVM_DIR="$LLVM_CMAKE_DIR"
  cmake --build build
fi

# ValeRuster: Rust. Pinned to nightly-2025-12-09 via repo-root rust-toolchain.toml, so the first
# build may download the toolchain + rust-src + rust-docs-json components. Debug build is fine
# for development; for a shippable release, replace with `cargo build --release` and update the
# test script's VALE_RUSTER_PATH default accordingly (or override via env var).
if [ -z "${SKIP_VALERUSTER:-}" ]; then
  echo ""
  echo "=== Building ValeRuster ==="
  cd "$REPO_ROOT/ValeRuster"
  cargo build
fi

# Coordinator: written in Vale itself, so we need the bootstrapping valec to build it.
if [ -z "${SKIP_COORDINATOR:-}" ]; then
  echo ""
  echo "=== Building Coordinator ==="
  cd "$REPO_ROOT/Coordinator"
  ./build.sh "$BOOTSTRAPPING_VALEC_DIR"
fi

# Divination: vendored at $REPO_ROOT/Divination. The runtime pipeline needs it to size/link Rust
# types. Overridable via DIVINATION_DIR for testing against an external checkout.
DIVINATION_DIR="${DIVINATION_DIR:-$REPO_ROOT/Divination}"
if [ -z "${SKIP_DIVINATION:-}" ]; then
  echo ""
  echo "=== Building Divination ($DIVINATION_DIR) ==="
  if [ ! -f "$DIVINATION_DIR/Cargo.toml" ]; then
    echo "ERROR: DIVINATION_DIR=$DIVINATION_DIR doesn't contain Cargo.toml" >&2
    exit 1
  fi
  cd "$DIVINATION_DIR"
  cargo build
fi

# TesterRust: Rust port of Tester (matches Tester's CLI/semantics plus four rust-interop
# additions and a directory-scanned tests/rust-interop/ path). Used to drive both the
# Vale corpus and the rust-interop test suite.
if [ -z "${SKIP_TESTERRUST:-}" ]; then
  echo ""
  echo "=== Building TesterRust ==="
  cd "$REPO_ROOT/TesterRust"
  cargo build
fi

echo ""
echo "Build complete. Artifacts:"
[ -z "${SKIP_FRONTEND:-}" ]    && echo "  Frontend:    $REPO_ROOT/Frontend/Frontend.jar"
[ -z "${SKIP_BACKEND:-}" ]     && echo "  Backend:     $REPO_ROOT/Backend/build/backend"
[ -z "${SKIP_VALERUSTER:-}" ]  && echo "  ValeRuster:  $REPO_ROOT/ValeRuster/target/debug/ValeRuster"
[ -z "${SKIP_COORDINATOR:-}" ] && echo "  Coordinator: $REPO_ROOT/Coordinator/build/valec"
[ -z "${SKIP_DIVINATION:-}" ]  && echo "  Divination:  $DIVINATION_DIR/target/debug/Divination"
[ -z "${SKIP_TESTERRUST:-}" ]  && echo "  TesterRust:  $REPO_ROOT/TesterRust/target/debug/testvalec"
echo ""
echo "Run \`./TesterRust/target/debug/testvalec ... ri_\` to drive the rust-interop suite — see scripts/README.md."
