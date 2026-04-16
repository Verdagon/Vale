#!/usr/bin/env bash
# End-to-end smoke test for the Rust interop pipeline.
#
# Builds a minimal Vale program that imports a Rust type (`rust.std.vec.Vec`),
# runs the full toolchain (ValeRuster → Frontend → Backend → Divination → cargo cbuild → clang),
# and asserts the produced binary exits with code 42 (the capacity we requested from Vec::with_capacity).
#
# Usage:
#   ./scripts/test-rust-interop.sh
#
# Required environment variables (no sensible defaults — they live outside this repo):
#   DIVINATION_PATH     Path to the Divination binary (e.g. /Volumes/V/Divination/target/debug/Divination)
#   RUST_CARGO_TOML     Path to a Cargo.toml listing the Rust dependencies to reflect on
#
# Optional overrides (auto-detected if not set):
#   VALEC_PATH          Coordinator's valec binary (default: ./Coordinator/build/valec)
#   FRONTEND_JAR        Frontend.jar (default: ./Frontend/Frontend.jar)
#   BACKEND_PATH        Backend binary (default: ./Backend/build/backend)
#   BUILTINS_DIR        Backend builtins (default: ./Backend/builtins)
#   VALE_RUSTER_PATH    ValeRuster binary (default: ./ValeRuster/target/debug/ValeRuster)
#   CLANG_PATH          clang to use (default: /usr/bin/clang — avoids swiftly's clang shim which
#                       has hung on mach IPC in our tests)
#   TEST_WORKDIR        Where to place the test program (default: /tmp/vale_rust_interop_test)

set -euo pipefail

# Resolve the repo root relative to this script so the test works regardless of CWD.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

: "${DIVINATION_PATH:?Set DIVINATION_PATH to the Divination binary}"
: "${RUST_CARGO_TOML:?Set RUST_CARGO_TOML to a Cargo.toml with the Rust deps}"

VALEC_PATH="${VALEC_PATH:-$REPO_ROOT/Coordinator/build/valec}"
FRONTEND_JAR="${FRONTEND_JAR:-$REPO_ROOT/Frontend/Frontend.jar}"
BACKEND_PATH="${BACKEND_PATH:-$REPO_ROOT/Backend/build/backend}"
BUILTINS_DIR="${BUILTINS_DIR:-$REPO_ROOT/Backend/builtins}"
VALE_RUSTER_PATH="${VALE_RUSTER_PATH:-$REPO_ROOT/ValeRuster/target/debug/ValeRuster}"
CLANG_PATH="${CLANG_PATH:-/usr/bin/clang}"
TEST_WORKDIR="${TEST_WORKDIR:-/tmp/vale_rust_interop_test}"

# Verify all required files exist before we start.
for var in VALEC_PATH FRONTEND_JAR BACKEND_PATH VALE_RUSTER_PATH DIVINATION_PATH RUST_CARGO_TOML CLANG_PATH; do
  path="${!var}"
  if [ ! -e "$path" ]; then
    echo "ERROR: $var=$path does not exist. Build the missing component or set the env var." >&2
    exit 1
  fi
done
if [ ! -d "$BUILTINS_DIR" ]; then
  echo "ERROR: BUILTINS_DIR=$BUILTINS_DIR is not a directory." >&2
  exit 1
fi

echo "Using:"
echo "  VALEC_PATH=$VALEC_PATH"
echo "  FRONTEND_JAR=$FRONTEND_JAR"
echo "  BACKEND_PATH=$BACKEND_PATH"
echo "  VALE_RUSTER_PATH=$VALE_RUSTER_PATH"
echo "  DIVINATION_PATH=$DIVINATION_PATH"
echo "  RUST_CARGO_TOML=$RUST_CARGO_TOML"
echo "  CLANG_PATH=$CLANG_PATH"
echo "  TEST_WORKDIR=$TEST_WORKDIR"
echo ""

# Set up the test program. Vec<int>::with_capacity(42) should give a Vec with capacity 42;
# we return that as the process exit code so the test can observe it without parsing stdout.
rm -rf "$TEST_WORKDIR"
mkdir -p "$TEST_WORKDIR/src"
cat > "$TEST_WORKDIR/src/main.vale" <<'EOF'
import rust.std.vec.Vec;

exported func main() int {
  v = Vec<int>.with_capacity(42i64);
  return v.capacity().TruncateI64ToI32();
}
EOF

BUILD_LOG="$TEST_WORKDIR/build.log"
echo "Running pipeline (full log → $BUILD_LOG)..."

# --no_std true because the minimal test program doesn't need the Vale stdlib — keeps the build
# fast and avoids pulling in things unrelated to what we're testing.
"$VALEC_PATH" build \
  --frontend_path_override "$FRONTEND_JAR" \
  --backend_path_override "$BACKEND_PATH" \
  --builtins_dir_override "$BUILTINS_DIR" \
  --clang_override "$CLANG_PATH" \
  --vale_ruster_path "$VALE_RUSTER_PATH" \
  --divination_path "$DIVINATION_PATH" \
  --rust_cargo_toml "$RUST_CARGO_TOML" \
  --output_dir "$TEST_WORKDIR/build" \
  --no_std true \
  mymodule="$TEST_WORKDIR/src" > "$BUILD_LOG" 2>&1

if [ ! -x "$TEST_WORKDIR/build/main" ]; then
  echo "FAIL: build succeeded but $TEST_WORKDIR/build/main is missing or not executable" >&2
  tail -30 "$BUILD_LOG" >&2
  exit 1
fi

# Run the binary and check its exit code.
set +e
"$TEST_WORKDIR/build/main"
actual_exit=$?
set -e

if [ "$actual_exit" -ne 42 ]; then
  echo "FAIL: expected exit code 42 (Vec capacity), got $actual_exit" >&2
  exit 1
fi

echo ""
echo "PASS: Vale → Rust interop round-trip returned 42 as expected."
