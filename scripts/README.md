# Rust Interop Scripts

Two scripts for the Vale → Rust interop pipeline:

- `build-rust-interop.sh` — builds every component the pipeline needs.
- `test-rust-interop.sh` — smoke-tests the pipeline against pre-built components.

## What is "Rust interop"?

Vale code like this:

```vale
import rust.std.vec.Vec;

exported func main() int {
  v = Vec<int>.with_capacity(42i64);
  return v.capacity().TruncateI64ToI32();
}
```

compiles into a native binary that calls directly into Rust's real `Vec::with_capacity` and `Vec::capacity`. The pipeline works by:

1. **ValeRuster** reads rustdoc JSON for `std::vec::Vec`, emits a `Vec.vale` file with `extern struct Vec<T>` + method declarations.
2. **Frontend** (Vale's Scala compiler) parses your program, parses `Vec.vale`, type-checks everything, emits `.vast` files.
3. **Backend** (Vale's C++/LLVM codegen) reads the `.vast`, invokes **Divination** for each imported Rust type.
4. **Divination** generates a Rust crate that re-exports the needed types with `#[no_mangle]` C shims, runs `cargo cbuild` to produce `libexample-project.a` and cbindgen's `rust_deps.h`.
5. **Backend** emits `.o` + `.c` wrappers that call into the C shims.
6. **clang** links the `.o`, wrappers, and `libexample-project.a` into a native executable.

## Prerequisites

You need these installed on your machine before running any script here:

- **LLVM 16** — `brew install llvm@16`. The Backend's cmake build requires exactly 16.
- **Java 11+** and **sbt** — for building the Frontend.
- **Rust nightly** — pinned to `nightly-2025-12-09` via this repo's `rust-toolchain.toml`. `rustup` will install it on first `cargo` invocation. Required components (`rust-src`, `rust-docs-json`) are declared in the toolchain file and fetched automatically.
- **cargo-c** — `cargo install cargo-c`. Divination's `cargo cbuild` needs this. Rebuild with `--force` after any `libgit2` version bump from Homebrew, or the old binary will fail to load its dylib.
- **A bootstrapping Vale compiler** — a pre-built `valec` + `Frontend.jar` + `backend` + `stdlib/` + `builtins/`, used to compile the Coordinator (which is itself written in Vale). See [Vale's main README](../../README.md) for where to get one.
- **Divination** — cloned separately (`git clone <divination-repo>`). It's a separate repo because it's also used by non-Vale projects.

On M-series Macs: if you have `swiftly` installed (Swift toolchain manager), its `~/.swiftly/bin/clang` shim can hang on mach IPC when invoked as a subprocess. The test script defaults `CLANG_PATH` to `/usr/bin/clang` to avoid this. If you use a different clang, pass `CLANG_PATH=...` to the test script.

## Build

```bash
./scripts/build-rust-interop.sh ~/BootstrappingValeCompiler
```

First arg is the path to your bootstrapping Vale compiler. To also build Divination in the same run:

```bash
DIVINATION_DIR=~/src/Divination ./scripts/build-rust-interop.sh ~/BootstrappingValeCompiler
```

To rebuild only part of the stack (useful during development):

```bash
SKIP_FRONTEND=1 SKIP_BACKEND=1 ./scripts/build-rust-interop.sh ~/BootstrappingValeCompiler
```

Artifacts end up at predictable paths:

| Component    | Output                                        |
|--------------|-----------------------------------------------|
| Frontend     | `Frontend/Frontend.jar`                       |
| Backend      | `Backend/build/backend`                       |
| ValeRuster   | `ValeRuster/target/debug/ValeRuster`          |
| Coordinator  | `Coordinator/build/valec`                     |
| Divination   | `$DIVINATION_DIR/target/debug/Divination`     |

The build script uses debug profiles for Rust crates to keep iteration fast. For a release build, edit the script or rebuild manually with `cargo build --release` and point the test script's `VALE_RUSTER_PATH` at `target/release/` instead.

## Test

```bash
DIVINATION_PATH=~/src/Divination/target/debug/Divination \
  RUST_CARGO_TOML=/path/to/Dependencies.toml \
  ./scripts/test-rust-interop.sh
```

`DIVINATION_PATH` and `RUST_CARGO_TOML` are required env vars — they point at files outside this repo and have no sensible default. In-repo component paths (valec, Frontend.jar, backend, etc.) auto-detect to the standard build-output locations but can be overridden via env vars (`VALEC_PATH`, `FRONTEND_JAR`, `BACKEND_PATH`, `BUILTINS_DIR`, `VALE_RUSTER_PATH`, `CLANG_PATH`).

The script writes a minimal `Vec<int>.with_capacity(42i64)` program, builds it through the full pipeline, runs the resulting binary, and checks that it exits with code 42 (the capacity we requested). All paths are validated before the build starts, so a missing prerequisite fails fast instead of mid-pipeline.

The pipeline's full log lands at `/tmp/vale_rust_interop_test/build.log` for post-mortem debugging.

## Writing your own Rust-interop Vale program

`valec` invocation with Rust interop takes four extra flags beyond a normal build:

```bash
valec build \
  --vale_ruster_path ~/src/ValeRustInterop/ValeRuster/target/debug/ValeRuster \
  --divination_path  ~/src/Divination/target/debug/Divination \
  --rust_cargo_toml  /path/to/Dependencies.toml \
  --rust_output_dir  ./build/rust \
  myproject=src
```

`--rust_output_dir` defaults to `<output_dir>/rust` when the first three are set, so in practice you only need three. The rest of `valec`'s interface works normally.

The `Dependencies.toml` passed as `--rust_cargo_toml` is a regular Cargo.toml that lists the Rust crates you want to reflect on. ValeRuster and Divination both read it so they can query rustdoc and generate bindings. At minimum it needs `libc` and whichever std-detect helpers your imports pull in — start by copying one from an existing interop project.

In your `.vale` files, `import rust.std.vec.Vec;` (or `rust.$crate.$path.$Type`) triggers the pipeline. Methods usable from Vale are whichever ones ValeRuster's simplifier can translate — you'll see `// Skipping (self) method foo: <reason>` comments in the generated binding for anything it can't. Current limitations include raw pointers, slices, lifetimes, method generics, and types outside the whitelisted set (add more via additional `import` lines).

## Troubleshooting

- **`ValeRuster failed for type X ... Not found: X`** — the type path doesn't resolve. Check spelling and that the crate is listed in your `Dependencies.toml`.
- **`error: package ID specification 'std' did not match any packages`** — the pinned toolchain is missing the `rust-docs-json` component. Run `rustup component add rust-docs-json --toolchain nightly-2025-12-09`, or let the repo's `rust-toolchain.toml` handle it on the next `cargo` invocation.
- **`dyld: Library not loaded: libgit2.1.7.dylib`** — your `cargo-cbuild` was built against an older libgit2 that brew has since upgraded. Rebuild with `cargo install cargo-c --force`.
- **`clang` hangs indefinitely** — you're probably invoking `~/.swiftly/bin/clang`. Pass `CLANG_PATH=/usr/bin/clang` explicitly.
- **Build succeeds but binary crashes with architecture mismatch** — your bootstrapping `valec` is x86_64 and Rosetta'd clang defaulted its output architecture to x86_64, but the Rust static lib was built for arm64. The Coordinator normally derives `-arch` from the Rust target triple to prevent this; if your `valec` is arm64 and you still hit it, the derivation isn't finding the target dir — check that `<rust_output_dir>/target/<triple>/release/` exists.
- **The pipeline hangs after "Running Divination..."** — this used to happen when Divination's verbose stdout outpaced the Vale stdlib's blocking-fgetc subprocess reader. Currently worked around by having the Backend redirect Divination's output to `<rust_output_dir>/divination.log`. If you see this again, check the log for the actual error.
