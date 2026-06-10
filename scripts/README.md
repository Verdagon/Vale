# Rust Interop Scripts

One script for the Vale → Rust interop pipeline:

- `build-rust-interop.sh` — builds every component the pipeline needs (Frontend, Backend, ValeRuster, Coordinator, Divination, TesterRust).

Tests are driven by `TesterRust/target/debug/testvalec` (the Rust port of Tester) — see the **Test** section below and `tests/rust-interop/README.md`.

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
- **Divination** — vendored at `Divination/` in this repo (a flat copy of the upstream `github.com/Verdagon/Divination` source). Built automatically by `build-rust-interop.sh`.

On M-series Macs: if you have `swiftly` installed (Swift toolchain manager), its `~/.swiftly/bin/clang` shim can hang on mach IPC when invoked as a subprocess. Pass `--clang_path /usr/bin/clang` to `testvalec` to avoid this.

## Build

```bash
./scripts/build-rust-interop.sh ~/BootstrappingValeCompiler
```

First arg is the path to your bootstrapping Vale compiler. Divination is built automatically from the vendored `Divination/` source; to build against an external Divination checkout instead:

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
| Divination   | `Divination/target/debug/Divination`          |
| TesterRust   | `TesterRust/target/debug/testvalec`           |

The build script uses debug profiles for Rust crates to keep iteration fast. For a release build, rebuild manually with `cargo build --release` and point `testvalec`'s path flags at `target/release/` instead.

## Test

TesterRust (the Rust port of Tester) drives the suite. From the repo root:

```bash
./TesterRust/target/debug/testvalec \
  --frontend_path Frontend/Frontend.jar \
  --backend_path Backend/build/backend \
  --builtins_dir Backend/builtins \
  --valec_path Coordinator/build/valec \
  --clang_path /usr/bin/clang \
  --backend_tests_dir Backend/test \
  --frontend_tests_dir Frontend \
  --stdlib_dir stdlib \
  --vale_ruster_path ValeRuster/target/debug/ValeRuster \
  --divination_path Divination/target/debug/Divination \
  --rust_cargo_toml Catter/Dependencies.toml \
  --rust_interop_tests_dir tests/rust-interop \
  --concurrent 4 \
  --verbose false \
  ri_
```

The trailing `ri_` is a positional substring filter that selects exactly the rust-interop tests (their filenames all begin with that prefix). Omit it to also run the canonical Vale corpus from `Frontend/Tests/test/main/resources/programs/`, or replace with another substring (e.g. `vec_capacity`) to scope further.

`Catter/Dependencies.toml` is the vendored reference Cargo.toml (the in-repo descendant of the original Catter proof of concept). All in-tree tests work against it; for your own program, copy it as a starting point.

TesterRust supports `--concurrent N` for parallel execution; the per-test build dirs land under `testbuild/<test_name>_<region>/`. See `tests/rust-interop/README.md` for how to add a new test.

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
