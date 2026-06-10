# Rust Interop — Usage Guide

This guide covers how to write and compile a Vale program that uses Rust types and functions via the automated interop pipeline. It assumes you can already build Vale programs without interop; if not, start with `scripts/mac/build-compiler.sh` or the existing Vale compiler docs.

The interop pipeline lets you write:

```vale
import rust.std.vec.Vec;

exported func main() int {
  v = Vec<int>.with_capacity(42i64);
  return v.capacity().TruncateI64ToI32();
}
```

and have `valec build` automatically reflect on the Rust type, generate Vale bindings, compile a Rust C-API wrapper, and link it into your final binary. No hand-written FFI code.

---

## Prerequisites

### Rust toolchain

A specific Rust nightly is required. The root `rust-toolchain.toml` in this repo pins it to `nightly-2025-12-09`, so running any `cargo` command inside this repo picks it up automatically via rustup. You need:

- `nightly-2025-12-09` toolchain
- `rust-src` component (for both ValeRuster's `cargo rustdoc -p std` and Divination's `-Zbuild-std`)
- `rust-docs-json` component (ValeRuster prefers the prebuilt JSON at `share/doc/rust/json/`)

Both components are declared in `rust-toolchain.toml` — rustup will install them on first use. To verify:

```
rustup show
# should include:
#   nightly-2025-12-09-aarch64-apple-darwin
# and its components including rust-src and rust-docs-json
```

### `cargo-c`

Divination's `cargo cbuild` step requires the `cargo-c` crate to be installed as a cargo binary:

```
cargo install cargo-c --force
```

Use `--force` if upgrading — the installed binary statically links libgit2 and other libs at a specific version, and upgrading the system libgit2 without rebuilding cargo-c will break it (`dyld: Library not loaded: .../libgit2.1.X.dylib`). We hit this during development; the fix is exactly `cargo install cargo-c --force`.

### Four binaries

The interop pipeline orchestrates four binaries. Paths below are relative to the `ValeRustInterop` repo root.

| Binary | Default location | How to build |
|---|---|---|
| `Frontend.jar` | `Frontend/Frontend.jar` | `cd Frontend && sbt 'set test in assembly := {}' assembly` |
| `backend` | `Backend/build/backend` | `cd Backend && cmake -B build -DLLVM_DIR=$(brew --prefix llvm@16)/lib/cmake/llvm && cmake --build build` |
| `ValeRuster` | `ValeRuster/target/debug/ValeRuster` | `cd ValeRuster && cargo build` |
| `valec` (Coordinator) | `Coordinator/build/valec` | `cd Coordinator && ./build.sh ~/BootstrappingValeCompiler` |
| `Divination` | `Divination/target/debug/Divination` | `cd Divination && cargo build` (vendored in this repo) |

### clang

**Use `/usr/bin/clang`, not whatever `clang` resolves to on your PATH.** On machines with `swiftly` installed (Apple's Swift toolchain manager), `which clang` may point at `~/.swiftly/bin/clang` — a shim that in our testing hung indefinitely on mach IPC (`CFRunLoop` waiting on `mach_msg2_trap`) when invoked as a subprocess. Pass `--clang_override /usr/bin/clang` explicitly to bypass it.

### A Cargo.toml with your Rust dependencies

You need a `Dependencies.toml` file (name is arbitrary) that lists every Rust crate your program will pull types from. Divination reads this to know what crates to reflect on. Example:

```toml
[package]
name = "example-project"
version = "0.1.0"
edition = "2021"

[dependencies]
libc = { version = "0.2", optional = true }
static_assertions = "1.1.0"
subprocess = "0.2.9"
```

The `[package]` metadata and exact deps don't matter much for a program that only uses `std::vec::Vec` — the reference file at `/Volumes/V/Catter/Dependencies.toml` works fine as a starting point even if your program doesn't actually touch `libc` or `subprocess`.

---

## Quickstart

After the prerequisites are met, drive the 32-test suite end-to-end with TesterRust:

```
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
  --rust_cargo_toml /path/to/your/Dependencies.toml \
  --rust_interop_tests_dir tests/rust-interop \
  --concurrent 4 \
  ri_
```

The `ri_` substring filter selects exactly the rust-interop tests. Each test program lives in `tests/rust-interop/ri_*.vale` with a `// expected_exit: N` header. If the summary line is `Done! Passed 31/32` (32 if Bug 4 is fixed), your environment is wired correctly.

---

## The `valec build` flags

The Coordinator (`valec`) gates Rust interop on four flags. Either provide all four or omit all four — passing some and not others triggers a panic with a clear message.

| Flag | Description | Example |
|---|---|---|
| `--vale_ruster_path` | Path to the ValeRuster binary | `/.../ValeRuster/target/debug/ValeRuster` |
| `--divination_path` | Path to the Divination binary | `/.../Divination/target/debug/Divination` |
| `--rust_cargo_toml` | Path to your Cargo.toml with Rust deps | `/.../Dependencies.toml` |
| `--rust_output_dir` | Where to put generated Rust artifacts | Optional; defaults to `<output_dir>/rust` |

A complete invocation:

```bash
valec build \
  --frontend_path_override /path/to/Frontend.jar \
  --backend_path_override /path/to/backend \
  --builtins_dir_override /path/to/Backend/builtins \
  --clang_override /usr/bin/clang \
  --vale_ruster_path /path/to/ValeRuster \
  --divination_path /path/to/Divination \
  --rust_cargo_toml /path/to/Dependencies.toml \
  --output_dir ./build \
  mymodule=./src
```

The `--frontend_path_override`, `--backend_path_override`, `--builtins_dir_override`, and `--clang_override` flags are pre-existing (non-interop) — you'd pass them for any out-of-tree build. The last four (`--vale_ruster_path` through `--rust_output_dir`) are the ones this feature adds.

You can pass `--no_std true` to skip the Vale stdlib when your program doesn't need it. That's what the smoke test does. For real programs that want things like `println` or `List`, leave `--no_std` at its default (false) and ensure `<valec_path>/stdlib/src` exists.

---

## Writing a Vale program that uses Rust types

### Import syntax

The module name `rust` is reserved for Rust interop imports:

```vale
import rust.<crate>.<module_path>.<Type>;
```

For example:

```vale
import rust.std.vec.Vec;
import rust.std.option.Option;
import rust.core.cell.RefCell;
```

The first segment after `rust` is the crate (`std`, `core`, `alloc`, or any crate listed in your `Dependencies.toml`). Subsequent segments are the module path inside that crate, ending with the type name.

One `import` line = one type. If you want `Vec` and `Option`, write two `import` lines. ValeRuster is invoked once per import.

### What you get

The generated `.vale` file for `Vec` (you can inspect it at `<rust_output_dir>/vale_bindings/std/vec/Vec.vale`) looks like:

```vale
#!DeriveStructConstructor extern struct Vec<T> imm {
  extern func new() Vec<T>;
  extern func with_capacity(capacity i64) Vec<T>;
  // Skipping (self) method try_with_capacity: Didn't contain whitelisted type: TryReserveError
  // Skipping (self) method from_raw_parts: Encountered raw pointer type
  extern func capacity(self &Vec<T>) i64;
  extern func reserve(self &Vec<T>, additional i64);
  // ... etc
}
```

Key points:

- **`#!DeriveStructConstructor`** suppresses Vale's auto-generated struct constructor. You can't construct `Vec<T>` with `Vec(…)` — it's opaque. Use the extern methods instead.
- **`imm`** marks the type as immutable for Vale's purposes. Vale doesn't see inside the Rust type; mutation happens via borrowed method calls.
- **`self &Vec<T>`** for methods means Vale passes a borrow. Rust's `&self` and `&mut self` both map to this borrow — Vale doesn't distinguish mutability at the Vale level (yet).
- **Skipped methods** appear as comments. Common skip reasons:
  - `Encountered raw pointer type` — `*const T`/`*mut T` aren't supported
  - `Encountered slice type` — `&[T]`/`&mut [T]` aren't supported yet
  - `Method generics unsupported` — methods like `fn retain<F: Fn(&T) -> bool>(...)` where the method itself is generic
  - `Encountered defaulted rune` — allocator generics like `new_in(alloc: A)`
  - `Didn't contain whitelisted type: <TypeName>` — the method's signature mentions a type you didn't explicitly `import rust.` (only the types you import + their transitive closure through primitives are whitelisted)
  - `Couldn't resolve foreign path step '<name>' in crate '<crate>'` — ValeRuster's foreign-crate walk hit a dead end (usually a private module or a re-export it can't follow)

### Calling Rust methods

Methods appear as normal Vale functions taking `self`:

```vale
v = Vec<int>.with_capacity(42i64);  // static constructor
n = v.capacity();                   // method call on borrow
```

Argument and return types follow a straightforward mapping:

| Rust | Vale |
|---|---|
| `i32`, `u32` | `int` |
| `i64`, `u64`, `usize`, `isize` | `i64` |
| `bool` | `bool` |
| `f32` | `float` |
| `&T` / `&mut T` | `&T` |
| `T` (owned return) | `T` |
| `()` | (no return value) |

`usize`/`isize` map to `i64` because Vale doesn't have a pointer-sized integer. On 64-bit targets this is fine; on 32-bit it would silently truncate.

### What's not supported today

- **Traits** — you can call methods on concrete types, but you can't work with `dyn Trait` or bounds like `T: Iterator`.
- **Closures as arguments** — `Vec::retain` and friends are skipped.
- **Lifetimes as generics** — `fn from(s: &'a str)` is skipped.
- **Allocators** — anything with a defaulted `A: Allocator` generic is skipped.
- **Slices / raw pointers / `str`** — supported types pass through but methods returning/taking them are skipped.
- **Transitive imports** — if `Vec::pop` returns `Option<T>`, you can only call it if you've also written `import rust.std.option.Option;`. The whitelist is the set of types you explicitly import.

---

## Where the outputs go

Given `--output_dir ./build` and default `--rust_output_dir`:

```
build/
  vast/                           ← Frontend output (.vast per Vale package)
    mymodule.vast
    rust.std.vec.vast             ← Backend's input for Rust types
  vpst/                           ← Frontend output (.vpst per .vale file)
    main.vpst
    Vec.vpst                      ← your imported Rust bindings, parsed
    ...
  include/                        ← Backend-generated C headers
    ValeBuiltins.h
    rust/
      alloc_vec_Vec_1__i32___with_capacity.h
      ...
  abi/                            ← Backend-generated C wrappers
    rust/
      alloc_vec_Vec_1__i32___with_capacity.c
      ...
  build.o, build.s                ← Backend-generated LLVM output
  rust/                           ← Rust interop dir (rust_output_dir)
    vale_bindings/                ← ValeRuster output
      std/vec/Vec.vale
    Cargo.toml                    ← Divination-generated
    Cargo.lock
    cbindgen.toml
    src/
      lib.rs
      capi.rs                     ← Divination-generated Rust wrappers
    sizes.txt                     ← Divination's type sizes + function mangling
    divination.log                ← stdout/stderr from Divination invocation
    target/                       ← cargo cbuild output
      aarch64-apple-darwin/release/
        libexample-project.a      ← the static library that gets linked
        rust_deps.h               ← cbindgen's header
  main                            ← the final executable
```

---

## Troubleshooting

### "ValeRuster failed for type std.vec.Vec (exit code 2)"

Usually means ValeRuster's CLI rejected the args. Check that `--vale_ruster_path` points to an actual binary and your `--rust_cargo_toml` exists. ValeRuster expects subcommand `list` at the END of its args — the Coordinator/Frontend handle this automatically, but if you're invoking ValeRuster directly, the order matters.

### "Error: invalid type: integer \`9314\`, expected a string at line 1 column 12"

ValeRuster's `rustdoc-types` crate version doesn't match your installed nightly's rustdoc JSON format. If you're on the pinned `nightly-2025-12-09` and hit this, the pin wasn't picked up — check `rustc --version` from inside the repo.

### "package ID specification \`std\` did not match any packages"

Your nightly is missing the `rust-src` component. `rustup component add rust-src --toolchain nightly-2025-12-09`.

### "error: invalid type: integer `9314`, expected a string" or similar rustdoc JSON parse errors

ValeRuster is probably falling back to `cargo rustdoc -p std` because the prebuilt JSON isn't installed. Add the `rust-docs-json` component — it's in the pinned `rust-toolchain.toml` but may need `rustup component add rust-docs-json --toolchain nightly-2025-12-09` to install.

### "panic_immediate_abort is now a real panic strategy!"

Your Divination is out of date. The old `-Z build-std-features=panic_immediate_abort` was removed when this became a real strategy; the current Divination removes that flag.

### "Library not loaded: .../libgit2.1.7.dylib"

`cargo-cbuild` is built against an older libgit2 than what you have installed. Fix: `cargo install cargo-c --force` to rebuild it against your current libgit2.

### Build hangs after "Compiling panic_unwind v0..."

The Coordinator's Subprocess module reads subprocess stdout one char at a time via blocking `fgetc`. When Divination's verbose cbuild output overflows the pipe, the parent can't drain fast enough. The Backend now redirects Divination's output to `rust_output_dir/divination.log` to avoid this — if you see the hang, check that your Backend is up to date (commit `a01d3d64` or later).

### Build succeeds, then clang runs forever (13+ minutes on a small program)

Your `clang` is actually `swiftly` and it's hung on mach IPC. Use `--clang_override /usr/bin/clang` explicitly.

### Program builds but segfaults / links fail with arch mismatch

The Coordinator derives `-arch` from the Rust target triple by inspecting `target/<triple>/release/`. If somehow the wrong triple gets inferred (e.g. stale `target/` with a different triple than what Divination used this run), delete `<output_dir>/rust/target/` and rebuild.

### "Couldn't find anything with the name 'Vec'"

Your `import rust.std.vec.Vec;` line was parsed but the generated `.vale` file never landed. Check that:
1. `<rust_output_dir>/vale_bindings/std/vec/Vec.vale` exists after the build
2. If yes, the Frontend's resolver may have failed; check `build.log` for "Couldn't find: rust.std.vec"
3. If no, ValeRuster failed or wasn't invoked; check its output earlier in the log

---

## Limitations you'll want to know about

- **Type whitelist is flat.** If `Vec::pop` returns `Option<T>`, you can't call `pop` unless you've separately written `import rust.std.option.Option;`. ValeRuster doesn't auto-follow reachable types.
- **The generated `rust_deps` crate has a fixed `[package.metadata.capi.library].name = "example-project"`.** This is hardcoded by Divination. It's fine for a single-user program but means two programs in the same tree can collide if they share a `rust_output_dir`.
- **Every build rebuilds Rust's core/std/alloc.** `-Zbuild-std` recompiles the stdlib to get the right panic strategy. Expect 10–15s added to every build even for trivial programs.
- **No incremental reflection.** ValeRuster regenerates the `.vale` binding every build. For many imports this adds up.
- **The pinned nightly is a moving target.** When you bump the `rust-toolchain.toml`, you likely also need to bump `rustdoc-types` in `ValeRuster/Cargo.toml` to match the new JSON format, and possibly touch Divination if the cbuild surface changed. See the commit message on `rust-toolchain.toml` for context.

## See also

- [../arcana/ClangArchDerivedFromRustTriple-CADFRTZ.md](../arcana/ClangArchDerivedFromRustTriple-CADFRTZ.md) — why `-arch` matters when running under Rosetta.
- [../arcana/DivinationStdoutToLogFileNotPipe-DSLFNPZ.md](../arcana/DivinationStdoutToLogFileNotPipe-DSLFNPZ.md) — why Divination's output lands in a file.
- [../arcana/RustResolverPrecedesGenericResolver-RRPGRZ.md](../arcana/RustResolverPrecedesGenericResolver-RRPGRZ.md) — resolver chain ordering detail.
- [../migration/rust-interop.md](../migration/rust-interop.md) — known ephemeral limitations.
