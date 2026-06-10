# Rust Interop — Architecture

This document explains how `import rust.std.vec.Vec` becomes a working function call in a Vale binary. It covers every component in the pipeline, the data that flows between them, the file layout at build time, and why each piece exists.

For user-facing instructions (how to run `valec build`, what types are supported, troubleshooting), see `rust-interop-usage.md`.

---

## Components

Six tools cooperate to turn `import rust.*` into a linkable binary:

| Component | Repo | Role |
|---|---|---|
| **ValeRuster** | `ValeRustInterop/ValeRuster/` | Reads Rust's rustdoc JSON and emits `.vale` files declaring `extern struct` with extern methods for a requested type. |
| **Frontend** | `ValeRustInterop/Frontend/` | Vale's Scala-based front half (lex/parse/postparse/higher-typing/typing/simplifying/instantiating). Invokes ValeRuster as part of package resolution; emits `.vast` files. |
| **Backend** | `ValeRustInterop/Backend/` | Vale's C++ LLVM codegen. Reads `.vast`, generates C wrappers and LLVM IR, invokes Divination to learn Rust type sizes. |
| **Divination** | `ValeRustInterop/Divination/` (vendored from `github.com/Verdagon/Divination`) | Reads rustdoc JSON, generates a Rust C-API crate (`src/capi.rs`, `Cargo.toml`), runs `cargo cbuild` to produce a static library, and writes a `sizes.txt` describing type sizes/alignments and function mangled names. |
| **Coordinator** (`valec`) | `ValeRustInterop/Coordinator/` | The top-level orchestrator written in Vale. Parses user flags, invokes Frontend → Backend → clang in sequence, threads Rust interop flags through to each component, locates the produced Rust `.a` and its cbindgen header, derives the target arch. |
| **cargo-c** (`cargo cbuild`) | External (`cargo install cargo-c`) | Builds a Rust crate as a C-compatible static library, emitting a cbindgen header (`rust_deps.h`) and a staticlib (`lib<name>.a`) under `target/<triple>/release/`. |

These five together (plus clang for final linking and Apple's `ld`) turn Vale source + user Rust crates into a native executable.

---

## The high-level flow

For `import rust.std.vec.Vec` in a program that uses `Vec<int>`:

```
           ┌──────────────────────────────────────────────────────┐
User →     │ valec build ... --vale_ruster_path=... ... main.vale │
           └───────────────────────┬──────────────────────────────┘
                                   ▼
                        ┌────────────────────┐
                        │  Coordinator       │  Vale, built from Coordinator/src
                        │  (valec)           │
                        └──────┬──────┬──────┘
                               │      │
                        invoke │      │ invoke
                               ▼      ▼
              ┌─────────────────────┐  ┌─────────────────────┐
              │ Frontend (Java)     │  │ Backend (C++)       │
              │ PassManager,        │  │ doRustyThings()     │
              │ LexAndExplore,      │  │ in vale.cpp         │
              │ etc.                │  │                     │
              └──────┬──────────────┘  └──────┬──────────────┘
                     │ invokes per import      │ invokes once
                     ▼                          ▼
              ┌──────────────┐            ┌──────────────┐
              │ ValeRuster   │            │ Divination   │
              │ (Rust)       │            │ (Rust)       │
              └──────┬───────┘            └──────┬───────┘
                     │ reads                     │ reads
                     ▼                           ▼
          rustdoc JSON (std.json,        rustdoc JSON,
          alloc.json, core.json,         user's Cargo.toml
          user-crate JSON)                     │
                                               │ invokes
                                               ▼
                                      ┌────────────────────┐
                                      │ cargo cbuild       │
                                      │ (cargo-c)          │
                                      └──────┬─────────────┘
                                             │ produces
                                             ▼
                               target/<triple>/release/
                                 lib<name>.a, rust_deps.h

                               ┌──────────────────────────┐
Coordinator gathers all        │ clang                    │
outputs + user C wrappers  →   │ (final link)             │ → main executable
                               └──────────────────────────┘
```

---

## A walkthrough: `Vec<int>.with_capacity(42i64)`

Let's trace every step of compiling:

```vale
import rust.std.vec.Vec;

exported func main() int {
  v = Vec<int>.with_capacity(42i64);
  return v.capacity().TruncateI64ToI32();
}
```

### Step 1: Coordinator parses user flags

`Coordinator/src/build.vale::build_stuff` reads `--vale_ruster_path`, `--divination_path`, `--rust_cargo_toml`, and `--rust_output_dir` from argv. If any of these is set, all must be set — `rust_interop_active` becomes `true`, and `rust_output_dir` defaults to `<output_dir>/rust` if unspecified.

The Coordinator then calls `invoke_frontend`, forwarding `vale_ruster_path`, `rust_cargo_toml`, and `rust_output_dir` as params. These are empty strings when interop is disabled, which `invoke_frontend` detects to decide whether to pass the flags to the Frontend CLI.

### Step 2: Frontend pre-scans for `import rust.*`

In `Frontend/PassManager/src/dev/vale/passmanager/PassManager.scala`, `build()` calls `invokeValeRusterIfNeeded(opts, allInputs)` before constructing the compilation. This function:

1. Reads every input source (`SourceInput.code`, directory-scanned `.vale` files from `ModulePathInput`, and `DirectFilePathInput` files).
2. Runs the regex `import\s+rust\s*\.([\w\.]+)` against each to extract dotted type paths (e.g. `std.vec.Vec`).
3. For each unique path, shells out to `ValeRuster --crate <crate> --cargo_toml <path> --vale_bindings_dir <rust_output_dir>/vale_bindings --output_dir <rust_output_dir>/rust_scratch --type <dotted.path> list`.
4. Panics if any ValeRuster invocation returns non-zero.

After this pre-scan, `<rust_output_dir>/vale_bindings/std/vec/Vec.vale` exists on disk.

### Step 3: ValeRuster generates `Vec.vale`

ValeRuster's `list` subcommand (in `ValeRuster/src/main.rs`):

1. Accepts either an `--input_file` with `import rust.*` lines OR a `--type <path>` that synthesizes a single import line.
2. Runs `cargo rustdoc --output-format json` implicitly by reading the prebuilt JSON files at `$(rustc --print sysroot)/share/doc/rust/json/{std,alloc,core}.json`, plus running `cargo rustdoc -p <crate>` for user crates (from `--cargo_toml`). The prebuilt files come from the `rust-docs-json` rustup component.
3. Resolves the dotted path `std.vec.Vec` to a concrete rustdoc `Item` by walking crate indexes and following `use` re-exports. See `ValeRuster/src/resolve.rs` and `resolve_id.rs`.
4. For each `impl` block on `Vec`, iterates the methods and calls `simplify_type` on each parameter and return type. `simplify_type` either succeeds (producing a `SimpleType` we can emit) or returns `SimplifyError::Unsupported(reason)` for things like raw pointers, slices, `dyn Trait`, or types outside the whitelist.
5. Emits one Vale `extern func` line per method that succeeded, and a `// Skipping ...` comment for each that didn't.
6. Writes the result to `<vale_bindings_dir>/std/vec/Vec.vale`.

The generated file declares `#!DeriveStructConstructor extern struct Vec<T> imm { ... }` — `#!DeriveStructConstructor` suppresses Vale's auto-constructor (you can't construct a Rust Vec from Vale data), and `imm` marks it immutable for Vale's purposes (mutation happens via extern method calls which Vale can't see into).

### Step 4: Frontend's compilation begins

PassManager constructs a `FullCompilation` with a package resolver built as:

```scala
Builtins.getCodeMap(interner, keywords)
  .or(packageCoord => resolveRustPackageContents(rustBindingsDir, packageCoord))
  .or(packageCoord => resolvePackageContents(interner, allInputs, packageCoord))
```

The order matters: `resolvePackageContents` always returns `Some(Map.empty)` for packages it can't find (it doesn't return `None`), which would short-circuit any fallback chained after it. `resolveRustPackageContents` is placed *before* it — for rust packages, this resolver finds the generated `.vale` files; for non-rust packages it returns `None` and the fallback to `resolvePackageContents` proceeds.

`resolveRustPackageContents` builds the path `<rust_bindings_dir>/<package_steps_joined_by_slash>/` and reads every `.vale` file in it. For `rust.std.vec`, that's `<rust_bindings_dir>/std/vec/Vec.vale`.

### Step 5: LexAndExplore processes `rust.std.vec` like any other package

`Frontend/LexingPass/src/dev/vale/lexing/LexAndExplore.scala` used to have an explicit `if (neededPackageCoord.module.str == "rust") { /* skip */ }` guard — this existed because no resolver could handle rust packages, so the lexer would silently drop them to avoid crashing. That guard is now removed: rust packages flow through the same `resolver.resolve(neededPackageCoord)` call as any other, and the resolver chain above handles them.

The lexer parses `Vec.vale`, produces lexing tokens (`TopLevelStructL`, `TopLevelFunctionL`, etc.), and discovers any further imports in it — though our generated files don't transitively import other rust types, so this stops there.

### Step 6: The rest of Frontend runs normally

Parser → PostParser → HigherTyping → Typing → Simplifying → Instantiating. The `extern struct Vec<T>` declaration and its `extern func` methods go through all the usual passes. When `main.vale`'s `Vec<int>.with_capacity(42i64)` is typed, the overload resolver finds `with_capacity(capacity i64) Vec<T>` in the `rust.std.vec` package and instantiates it with `T = int`.

At some point during typing, the `sanityCheckConclusion` in `Frontend/TypingPass/src/dev/vale/typing/Compiler.scala` validates that any placeholder types in the inferred conclusions belong to the current root denizen (see the `OWPFRD` rule in `Generics.md`). For **lifted** struct methods — user-defined methods whose first parameter is `self T` for a struct `T`, like the builtin `drop(self Ok<T,E>)` — the placeholder's name prefix is `[drop]` (the function template's path before lifting) while the post-lift env `id` is `[Ok<T,E>, drop(args)]`. The check must compare against `rootDenizenEnv.denizenTemplateId` (pre-lift template) rather than `rootDenizenEnv.id` (post-lift instantiation) for the `startsWith` comparison to succeed. This fix applies universally, not just for rust imports — the pre-fix code crashed on any program that caused Result's drop to be compiled, including `exported func main() {}`.

Frontend writes `.vast` files per package to `<output_dir>/vast/`:

- `mymodule.vast` contains user's compiled program
- `rust.std.vec.vast` contains the compiled extern struct + methods, with the concrete `Vec<i32>` instantiation

### Step 7: Backend reads `.vast` and identifies Rust work

`Backend/src/vale.cpp::compileValeCode` loads all `.vast` files. For each package with `projectName == "rust"`, `doRustyThings()` (line ~1320):

1. Iterates the package's `functionToExtern` map. For each extern function, calls `rustifySimpleId(externFunc->simpleId, true)` to produce a Rust-style string like `"std::vec::Vec<i32>::with_capacity"`. Appends `#pragma rsfn <rust_func_str>` to a stringstream.
2. Iterates the package's `kindToExtern` map. For each extern kind, produces `"std::vec::Vec<i32>"` and appends `#pragma rsuse <rust_kind_str>`.
3. Writes the stringstream to `rust_externs.h` in CWD — this is Divination's input.
4. If the input is non-empty, validates that `--divination_path`, `--rust_cargo_toml`, `--rust_output_dir` are all set (via CLI flags → `ValeOptions`), and errors if not.
5. Invokes Divination via `std::system`:

```
<divinationPath> --crate std --cargo_toml <rustCargoToml> \
  --output_dir <rustOutputDir> \
  --output_sizes <rustOutputDir>/sizes.txt \
  --input_file rust_externs.h \
  instantiate \
  > <rustOutputDir>/divination.log 2>&1
```

The `> ... 2>&1` redirect is critical: Divination's cbuild step produces megabytes of output (rebuilding core/std/alloc), and the Vale stdlib's `Subprocess` module reads subprocess stdout via a blocking `fgetc` one char at a time. When a parent process (like the Coordinator) captures Backend's stdout, the pipe fills up and the parent can't drain it fast enough, deadlocking. Redirecting to a file avoids the problem entirely. The log remains for debugging.

### Step 8: Divination generates the Rust crate and builds it

`Divination/src/main.rs`'s `instantiate` subcommand:

1. Reads `rust_externs.h` for the `#pragma rsfn`/`#pragma rsuse` directives.
2. Reads the same rustdoc JSON files as ValeRuster to understand Rust type layout.
3. Writes `<rust_output_dir>/Cargo.toml` (based on a template that references the user's `--cargo_toml` for dependencies), `<rust_output_dir>/src/lib.rs` (mostly a stub), and `<rust_output_dir>/src/capi.rs` (the C API — `extern "C"` wrappers for each requested function).
4. Runs a tiny helper program via `cargo run` to probe sizes and alignments of each requested type (by doing `size_of` / `align_of` at Rust-time). This populates `sizes.txt`.
5. Invokes `cargo cbuild` with the detected host triple (from `rustc -vV`'s `host:` line — previously hardcoded to `aarch64-apple-darwin`):

```
cargo +nightly cbuild --release \
  --manifest-path <rust_output_dir>/Cargo.toml \
  --destdir=clibthing \
  --target <host_triple> \
  -Z build-std=std,panic_abort \
  --library-type staticlib
```

`cargo cbuild` (from the cargo-c plugin) does three things: compiles the Rust crate with `-Zbuild-std` to a staticlib, runs cbindgen to generate a C header (`rust_deps.h`), and writes both to `target/<triple>/release/`.

6. Writes `sizes.txt` to the path Backend passed in. Format is one record per line:

```
type/<rust_type>/<mangled_name>/<size>/<alignment>
fn/<rust_fn_sig>/<mangled_name>/<return_rust_type>/<param1>/<param2>/...
```

Example:

```
type/&str/iref_1__str/16/8
type/std::vec::Vec<i32>/alloc_vec_Vec_1__i32/24/8
fn/std::vec::Vec<i32>::with_capacity/alloc_vec_Vec_1__i32___with_capacity/alloc::vec::Vec::<i32>/usize
fn/std::vec::Vec<i32>::capacity/alloc_vec_Vec_1__i32___capacity/usize/&alloc::vec::Vec::<i32>
```

### Step 9: Backend reads sizes.txt and generates C wrappers

Back in `doRustyThings()`:

1. Reads `<rust_output_dir>/sizes.txt` and populates `typeStrToMangledNameAndSizeAndAlignment` and `funcStrToMangledNameAndRetAndParams`.
2. For each extern Rust kind, declares an RC opaque and a linear opaque in the Backend's type system with the size/alignment from `sizes.txt`. The opaque is opaque to Vale — Vale can't see inside — but Backend knows the size for copy/move purposes.
3. For each extern Rust function, emits a C wrapper file under `<output_dir>/abi/rust/<mangled>.c` that bridges Vale's calling convention (first-class return by value) to the C API cbindgen produced (return via out-pointer). Example wrapper for `Vec::with_capacity`:

```c
#include "rust/alloc_vec_Vec_1__i32___with_capacity.h"
extern void  vale_abi_rust_alloc_vec_Vec_1__i32___with_capacity(
    rust_alloc_vec_Vec_1__i32* __ret, int64_t param0) {
  *__ret = rust_alloc_vec_Vec_1__i32___with_capacity(param0);
}
```

And a corresponding header at `<output_dir>/include/rust/<mangled>.h`:

```c
#include "ValeBuiltins.h"
#include "rust_deps.h"    // ← provided by cargo cbuild in target/<triple>/release/
extern rust_alloc_vec_Vec_1__i32 rust_alloc_vec_Vec_1__i32___with_capacity(uintptr_t param0);
extern void  vale_abi_rust_alloc_vec_Vec_1__i32___with_capacity(rust_alloc_vec_Vec_1__i32* __ret, int64_t param0);
```

4. Generates `<output_dir>/build.o` (LLVM-compiled Vale code) that calls `vale_abi_rust_...` for each extern invocation.

### Step 10: Coordinator locates Rust artifacts

Back in `Coordinator/src/build.vale`, after the Backend exits:

```
maybe_rust_static_lib Opt<Path> = None<Path>();
maybe_rust_include_dir Opt<Path> = None<Path>();
```

If `rust_interop_active`, it walks `<rust_output_dir>/target/*/release/` (there's usually only one triple directory since we build for a single host) looking for top-level `lib*.a` files that are regular files (not in `deps/`). It records:

- `maybe_rust_static_lib = Some(<target>/<triple>/release/libexample-project.a)` — what clang will link
- `maybe_rust_include_dir = Some(<target>/<triple>/release/)` — where cbindgen's `rust_deps.h` lives

The triple is recovered implicitly from the parent dir name of `maybe_rust_include_dir` — this is done in `clang.vale` rather than passed as a separate variable to avoid the `Opt<str>` + reassign pattern that the bootstrap Vale compiler can't type-check (see below).

### Step 11: invoke_clang builds the final binary

`clang.vale::invoke_clang` receives:

- `clang_inputs` — the list of `.o`/`.c` files + the Rust `.a`
- `rust_include_dir` — the empty string when interop is off, otherwise the path containing `rust_deps.h`

It derives `rust_target_triple` by taking `Path(rust_include_dir).parent().name()` — for `.../target/aarch64-apple-darwin/release/`, that yields `"aarch64-apple-darwin"`.

It assembles clang args:
- `-I<output_dir>/include` (always — Vale builtins)
- `-I<rust_include_dir>` (when interop is active — for `rust_deps.h`)
- `-arch arm64` when the triple is `aarch64-apple-darwin`, `-arch x86_64` when it's `x86_64-apple-darwin`, nothing otherwise. This is necessary because the bootstrap `valec` binary is x86_64; when it runs under Rosetta, child `clang` defaults to x86_64 output, which can't link against an arm64 `.a`. Explicit `-arch` overrides the inherited default.
- Other flags (`-o`, `-lm`, `-fPIC`, `-fPIE`, etc.) as usual.

clang compiles any remaining `.c` files (the Backend-generated ABI wrappers under `<output_dir>/abi/rust/*.c`), then invokes `ld` to link `build.o` + builtin C files + ABI wrappers + `libexample-project.a` into `<output_dir>/main`.

### Step 12: The binary runs

`./main` starts. Vale-side code calls `vale_abi_rust_alloc_vec_Vec_1__i32___with_capacity(&ret, 42)`. The wrapper calls `rust_alloc_vec_Vec_1__i32___with_capacity(42)`, which is the cbindgen'd export from `libexample-project.a`, which internally calls `alloc::vec::Vec::<i32>::with_capacity(42)`. Rust allocates, returns a `Vec<i32>` whose size is 24 bytes (from `sizes.txt`). The Vale-side stores it in `v` as an opaque 24-byte blob.

Then `v.capacity()` fires another `vale_abi_rust_...` wrapper, returning `usize = 42`. Vale truncates to i32, returns from main. Exit code: 42.

---

## Data formats

### Generated `.vale` binding

Written by ValeRuster, consumed by Frontend. Example (`<rust_output_dir>/vale_bindings/std/vec/Vec.vale`):

```vale
#!DeriveStructConstructor extern struct Vec<T> imm {
  extern func new() Vec<T>;
  extern func with_capacity(capacity i64) Vec<T>;
  // Skipping (self) method try_with_capacity: Didn't contain whitelisted type: TryReserveError
  extern func capacity(self &Vec<T>) i64;
  extern func reserve(self &Vec<T>, additional i64);
  // ...
}
```

The type is opaque (Vale doesn't see fields). Methods taking `self` are emitted as taking `self &Vec<T>` — Rust's distinction between `&self` and `&mut self` is flattened to Vale's borrow (Vale doesn't track mutability through extern calls).

### rustdoc JSON

Consumed by ValeRuster and Divination. Lives at `$(rustc --print sysroot)/share/doc/rust/json/{std,alloc,core,proc_macro,std_detect,test}.json` (the prebuilt files from the `rust-docs-json` rustup component). For user crates, both tools run `cargo rustdoc -p <crate> -Zunstable-options --output-format json` on demand.

Format is the `rustdoc_types` crate's public API. The bindings must match the compiler's JSON version — the pipeline pins `rustdoc-types = "0.57.3"` in `ValeRuster/Cargo.toml` to match `nightly-2025-12-09`. Bumping one usually requires bumping both.

Breaking changes between rustdoc-types 0.25 and 0.57 touched:
- `Id`: `Id(String)` → `Id(u32)` — breaks any code stringifying IDs directly
- `ItemEnum::Import` → `ItemEnum::Use`, and the inner struct renamed to `Use`
- `Path.name` → `Path.path`
- `BorrowedRef { mutable, ... }` → `BorrowedRef { is_mutable, ... }`
- `Function.decl` → `Function.sig` (and decl's fields moved into a new `FunctionSignature` struct)
- `GenericParamDefKind::Type { synthetic, ... }` → `{ is_synthetic, ... }`
- `ItemEnum::ForeignType` → `ItemEnum::ExternType`
- `ItemEnum::OpaqueTy` — removed entirely
- `ItemEnum::Constant` — went from tuple variant to struct variant
- New enum variants: `GenericArgs::ReturnTypeNotation`, `GenericBound::Use`

### `sizes.txt`

Written by Divination, consumed by Backend. Path: `<rust_output_dir>/sizes.txt`. Line format:

```
type/<rust_type>/<mangled_c_name>/<size>/<alignment>
fn/<rust_fn_sig>/<mangled_c_name>/<return_type>/<param1_type>/<param2_type>/...
```

Separator is `/`. The mangled C name is what shows up as an `extern "C"` symbol in the cbindgen-generated header. For functions, the return type appears before params (contrary to what you might guess).

Example:
```
type/&str/iref_1__str/16/8
type/()/tuple_/0/1
type/std::vec::Vec<i32>/alloc_vec_Vec_1__i32/24/8
fn/std::vec::Vec<i32>::with_capacity/alloc_vec_Vec_1__i32___with_capacity/alloc::vec::Vec::<i32>/usize
fn/std::vec::Vec<i32>::capacity/alloc_vec_Vec_1__i32___capacity/usize/&alloc::vec::Vec::<i32>
```

### Generated Rust crate

Written by Divination at `<rust_output_dir>/`. Files:

- `Cargo.toml` — package metadata, `[profile.release]` with `panic = "abort"`, `[features.capi = ["libc"]]`, `[package.metadata.capi.library] name = "example-project"`.
- `Cargo.lock` — reproducible deps.
- `cbindgen.toml` — cbindgen config for the C header.
- `src/lib.rs` — conditional `mod capi;` gated on the `capi` feature.
- `src/capi.rs` — `#[no_mangle] pub extern "C" fn rust_<mangled>(...)` wrappers for each requested function, plus helpers like `rust_StrFromCStr` for `&str` marshalling.

After `cargo cbuild` runs, `<rust_output_dir>/target/<triple>/release/` contains:

- `libexample-project.a` (hyphenated — this is the one cbindgen writes from the `capi.library.name` metadata)
- `libexample_project.a` (underscored — symlink or secondary, equivalent content)
- `rust_deps.h` (cbindgen's generated header, referenced by Backend's generated `#include "rust_deps.h"`)
- `deps/` (intermediate `.a` and `.rlib` files with hash suffixes — not useful as link inputs)

---

## Directory layout at build time

Given `--output_dir ./build` and default `--rust_output_dir`:

```
build/
├── vast/                       # Frontend output per package
│   ├── __vale.vast
│   ├── mymodule.vast
│   └── rust.std.vec.vast       # Backend's primary input for Rust extern info
├── vpst/                       # Frontend output per .vale file (parse tree JSON)
│   ├── main.vpst
│   ├── Vec.vpst                # ← from ValeRuster-generated Vec.vale
│   └── ...                     # builtins.*.vpst, stdlib.*.vpst, etc.
├── include/
│   ├── ValeBuiltins.h          # Vale's runtime builtins
│   └── rust/
│       ├── alloc_vec_Vec_1__i32___capacity.h      # Backend-generated ABI bridge headers
│       └── alloc_vec_Vec_1__i32___with_capacity.h
├── abi/
│   └── rust/
│       ├── alloc_vec_Vec_1__i32___capacity.c      # Backend-generated ABI bridge sources
│       └── alloc_vec_Vec_1__i32___with_capacity.c
├── build.o                     # Backend-compiled Vale code (LLVM → object)
├── build.s                     # Same, as asm
├── rust/                       # The Rust interop dir (--rust_output_dir)
│   ├── vale_bindings/          # ValeRuster output
│   │   └── std/
│   │       └── vec/
│   │           └── Vec.vale    # ← consumed by Frontend as rust.std.vec package
│   ├── rust_scratch/           # ValeRuster's rustdoc JSON cache (for non-std crates)
│   ├── Cargo.toml              # Divination-generated
│   ├── Cargo.lock
│   ├── cbindgen.toml
│   ├── src/
│   │   ├── lib.rs
│   │   └── capi.rs             # Divination-generated Rust wrappers
│   ├── sizes.txt               # Divination output, Backend input
│   ├── divination.log          # Captured stdout/stderr from Divination (for debugging)
│   └── target/
│       └── aarch64-apple-darwin/
│           └── release/
│               ├── libexample-project.a     # ← Coordinator picks this up for linking
│               ├── rust_deps.h              # ← Coordinator -I's this for clang
│               └── deps/                    # intermediate cargo artifacts (ignored)
└── main                        # Final executable produced by clang
```

---

## CLI flag propagation

The four top-level user flags propagate as follows (empty string = not set):

| User flag to valec | Forwarded to Frontend CLI | Forwarded to Backend CLI |
|---|---|---|
| `--vale_ruster_path` | `--vale_ruster_path` | *(not forwarded)* |
| `--divination_path` | *(not forwarded)* | `--divination_path` |
| `--rust_cargo_toml` | `--rust_cargo_toml` | `--rust_cargo_toml` |
| `--rust_output_dir` | `--rust_output_dir` | `--rust_output_dir` |

The Frontend and Backend each validate that the flags they need are provided before attempting to invoke subprocesses.

The Coordinator additionally uses `--rust_output_dir` directly (not forwarded): it walks `<rust_output_dir>/target/*/release/` after Backend finishes to locate the static library and derive the include path + target triple for clang.

---

## Key design decisions

### Why resolve rust packages at the resolver level, not post-parse

An earlier approach commented in `HigherTypingPass.scala` (and mentioned in the original `quest.md`) was to invoke ValeRuster from the HigherTypingPass after encountering `ImportS` nodes. That's the wrong level: HigherTypingPass runs after parsing, but the generated `.vale` files would need to be *parsed* themselves. Invoking ValeRuster at that point means restarting the parser.

Hooking into the resolver (between lexer import discovery and file resolution) is cleaner: the Frontend's `FileCoordinateMap` resolver chain can inject sources by returning `Some(Map[filepath -> code])` for a requested package. The rust fallback plugs in there.

However, the actual invocation happens earlier still — as a *pre-scan* in `PassManager.build()` before the compilation starts. This is because ValeRuster is expensive (reads hundreds of MB of rustdoc JSON, runs `cargo rustdoc` for user crates) and we want to pay that cost once, not per-resolution. The pre-scan reads all inputs' `.vale` text, extracts `import rust.*` lines, and invokes ValeRuster for each. The resolver then just reads already-generated files from disk.

### Why pass `-arch arm64` explicitly

On an Apple Silicon Mac, `/usr/bin/clang` defaults to arm64 output when run from an arm64 process. But the bootstrap `valec` binary is x86_64 (it was built on/for x86_64 and runs under Rosetta on Apple Silicon). When a Rosetta-translated process spawns clang, clang inherits some arch preference and defaults to x86_64 output, which then fails to link against the arm64 Rust `.a`.

Solution: pass `-arch arm64` (or `-arch x86_64`) explicitly, derived from the Rust target triple. The triple comes from Divination's `rustc -vV` detection, flows through the `target/<triple>/release/` path structure, and is recovered by the Coordinator as the parent directory name of the Rust include dir.

### Why Divination redirects to a log file

The Vale stdlib's `Subprocess` reads child stdout via `read_into_buffer` in `subprocess.c`, which loops `fgetc(stream)` up to `bytes` times (1000 chars per read). `fgetc` is blocking; if the pipe is empty but not closed, the call hangs. The `consume_and_join` loop drains stdout and stderr in turn, so in principle it should keep up. But when cargo cbuild floods the pipe (megabytes of output during std/core/alloc rebuild), the one-char-at-a-time drain can't keep pace with the producer, and backpressure on the write side eventually wedges everything.

We papered over the symptom by redirecting Divination's output to a file (`divination.log`). The underlying stdlib bug remains — any other caller that spawns a chatty subprocess will hit the same deadlock. Proper fix is a larger `read()` buffer or non-blocking IO, at the cost of touching the stdlib and its Vale consumers.

### Why the resolver fallback is ordered before `resolvePackageContents`

`resolvePackageContents` (the normal resolver) was written before interop existed. It returns `Some(Map.empty)` when it can't find a package — i.e., "I didn't find anything but I'm not saying anyone else should try." The `.or` combinator treats `Some` as a hit and short-circuits subsequent fallbacks.

Two ways to fix this:

1. Change `resolvePackageContents` to return `None` for empty results — but this might break other callers that expect a Some contract.
2. Chain the rust fallback *before* it — if the rust fallback returns `None` (which it does for non-rust packages), `resolvePackageContents` runs normally after; if the rust fallback returns `Some(...)` (for rust packages), we skip the normal resolver which wouldn't find them anyway.

We went with option 2. It's a surgical fix with no risk to existing behavior.

### Why `Opt<str>` isn't used for the target triple

Passing the triple as an `Opt<str>` from `build.vale` into `invoke_clang` would be the cleanest API, but the bootstrap Vale compiler (used to build the Coordinator itself) has a type-checker bug where `Opt<str>` combined with `set` reassignment inside a closure triggers a `scala.MatchError: StrT()` during call resolution. The bug was fixed in the current (in-repo) Frontend, but the bootstrap is an older snapshot.

The workaround: derive the triple inside `invoke_clang` from the already-passed `rust_include_dir` path (the parent directory name). This avoids routing an `Opt<str>` through the call boundary. Future bumps of the bootstrap compiler should make this unnecessary, and the cleaner design can be restored.

---

## Files modified by this feature, by purpose

### Frontend (Scala)

- `Frontend/PassManager/src/dev/vale/passmanager/PassManager.scala` — three new CLI options parsed into `Options`, pre-scan via `invokeValeRusterIfNeeded`, `resolveRustPackageContents` plugged into the resolver chain before `resolvePackageContents`.
- `Frontend/LexingPass/src/dev/vale/lexing/LexAndExplore.scala` — removed the `if (module == "rust") skip` guard.
- `Frontend/TypingPass/src/dev/vale/typing/Compiler.scala` — OWPFRD sanity check uses `rootDenizenEnv.denizenTemplateId` instead of `rootDenizenEnv.id`, fixing lifted-function support.

### Backend (C++)

- `Backend/src/valeopts.h`, `valeopts.cpp` — three new CLI options (`--divination_path`, `--rust_cargo_toml`, `--rust_output_dir`) added to `ValeOptions`.
- `Backend/src/vale.cpp` — `doRustyThings()` uses options instead of hardcoded paths, adds validation, redirects Divination's stdio to a log file.

### ValeRuster (Rust)

- `ValeRuster/Cargo.toml` — `rustdoc-types` bumped to `0.57.3`.
- `ValeRuster/src/main.rs`, `resolve.rs`, `resolve_id.rs`, `simplify.rs` — updates for the new rustdoc-types API (renamed fields, new variants, Id type change), added `--type` CLI flag, added `ResolveError::Unsupported` for graceful skipping of unresolvable methods, changed synthetic generic-placeholder UID representation (name in `crate_name` since `Id` is no longer a string).

### Coordinator (Vale)

- `Coordinator/src/build.vale` — four new flags parsed, `rust_interop_active` derived, validation, forwarding to Frontend/Backend invocations, post-Backend walk of `target/<triple>/release/` to locate `.a` and include dir.
- `Coordinator/src/valestrom.vale` — `invoke_frontend` accepts + forwards three rust interop args.
- `Coordinator/src/midas.vale` — `invoke_backend` accepts + forwards three rust interop args.
- `Coordinator/src/clang.vale` — accepts `rust_include_dir`, derives `rust_target_triple` from its parent path, adds `-arch` flag when interop is active.

### Divination (Rust, separate repo)

- `Divination/src/main.rs` — detects host triple from `rustc -vV`'s `host:` line, uses it in `cargo cbuild --target`; removes the now-invalid `-Z build-std-features=panic_immediate_abort` flag.

### Top-level

- `rust-toolchain.toml` — pins `nightly-2025-12-09`, declares required components (`rust-src`, `rust-docs-json`).
- `tests/rust-interop/` — 32 end-to-end test programs, each with a `// expected_exit:` (or `// expects_build_fail: true`) header. Driven by `TesterRust/target/debug/testvalec`.

---

## What isn't wired up (yet)

- **Closures as function args.** `Vec::retain<F: Fn(&T) -> bool>(...)` is skipped. Implementing this needs a calling convention for passing Vale closures through a C ABI to a Rust `Fn` trait object. Not impossible but not trivial.
- **Trait objects (`dyn Trait`).** ValeRuster bails on these. Vale's interface system could model them but the vtable layout work isn't done.
- **Slices and `&str`.** `sizes.txt` has `&str` (16, 8) in it, and there's scaffolding in ValeRuster to alias it as `$rust_str_ref`, but the Vale-side ergonomics for constructing and passing them aren't in place.
- **Auto-import of transitive types.** If `Vec::pop` returns `Option<T>`, you must separately `import rust.std.option.Option`. The tools could follow the reachable-types closure automatically.
- **Incremental reflection.** Every build re-runs ValeRuster and regenerates every `.vale` binding. Cheap for one type, expensive for a program that imports twenty.
- **Caching of `cargo cbuild`.** Each build re-runs `-Zbuild-std` and rebuilds std/core/alloc (10–15 seconds). cargo does its own caching based on source hashes, so repeated builds should be fast after the first — but `rm -rf build/` starts from zero.
- **Non-macOS targets.** The `-arch` logic covers Darwin; Linux and Windows go through but haven't been tested. cbuild should handle other targets given the triple-detection in Divination.
- **Divination's own toolchain pin.** Divination lives in a separate repo and would benefit from its own `rust-toolchain.toml` matching this one, so its build doesn't drift.

## See also

- [../reasoning/rust-interop.md](../reasoning/rust-interop.md) — alternatives considered for each design decision (hook point, pre-scan, redirect, Opt<str>, OWPFRD).
- [../arcana/PlaceholdersNamedByDenizenTemplate-PNBDTZ.md](../arcana/PlaceholdersNamedByDenizenTemplate-PNBDTZ.md) — why the OWPFRD sanity check uses pre-lift `templateId`.
- [../arcana/RustResolverPrecedesGenericResolver-RRPGRZ.md](../arcana/RustResolverPrecedesGenericResolver-RRPGRZ.md) — resolver chain ordering.
- [../arcana/DivinationStdoutToLogFileNotPipe-DSLFNPZ.md](../arcana/DivinationStdoutToLogFileNotPipe-DSLFNPZ.md) — Backend's file redirect and the pipe-deadlock bug.
- [../arcana/ClangArchDerivedFromRustTriple-CADFRTZ.md](../arcana/ClangArchDerivedFromRustTriple-CADFRTZ.md) — Coordinator's `-arch` derivation.
- [../migration/rust-interop.md](../migration/rust-interop.md) — ephemeral limitations and bootstrap compiler workarounds.
