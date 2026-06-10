# Rust Interop Tests

Each `.vale` file in this directory is a standalone end-to-end test of the Vale → Rust interop pipeline. TesterRust (`TesterRust/target/debug/testvalec`) iterates over them — builds each one through valec (which invokes ValeRuster, Frontend, Backend, Divination, cargo cbuild, and clang), runs the resulting binary, and checks the exit code against an annotation at the top of the source file.

## Test file format

```vale
// expected_exit: 42
// summary: one-line description of what this test exercises

import rust.std.vec.Vec;

exported func main() int {
  // ...
  return some_value.TruncateI64ToI32();
}
```

The headers:

| Header | Required? | Effect |
|---|---|---|
| `// expected_exit: <integer 0-255>` | mandatory (or `expects_build_fail`) | Build must succeed; run must exit with this code. |
| `// expects_build_fail: true` | mandatory (or `expected_exit`) | Build must FAIL. Used to pin documented unsupported features. |
| `// summary: ...` | mandatory by convention | Humans grep for it. Runner doesn't read it. |
| `// include_stdlib: true` | optional, default false | Opt the build in to the Vale stdlib at `<stdlib_dir>/src`. Needed for `println`, `castI64Str`, etc. |
| `// expected_stdout: "..."` | optional | Assert on captured stdout. Quoted string supports `\n`, `\t`, `\r`, `\"`, `\\` escapes. |

Default mode (no `include_stdlib`) compiles against `--no_std true` — keep the program self-contained against the Rust import, no `import v.builtins.*`. Exit code is the usual assertion channel (0-255 on POSIX); `expected_stdout` is for programs whose value is in what they print (like `ri_29_catter`).

## Running

From the repo root, with the toolchain built (`./scripts/build-rust-interop.sh ~/BootstrappingValeCompiler`):

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
  ri_
```

The `ri_` positional arg is a substring filter that matches all rust-interop tests (their filenames all start with that prefix) and nothing in the Vale corpus. To run a single test, swap `ri_` for e.g. `ri_01_vec_capacity_42`. To run a subset, use a more specific substring like `vec_capacity`. Multiple positional args AND together (`vec bool` ⇒ tests whose names contain both).

Per-test build dirs land at `testbuild/<test_name>_resilient-v3/` for post-mortem. The summary `Done! Passed N/M` prints at the end.

## Adding a test

1. Pick a feature of the binding pipeline you want to assert on (a new Rust method, a new arg shape, a parity between two methods, …).
2. Encode the assertion as "this Vale program exits with code N." Whole-program exit codes keep the runner trivial; richer assertions belong in Scala unit tests, not here.
3. Add the file as `ri_NN_<short_name>.vale` — the `ri_` prefix lets `testvalec ... ri_` filter to exactly these tests without matching any Vale-corpus name, and the numeric NN sorts tests deterministically. Use `ri_uNN_<short_name>.vale` for an expected-to-build-fail test.
4. Make sure the program imports only Rust types whose bindings ValeRuster can currently generate — see [docs/usage/rust-interop.md](../../docs/usage/rust-interop.md) for the "supported subset" list.

## What's currently tested

`ri_01_` through `ri_28_` plus `ri_29_catter` are expected-to-pass; `ri_u02_` through `ri_u04_` are expected-to-build-fail (documented unsupported Rust features). `ri_u01_vec_push` is currently disabled — see its file for why.

| File | Asserts |
|---|---|
| `ri_01_vec_capacity_42.vale` | Baseline smoke from `edb294c0a`: `Vec::with_capacity(42)`/`.capacity()` round-trips 42. |
| `ri_02_vec_capacity_0.vale` | `Vec::with_capacity(0)` returns 0 — boundary case. |
| `ri_03_vec_capacity_7.vale` | Small non-baseline value — catches "always returns 42" regressions. |
| `ri_04_vec_capacity_100.vale` | Two-digit value survives the `i64`→`i32` truncate. |
| `ri_05_vec_capacity_then_len.vale` | `Vec::len()` of a `with_capacity(N)` is still 0 — distinct from capacity, exercises a second method on the same struct. |
| `ri_06_vec_new_capacity.vale` | `Vec::new()` (no-arg associated fn) gives capacity 0. |
| `ri_07_vec_capacity_twice.vale` | Calling `capacity()` twice on the same Vec is consistent. |
| `ri_08_two_independent_vecs.vale` | Two Vecs with different capacities don't share state. |
| `ri_09_vec_is_empty_new.vale` | `Vec::new().is_empty()` — `bool` return through the cbindgen → Vale boundary. |
| `ri_10_vec_is_empty_with_capacity.vale` | `is_empty` distinguishes len from capacity (still empty after `with_capacity`). |
| `ri_11_vec_i64.vale` | `Vec<i64>` — i64 generic substitution. |
| `ri_12_vec_bool.vale` | `Vec<bool>` — bool generic substitution. |
| `ri_13_vec_float.vale` | `Vec<float>` — f64 generic substitution. |
| `ri_14_string_new_len.vale` / `ri_15_string_with_capacity.vale` / `ri_22_string_is_empty.vale` | Second std type — `rust.std.string.String`. |
| `ri_16_vec_and_string.vale` | Multiple distinct `import rust.*` in one program. |
| `ri_17_struct_holds_vec.vale` / `ri_24_struct_holds_string.vale` / `ri_25_struct_holds_two_externs.vale` | Vale struct composition with extern types. |
| `ri_18_pass_vec_to_helper.vale` / `ri_27_pass_string_to_helper.vale` | Extern types as Vale fn parameters. |
| `ri_19_vec_bool_is_empty.vale` / `ri_20_vec_float_len.vale` / `ri_21_vec_i64_full.vale` | Combinations of generic param + bool/method dispatch. |
| `ri_23_two_vec_diff_generics.vale` | `Vec<int>` AND `Vec<bool>` in one program — currently surfaces Backend Bug 4. |
| `ri_26_fn_returns_vec.vale` | Extern type returned from Vale fn. |
| `ri_28_struct_pass_through_helper.vale` | Vale struct containing extern, passed through a fn. |
| `ri_29_catter.vale` | Catter-derived — imports `rust.std.option.Option` + `rust.std.vec.Vec` in the same program. Catter (vendored at `/Catter/`) is the original 2024 Vale↔Rust interop proof of concept; this test preserves its Option+Vec import shape (no other test imports Option). |
| `ri_u01_vec_push.vale` | DISABLED. Build now succeeds with the Backend `&mut` C-emit fix, but Vale's by-value extern storage means push mutations don't propagate. See the file's comment for the full diagnosis. |
| `ri_u02_hashmap_new.vale` | Allocator-generic default — expected to fail to build. |
| `ri_u03_box_new.vale` | Allocator-generic default — expected to fail to build. |
| `ri_u04_vec_iter.vale` | Lifetime generic — expected to fail to build. |

Most tests here import `rust.std.vec.Vec` or `rust.std.string.String`; `ri_29_catter` adds `rust.std.option.Option`. Add more import targets as ValeRuster's filter list expands.
