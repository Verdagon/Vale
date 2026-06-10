# Rust Interop — Migration Status

Ephemeral items that will resolve as the surrounding infrastructure catches up. Delete entries as they get fixed.

## Bootstrap compiler `Opt<str>` bug

The bootstrap `valec` (at `~/BootstrappingValeCompiler/Frontend.jar`, shipped with older compilers) crashes with `scala.MatchError: StrT()` at `CallCompiler.evaluateCall(CallCompiler.scala:39)` when compiling Vale code of the form:

```vale
maybe_thing Opt<str> = None<str>();
some_list.each((x) => {
  set maybe_thing = Some(x.name());
});
```

Reassigning an `Opt<str>` via `set` inside a nested closure triggers an incomplete pattern match during overload resolution. Identical code with `Opt<Path>` works fine. The bug is fixed in the in-repo `Frontend/` (the current-branch compiler) but not in the bootstrap.

**Workaround in play:** `Coordinator/src/clang.vale` derives the Rust target triple from the parent directory name of `rust_include_dir` instead of accepting a separate `Opt<str>` parameter. See @CADFRTZ and `docs/reasoning/rust-interop.md`.

**Resolution:** when the bootstrap `valec` is rebuilt from a newer Frontend, the workaround can be removed. The cleaner design — passing the triple as an `Opt<str>` argument — becomes available then.

## `rustdoc-types` version tied to pinned nightly

`ValeRuster/Cargo.toml` pins `rustdoc-types = "0.57.3"`. This version tracks the rustdoc JSON schema produced by `nightly-2025-12-09` (what `rust-toolchain.toml` pins). Bumping the pinned nightly likely means bumping `rustdoc-types` too, usually with breaking changes that require code changes in ValeRuster.

**Resolution:** this is structural, not a bug. Document the lockstep bump in the commit message when you bump either.

## Vale stdlib `Subprocess` pipe-buffer deadlock

`stdlib/src/command/native/subprocess.c::read_into_buffer` reads subprocess stdout via `fgetc` one character at a time. When a subprocess produces megabytes of output fast (e.g. `cargo cbuild -Zbuild-std` rebuilding core/std/alloc), the reader can't keep up, the pipe fills, the writer blocks on backpressure, and the parent's read-then-check-alive loop hangs forever.

**Workaround in play:** Backend's `doRustyThings` redirects Divination's stdout to a log file via shell `> file 2>&1`, so nothing ever lands in the pipe. See @DSLFNPZ.

**Resolution:** fix `read_into_buffer` to use a larger `fread`-style block read, or move to non-blocking I/O. That's a stdlib change with downstream API implications — we chose the workaround.

## Known unsupported Rust features

The ValeRuster → binding generation path skips methods involving:

- Closures as arguments (`Vec::retain<F: Fn(&T) -> bool>`)
- Trait objects (`dyn Trait`) and generic trait bounds beyond simple types
- Lifetime generics (`fn from<'a>(s: &'a str)`)
- Allocator generics (`A: Allocator` defaulted)
- Slices (`&[T]`, `&mut [T]`) and raw pointers (`*const T`, `*mut T`)
- Method-level generics (`fn map<U>(...)`) beyond what the struct already provides
- Types not explicitly imported (no transitive-closure auto-import)

These show up as `// Skipping (self) method <name>: <reason>` comments in the generated `.vale` file. Resolving any of them is a feature-work task, not a migration issue.

## x86_64 bootstrap `valec` requires explicit clang arch flag

The bootstrap `valec` binary is x86_64, so it runs under Rosetta on Apple Silicon. Rosetta causes child clang to emit x86_64 by default, which can't link against an arm64 Rust `.a`. See @CADFRTZ for the workaround.

**Resolution:** rebuild the bootstrap as arm64 (or universal). The explicit `-arch` in `clang.vale` is still correct after the bootstrap is rebuilt — it just becomes a no-op on single-arch machines.

## End-user must pass `--clang_override /usr/bin/clang`

If the user has `swiftly` (Apple's Swift toolchain manager) installed, `which clang` resolves to `~/.swiftly/bin/clang`, which in our testing hung on mach IPC when invoked as a subprocess. The Coordinator doesn't detect this and fall back; it just uses whatever's on PATH.

**Resolution:** either document the requirement (done in `docs/usage/rust-interop.md`), make the Coordinator default `--clang_override` to `/usr/bin/clang` on macOS, or detect the swiftly shim and warn. Any of these is small; we did the docs version.
