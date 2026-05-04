# Stabilizing a Branch

A runbook for driving a Vale branch toward release-worthiness: backend test suite green, FrontendRust builds, smoke tests intact. Targeted at someone landing on a branch that's mid-migration with `vimpl()` / `vwat()` / `vassert` placeholders firing across many tests.

## Goal

Three things need to be true:

1. `cd Backend && bash test.sh --clang_path /usr/bin/clang` reports `Done! Passed 198/198`. (The `--clang_path` override is needed on Apple Silicon — see "Common pitfalls".)
2. `cargo build --manifest-path FrontendRust/Cargo.toml --lib` succeeds (the in-progress Rust port).
3. `scripts/test-rust-interop.sh` passes (the rust-interop smoke test).
4. `./scripts/mac/build-compiler.sh ~/BootstrappingValeCompiler --test=all scripts/VERSION` produces `release-mac/Vale-Mac-0.zip` and passes 99/99 (resilient-v3 only) against the bundled artifacts. This is the actual release-packaging gate.

Backend tests are by far the longest pole. Start there.

## One-time setup

You need a **bootstrapping valec** to build the Tester (which is itself a Vale program). The bootstrapping compiler lives at `~/BootstrappingValeCompiler/` by convention. If you don't have one, ask whoever owns the org's compiler artifacts.

Build everything in dependency order:

```bash
# Frontend.jar (Scala — the active compiler frontend)
cd Frontend && sbt 'set test in assembly := {}' clean assembly        # ~90s clean, ~16s incremental

# Backend (C++ via cmake). Not pre-built on a fresh clone — you need to do this.
# On Apple Silicon, brew's LLVM 16 isn't on cmake's default search path, so override:
cd Backend && cmake -B build -DLLVM_DIR="$(brew --prefix llvm@16)/lib/cmake/llvm" && cmake --build build -j

# Coordinator (valec wrapper, built by bootstrapping valec). Not pre-built either.
cd Coordinator && bash build.sh ~/BootstrappingValeCompiler

# Tester (Vale program built by bootstrapping valec)
cd Tester && bash build.sh ~/BootstrappingValeCompiler                # one-time
```

The repo's `scripts/build-rust-interop.sh` wraps these (with `SKIP_FRONTEND` / `SKIP_BACKEND` / `SKIP_VALERUSTER` / `SKIP_COORDINATOR` env vars) but you don't need it for the main Backend test loop.

For producing a release zip, `scripts/mac/build-compiler.sh` does all four of the above plus stages everything into `release-mac/` and zips it as `Vale-Mac-0.zip`. With `--test=all` it also re-runs the Tester against the *bundled zip* (resilient-v3 only, 99/99). See "Packaging a release" below.

## The main inner loop

```bash
# 1. Make a change.
# 2. Rebuild Frontend.jar (the Scala compiler — most fixes will be here).
cd Frontend && sbt 'set test in assembly := {}' assembly             # ~25s incremental

# 3. Run the tests, capture the full trace to a file.
# On Apple Silicon, pass --clang_path /usr/bin/clang (test.sh forwards "$@" to testvalec).
cd Backend && bash test.sh --clang_path /usr/bin/clang > ../tmp/trace.txt 2>&1

# 4. Triage.
tail -3 ../tmp/trace.txt                                              # final "Passed N/198"
grep -oE "Assertion failed!?\s*\w*" ../tmp/trace.txt | sort | uniq -c # tally failure types
grep -B1 "Error 1 building test" ../tmp/trace.txt | head              # first few failing tests
```

`tmp/` is gitignored — use it freely for scratch traces.

**Always tee to a file before grepping.** sbt and the test harness produce a lot of output; piping `sbt | grep` will silently drop output and lose information. Save first, grep second.

## Anatomy of a failure

The tester runs each test program through the full pipeline (Frontend → Backend → linker → execution) across multiple region modes. A test counts toward the 198 total as one program; running it across e.g. resilient-v3 + naive-rc + …  doesn't multiply the count. So 14 distinct exception stack traces in a 0/198 run usually means 14 distinct bugs, but each affects many programs.

A typical failure block in the trace looks like:

```
Starting <testname>, region <mode>...
Error 1 building test <testname> (region <mode>).
stdout:
<the java -cp ... PassManager build invocation>
Compiling function: ...
Exception in thread "main" Assertion failed! <message>
        at <stack frames>
Frontend returned error code 1, aborting.
```

The "Compiling function:" lines come from the typing pass; the exception is what stopped it. Stack traces are gold — they pinpoint the file:line where the assertion fires.

Most of the asserts you'll see are intentional placeholders left over from the Sylvan→Rust migration:

- `vimpl()` — "not yet implemented" (alias `panic!()` in Rust port)
- `vwat()` — "should never reach here"
- `vassertSome` — unwrapping an `Option`
- `vassert(...)` — generic precondition

Many were introduced wholesale by WIP commits with `DO NOT SUBMIT` markers. Commit `79805fad` ("Extern struct methods work!") is a notorious one — it added a 30-line block to `FunctionCompilerSolvingLayer.assembleKnownTemplatas` containing several half-finished `vimpl()` arms that we've been working through one at a time.

## Reproducing a single failure

The test harness runs every program in the trace in sequence; if you want to iterate fast on one, run the Frontend on the program directly:

```bash
java -cp Frontend/Frontend.jar dev.vale.passmanager.PassManager build \
  --output_dir tmp/x_out --sanity_check true \
  vtest=Frontend/Tests/test/main/resources/programs/<file>.vale \
  > tmp/single.txt 2>&1
echo "Exit: $?"
tail tmp/single.txt
```

Caveat: not every test that fails in `bash test.sh` will reproduce standalone. The harness runs across multiple region modes, and some failures only manifest in specific modes. Look at the harness's exact invocation in the trace and copy it verbatim if standalone doesn't reproduce.

Find a test that exercises a particular code path:

```bash
grep -rl "<vale syntax fragment>" Frontend/Tests/test/main/resources/programs/
```

## Diagnosing with println

The Scala compiler doesn't have a debugger workflow — instrument with `println`. Pattern:

1. Find the assert site (file:line from the stack trace).
2. Add `println("DBG <site>: " + <relevant data>)` lines just before the assert.
3. Rebuild Frontend.jar (`cd Frontend && sbt 'set test in assembly := {}' assembly`).
4. Re-run the failing test (single-test reproduction, ideally — keeps output manageable).
5. `grep -A 8 "DBG <site>" tmp/single.txt` to see what's happening.

For investigations that span multiple rounds of instrumentation, write a markdown file in `investigations/` and update it as you go. Examples in that directory show the format. The `collapsed-call-tree` skill (`/collapsed-call-tree`) automates this loop.

**sbt incremental build can miss external file changes.** If you edit a file and your println doesn't appear in the next run, run `sbt clean assembly` or `touch` the file you changed.

## Common pitfalls

- **swiftly's clang at `~/.swiftly/bin/clang` hangs on mach IPC** when invoked as a subprocess from x86_64-Rosetta'd valec, AND it'll be picked up over `/usr/bin/clang` if it's first on PATH. Override to `/usr/bin/clang` either via the Tester flag (`--clang_path /usr/bin/clang`, forwarded by `bash test.sh "$@"`) or, for the rust-interop scripts, `CLANG_PATH=/usr/bin/clang`.
- **The bootstrapping valec is x86_64.** Under Rosetta on Apple Silicon, child clang processes default to x86_64 output, which won't link arm64 `build.o` (or arm64 Rust libs). Even invoking `/usr/bin/clang` directly inherits the parent's x86_64. `Coordinator/src/clang.vale` now passes `-arch arm64` unconditionally on non-Windows to defeat this inheritance — that's what makes `--clang_path /usr/bin/clang` actually work on this branch. If you're driving clang manually outside the Coordinator, pass `-arch arm64` yourself. (Earlier descriptions of "the Coordinator derives -arch from the rust target triple" were aspirational — it's hardcoded.)
- **`sbt assembly` without `'set test in assembly := {}'`** runs the Scala test suite first, which takes minutes and is unrelated to backend tests.
- **Hooks in `.claude/settings.json`** may block edits to `.scala` files. If your edit silently doesn't apply, check the hooks; you may need to relax them temporarily for Scala files.
- **`cargo` invocations need the manifest path**: `cargo build --manifest-path FrontendRust/Cargo.toml --lib` rather than `cd FrontendRust && cargo build` (per project convention).

## Where to look

- **`quest.md`** (top-level) — the current state of the stabilization push. What's fixed, what's left, what the dominant failure cluster is. Update this as you go so the next person picks up cleanly.
- **`investigations/`** — debugging notes from prior sessions. Recent ones describe the structure of the typed-AST/I-AST shape mismatch, anonymous-substruct cluster, identifiability fixes, and the Rust-interop pipeline integration.
- **`Frontend/docs/arcana/`** — cross-cutting design docs. Each has a code (`@SMLRZ`, `@ICIPCRZ`, `@PNBDTZ` etc.); annotations of those codes appear in the source at related sites. Search for the code to find every file that touches the concept.
- **`Frontend/docs/historical/`** — session retrospectives. The `2026-04-16-...md` doc covers the Group A–G regression-fix push and the rust-interop discovery — long but useful for context on why the typing pass looks the way it does.
- **The `79805fad` commit** — `git show 79805fad` reveals the WIP cluster that introduced most of the `vimpl()`s. When you see one of those, this commit is usually the provenance.

## Packaging a release

`scripts/mac/build-compiler.sh` is the release-packaging script. It rebuilds Frontend / Backend / Coordinator from scratch, stages the artifacts into `release-mac/`, and zips them as `release-mac/Vale-Mac-0.zip`. The `--test=all` mode rebuilds the Tester against the *bundled zip* (not the in-tree binaries) and runs it against `@resilient-v3` — that's the gate that proves the packaged artifacts work standalone.

```bash
./scripts/mac/build-compiler.sh ~/BootstrappingValeCompiler --test=all scripts/VERSION
```

The script reads the version from `scripts/VERSION` (currently `0.2.1.0`) and interpolates it into the bundled `valec-version.txt` / help text / README. Bump that file when cutting a new version.

`CLANG_PATH` env var overrides the clang used in the test step (defaults to `/usr/bin/clang`, which works on Apple Silicon thanks to the `-arch arm64` in `Coordinator/src/clang.vale`). The `--test=all` path runs only the resilient-v3 region (99 tests), not the full 198 across all three regions — for the full suite, run `cd Backend && bash test.sh --clang_path /usr/bin/clang` separately.

The script's `--test=smoke` and `--test=none` skip the post-package test step. Always pass one of the three forms; the script aborts if you don't.

The script defaults to bash now; the previous `source ~/.zshrc` line is guarded behind `$ZSH_VERSION` so the script works under either shell.

## Splitting work across commits

This codebase prefers each commit to be one fix for one problem, with the commit message explaining root cause. Mid-investigation diagnostic `println`s should be cleaned up before the fix-commit (or staged into a separate commit and squashed before merge).

If you've already committed a too-large blob, `git reset --soft HEAD~N` is non-destructive and lets you re-stage in groups: `git reset HEAD` (unstage to working tree), then `git add -p` for hunk-level splits or temporarily edit-out one change to commit the other. The `--soft` flag keeps everything in the index — no work is lost.
