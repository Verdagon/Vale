# Quest: Make `rustinterop-merged` Release-Worthy

## Status

The Rust-interop pipeline is **wired up and working** for its smoke test (`scripts/test-rust-interop.sh` builds a Vale program that calls `Vec<int>.with_capacity(42i64).capacity()` and exits 42).

The `rustinterop-merged` branch is **not** release-worthy yet:

- **Backend test suite: 0/198 passing.** The Scala Frontend.jar throws assertions on every test program.
- **`FrontendRust` (the in-progress Rust port): does not compile.** `OptimizedSolverState` is missing the `new` method now required by the `ISolverState` trait.

## What's done

### Fix #1 (this commit)

`FunctionCompilerSolvingLayer.assembleKnownTemplatas` for the `StructNameT` case had `KindPlaceholderTemplateNameT(0, rune)` hard-coded. A struct's `instantiatedCitizen.id.localName.templateArgs` is a vector of placeholders at indices `0, 1, 2, …`, so any struct with ≥2 generic params hit `vimpl()` on its second placeholder. Every test program pulls in stdlib types like `Tup2`/`Tup3`/`Result`/`HashMap`, so the `vimpl` fired during compilation of nearly every program. Loosened to `KindPlaceholderTemplateNameT(_, rune)` since the index is redundant — the position is established by the zip.

The literal `0` was introduced wholesale in commit `79805fad` ("Extern struct methods work!"), which contained multiple "DO NOT SUBMIT" markers and `vimpl()` placeholders alongside this one. The commented-out fallback `InitialKnown(genericParam.rune, explicitArg)` cannot typecheck (`ITemplataT` has no `.rune`), confirming the index-`0` was prototyping with a single-generic-param case and never generalized.

## Remaining failures (after Fix #1, still 0/198)

The dominant cluster is now downstream:

1. **48× `vwat()` at `Instantiator.assemblePlaceholderMap`** (Instantiator.scala:1032). Documented in `investigations/iast-template-vs-instantiation.md`. Diagnosis: macro-generated drops on non-generic structs produce a function id whose initSteps contains a bare `StructTemplateNameT(...)` (template form) instead of `StructNameT(template, Vector())` (instantiation form). The macro registers drops via `structName.addStep(funcName)` where `structName.localName` is the bare template name, and `assembleName`'s self-detection skips the lifted path because the macro names the parameter `keywords.thiss` (= "this") rather than `keywords.self`. Recommended fix: rename `thiss` → `self` in the macro to align with user-written drops, which already use the lifted path correctly. Doc-update will also be required (@SMLRZ doc currently has self-contradicting prescriptions).

2. **5× `vimpl()` at `FunctionCompilerClosureOrLightLayer.scala:231`** — `evaluateGenericLightFunctionFromNonCall`, hit on `AnonymousSubstructNameT(...)` IDs containing `KindPlaceholderTemplateNameT(0, AnonymousSubstructMemberRuneS(...))`. Triggered by tests like `upcastif.vale` and `interfacemutreturnexport/test.vale`. Likely the same kind of unfinished pattern as Fix #1 but for anonymous-substruct-method dispatch.

3. **6× `vassertSome` "Expected non-empty"** — somewhere in monomorphization unwrapping an `Option`. Need to capture full stack trace.

## `FrontendRust` build error

```
error[E0046]: not all trait items implemented, missing: `new`
   --> src/solver/optimized_solver_state.rs:196:1
```

`OptimizedSolverState`'s `ISolverState` impl is missing `fn new() -> Self`. The trait method was added but `OptimizedSolverState` wasn't updated. Could be a real impl, a `panic!()` placeholder (per @TUCMPX), or — given the recent Scala-side commit `74371d69` that comments out `OptimizedSolverState` entirely on the Scala side — a sign that the Rust port should likewise drop `OptimizedSolverState` and only ship `SimpleSolverState`. Pick whichever matches Scala parity (per @CSTNFX / @RSMSCPX).

## Plan

### Step 1 — Backend test suite green (Scala Frontend)

1. Apply the macro `thiss` → `self` rename and @SMLRZ doc update — addresses cluster #1 above.
2. Investigate cluster #2 (anonymous-substruct dispatch). Likely needs a new match arm.
3. Capture full stack traces for cluster #3 and identify the missing key.

For each: prefer fixing the upstream cause (so the assertion is genuinely reachable only on bugs) over relaxing the assertion. Verify with `cd Backend && bash test.sh` until `Passed N/198` climbs. Target: 198/198.

### Step 2 — `FrontendRust` builds + tests pass

1. Resolve the `OptimizedSolverState::new` gap. Mirror Scala parity (likely retire `OptimizedSolverState` entirely, since Scala-side `74371d69` did).
2. `cargo build --manifest-path FrontendRust/Cargo.toml --lib` until clean.
3. `cargo test  --manifest-path FrontendRust/Cargo.toml --lib` until clean.

### Step 3 — Smoke + interop tests

1. `scripts/test-rust-interop.sh` (with `DIVINATION_PATH` and `RUST_CARGO_TOML` set) — should still pass.
2. Add at least one Rust-interop test to the regular Backend test suite so the interop pipeline doesn't silently regress.

## Reproduction

```bash
cd Frontend && sbt 'set test in assembly := {}' clean assembly      # ~90s
cd Tester && bash build.sh ~/BootstrappingValeCompiler              # one-time

cd Backend && bash test.sh 2>&1 | tee tmp/trace.txt
grep -oE "Assertion failed!?\s*\w*" tmp/trace.txt | sort | uniq -c
tail -1 tmp/trace.txt   # "Done! Passed N/198"
```
