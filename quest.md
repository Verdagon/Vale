# Quest: Make `rustinterop-merged` Release-Worthy

## Status

The Rust-interop pipeline is **wired up and working** for its smoke test (`scripts/test-rust-interop.sh` builds a Vale program that calls `Vec<int>.with_capacity(42i64).capacity()` and exits 42).

The `rustinterop-merged` branch is **not** release-worthy yet:

- **Backend test suite: 0/198 passing.** The Scala Frontend.jar throws assertions on every test program.
- **`FrontendRust` (the in-progress Rust port): does not compile.** `OptimizedSolverState` is missing the `new` method now required by the `ISolverState` trait.

## What's done

### Fix #1 — `assembleKnownTemplatas` index match

`FunctionCompilerSolvingLayer.assembleKnownTemplatas` had `KindPlaceholderTemplateNameT(0, rune)` hard-coded; loosened to `(_, rune)` so all placeholder indices match. Leftover from `79805fad` ("Extern struct methods work!").

### Fix #2 — Macro-generated drop routes through `assembleName`'s self-struct path (this commit)

`StructDropMacro` now names its parameter `keywords.self` (was `keywords.thiss`). With `lift = true` already set, this is the missing piece that makes `FunctionCompilerMiddleLayer.assembleName`'s self-detection succeed (it requires both flags). The drop body uses `ArgLookupTE(0, ...)` — positional, not by name — so the rename is body-safe. The function now flows through `selfStructId.addStep(funcName)` uniformly with user-written `func drop<T>(self XSome<T>) void`. `selfStructId` is always in instantiation form (`StructNameT(template, [placeholders])` — non-generic structs just have an empty placeholder vector), so generic and non-generic structs are handled by the same code with no special-casing on arity. @SMLRZ doc updated accordingly: lines 80–83 used to claim "Generated drop functions are not lifted; they use `addStep`; drop functions don't have a `self` parameter (they use `thiss`)" — that contradicted line 87 of the same doc which says the function ID must contain `StructNameI` not bare `StructTemplateNameI` or `NameHammer.simplifyName` crashes. The contradiction was the bug; the doc now reflects the new uniform routing.

Closes the dominant cluster from Fix #1's downstream (48× `vwat()` at `Instantiator.assemblePlaceholderMap`).

## Remaining failures (after Fix #2, still 0/198)

1. **`vimpl()` at `FunctionCompilerSolvingLayer.assembleKnownTemplatas` second arm** — `assembleKnownTemplatas` has only a `CoordTemplataT(_, _, KindPlaceholderT(...))` arm, so non-Coord placeholders (Int, Mutability, Variability — e.g. `StaticSizedArrayIter<N, M, V, E>`'s `N`/`M`/`V`) fall through to `vimpl()`. Same WIP-leftover provenance as Fix #1 (commit 79805fad). Re-routing macro-generated drops through `assembleName` (Fix #2) made this code path reachable for more inputs, surfacing the gap.

2. **`vassertSome "Expected non-empty"` at `Instantiator.scala:3195`** (`translateCoord`). The substitutions map is missing a placeholder it expects to find. Need full stack trace and which substitutions table is being consulted.

3. **`vimpl()` at `FunctionCompilerClosureOrLightLayer.scala:231`** — fires on `AnonymousSubstructNameT(_, [CoordTemplataT(_, _, KindPlaceholderT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, AnonymousSubstructMemberRuneS(...)))))])`. Triggered by tests like `upcastif.vale`. Likely another WIP-leftover for anonymous-substruct dispatch.

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
