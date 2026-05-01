# Quest: Make `rustinterop-merged` Release-Worthy

## Status

The Rust-interop pipeline is **wired up and working** for its smoke test (`scripts/test-rust-interop.sh` builds a Vale program that calls `Vec<int>.with_capacity(42i64).capacity()` and exits 42).

The `rustinterop-merged` branch is **not** release-worthy yet:

- **Backend test suite: 175/198 passing** (was 0/198). Remaining failures are clusters #2/#3 (`AnonymousSubstructMemberRuneS`) plus a possible `interfaceimmparam*` issue.
- **`FrontendRust` (the in-progress Rust port): does not compile.** `OptimizedSolverState` is missing the `new` method now required by the `ISolverState` trait.

## What's done

### Fix #1 — `assembleKnownTemplatas` index match

`FunctionCompilerSolvingLayer.assembleKnownTemplatas` had `KindPlaceholderTemplateNameT(0, rune)` hard-coded; loosened to `(_, rune)` so all placeholder indices match. Leftover from `79805fad` ("Extern struct methods work!").

### Fix #2 — Macro-generated drop routes through `assembleName`'s self-struct path

`StructDropMacro` now names its parameter `keywords.self` (was `keywords.thiss`). With `lift = true` already set, this is the missing piece that makes `FunctionCompilerMiddleLayer.assembleName`'s self-detection succeed (it requires both flags). The drop body uses `ArgLookupTE(0, ...)` — positional, not by name — so the rename is body-safe. The function now flows through `selfStructId.addStep(funcName)` uniformly with user-written `func drop<T>(self XSome<T>) void`. `selfStructId` is always in instantiation form (`StructNameT(template, [placeholders])` — non-generic structs just have an empty placeholder vector), so generic and non-generic structs are handled by the same code with no special-casing on arity. @SMLRZ doc updated accordingly: lines 80–83 used to claim "Generated drop functions are not lifted; they use `addStep`; drop functions don't have a `self` parameter (they use `thiss`)" — that contradicted line 87 of the same doc which says the function ID must contain `StructNameI` not bare `StructTemplateNameI` or `NameHammer.simplifyName` crashes. The contradiction was the bug; the doc now reflects the new uniform routing.

Closes the dominant cluster from Fix #1's downstream (48× `vwat()` at `Instantiator.assemblePlaceholderMap`).

### Fix #3 — `assembleKnownTemplatas` accepts non-Coord placeholders (this commit)

Fix #2 made the `StructNameT` arm of `assembleKnownTemplatas` reachable for more inputs (the macro-generated drop's prefix is now an instantiation form), which surfaced a sibling WIP-leftover in the same function: it only had a `CoordTemplataT(_, _, KindPlaceholderT(...))` arm. For citizens with Int/Mutability/Variability generic params (e.g. `StaticSizedArrayIter<N, M, V, E>`), those parameters appear as `PlaceholderTemplataT(_, IntegerTemplataType)` and similar — not wrapped in `CoordTemplataT(CoordT(...))` — and fell to `vimpl()`. Added a parallel arm matching `PlaceholderTemplataT(IdT(_, _, NonKindNonRegionPlaceholderNameT(_, rune)), _)`. Same WIP-leftover provenance as Fix #1 (commit 79805fad).

### Fix #4 — Collapse Instantiator's `substitutions` to a single-level map

The Instantiator threaded a two-level `Map[IdT[INameT], Map[IdT[IPlaceholderNameT], ITemplataI[sI]]]` through ~42 functions. The outer level was a fossil from when placeholder ids were `Int` indices needing per-denizen disambiguation; once the inner key became a fully path-encoded `IdT[IPlaceholderNameT]` (commit `11acc981`, Jan 2023), the outer level became redundant. Producers had to choose an outer key that consumers' `placeholderId.initId(interner)` lookups would reproduce, which was fine for plain functions but silently broke for @SMLRZ-lifted methods (the prefix denizen and the placeholders' owning denizen disagreed). Cluster #1 was the consequence.

Flattened to `Map[IdT[IPlaceholderNameT], ITemplataI[sI]]` everywhere: 42 signatures, 8 producer sites, 5 consumer sites, plus the higher-order callback in `runInNewPureRegion`. Override-dispatcher's two-key producer (line 723) collapsed via `++` — the existing disjointness assertion now also serves as the runtime check that flat-merge is safe (per @PASDZ).

See `investigations/cluster1-vassertSome-translateCoord.md` and `Frontend/docs/arcana/PlaceholdersAreSelfDisambiguating-PASDZ.md`.

### Fix #5 — Backend skips Divination when no Rust externs

`doRustyThings` in `Backend/src/vale.cpp` ran Divination unconditionally. For programs with no rust externs, `divinationInputStr` is empty, and the configured paths (`--divination_path`, `--rust_cargo_toml`, `--rust_output_dir`) are correctly absent — but the code still built a command with empty paths and tried to redirect to `/divination.log`, hitting "Read-only file system" on macOS. Added an early `return` when `divinationInputStr.empty()`.

### Fix #6 — `-arch arm64` on Darwin

`Coordinator/src/clang.vale` only passed `-arch` when a Rust target triple was set. For non-rust tests, clang invoked from a Rosetta'd valec inherited x86_64 and produced x86_64 output that wouldn't link the arm64 `build.o`. Now passes `-arch arm64` whenever `IsDarwin()` is true. Added a Coordinator-local `IsDarwin` extern (the bootstrapping compiler's bundled stdlib only has `IsWindows`); when the bootstrapping compiler is rebuilt against the in-tree stdlib, this can move to `stdlib.os`.

### Fix #7 — `makeIncludeDirectory` actually creates the directory

`Backend/src/vale.cpp:makeIncludeDirectory` returned the path without calling `makeDirectory` (its siblings do). For tests with no externs, the sibling `makeModuleIncludeDirectory` never runs, so `<output_dir>/include` doesn't exist when `generateExports` writes `ValeBuiltins.h` — write fails. Now creates the directory.

## Remaining failures (after Fixes #1–7, currently 175/198)

1. **`vimpl()` at `FunctionCompilerClosureOrLightLayer.scala:231`** — fires on `AnonymousSubstructNameT(_, [CoordTemplataT(_, _, KindPlaceholderT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, AnonymousSubstructMemberRuneS(...)))))])`. Triggered by tests like `upcastif.vale`. Likely another WIP-leftover for anonymous-substruct dispatch (~5 occurrences).

2. **`Assertion failed! IdT(...)`** — same site as #1 with the same `AnonymousSubstructMemberRuneS` shape. Same root cause (~2 occurrences).

3. **`interfaceimmparam{,deep}{extern,export}` (resilient-v3)** — possibly an extern-related issue distinct from the anonymous-substruct cluster; needs investigation.

## `FrontendRust` build error

```
error[E0046]: not all trait items implemented, missing: `new`
   --> src/solver/optimized_solver_state.rs:196:1
```

`OptimizedSolverState`'s `ISolverState` impl is missing `fn new() -> Self`. The trait method was added but `OptimizedSolverState` wasn't updated. Could be a real impl, a `panic!()` placeholder (per @TUCMPX), or — given the recent Scala-side commit `74371d69` that comments out `OptimizedSolverState` entirely on the Scala side — a sign that the Rust port should likewise drop `OptimizedSolverState` and only ship `SimpleSolverState`. Pick whichever matches Scala parity (per @CSTNFX / @RSMSCPX).

## Plan

### Step 1 — Backend test suite green (Scala Frontend)

1. Capture full stack traces for the dominant `vassertSome` cluster at `Instantiator.scala:3195` and identify the missing substitution key.
2. Investigate the `AnonymousSubstructMemberRuneS` cluster at `FunctionCompilerClosureOrLightLayer.scala:231`. Likely needs a new match arm — same WIP-leftover provenance as Fixes #1/#3.

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
