# Quest: Make `rustinterop-merged` Release-Worthy — Final Status

**Branch**: `refactor-rollback-smlrz` (forked from `rustinterop-merged` at `5fb81160`).

**Status**: The architectural rollback of @SMLRZ + the follow-on explicit-container-template-args refactor are **complete**. The Vale → Rust interop pipeline works end-to-end via `scripts/test-rust-interop.sh`. Frontend test suites that were pre-existing on the rollback baseline remain at their baseline state; no new regressions introduced. Some open follow-ups are tracked at the bottom.

This document is the historical record of the work; further sessions should keep their own quests.

## What was the problem

`rustinterop-merged` had been teaching the typing pass to produce Rust-shaped function ids — `[Vec<i32>, push]<>(...)` — so `ValeRuster` could match Rust impl blocks structurally. The mechanism was @SMLRZ (Struct Method Lift Rules): a `lift: Boolean` on `FunctionS`, an `inherited: Boolean` on `GenericParameterS`, an id-shape transform in `assembleName`, an env-hierarchy split for lifted methods, and several follow-on fixes (notably Fix #9: lifted methods compile against `declaringEnv`).

Each piece was individually defensible, but collectively they over-constrained the typing pass — conflicting with @PNBDTZ (placeholder ownership), @ICIPCRZ (identifiability), and OWPFRD (single-prefix invariant). The May-02 session attempted to remove the `lift` flag (Refactor #1) and stalled when those constraints proved load-bearing for each other; that stall motivated the architectural backtrack documented below.

The truthful internal model is **UFCS-flat**: function ids are `[Vec, push]<i32>(&Vec<i32>, ...)` — citizen step is bare in `initSteps`, all type args on the leaf. Rust's `Vec<i32>::push` shape is a *presentation* concern, not a typing-pass concern; it lives at the SimplifyingPass→Backend boundary.

## What was done

### Phase A — Rollback (committed as `cb8e70ad` in this branch's history; landed in the May-03 session)

- Removed `lift: Boolean` from `FunctionS` / `FunctionA` and all macro/Scout sites.
- Removed `inherited: Boolean` from `GenericParameterS` and all sites.
- Reverted `FunctionCompilerMiddleLayer.assembleName` to master form (no @SMLRZ id-shape transform).
- Reverted Fix #9: dropped the `declaringEnv` plumbing through `StructCompilerCore`. Internal methods compile against `outerEnv` again. Single deferred-compile loop.
- Registered all internal methods in `structOuterEnv` unconditionally (no `lift` filter).
- `StructDropMacro` reverted `self` → `thiss`.
- `NameHammer.simplifyName` accepts template-form citizen steps (`StructTemplateNameI` / `InterfaceTemplateNameI`).
- New: `Frontend/SimplifyingPass/.../RustShapeProjector.scala` at the SimplifyingPass→Backend boundary. (Initial version was incorrect — see Phase 7 below.)
- The May-03 session's first cut also added a **dual-home stopgap**: registered internal methods in *both* `structOuterEnv` AND `structResolvedEnv`. This made the smoke-test path (`Vec<int>.with_capacity(42).capacity()` via UFCS for `capacity`) pass, but broke the stricter integration tests that go through Vivem instantiation.

### Phase B — Explicit-container-template-args refactor (May-04 session, this branch's commits `e0193978` → `c1f820ba`)

Ten commits replacing the dual-home stopgap with a clean callsite-supplied channel. See the May-04 conversation log in `docs/historical/valerustinterop/` for the full design discussion. Highlights:

| Commit | Phase |
|---|---|
| `e0193978` | Phase 0: stale-pattern cleanup; identified the two failing `Extern rust Vec*` integration tests as the regression bar. |
| `0d703a4b` | Phase 1: `OutsideLoadSE` reshaped — `containerLookups: Map[IImpreciseNameS, (rules, RuneUsage)]` and `explicitArgsByTemplate: Map[IImpreciseNameS, Vector[RuneUsage]]` keyed by template. |
| `fa3d372b` | Phase 2: explicit `extraInitialKnowns: Vector[InitialKnown]` parameter threaded through the call chain (`CallCompiler` → `OverloadResolver` → `FunctionCompiler*Layer`). `ExpressionCompiler` populates it from container resolution. The `outerEnv.id.initId` walk-up in `assembleKnownTemplatas` is removed. |
| `d8a27e43` | Phase 3: dual-home registration removed (`StructCompilerGenericArgsLayer.scala`); Vivem extern stub keys realigned to template-form names (`Vec.new`, `Vec.with_capacity`, `Vec.capacity`). |
| `fbfd4a29` | Tracked `ExternFunctionNameI` lossy-humanizer follow-up (open item). |
| `69566d2d` | Phase 4: ICIPCRZ arcana updated. The identifiability *predicate* is unchanged — parent runes are still identifying inputs because the callsite must supply them via the container prefix. The *runtime delivery* shifted from "inherited via genericParameters + walk-up" to "explicit callsite map + extraInitialKnowns". |
| `fa9ebfa4` | Phase 5: `CompilerTests` "Extern rust Vec len" structural pattern updated for the new typed-id shape (template-form citizen step + function-owned placeholder). |
| `f721be74` | Phase 6: cleanup (WC_DEBUG instrumentation, stale `DO NOT SUBMIT` comment blocks). |
| `24ab3866` | Phase 7: `RustShapeProjector` rewritten. The original version assumed `initSteps` already had instantiated `StructNameI(template, [args])`; under the post-Phase-3 shape it has bare `StructTemplateNameI` with all args on the leaf. The rewrite walks `initSteps`, looks up each citizen template's arity in `HinputsI`, peels that many args off the front of the leaf, and rebuilds the citizen step in instantiated form. Smoke test passes after this. |
| `2f26b74e` | New @ETAKBTZ arcana written: `Frontend/docs/arcana/ExplicitTemplateArgsKeyedByTemplate-ETAKBTZ.md`. Documents why the carrier is keyed by template (the load-bearing reason: default arguments — `Outer<Q, P = List<Q>>` callsites that elide P need missing-entry semantics that flat vectors can't provide). Composes with future overload-set values and typed aliases. |
| `c1f820ba` | Cross-linked @ETAKBTZ from the four material construction/consumption sites. |

## How the data now flows

For a callsite `Vec<int>.with_capacity(42i64)`:

1. **Parser**: emits `MethodCallPE` with subject `LookupPE("Vec", <int>)` and method `LookupPE("with_capacity")`.
2. **PostParser** (`ExpressionScout.coerceOutsideLookupResult`): builds `OutsideLoadSE` with
   - `containerLookups = Map(CodeNameS("Vec") → (Vec-resolution-rules, vec_result_rune))`
   - `explicitArgsByTemplate = Map(CodeNameS("Vec") → [int_rune])`
   - `name = CodeNameS("with_capacity")`
3. **TypingPass** (`ExpressionCompiler` qualified-container case): runs `RuneTypeSolver` + `inferCompiler.solveForResolving` on Vec's rules → gets `templatasByRune` and `structId = Vec<int>`. Looks up `structDef = coutputs.lookupStruct(structId)`. Zips `structDef.instantiatedCitizen.id.localName.templateArgs` (placeholder form, with rune embedded) positionally with `explicitArgsByTemplate(Vec)` → produces `containerInitialKnowns = [InitialKnown(Vec.$T, IntTemplata)]`. Passes those as `extraInitialKnowns` through `CallCompiler.evaluatePrefixCall` → `OverloadResolver.findFunction` → `FunctionCompiler.evaluateGenericLightFunctionFromCallForPrototype` → `FunctionCompilerSolvingLayer.evaluateGenericFunctionFromCallForPrototype`.
4. **FunctionCompilerSolvingLayer.assembleKnownTemplatas**: master-form one-liner — `function.genericParameters.zip(explicitTemplateArgs)`. The call site appends `extraInitialKnowns`. Solver receives `T → int` as an InitialKnown. Function compiles successfully; T is bound.
5. **Instantiator → SimplifyingPass**: produces an `IdI[cI, IFunctionNameI[cI]]` like `[StructTemplateNameI(Vec), ExternFunctionNameI(with_capacity, [i32], [i64])]`.
6. **`Hammer.functionExterns`**: invokes `RustShapeProjector.projectFunctionId(hinputs, prototype.id)`. Walks `initSteps`, finds Vec's arity = 1 in `hinputs.structs`, peels 1 arg from the leaf's `templateArgs`, rebuilds the citizen step as `StructNameI(StructTemplateNameI(Vec), [i32])`. Result: `[StructNameI(Vec, [i32]), ExternFunctionNameI(with_capacity, [], [i64])]`.
7. **Backend**: emits Divination pragma `std::vec::Vec<i32>::with_capacity` (i32 on Vec, not on method).
8. **Divination**: produces `Vec::<i32>::with_capacity` turbofish — correct Rust.

## Verification

- **`scripts/test-rust-interop.sh`**: PASS — `Vec<int>.with_capacity(42i64).capacity().TruncateI64ToI32()` exits 42 through the full pipeline (Frontend → Backend → Divination → cargo cbuild → clang → run).
- **IntegrationTestsA**: 46/46 pass.
- **CompilerTests**: 97/97 pass (3 ignored).
- **PostParser tests** (PostParserTests + PostParserVariableTests + PostParsingParametersTests): 72/72 pass.
- **AfterRegions* suites**: 41 pre-existing failures unchanged. These are region-feature WIP tests (target-state placeholders); they fail on the rollback baseline and remain failing — not regressions from this work.
- **Backend test suite**: 180/198 (matches the rollback baseline). Region-pair failures in Buckets C and D below are pre-existing.

## Open items

### Tracked follow-ups in this work

1. **`ExternFunctionNameI` lossy humanizer** (`InstantiatedHumanizer.humanizeName` line 150). Renders the human name only, dropping templateArgs + parameters. Currently fine — Vivem stubs disambiguate via `ref: PrototypeH`, and the real Backend uses `RustShapeProjector`'s separate rendering path. But the rendered string collides across instantiations, which would surprise any future consumer that reads `IdH.fullyQualifiedName` expecting type info. Cleanup: mirror `FunctionNameIX`'s humanizer shape and update Vivem stubs to match.

2. **Backend-suite Rust-interop test**. The smoke test (`scripts/test-rust-interop.sh`) is not part of any automated suite, and Phase 0 explicitly chose pure-Vale integration tests as the regression guard rather than building Backend-suite Rust scaffolding. Quest.md Step 7 (the May-02 plan) called for adding at least one Rust-interop test to the regular Backend suite so the interop pipeline can't silently regress. Still open.

### Pre-existing failures (not addressed here)

3. **`FrontendRust` (in-progress Rust port): does not compile.** Step 5 of the May-02 plan. `OptimizedSolverState::new` gap likely needs the `74371d69` style retirement.

4. **Bucket B residual** (3 region-pairs, all resilient-v3): `foreach`, `stdlib_hashmap`, `stdlib_hashset`. Lambda-inside-`add`-inside-`HashSet` placeholder-rooting mismatch. Quest.md hypothesized this resolves as a fallout of the rollback (placeholders flatten under their owning function, no cross-denizen rooting). Hasn't been re-verified post-refactor; needs a quick check.

5. **Bucket C — clang link errors on extern ABI mismatch** (14 region-pairs). Tests: `interfaceimmparam{,deep}{extern,export}`, `interfacemutparamexport`, `rsamutparam­export`, `ssamutparamexport`, `structmutparam{,deep}export`. Backend emits `T*` for interface/struct/array extern parameters where test programs declare `T` by-value. Architecturally orthogonal to typing-pass concerns.

6. **Bucket D — runtime mismatch** (1 region-pair). `ssaimmreturnextern` resilient-v3 builds, links, runs but produces wrong output. Codegen or determinism-replay issue.

## Architecture references

- @ETAKBTZ — `Frontend/docs/arcana/ExplicitTemplateArgsKeyedByTemplate-ETAKBTZ.md`. The carrier shape (Map keyed by template) and why it's not a flat Vector.
- @ICIPCRZ — `Frontend/docs/arcana/IdentifiabilityChecksIncludeParentCitizenRunes-ICIPCRZ.md`. Updated to reflect the post-rollback runtime mechanism (callsite supplies parent runes via @ETAKBTZ; not the previous walk-up).
- @PASDZ — `Frontend/docs/arcana/PlaceholdersAreSelfDisambiguating-PASDZ.md`. Unchanged.
- @SMLRZ — `Frontend/docs/arcana/StructMethodLiftRules-SMLRZ.md`. **Deprecated** — describes the rolled-back mechanism. Keep for historical context; not a current rule.

## Tier-1 / Tier-2 / Tier-3 cleanup PRs to master

These commits have value independent of the rollback and could be cherry-picked to master:

- **Tier 1 (pure refactors)**: solver-API decomposition cluster (`aa26c43c`, `e4b06baa`, `f2371ad0`, `9f1503a0`, `74371d69`, `7bc37595`, `e34d34bc`); Instantiator substitutions Map flatten (`57572f70`).
- **Tier 2 (latent bug fixes)**: `makeIncludeDirectory` creates dir (`149ec433`); Coordinator `-arch arm64` on Darwin (`3b071a7c`).
- **Tier 3 (language improvements that happen to support Rust)**: lambda generic-param synthesis @LAGTNGZ (`6f09d88f`); `NonKindNonRegionPlaceholderNameT` arm (`0d365a79`); `assembleKnownTemplatas` index loosening (`43e631c6`).

Tier 4 (the @SMLRZ scaffolding and follow-on fixes) is **subsumed by the rollback** — those commits should not go to master.

## Reproduction

```bash
cd /Volumes/V/ValeRustInterop/Frontend && sbt 'set test in assembly := {}' clean assembly      # ~90s

cd /Volumes/V/ValeRustInterop/Backend && bash test.sh --clang_path /usr/bin/clang 2>&1 | tee /tmp/suite.txt
grep "Done!" /tmp/suite.txt

# Smoke test (requires Divination + a Rust deps Cargo.toml):
DIVINATION_PATH=/Volumes/V/Divination/target/debug/Divination \
RUST_CARGO_TOML=/Volumes/V/Divination/TestProject/rust/Cargo.toml \
  bash /Volumes/V/ValeRustInterop/scripts/test-rust-interop.sh
```

## Investigation docs (historical)

- `investigations/cluster1-vassertSome-translateCoord.md` — substitutions Map flatten (Fix #4).
- `investigations/cluster2-overloadresolver-bound-prefix-mismatch.md` — bound-prefix mismatch + Fix #9 architectural fix (Fix #9 itself rolled back; this doc is historical context).
- `investigations/refactor1-remove-lift-flag.md` — the May-02 Refactor #1 attempt that stalled and motivated the architectural backtrack.

## Conversation logs (this work)

In `docs/historical/valerustinterop/`:

- `2026-04-30-cluster1-substitutions-map-flatten-f27654bd.md`
- `2026-05-02-cluster2-fix9-refactor1-stall-43c09989.md`
- `2026-05-03-smlrz-rollback-execution-dual-home-c39831b7.md`
- `2026-05-04-explicit-container-args-refactor-1bb7dbf2.md`
