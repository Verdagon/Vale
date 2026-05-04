# Quest: Make `rustinterop-merged` Release-Worthy

## Status

Architectural rollback IN PROGRESS on branch `refactor-rollback-smlrz`. Per the new plan
(see `~/.claude/plans/please-make-a-very-scalable-whistle.md`), Phases 1-7 of the rollback
are landed and the typing pass is now UFCS-flat:

- **Backend test suite: 180/198** (unchanged from baseline; rollback is behaviorally neutral
  on the existing suite, which doesn't exercise Rust interop).
- **`lift`, `inherited` fields removed** from FunctionS/A and GenericParameterS.
- **`assembleName` reverted to master form** (no @SMLRZ id-shape transform).
- **StructCompilerCore** loops normalized; `declaringEnv` parameter removed (Fix #9 reverted).
- **NameHammer** accepts template-form citizen steps (post-rollback shape).
- **`RustShapeProjector`** added at the SimplifyingPass→Backend boundary; strips leading
  inherited template args from extern function ids so Rust paths render as `Vec<i32>::capacity`
  (one arg per structural level).
- **Smoke test (`scripts/test-rust-interop.sh`)**: still failing on `v.capacity()` UFCS lookup
  (only the runtime_sized_array `capacity` candidate is found). Same failure on baseline —
  pre-existing, not a rollback regression. Investigation needed: param-env walk into Vec's
  env doesn't surface the registered `capacity` internal method.
- **`FrontendRust` (in-progress Rust port): does not compile.** Unchanged from prior status.

## What this branch is doing

Rust requires function paths in the form `Vec<i32>::push` (type args on the receiver-type step). Vale historically only had `Vec.push<i32>` form (type args on the function step). This branch was supposed to teach Vale's typing pass to produce the Rust shape so `ValeRuster` could match impl blocks structurally without rearrangement.

## Architectural realization (this session)

The branch's premise — *teaching the typing pass to produce Rust-shaped ids* — was the wrong architectural choice. The truth being encoded:

- Vale doesn't have specialization. Doesn't have impl-coherence. Doesn't need type-first method dispatch. None of the reasons Rust uses `Vec<i32>::push` apply to Vale.
- A function's type args belong to the *function*, not to the *receiver type*. `Vec.push<i32>` is the truthful form: `push` is a function with one generic param `T`; when called with `Vec<i32>`, T = i32. The "type args on the receiver" form is Rust's presentation choice, driven by specialization.
- Rust's `Vec<i32>::push` form is itself a fiction layered over the truth — even the "function lives in impl<T> Vec<T>" framing is presentation. The truth is "function with parameters and bounds, defined at some location."

The branch added @SMLRZ + `lift` + `inherited` + Fix-#9's env-hierarchy split + the OWPFRD entanglement to bake the Rust presentation into the typing pass id. Each piece individually defensible; collectively they form an over-constrained architecture that conflicts with @PNBDTZ (placeholder ownership), @ICIPCRZ (identifiability), and OWPFRD (single-prefix invariant). Refactor #1 (remove `lift`) attempted this session demonstrated the conflict: the layers can't be removed individually because they're load-bearing for each other.

## Where we want to go

**Internal model**: UFCS-flat. The typing pass produces ids in the truthful form `[Vec, push]<i32>(&Vec<i32>, i32)`:

- Citizen steps in `initSteps` are *registration locations* (where the function was declared / where lookup finds it via param-type-env). They carry no template args at the step level.
- Type args live on the function (leaf) step. That's where they actually parameterize.
- Placeholders are rooted at their owning function (the one whose generic params they instantiate), not at any "declaring denizen." `drop`'s `$X` is `[drop, $X]`. No `@PNBDTZ`-style cross-denizen rooting.
- No `lift` flag. No `inherited` flag (or, if kept, it's pure render metadata, no compile behavior).
- OWPFRD's strict single-prefix invariant holds trivially because all placeholders flatten under the function being compiled.

**Output projection** (Rust shape, at NameHammer or a dedicated rendering step):

For each ID, walk its `initSteps` left-to-right. For each step, look up the template's arity (number of generic params expected). Consume that many args from the function's `templateArgs`, render the step with those args. Any args left at the end stay on the leaf. The resulting form is the Rust path.

```
[Vec, push]<i32>             → arity(Vec)=1: consume i32 → Vec<i32>::push()
[Vec, map]<i32, String>      → arity(Vec)=1: consume i32 → Vec<i32>::map<String>()
[push]<T>                    → no namespace step → push<T>()
[Outer, Inner, foo]<A, B, C> → consume A, B → Outer<A>::Inner<B>::foo<C>()
```

The walk is purely structural (arity per template + left-to-right consumption). No "receiver" concept. No `inherited` flag. ~30 lines in the renderer.

The function's templateArgs ordering (parent runes before own runes) is already established in Vale's source-level rule (per `extraGenericParamsFromParentS ++ functionUserSpecifiedGenericParametersS`), so consumption-by-arity reproduces the right partition without source changes.

## What's done

### Fix #1 — `assembleKnownTemplatas` index match (`43e631c6`)

Loosened hard-coded `KindPlaceholderTemplateNameT(0, _)` to `(_, _)`. WIP-leftover from `79805fad`.

### Fix #2 — Macro-generated drop routes through `assembleName`'s self-struct path (`76cd7e0c`)

`StructDropMacro` now names its parameter `keywords.self` and `lift = true`. Closes 48× `vwat()` at `Instantiator.assemblePlaceholderMap`. **Architecturally**: this is part of the @SMLRZ machinery the new direction wants to remove. Its symptom-fix value remains while the larger architecture is in flux.

### Fix #3 — `assembleKnownTemplatas` accepts non-Coord placeholders (`0d365a79`)

Added `NonKindNonRegionPlaceholderNameT` arm. WIP-leftover from `79805fad`. Architecturally orthogonal to @SMLRZ — this fix stays valuable in the new direction.

### Fix #4 — Collapse Instantiator's `substitutions` to a single-level map (`57572f70`)

Flattened `Map[IdT[INameT], Map[IdT[IPlaceholderNameT], …]]` to `Map[IdT[IPlaceholderNameT], …]`. The two-level structure was a fossil from the `Vector[…]` placeholder-index era; once placeholder ids became globally unique (per @PASDZ), the outer level became redundant. Architecturally orthogonal to @SMLRZ — clean simplification, stays.

### Fix #5 — Backend skips Divination when no Rust externs (`052d8b0c`)

Early-return in `doRustyThings` when `divinationInputStr.empty()`. Architecturally orthogonal — stays.

### Fix #6 — `-arch arm64` on Darwin (`3b071a7c`)

Coordinator now passes `-arch arm64` whenever `IsDarwin()` is true. Architecturally orthogonal — stays.

### Fix #7 — `makeIncludeDirectory` actually creates the directory (`149ec433`)

Backend bugfix. Architecturally orthogonal — stays.

### Fix #8 — `AnonymousSubstructNameT` match-arm completions (`5ad47abe`)

Two TypingPass match statements added arms for the instantiated form. **Architecturally**: only needed because @SMLRZ creates instantiated forms in places that weren't expected. In the new direction these arms become unnecessary (typing pass keeps template form).

### Fix #9 — Lifted/sibling methods compile against `declaringEnv` (`5fb81160`)

Changed `StructCompilerCore.iter`'s pairing for lifted/sibling methods. Aimed to fix cluster #2 by making drop's parentEnv chain skip the citizen env. **Architecturally**: this is a symptom-fix for a problem created by @SMLRZ. In the new direction (typing pass UFCS-flat, no citizen prefix in templateId), the cluster #2 duplicate-bound issue doesn't arise — the substruct's contract bound is reachable to *callers* via param-env lookup, not to *drop's body* via parent walk.

## Remaining failures (180/198, 18 region-pairs)

### Bucket B residual — `foreach`, `stdlib_hashmap`, `stdlib_hashset` (3 region-pairs, all resilient-v3)

Lambda-inside-`add`-inside-`HashSet` placeholder-rooting mismatch. Different sub-cluster than cluster #2 main case. **Architecturally**: probably becomes irrelevant under the new direction — placeholders rooted at the function (no cross-denizen ownership) sidesteps the rooting mismatch entirely. May resolve as a fallout of the architectural backtrack rather than as its own fix.

### Bucket C — clang link errors on extern ABI mismatch (14 region-pairs)

Tests: `interfaceimmparam{,deep}{extern,export}`, `interfacemutparamexport`, `rsamutparamexport`, `ssamutparamexport`, `structmutparam{,deep}export`. Backend emits `T*` for interface/struct/array extern parameters, but test programs declare `T` by-value. Backend codegen bug or stale test C — architecturally orthogonal to typing-pass concerns.

### Bucket D — runtime mismatch (1 region-pair)

`ssaimmreturnextern` (resilient-v3) builds, links, runs but produces wrong output. Codegen or determinism-replay issue, architecturally orthogonal.

## Plan

### Step 1 — Roll back the @SMLRZ direction

Revert the typing-pass machinery that bakes the Rust shape into compiled ids. Specifically:

- Remove `lift: Boolean` from `FunctionS`/`FunctionA`.
- Remove `inherited: Boolean` from `GenericParameterS` (or keep as render-metadata only).
- Remove the @SMLRZ id-shape transform in `FunctionCompilerMiddleLayer.assembleName`. Function ids stay as `parentEnv.id.addStep(makeFunctionName(templateArgs, paramTypes))` — citizen-step-bare, type-args-on-function.
- Remove `extraGenericParamsFromParentS`'s contribution to `function.genericParameters` — placeholders flatten under the function.
- Revert `StructCompilerCore.iter`'s `declaringEnv`-skip (Fix #9). Lifted methods compile against the citizen env naturally; lookup walk-up traverses the citizen, surfacing whatever's there. NBIFP-imported bounds are still local; ancestral bound declarations are treated normally (no longer cause cluster #2 duplicate, because the typing-pass id form changes).
- Update `NameHammer` and downstream to handle the simplified id form.

The rollback is large in lines-of-code but mechanical. Each removal is a structural undo.

### Step 2 — Add the Rust-shape projection step

At NameHammer (or as a dedicated rendering step before `rustifySimpleIdStep`):

- Walk the function's id `initSteps`.
- For each step, look up the citizen template's arity in coutputs.
- Consume that many args from the function's `templateArgs`.
- Render the step as `<TemplateName>` followed by `<consumed args>` (or skip the brackets if 0 args).
- The leaf step gets remaining args.
- Path joined with `::` for Rust.

~30 lines. The arity lookup uses existing `coutputs.lookupCitizen` or equivalent.

### Step 3 — Verify cluster #2 main case still works without Fix #9

Under the new typing-pass shape, drop's id is `[anon:I, drop:0]<member-rune>(&anon:I<...>, ...)`. The cluster-#2 lookup-walk-up duplicate either:

- Doesn't appear because the resolver's narrow-down sees structurally consistent forms (both candidates have `[anon:I, ...]` prefix), OR
- Appears but `startsWith` succeeds because both prefixes are bare-template form.

If it still fails, the cluster #2 fix shifts to a small lookup-walk filter (the alternative we considered earlier) — not a re-introduction of @SMLRZ.

### Step 4 — Bucket B residual

Likely resolved as a fallout of Step 1's placeholder-flattening. If not, investigate the lambda env-construction site under the new architecture.

### Step 5 — `FrontendRust` builds + tests pass

1. Resolve `OptimizedSolverState::new` gap. Mirror Scala parity (likely retire `OptimizedSolverState` per `74371d69`).
2. `cargo build --manifest-path FrontendRust/Cargo.toml --lib` until clean.
3. `cargo test --manifest-path FrontendRust/Cargo.toml --lib` until clean.

### Step 6 — Bucket C and D

Diagnose extern ABI mismatch (10 pairs) and `ssaimmreturnextern` runtime issue (1 pair). Each is its own session; architecturally orthogonal to the typing-pass cleanup.

### Step 7 — Smoke + interop tests

1. `scripts/test-rust-interop.sh` should still pass after all the above.
2. Add at least one Rust-interop test to the regular Backend test suite so the interop pipeline doesn't silently regress.

## Tier-1 / Tier-2 / Tier-3 cleanup PRs to master

Independent-value commits could merge to master ahead of the typing-pass rework. From most independent to least:

- **Tier 1 (pure refactors)**: solver-API decomposition cluster (`aa26c43c`, `e4b06baa`, `f2371ad0`, `9f1503a0`, `74371d69`, `7bc37595`, `e34d34bc`); Instantiator substitutions Map flatten (`57572f70`).
- **Tier 2 (latent bug fixes)**: `makeIncludeDirectory` creates dir (`149ec433`); Coordinator `-arch arm64` on Darwin (`3b071a7c`).
- **Tier 3 (language improvements that happen to support Rust)**: lambda generic-param synthesis @LAGTNGZ (`6f09d88f`); `NonKindNonRegionPlaceholderNameT` arm (`0d365a79`); `assembleKnownTemplatas` index loosening (`43e631c6`).

**Tier 4 (revisit under new direction)**: @SMLRZ machinery, `lift`/`inherited` flags, Fix #2 (`76cd7e0c`), Fix #8 (`5ad47abe`), Fix #9 (`5fb81160`). These are scaffolding for the wrong architecture; the rollback subsumes them.

## Reproduction

```bash
cd Frontend && sbt 'set test in assembly := {}' clean assembly      # ~90s
cd Tester && bash build.sh ~/BootstrappingValeCompiler              # one-time

cd Backend && bash test.sh --clang_path /usr/bin/clang 2>&1 | tee /tmp/suite.txt
grep "Done!" /tmp/suite.txt
```

## Phase 0 baseline (refactor regression bar)

Two existing tests in `Frontend/IntegrationTests/test/dev/vale/IntegrationTestsA.scala` exercise the qualified-call-to-internal-method syntax (`Vec<int>.X(...)`) that the explicit-container-template-args refactor is fixing. **Both currently fail** on `refactor-rollback-smlrz` after the dual-home stopgap:

- **`Extern rust Vec`** (line ~340) — `Vec<int>.new()` (qualified call to self-less internal method, T only in return). Fails at `Instantiator.translateCollapsedFunction:1780` with `vfail()` — `newHeader.toPrototype != desiredPrototypeC`. Same root cause as the dual-PrototypeT issue, surfaced one phase later.
- **`Extern rust Vec capacity`** (line ~357) — `Vec<int>.with_capacity(42i64)` plus `Vec<int>.capacity(v)` (qualified call to self-having internal method). Fails earlier with the dual-PrototypeT assertion: the same logical method has two `PrototypeT` shapes (one with `StructTemplateNameT(Vec)` in initSteps, one with `StructNameT(Vec, [int])`) — the dual-home stopgap creates both and downstream code asserts they should be the same.

These tests **are** the Phase 0 guard. The dual-home fix only rescues the smoke-test path (UFCS `v.capacity()`, no qualified self-having call) — it doesn't satisfy the stricter Vivem instantiation pipeline that integration tests run through. Phase 1+2 must make both tests green; that's the bar.

Note: the original Phase 0 plan called for adding a new pure-Vale test in this shape. Investigation found that (a) any pure-Vale internal method whose T appears only in the return type fails at HigherTyping ("Couldn't solve some runes: T") because the rune-type-solver can't determine T's type without rules referencing it, and (b) the existing `extern struct Vec<T>` tests already cover the same typing-pass path. So the existing two tests stand as the Phase 0 baseline; no new test added.

## Follow-up: tighten `ExternFunctionNameI` humanizer

`InstantiatedHumanizer.humanizeName` renders `ExternFunctionNameI(humanName, templateArgs, parameters)` as just `humanName.str` — drops the templateArgs and parameters. Compare `FunctionNameIX` (the non-extern variant), which renders both via `humanizeGenericArgs`. This means every monomorphization of e.g. `Vec<T>.new` renders to the same string `"Vec.new"` in `IdH.fullyQualifiedName`.

Currently this is fine because:
- Vivem dispatches by the rendered name and the stub disambiguates via the full `ref: PrototypeH` (which still has templateArgs in `ref.id.localName`).
- Real Backend uses `RustShapeProjector` (a separate rendering path) for link-time symbol resolution, producing distinct names like `Vec<i32>::new`.

The two consumers, two views arrangement works for the immediate refactor. But the lossy humanizer is fragile — if any future consumer reads `fullyQualifiedName` expecting type info, it'd silently collide across instantiations. Cleanup: have `ExternFunctionNameI`'s humanizer include templateArgs (mirror `FunctionNameIX`'s shape), update Vivem stub keys accordingly. Out of scope for the current refactor; tracked here.

## Arcana to write this session

- Arcana documenting why the callsite explicit-template-args carrier is a `Map[IImpreciseNameS, Map[IRuneS, ITemplata]]` (template-id → rune → templata) rather than a flat `Vector[ITemplata]`. The reason: default arguments. With `Outer<Q, P = List<Q>>`, a callsite `Outer<i32>.Inner<bool>` provides Q but elides P. A flat vector can't represent the hole at P's position without sentinel/positional metadata; the Map represents it naturally as a missing inner-Map entry, and the default mechanism fills it. Same shape composes with future `Vec<i32>.foo` overload-set values and typed aliases (`alias Bytes: Something = Vec<u8>`).

## Investigation docs

- `investigations/cluster1-vassertSome-translateCoord.md` — substitutions Map flatten (Fix #4). Stays valid in new direction.
- `investigations/cluster2-overloadresolver-bound-prefix-mismatch.md` — bound-prefix mismatch + Fix #9 architectural fix. Becomes historical context once Step 1 lands.
- `investigations/refactor1-remove-lift-flag.md` — Refactor #1 attempt (stalled). Documents the entanglement that motivated the architectural backtrack. Reading it explains why Step 1 (rollback) is the right move rather than yet another forward refactor.

## See also

- `Frontend/docs/arcana/StructMethodLiftRules-SMLRZ.md` — current SMLRZ rules. To be deprecated/removed under Step 1.
- `Frontend/docs/arcana/PlaceholdersAreSelfDisambiguating-PASDZ.md` — placeholder ids globally unique. Stays valid.
- `Frontend/docs/arcana/PlaceholdersNamedByDenizenTemplate-PNBDTZ.md` — current placeholder-ownership rule. To be revised: under the new direction, placeholders are rooted at the *using* function, not the declaring denizen.
- `Frontend/docs/arcana/IdentifiabilityChecksIncludeParentCitizenRunes-ICIPCRZ.md` — current identifiability rule. May change under the new direction depending on how parent-rune visibility is preserved (likely via param-type-env lookup, no longer via inheritance into function genericParams).
- `docs/background/id-shorthand-notation.md` — `anon:I`, `$functor:moo`, ownership prefixes, bare-vs-`<>`. Notation stays valid; the *idiom* of "citizen step has args" becomes a render-only convention.
