# Quest: Fix the 12 Remaining AfterRegions Regressions

**Status:** 47 AfterRegions tests total → 28 pass / 12 fail / 7 ignored. All 12 failures are **regressions** (capabilities that worked in the template system, broken during the templates→generics refactor or the preceding monomorphization-prep work of Aug–Sep 2022). The 7 ignored tests are aspirational/never-worked features; they're parked and not counted here. Historical record of what was tried and learned: `docs/historical/after-regions-fixing-tests-quest.md`.

**This document** groups the 12 regressions by root cause, explains why each is broken, and sketches what fixing each would require. Families 1 and 2 together account for 8 of the 12 — invest there for the biggest unblock.

## How to verify current state

From the repo root:

```bash
cd Frontend
sbt 'testOnly dev.vale.AfterRegionsIntegrationTests dev.vale.typing.AfterRegionsTests dev.vale.typing.AfterRegionsErrorTests dev.vale.parsing.AfterRegionsTests dev.vale.parsing.functions.AfterRegionsFunctionTests dev.vale.postparsing.AfterRegionsErrorTests' 2>&1 | tee /tmp/quest-status.txt
grep -E "Tests: succeeded|FAILED \*\*\*" /tmp/quest-status.txt
```

Expect `Tests: succeeded 28, failed 12, canceled 0, ignored 7, pending 0` and exactly the 12 failing tests enumerated below.

## Vocabulary

Shared across all families. See `docs/Generics.md` for full treatment.

- **Templates (old system)** — generic functions were expanded per call site, like C++ templates. Each call got a specialized copy. The function body wasn't type-checked in isolation; errors surfaced only at call sites.
- **Generics (new system)** — generic functions compile ONCE with abstract placeholders (`KindPlaceholderT`, `RegionT`). A separate `Instantiator` pass walks the call graph later and stamps out concrete versions. Requires bounds (`where implements(T, IShip)`, `where func(&T)int`) because the body is type-checked with only the placeholder + whatever bounds promise.
- **Bound** — a constraint declared in a `where` clause. E.g. `where func drop(T)void` promises "some function named `drop` taking `T` and returning `void` is available." Bounds let the compiler type-check the generic body without knowing what T will be.
- **OverloadSet** — a kind representing a set of function overloads under a name. When the user writes `myfunc` as a value (not a call), the compiler creates `CoordT(ShareT, _, OverloadSetT(...))` to carry the unresolved set.
- **Acronyms used below:** CFWG (Concept Functions With Generics), IRBFPTIPT (Inherit Reachable Bounds For Params), MKRFA (Must Know Runes From Above), BRRZ (Bound Return Resolution), DRSINI (Default Rules Should Be Incremental Not Initial), NBIFP (Need Bound Information From Parameters), SFWPRL (Solve First With Predictions, Resolve Later), ECSIIOSZ (Each Call Site Is Its Own Solve).

---

# Family 1: Impl-bound propagation into overload resolution (6 tests)

## The shared root cause

In the template system, writing `x.launch()` on a generic-typed `x: T` Just Worked — the template was expanded per call site, so at expansion time `T` was concrete and the compiler looked up `launch` on the concrete type.

In the generic system, `x.launch()` has to type-check against an abstract `T`. The programmer declares bounds: `where implements(T, IShip)` (meaning: any `T` passed must implement `IShip`) or `where func launch(&T)void` (meaning: there must be a function named `launch` taking `&T`). The compiler is supposed to use those bounds to validate the call: "does IShip have `launch`? Yes. Does the `implements` bound guarantee T has IShip's methods? Yes. So `x.launch()` type-checks."

**The gap:** the machinery for bounds-informing-overload-search exists partially (see `NBIFP` in `Generics.md:489+` and the existing plumbing in `FunctionCompilerSolvingLayer.scala:555-557`) but doesn't cover all cases. Specifically:

- `where implements(T, Interface)` doesn't propagate into the overload resolution for method calls on `T` (the `No ancestors satisfy call` failure).
- `where func(&F, &T)void` with F being an `OverloadSetT` hits the `functor1.vale` hack, which has hardcoded "drop" in its where clause — works only when the overload set is named `drop`.
- The `CFWG` (Concept Functions With Generics) design described in `docs/Generics.md:201` is the intended solution but hasn't been implemented. Three rule types (`ResolveSR`, `CallSiteFuncSR`, `DefinitionFuncSR`) exist but aren't wired up to perform the overload→prototype coercion the doc describes.

## What fixing this family requires

Broadly: **implement CFWG as described in `docs/Generics.md:201-258`.** The doc describes two alternative designs (prototype-based and placeholder-based); either is viable. Concretely:

1. Rework `functor1.vale` to not hardcode "drop" — the current `where F Prot = func drop(P1)R` only resolves for the drop overload set. Replace with a mechanism that uses the calling context's overload-set name.
2. Implement `OverloadSetT → Prot` coercion in the solver — when a function parameter expects a callable `F` and the argument is an `OverloadSetT`, the solver should pick the overload matching the required signature and substitute the resulting prototype.
3. Wire `where implements(T, Interface)` into `OverloadResolver.scala`'s overload search — when resolving `x.launch()` where `x: T`, check bounds-declared impls before rejecting with "No ancestors satisfy call". The current filter at `EdgeCompiler.scala:421-428` strips nested-type-arg bounds (that's the IRBFPTIPT gap for the ignored Diff iter test, but the same machinery location).
4. Coordinate with existing BRRZ work (`docs/arcana/BoundReturnResolution-BRRZ.md`) — BRRZ already relaxed `ResolveSR` to discover return types from name+params; extending it to interact with impl bounds is a natural follow-on.

**Scope estimate:** Multi-day project (3–7 days). Touches `CompilerSolver.scala`, `OverloadResolver.scala`, `InferCompiler.scala`, `EdgeCompiler.scala`, `FunctionCompilerSolvingLayer.scala`, `functor1.vale` (or its replacement), plus new documentation. Run the full AfterRegions suite after every non-trivial change; bound-propagation changes have wide blast radius.

**Unblocks:** All 6 tests in this family, plus potentially the ignored IRBFPTIPT and Generic-interface-anonymous-subclass tests if the machinery is generalized enough.

## The 6 tests in detail

### 1.1 Method call on generic data

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:22`

**Test shape:**
```vale
interface IShip { ... }
struct Raza { ... }
func launch(self &Raza) { }
impl IShip for Raza;

func launchGeneric<T>(x &T) where implements(T, IShip) {
  x.launch();
}

exported func main() int {
  launchGeneric(Raza());
  ...
}
```

**Current failure:**
```
Couldn't find a suitable function launch(&Kind$launchGeneric.T). Rejected candidates:
Candidate 1 (of 1): tvl:140
Bad super kind in isa: Raza
func launch(self &Raza) { }
```

The solver tries to find a `launch` for `T`. The candidate `func launch(self &Raza)` is rejected with "Bad super kind in isa: Raza" — the overload resolver doesn't consult the `implements(T, IShip)` bound to decide T could be dispatched to Raza's launch. The bound is declared but not informing the call.

**Historical status:** Regression. In templates, this worked because `launchGeneric` was expanded per call site with T=Raza, and the resulting body had `x.launch()` on a concrete `&Raza`, which trivially resolved.

**Fix:** Wire impl bounds into `OverloadResolver` overload search. When a rejection is "Bad super kind in isa" and the caller's bounds include `implements(T, SomeInterface)`, check whether the rejected candidate is a method on SomeInterface, and if so, accept the candidate.

**Also watch for:** Test ends with `vimpl()` after `expectCompilerOutputs()` (category B at time of earlier tracking). Once compilation works, the test body's `vimpl()` will also need replacing with real assertions.

### 1.2 Impl rule

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:170`

**Test shape:**
```vale
interface IShip { func getFuel(virtual self &IShip) int; }
struct Raza { fuel int; }
impl IShip for Raza;
func getFuel(self &Raza) int { return self.fuel; }

func genericGetFuel<T>(x T) int where implements(T, IShip) {
  return x.getFuel();
}
```

**Current failure:** `No ancestors satisfy call: (arg 0) = &Kind$genericGetFuel.T`

**Root cause:** Identical to Method call on generic data. Same family, same fix.

**Historical status:** Regression. The underlying feature (calling interface methods on generic-typed values) worked in templates.

### 1.3 Reports error (virtual dispatch error reporting)

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsErrorTests.scala:109`

**Test shape:** Interface A declares virtual `foo(&A)`; struct B implements A via `impl A for B`; but B lacks a `foo(&B)` override. The test expects an error saying "missing override," but currently gets:

```
Couldn't find a suitable function foo(&B)
```

**Root cause:** The override-search mechanism that looks up "does B have foo?" doesn't see B as implementing A. Same `implements` propagation gap as 1.1 and 1.2, just in error-reporting rather than valid-code context.

**Historical status:** Regression. Virtual interface dispatch through impls worked in the template system.

**Fix note:** Once the override search sees B as implementing A, the test will likely surface the RIGHT error (missing override). Then confirm the error text matches what the test expects, and replace the trailing `vimpl()` with a real assertion.

### 1.4 Lambda is incompatible anonymous interface

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsErrorTests.scala:191`

**Test shape:**
```vale
interface AFunction1<P Ref> {
  func __call(virtual this &AFunction1<P>, a P) int;
}
exported func main() {
  arr = AFunction1<int>((_) => { 4 });
}
```

The test expects `Err(BodyResultDoesntMatch(_, _, _, _))` — the lambda body should be incompatible with the interface method signature.

**Current failure:** Currently blocks on generic-interface-anonymous-subclass machinery (which is ignored as aspirational), so the test never reaches the body-result-matching phase.

**⚠️ Critical note — test is doubly-broken:**

Even if the anonymous subclass machinery worked, this test has a **design flaw**: the lambda body `(_) => { 4 }` returns `int`. The interface's `__call` method also returns `int`. They're compatible! The test would return `Ok`, not `BodyResultDoesntMatch`.

**Two-step fix required:**
1. First, Family 1's machinery needs to work (so the test gets past the generic interface anonymous subclass compile failure).
2. Then, the test body itself needs to be changed — the lambda must actually return something that MISMATCHES the interface return type (e.g., `(_) => { "hello" }` returning `str`).

`BodyResultDoesntMatch` is defined with 2 active throw sites in `FunctionBodyCompiler.scala` (lines 94, 130). The error type works; the test just doesn't trigger it.

**Historical status:** Regression-in-family-1 + test-design bug.

### 1.5 Test overload set

**File:** `Frontend/IntegrationTests/test/dev/vale/AfterRegionsIntegrationTests.scala:78`

**Test shape:**
```vale
import array.each.*;
func myfunc(i int) { }
exported func main() int {
  mylist = [#](1, 3, 3, 7);
  mylist.each(myfunc);   // myfunc is an OverloadSetT here
  42
}
```

**Current failure:** `Couldn't find a suitable function __call((overloads: myfunc), i32)` because the only `__call` candidate is the `functor1.vale` hack with hardcoded "drop".

**Root cause:** The `each` function in `array/each/each.vale` has `where func(&F,&T)void`. To satisfy this bound with `F=OverloadSetT(myfunc)` and `T=i32`, the solver looks up `__call(F, T)`. The functor1.vale __call has:

```vale
func __call<P1 Ref, R Ref>(v void, param P1) R
where F Prot = func drop(P1)R {
  F(param)
}
```

Two problems:
- The `v void` first param conflicts with the OverloadSet being passed as arg 0 (`_41111.kind: was (overloads: myfunc) but now concluding void`)
- The `drop` literal in the where clause is hardcoded — the rule emits `resolve-func StrI(drop)(...)` — so this builtin only resolves for the `drop` overload set, not `myfunc`

**Historical status:** Regression. Added 2022-04 in `IntegrationTestsA.scala` and was green for months before being parked in AfterRegions during the refactor.

**Fix:** This is the canonical CFWG case (`docs/Generics.md:201`). The functor1.vale hack is explicitly acknowledged in its own comment: *"It's not particularly great because it's got that `drop` name hardcoded in there. Hopefully soon we can upgrade our generics system to not need this, see CFWG."*

See the shared Family 1 fix description above.

### 1.6 Tests overload set and concept function

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:57`

**Test shape:**
```vale
func moo<X, F>(x X, f F)
where func(&F, &X)void, func drop(X)void, func drop(F)void {
  f(&x);
}
exported func main() {
  moo("hello", print);
}
```

**Current failure:** `Couldn't find a suitable function __call((overloads: print), str). No function with that name exists.`

**Root cause:** Identical to 1.5 — `where func(&F,&X)void` triggers `__call((overloads: print), str)` lookup, which fails for the same CFWG reason (hardcoded "drop" in functor1.vale).

**Historical caveat:** Earlier sessions claimed this test "passes" because `expectCompilerOutputs()` (typing-pass-only) succeeded. But `evalForKind()` (full pipeline) fails. The typing pass tolerates the unresolved bound; later phases don't.

**Fix:** Same CFWG work as 1.5. These two tests will likely go green together.

---

# Family 2: Anonymous-param lambda inference (2 tests)

## The shared root cause

The lambda syntaxes `(_) => body` (magic-param) and `(a, b) => body` (plain multi-param) rely on the postparser synthesizing an implicit rune for each untyped parameter and lifting it into `FunctionS.genericParams`. The current code synthesizes the rune but doesn't lift it, so lambdas end up with `genericParams.size == 0`. Per @LAGTNGZ, lambdas compile through the typing pass's templated-closure path (not the Instantiator), but that path still reads `function.genericParameters` to build its per-call-site solve — without the runes there, two call sites with different arg types can't differentiate.

Specifically:
- `(_) => body` is supposed to generate exactly 1 generic param (the `_` placeholder's type). The new system counts 0.
- Calling a lambda twice with different types (`lam(1, 2); lam(3.0, 4.0)`) is supposed to produce 2 distinct `LambdaCallFunctionNameT` entries in `CompilerOutputs.functions`. The new system produces 0 because the per-call-site solve has no rune to bind each call's arg types to.

Both gaps are in rune-identification in the postparser — once the runes are in `genericParams`, the typing pass's templated-closure path handles specialization naturally per LAGTNGZ.

## What fixing this family requires

1. **Postparser fix** — in `FunctionScout.scoutFunction`, when scouting a lambda body, detect `MagicParamLookupPE` references and generate one generic param per unique magic-param index. The comment at `FunctionScout.scala:254` already says "they might be anonymous params like in `(_) => { true }`"; the logic needs to use that knowledge to populate generic params.

2. **Instantiator fix** — when stamping a lambda function for a specific call site, ensure distinct types at distinct call sites produce distinct instantiations. Currently they collapse.

**Scope estimate:** Medium (2–3 days). Two self-contained subsystems. Verify against the pre-refactor reference tests (`PostParsingParametersTests.scala:120` has the verbatim original of test 2.1).

**Unblocks:** Both tests in this family. Probably also incidentally unblocks some lambda-related functionality elsewhere in the compiler.

## The 2 tests in detail

### 2.1 Test one-anonymous-param lambda identifying runes

**File:** `Frontend/PostParsingPass/test/dev/vale/postparsing/AfterRegionsErrorTests.scala:70`

**Test shape:**
```vale
exported func main() int {
  do((_) => { true })
}
```

Asserts `lambda.function.genericParams.size shouldEqual 1`. Currently gets 0.

**Historical status:** Regression. This is a verbatim copy of `PostParsingParametersTests.scala:120` — a test that was passing in the template era. Magic-param lambda syntax and rune identification predate the refactor by years; the refactor lost the wiring.

**Fix:** Postparser logic to count magic-param indices and create corresponding generic params. Compare current `FunctionScout.scala` behavior with the pre-refactor version at `git show 05996eb7^:Frontend/PostParsingPass/src/dev/vale/postparsing/FunctionScout.scala` to see what was removed/changed.

### 2.2 Test two instantiations of anonymous-param lambda

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:198`

**Test shape:** Uses a named lambda `lam = (a, b) => { a == b }` invoked twice with different types. Asserts `lambdaFuncs.size shouldEqual 2`. Currently gets 0.

**Note:** Despite the test name, the lambda uses `(a, b) =>`, not `(_) =>`. This tests plain multi-param monomorphization, which is pre-existing functionality.

**Historical status:** Regression. Multi-param lambdas worked in templates.

**Fix:** Instantiator per-call-type specialization for lambdas. When lam is called with different arg types, produce distinct `lambdaFunc` entries in the output.

---

# Family 3: Generic virtual dispatcher — abstract generics not in self-interface (1 test)

## 3.1 Map function

**File:** `Frontend/IntegrationTests/test/dev/vale/AfterRegionsIntegrationTests.scala:64`

**Test shape:** Calls `programs/genericvirtuals/mapFunc.vale` via `Tests.loadExpected`. Exercises a generic abstract method whose abstract has generics not reachable from the virtual `self` param — specifically:

```
abstract func map<T, R>(virtual opt &Opt<T>, func &IFunction1<mut, &T, R>) Opt<R>;
```

`T` appears in self (`&Opt<T>`), `R` does not. This is the common case for higher-order methods over interfaces (`map`, `flatMap`, `filterMap`-ish things).

## Full investigation in `investigations/family3_map_function.md`

Includes collapsed call tree, instrumentation findings, architectural audit vs `docs/Generics.md`, and the origin-of-vimpl git-archaeology. Read it before restarting this quest — it documents everything learned in the 2026-04-17 session, including three real fixes already landed that are correct but incomplete.

## What's already landed (2026-04-17 session)

Three layered changes sit on the `vale-after-regions` branch. All non-regressing (full AfterRegions suite stays at **28 pass / 12 fail / 7 ignored** — the same 12 by name). Map function still fails, but further down the pipeline each time.

### Layer 1 — `FunctionCompilerSolvingLayer.scala:493` (the original Family 3 fix)

Removed the `vimpl()` gate and flipped `registerWithCompilerOutputs: false → true` to match every other `createPlaceholder` call in the typing pass. Now when the preliminary solve of the virtual dispatcher leaves an abstract generic unsolved (because it's not in the self-interface type), a fresh dispatcher-owned placeholder is created for it — just like sibling `evaluateGenericFunctionFromNonCall` does.

**Why this is the right direction:** `docs/Generics.md` §§ GTCII, CDFGI, FODAIR, AFCTD document the dispatcher-per-impl architecture as the designed endgame, not a template holdover. The `vimpl()` was introduced in commit `960e0eec` (Dec 13 2022, "Now using incremental placeholdering for functions, structs, and interfaces.") as a scaffold-for-later-work when sibling paths were migrated to the incremental placeholdering model. Before that commit (earliest `12594576` Sep 29 2022), this branch had working one-shot placeholder creation. The `vimpl()` was a stale TODO, not a disabled real invariant.

### Layer 2 — `optutils.vale` `getOr` signature rewrite

The stdlib `getOr<T>(opt &Some<T>, default T) T { return opt.value; }` was template-era code that only works when T is provably share at the concrete call site. Under generics compilation, abstract T defaults to OwnT (mutable), and `softLoad` on a `ReferenceMemberLookupTE` with OwnT + UseP produces `BorrowT` (`LocalHelper.scala:166`); the return-type coercion check at `TemplataCompiler.scala:982` then rejects `BorrowT → OwnT`.

Rewrote to `&T`-in / `&T`-out, matching the canonical `opt.vale` pattern for `get<T>(opt &Some<T>) &T { return &opt.value; }`. Share-T call sites (like `b.getOr<bool>(false)`) still work via AugmentSR auto-collapse. Only `mapFunc.vale` imports `optutils`, so no other tests exercise this.

### Layer 3 — `EdgeCompiler.scala` fresh-placeholder inclusion

When Layer 1 created a fresh placeholder for `R`, the Instantiator crashed at `Instantiator.scala:3159` `vassertSome` in `translateCoord` — the substitutions map built by `assemblePlaceholderMap` only knew the impl-mimicked `T`, not `R`. Root cause: the dispatcher's `OverrideDispatcherNameT.templateArgs` was built from `dispatcherPlaceholders` (= impl-mimicked only), and `assemblePlaceholderMap` at `Instantiator.scala:1052-1053` zips the placeholdered name's templateArgs with the instantiated name's templateArgs — if R isn't in templateArgs, it never becomes a substitutable placeholder.

Fix at `EdgeCompiler.scala:346+`: after `evaluateGenericVirtualDispatcherFunctionForPrototype` returns, extract any inferences that are fresh (non-impl-mimicked) placeholders and concat them to `dispatcherPlaceholders` before calling `makeFunctionName`. See the `freshDispatcherPlaceholders` block in the diff.

## Current failure point (where the next attempt picks up)

`Instantiator.scala:665` in `translateOverride`:

```scala
val implPlaceholder =
  vassertSome(
    // This implPlaceholderToDispatcherPlaceholder has a map of the impl runes to the dispatcher runes, like:
    // - ri$I -> dis$I
    // - ri$J -> dis$J
    implPlaceholderToDispatcherPlaceholder
        .find(_._2 == dispatcherPlaceholderTemplata))._1
```

This iterates every dispatcher templateArg expecting each to correspond to some impl placeholder. Our fresh `R` has no impl counterpart, so `find` returns `None` and `vassertSome` fires. The whole `translateOverride` path at lines 654-678 is architecturally built around the invariant: **every dispatcher placeholder mimics an impl placeholder**. `docs/Generics.md` confirms this (every worked example has abstract generics fully reachable from self-interface); the `map<T, R>` case where an abstract generic doesn't appear in self-interface is not covered in the design doc.

## Architectural diagnosis

This isn't one bug — it's an **unconsidered design case**. Every layer of the override-resolution pipeline (typing-pass dispatcher compile → typing-pass dispatcher name construction → instantiator substitution map → instantiator translateOverride) was built on the invariant "abstract generics ⊆ impl placeholders." Three layers extended the pipeline to accept fresh placeholders; the fourth (`translateOverride`) remains.

Cross-reference:

- **OMCNAGP (`docs/Generics.md:1144-1191`)** handles the *symmetric* case — impl generics that DON'T appear in self-interface (Milano's `ZZ`). These become `OverrideDispatcherCaseNameT.independentImplTemplateArgs`, not dispatcher templateArgs. Line 1361: *"(Note that there's no `dis$ZZ` in here, we don't include independent runes, see OMCNAGP.)"*
- **AFCTD (`docs/Generics.md:1329-1369`)** explicitly says *"It's up to the instantiator to substitute `send$T` and `dis$X` correctly so that the arguments and parameters line up"* — mixing abstract+dispatcher placeholders at instantiator time is the documented expectation. So adding the new case isn't against the grain of the design, just filling a gap the doc didn't anticipate.

## What the final fix looks like

Extend `translateOverride`'s `dispatcherPlaceholderIdToSuppliedTemplata` build (`Instantiator.scala:654-678`) to distinguish impl-mimicked from fresh placeholders by inspecting the placeholder's rune shape:

- **Impl-mimicked:** `KindPlaceholderTemplateNameT(index, DispatcherRuneFromImplS(implRune))` — keep existing logic (look up via `implPlaceholderToDispatcherPlaceholder`, then index into `implIdC.localName.templateArgs`).
- **Fresh (new):** `KindPlaceholderTemplateNameT(index, CodeRuneS(name))` — bare rune, no `DispatcherRuneFromImplS` wrapper. Look up concrete value at `abstractFuncPrototypeC.id.localName.templateArgs(index)` — the `index` field on a fresh placeholder is the abstract's `genericParameters` index (set in `FunctionCompilerSolvingLayer`'s flatMap).

The downstream substitution machinery (`caseSubstitutions` map at line 706-709, consumed by `translatePrototype` / `translateFunctionId` at 1345, 3561-3564) already supports arbitrary placeholder→value entries. The only missing piece is populating it correctly for the fresh case.

Rough shape:

```scala
val dispatcherPlaceholderIdToSuppliedTemplata =
  dispatcherIdT.localName.templateArgs
    .map(dispatcherPlaceholderTemplata => {
      val dispatcherPlaceholderId =
        TemplataCompiler.getPlaceholderTemplataId(dispatcherPlaceholderTemplata)
      val index = /* extract from KindPlaceholderTemplateNameT */
      val rune = /* extract from KindPlaceholderTemplateNameT */
      val templataC =
        rune match {
          case DispatcherRuneFromImplS(_) => {
            // existing logic
            val implPlaceholder =
              vassertSome(implPlaceholderToDispatcherPlaceholder.find(_._2 == dispatcherPlaceholderTemplata))._1
            val implIndex = /* extract */
            implIdC.localName.templateArgs(implIndex)
          }
          case _: CodeRuneS => {
            // fresh case — use abstract's concrete args
            abstractFuncPrototypeC.id.localName.templateArgs(index)
          }
        }
      dispatcherPlaceholderId -> templataC.asInstanceOf[ITemplataI[sI]]
    })
```

## Historical status

Regression. Lived in `IntegrationTestsA.scala` since 2020-06, green for ~2 years, parked in AfterRegions during the Sep-Dec 2022 refactor work that introduced the dispatcher architecture.

## Unblocks

This test only. Doesn't share infrastructure with Families 1 or 2, but the fresh-placeholder machinery landed here might show up in other corners of override resolution if new abstract methods with self-unreachable generics are written — worth sanity-checking against the stdlib after the fix lands.

## Scope estimate (revised)

Multi-day. The vimpl itself is one-line; the architecture extension (three layers of matching fixes) is several hundred lines spread across `FunctionCompilerSolvingLayer`, `EdgeCompiler`, and `Instantiator`, plus a design decision on whether fresh placeholders should also flow through `OverrideT` explicitly (rather than being re-detected by rune shape). Original quest.md estimate of "1–2 days" was based on the vimpl-only diagnosis and understated the scope by 2–3×.

## Files touched by the 2026-04-17 session (on branch, unreviewed)

- `Frontend/TypingPass/src/dev/vale/typing/function/FunctionCompilerSolvingLayer.scala` — Layer 1
- `Frontend/TypingPass/src/dev/vale/typing/EdgeCompiler.scala` — Layer 3
- `Frontend/Tests/test/main/resources/optutils/optutils.vale` — Layer 2
- `investigations/family3_map_function.md` — full investigation doc
- (This file — quest.md Family 3 section)

To resume: pull branch, read `investigations/family3_map_function.md` top-to-bottom, then apply the Layer 4 `translateOverride` extension above. Re-run `sbt 'testOnly dev.vale.AfterRegionsIntegrationTests -- -z "Map function"'` and then the full suite to verify Map function flips green without regressing the other 28.

---

# Family 4: Pre-refactor-disabled tests with design questions (3 tests)

## The shared context

These three tests were each passing in production for months or years (between 2021-04 and 2022-08) before being explicitly `vimpl()`/`vfail()`-d during the pre-refactor monomorphization-prep churn of Aug–Sep 2022. Each disablement was accompanied by a design question the author wanted to answer before re-enabling.

Fixing them is less about compiler work and more about resolving the design questions. Not mechanical fixes.

## 4.1 imm tuple access

**File:** `Frontend/IntegrationTests/test/dev/vale/AfterRegionsIntegrationTests.scala:72`

**Test shape:** Accesses fields of an immutable tuple.

**Current state:** Starts with `vfail() // these tuples are actually mutable`.

**Historical status:** Regression. Added 2021-04-15 in the regions refactor merge; passing for ~16 months. Disabled at `c1f24496` (2022-09-05) — the "Milano case" fix — with the accompanying comment indicating the test's tuples weren't actually immutable as the test name implied.

**What fixing requires:**
- Investigate the "Milano case" commit to understand what changed about tuple mutability
- Decide: should the test's tuple construction actually produce immutable tuples? If yes, make it happen. If no, reframe or delete the test.
- The BRRZ session's fix to `Test returning empty seq` touched `Tup0` and made it `imm` — there may be adjacent work worth looking at.

**Scope:** Short investigation (half a day) followed by either a narrow fix or a test rewrite.

## 4.2 Can turn a borrow coord into an owning coord

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:143`

**Test shape:** (blocked by initial `vimpl()`) — the name suggests a test where a value typed as `&X` is re-typed as `^X` (owning).

**Current state:** `vimpl()` at top, with author comment "not sure this test ever really tested what it was supposed to."

**Historical status:** Regression. Added 2021-11-20 in "Passes all tests in Templar except a region one!"; passing for ~9 months. Disabled at `ab3cf9e6` (2022-08-21) with the skeptical comment.

**What fixing requires:**
- Read the test body to see what operation it attempts. The author wasn't sure it tested its named intent.
- Decide: is "turning a borrow into an owning coord" a meaningful operation in the new regions-aware type system? What would that even mean — a move? A shallow copy? Region-crossing?
- Either implement the operation (if it's a real language feature) or rewrite the test (if it was testing something else).

**Scope:** Design discussion, then either a language-feature implementation or a test rewrite. If the former, multi-day.

## 4.3 Report when downcasting to interface

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsErrorTests.scala:147`

**Test shape:** (blocked by initial `vimpl()`) — an error-reporting test expecting a specific error when downcasting to an interface fails.

**Current state:** `vimpl() // can we solve this by putting an impl in the environment for that placeholder?`

**Historical status:** Regression. Added 2021-05-21 in the runtime-arrays series; passing for ~15 months as a real error test. Disabled at `ab3cf9e6` with the design question about placeholder impls.

**What fixing requires:**
- Answer the design question: should `impls` be resolvable through placeholders (i.e., can the compiler treat a generic `T` as implementing `Interface` when there's an `impl Interface for T` in scope)? This interacts with Family 1's impl-bound propagation work.
- If yes: wire the machinery and re-enable the test.
- If no: reframe the test's expectation or delete it.

**Scope:** Design discussion first. Likely becomes easy once Family 1's impl-bound work lands — the design question may naturally resolve in that context.

---

# Suggested order of attack

If you want the most progress per unit effort:

## Phase 1: Family 1 (impl-bound propagation)

**Target:** 6 tests — Method call on generic data, Impl rule, Reports error, Lambda is incompatible anonymous interface (first step), Test overload set, Tests overload set and concept function.

**Approach:** Implement CFWG per `docs/Generics.md:201`. Rework `functor1.vale`, add OverloadSet→Prot coercion in the solver, and wire `implements` bounds into `OverloadResolver`. Coordinate with existing BRRZ work.

**Estimate:** 3–7 days.

**Risk:** Wide blast radius. Run the full test suite (not just AfterRegions) after every non-trivial change. Bounds changes can silently affect every function call in the codebase.

**Side benefit:** May also unblock the ignored `Generic interface anonymous subclass` and `IRBFPTIPT` tests if the machinery generalizes.

## Phase 2: Family 2 (anonymous-param lambdas)

**Target:** 2 tests — Test one-anonymous-param lambda identifying runes, Test two instantiations of anonymous-param lambda.

**Approach:** Postparser fix for magic-param rune counting + instantiator fix for per-call-type lambda specialization. Reference: `PostParsingParametersTests.scala:120` has a verbatim pre-refactor working version of test 2.1; use it as ground truth for expected behavior.

**Estimate:** 2–3 days.

**Risk:** Contained. Lambda monomorphization is a self-contained subsystem.

## Phase 3: Family 3 (Map function)

**Target:** 1 test.

**Approach:** Replace the `vimpl()` at `FunctionCompilerSolvingLayer.scala:493` with placeholder creation for generic virtual dispatcher params. Model on how regular generic functions create `KindPlaceholderT` instances.

**Estimate:** 1–2 days.

**Risk:** Low. Single well-identified vimpl.

## Phase 4: Family 4 (design questions)

**Target:** 3 tests — imm tuple access, Can turn a borrow coord into an owning coord, Report when downcasting to interface.

**Approach:** Discuss design questions first (ideally with the language author). Some may naturally resolve after Phase 1. Others may become test-rewrite tasks rather than compiler-fix tasks.

**Estimate:** Variable; depends on design choices.

---

# Common pitfalls — things earlier sessions got wrong

1. **Handoff documents go stale.** The "Borrowing toArray" handoff claimed `AugmentSR.scala:950` was a regions bug. It's actually intentional language semantics. The real fix was a one-character test change. Re-run the test and read the actual current error before trusting prior diagnosis.

2. **Immutables only have ShareT.** Ownership is meaningless for immutables (they're refcounted). Writing `&E` in generic code is correct: produces `BorrowT` for mutable E, collapses to `ShareT` for immutable E. `AugmentSR` and ~13 sibling locations in the typing pass do this collapse by design. Don't "fix" them.

3. **Vale arrays use `arr[i]` not `arr.get(i)`.** Universal across stdlib. Tests writing `arr.get(i)` will fail resolution — the fix is `arr[i]`.

4. **Capability vs test-syntax matters.** A failing test using new bounds-style syntax (`where implements(...)`) isn't automatically aspirational. The CAPABILITY may have worked in templates. Check with: `git log --all -S "test name"` to find origin, then `git show <commit>^:path/to/pre-refactor-file.scala | grep -A N "similar pattern"` to see if the feature was tested pre-refactor.

5. **"Verified PASS" can mean typing-only.** `expectCompilerOutputs()` only runs the typing pass; `evalForKind()` runs the full pipeline. Tests can pass the former and fail the latter. Always use `evalForKind` if end-to-end correctness is the goal.

6. **Named blocker codes — don't re-investigate.** When a test sits in a named-blocker bucket (CFWG, IRBFPTIPT, MKRFA, BRRZ), recognize the code and work at that level. Don't re-derive what the blocker is. CFWG is `docs/Generics.md:201`; IRBFPTIPT smoking gun is `EdgeCompiler.scala:421-423`.

7. **Scalatest's `ignore(...)` is the right tool** for tests that document roadmap but shouldn't run. Two of the 7 ignored tests had `// This test does not pass yet, use #[ignore].` comments sitting there unapplied — the suggestion had been there all along.

---

# Cross-references

**For context on the test lifecycle:**
- `docs/historical/after-regions-fixing-tests-quest.md` — full historical record of what was tried for each test across all recovery sessions. Read this before starting work on any of the 12 — the "lessons learned" section is dense with gotchas from prior sessions.

**For the generics/regions architecture:**
- `docs/Generics.md` — the design doc. Sections CFWG (line 201), NBIFP (line 489), SFWPRL (line 353), STCMBDP (line 392), DRSINI (search for "DRSINI"), SIPWDR (line 340).
- `docs/arcana/BoundReturnResolution-BRRZ.md` — recently-shipped bound-return inference; related but distinct from bound-informed overload search.
- `docs/arcana/EachCallSiteIsItsOwnSolve-ECSIIOSZ.md` — every call site spins up its own fresh solver.
- `docs/refactor-thoughts/mkrfa-protocol-leak.md` — MKRFA contract details; Family 1 work may touch the sites listed there.

**For the regions feature specifically:**
- `docs/regions/Regions.md` — region semantics, mutability, purity.

**For stdlib patterns:**
- `Frontend/Tests/test/main/resources/array/each/each.vale` — the `each` function whose where-clause is at the heart of Family 1 tests 1.5 and 1.6.
- `Frontend/Builtins/src/dev/vale/resources/functor1.vale` — the hardcoded-"drop" hack that Family 1 needs to replace.
- `Frontend/Tests/test/main/resources/programs/genericvirtuals/mapFunc.vale` — the map-function source underlying Family 3.
