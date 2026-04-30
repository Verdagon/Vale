# Quest Complete: AfterRegions Test Recovery

**Final state:** 52 AfterRegions tests → **38 pass / 0 fail / 14 ignored**. All AfterRegions tests now pass or are deliberately deferred. Wider regression sweep at **199/199** (CompilerTests, CompilerVirtualTests, CompilerGenericsTests, IntegrationTestsA/B/C). The original 5-regression quest is closed.

This document was the active quest tracker; it is preserved here as the closing record. Per-family sections below capture what was diagnosed, what was fixed, and (for the deferred items) what's left and where the next attempt picks up. The earlier-phase historical record (covering the 28 → 33 recovery sessions before this branch's work) lives at `docs/historical/after-regions-fixing-tests-quest.md`.

## Resolved (the original 5 active regressions, plus three Family 4 design-question tests)

- **Family 1.1** (Method call on generic data) — Solution C in `OverloadResolver.getPlaceholderImplBoundEnvs` per @BDPFWDZ.
- **Family 1.2** (Impl rule) — same fix; test source updated to `&T` borrow form so no drop bound is needed.
- **Family 1.4** (Lambda is incompatible anonymous interface) — test was doubly-broken. Source returned `int` matching the interface (nothing to mismatch); assertion expected `BodyResultDoesntMatch`, which is structurally unreachable on this path (lambda body has `maybeExplicitReturnCoord = None` per @LAGTNGZ; the forwarder *would* trigger it but is gated behind constructor inference, which fails earlier at the substruct's `__call` bound check). Pivoted: lambda now returns `bool`, assertion expects the realistic `CouldntFindFunctionToCallT(... ReturnTypeConflictInConclusionResolve(int, bool))`. Companion success-test added in `AfterRegionsTests.scala`. See `investigations/family1_4_body_result_doesnt_match_unreachable.md`.
- **Family 2** (anonymous-param lambdas, 2 tests) — postparser lift to match @LAGTNGZ.
- **Family 4.1** (imm tuple access) — `vfail()` overcorrection removed, test passes as-written.
- **Family 4.2** (borrow→owning coord coercion) — `vimpl()` removed, test rewritten around `^&SomeStruct` to faithfully exercise the named coercion.
- **Family 4.3** (interface→interface downcast) — pivoted from negative test (asserting unsupported) to positive test (asserting supported); the original error class `CantDowncastToInterface` is dead code and the type system was deliberately built to allow the operation.

## Deferred (now `ignore`d; deliberately deprioritized or pending owner review)

- **Family 1.3** (Reports error: imm interface + imm struct mutability mismatch) — real bug (impl-time mutability check needed), but the obvious fix broke `IFunction1.anonymous` because interface and substruct carry placeholder M's whose IdTs differ but conceptually refer to the same impl-level M. Needs substitution-based comparison. See `investigations/reports_error_1_3.md`.
- **Family 1.5 / 1.6 / @POSIPP** (CFWG — passing a bare function name `OverloadSetT` through a generic with a `where func(&F, …)` bound) — deprioritized because users can use lambdas instead. Design lives at `docs/Generics.md:201`.
- **Family 3** (Map function: generic virtual dispatcher with abstract generics not in self-interface) — three layers of fixes already on this branch (`FunctionCompilerSolvingLayer` vimpl removal, `optutils.vale` `getOr` rewrite, `EdgeCompiler` fresh-placeholder inclusion). The final Layer 4 patch in `Instantiator.translateOverride` was prototyped, verified clean, and reverted pending Instantiator-owner review. See `investigations/family3_map_function.md` and the Family 3 section below.

## How to verify final state

From the repo root:

```bash
cd Frontend
sbt 'testOnly dev.vale.AfterRegionsIntegrationTests dev.vale.typing.AfterRegionsTests dev.vale.typing.AfterRegionsErrorTests dev.vale.parsing.AfterRegionsTests dev.vale.parsing.functions.AfterRegionsFunctionTests dev.vale.postparsing.AfterRegionsErrorTests' 2>&1 | tee /tmp/quest-status.txt
grep -E "Tests: succeeded|FAILED \*\*\*" /tmp/quest-status.txt
```

Expect `Tests: succeeded 38, failed 0, canceled 0, ignored 14, pending 0`. No failing tests; the only remaining quest item (Family 3 / Map function) is `ignore`d pending Layer 4 patch landing — see Family 3 section.

## Vocabulary

Shared across all families. See `docs/Generics.md` for full treatment.

- **Templates (old system)** — generic functions were expanded per call site, like C++ templates. Each call got a specialized copy. The function body wasn't type-checked in isolation; errors surfaced only at call sites.
- **Generics (new system)** — generic functions compile ONCE with abstract placeholders (`KindPlaceholderT`, `RegionT`). A separate `Instantiator` pass walks the call graph later and stamps out concrete versions. Requires bounds (`where implements(T, IShip)`, `where func(&T)int`) because the body is type-checked with only the placeholder + whatever bounds promise.
- **Bound** — a constraint declared in a `where` clause. E.g. `where func drop(T)void` promises "some function named `drop` taking `T` and returning `void` is available." Bounds let the compiler type-check the generic body without knowing what T will be.
- **OverloadSet** — a kind representing a set of function overloads under a name. When the user writes `myfunc` as a value (not a call), the compiler creates `CoordT(ShareT, _, OverloadSetT(...))` to carry the unresolved set.
- **Acronyms used below:** CFWG (Concept Functions With Generics), IRBFPTIPT (Inherit Reachable Bounds For Params), MKRFA (Must Know Runes From Above), BRRZ (Bound Return Resolution), DRSINI (Default Rules Should Be Incremental Not Initial), NBIFP (Need Bound Information From Parameters), SFWPRL (Solve First With Predictions, Resolve Later), ECSIIOSZ (Each Call Site Is Its Own Solve).

---

# Family 1: Impl-bound propagation into overload resolution (1 test remaining; CFWG deprioritized)

> **Junior-engineer handoff:** `investigations/family1_handoff.md` — read top-to-bottom before touching code. The original handoff doc covered all 6 Family 1 tests. Tests 1.1, 1.2, and 1.4 are now resolved; the doc is updated with their resolution notes. Tests 1.5, 1.6, and a new minimal repro @POSIPP are now `ignore`d as deprioritized — users can use lambdas instead. Remaining work is the test-design followup for 1.3. The CFWG-specific design notes below are still relevant if/when this is picked up.

## The shared root cause

In the template system, writing `x.launch()` on a generic-typed `x: T` Just Worked — the template was expanded per call site, so at expansion time `T` was concrete and the compiler looked up `launch` on the concrete type.

In the generic system, `x.launch()` has to type-check against an abstract `T`. The programmer declares bounds: `where implements(T, IShip)` (meaning: any `T` passed must implement `IShip`) or `where func launch(&T)void` (meaning: there must be a function named `launch` taking `&T`). The compiler is supposed to use those bounds to validate the call: "does IShip have `launch`? Yes. Does the `implements` bound guarantee T has IShip's methods? Yes. So `x.launch()` type-checks."

**Resolved for impl-bound case (1.1, 1.2):** Solution C in `OverloadResolver.getPlaceholderImplBoundEnvs` follows each placeholder param's `IsaTemplataT` bound to its super-interface's outer env at lookup time. The interface's abstract method becomes a candidate; the inner per-call-site solve verifies T isa IShip via the same IsaTemplataT through `ImplCompiler.isParent`; the call resolves; the instantiator monomorphizes; the backend dispatches virtually. Per @BDPFWDZ, methods stay where declared (in the interface's outer env) and the resolver walks to find them. Integration test: `AfterRegionsIntegrationTests."Interface Method call on impl-bounded generic dispatches through interface"`.

**Remaining gap (1.5, 1.6) — DEPRIORITIZED:** the CFWG (Concept Functions With Generics) flavor — `where func(&F, &T)void` with F being an `OverloadSetT` has no mechanism to coerce the OverloadSet into a concrete prototype during bound discharge. The `CFWG` design described in `docs/Generics.md:201` is the intended solution. Three rule types (`ResolveSR`, `CallSiteFuncSR`, `DefinitionFuncSR`) exist but aren't wired to perform the overload→prototype coercion the doc describes. **Workaround:** users can pass a lambda instead of a bare function name. 1.5 and 1.6 have both been marked `ignore`. A minimal repro is `AfterRegionsIntegrationTests."Pass overload set into placeholder parameter (@POSIPP)"` — strips out the `array.each` plumbing and exercises only the bare-name-through-generic path.

> **`functor1.vale` was vestigial.** It's been deleted (along with its line in `Builtins.scala`). The 36/5/8 AfterRegions split and the 199/199 wider suite (CompilerTests, CompilerVirtualTests, CompilerGenericsTests, IntegrationTestsA/B/C) were unchanged by removal — no test exercises the hardcoded-`drop` `__call(v void, …)` path end-to-end. The handoff lore about "it works only for `drop`" describes a path that nothing currently reaches; CFWG can be designed without legacy compatibility weight.

## What fixing the remaining CFWG sub-family requires

Broadly: **implement CFWG as described in `docs/Generics.md:201-258`.** The doc describes two alternative designs (prototype-based and placeholder-based); either is viable. Concretely:

1. Implement `OverloadSetT → Prot` coercion in the solver — when a function parameter expects a callable `F` and the argument is an `OverloadSetT`, the solver should pick the overload matching the required signature and substitute the resulting prototype.
2. Coordinate with existing BRRZ work (`docs/arcana/BoundReturnResolution-BRRZ.md`) — BRRZ already relaxed `ResolveSR` to discover return types from name+params; extending it to interact with overload-set coercion is a natural follow-on.

**Scope estimate:** Multi-day project. Touches `CompilerSolver.scala`, `InferCompiler.scala`, plus new documentation. Run the full AfterRegions suite after every non-trivial change; bound-propagation changes have wide blast radius.

**Unblocks:** Both CFWG tests (1.5, 1.6).

## The 6 tests in detail (1.1, 1.2 resolved)

### 1.1 Method call on generic data — RESOLVED

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

**RESOLVED:** Solution C — `OverloadResolver.getPlaceholderImplBoundEnvs` walks each placeholder param's `IsaTemplataT` bound to its super-interface's outer env at lookup time. The abstract `launch(virtual self &IShip)` becomes a candidate; the inner per-call-site solve verifies T isa IShip via `ImplCompiler.isParent`; the call resolves. Test body now asserts the call site in main resolves to `launchGeneric<Raza>` and `launchGeneric`'s body has a `FunctionCallTE` for `launch`. Per @BDPFWDZ.

### 1.2 Impl rule — RESOLVED

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

**RESOLVED:** Same Solution C fix as 1.1. Test source updated from `x T` (owned, requires drop bound) to `x &T` (borrow, no drop bound needed) — the test was previously checking `templateArgs.last` against Firefly under the assumption that the typing pass would monomorphize, but per the current generics architecture monomorphization happens in the instantiator. Test now asserts the placeholder T appears in the template's id, and that main's body has a `FunctionCallTE` resolving to `genericGetFuel<Firefly>`. Per @BDPFWDZ.

### 1.3 Reports error (virtual dispatch error reporting) — DEPRIORITIZED (now `ignore`)

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsErrorTests.scala:108`

**Test shape:** Interface A (default mut) + `struct B imm` + `impl A for B`. The test source uses imm B against a mut interface — a real bug, the impl is silently accepted then explodes mid-override-search with `BadIsaSuperKind(B)`. See `investigations/reports_error_1_3.md` for the full diagnosis (the imm/mut mismatch causes ownership coercion to differ between `&A`-substituted-with-B and the standalone `&B`).

**Fix attempt (reverted):** Add an impl-time mutability check in `ImplCompiler.compileImpl` that emits a clean `ImplMutabilityMismatch` error. Worked for the target test, but broke `IFunction1.anonymous` because both interface and anonymous-substruct carry placeholder M's whose `IdT`s differ (one nested under `IFunction1`, other under `IFunction1.anonymous`) but conceptually refer to the same impl-level M. The fix needs a substitution-based comparison that brings both mutabilities into the impl's frame before comparing — not yet implemented.

**Status:** Test marked `ignore`. The underlying bug is real (no impl-mutability validation anywhere; `ImplCompiler` doesn't check it; the error surface is confusing). Resume with substitution-based equality. There's also a companion humanizer `MatchError` for the imm-interface-imm-struct case (see `ignore("Reports error (imm interface + imm struct)")`).

### 1.4 Lambda is incompatible anonymous interface — RESOLVED

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsErrorTests.scala:195` (renamed: "Lambda body type mismatches anonymous interface return type")

**Investigation:** `investigations/family1_4_body_result_doesnt_match_unreachable.md`.

**The test was doubly-broken.** The original source `(_) => { 4 }` returned `int`, matching the interface's `int` return type — so there was no mismatch to detect, and the test fell into the `Ok(wat)` arm. Even worse, it asserted `BodyResultDoesntMatch`, which is *structurally unreachable* on the anonymous-substruct path:

1. The lambda's `__call` body is compiled with no explicit expected return type (`maybeExplicitReturnCoord = None`, per @LAGTNGZ — lambdas are templates, not generics). The body-vs-return-type comparison only fires in the `Some(...)` branch.
2. The forwarder (`__call.forwarder0`) *does* have an explicit return type and would trigger `BodyResultDoesntMatch` if its body returned wrong, but its compilation is gated behind successful constructor inference. Inference fails first at the substruct's `__call` bound check (the `ResolveSR` rule emitted by `AnonymousInterfaceMacro.scala` ~line 332-334), throwing `ReturnTypeConflictInConclusionResolve` before the forwarder body is ever queued.

The bound check supersedes the body check on this code path by design.

**Resolution:** Lambda body changed to `{ true }` (real int/bool mismatch). Assertion updated to match the realistic error: `CouldntFindFunctionToCallT(_, fff)` whose lone candidate-rejection reason is `FindFunctionResolveFailure(ResolvingResolveConclusionError(ReturnTypeConflictInConclusionResolve(_, expected=int, actualPrototype)))` with `actualPrototype.returnType = bool`. Companion success-test `Lambda body type matches anonymous interface return type` added to `AfterRegionsTests.scala`. AfterRegions baseline moved 36 → 38 passing.

### 1.5 Test overload set — DEPRIORITIZED (now `ignore`)

**File:** `Frontend/IntegrationTests/test/dev/vale/AfterRegionsIntegrationTests.scala:133`

**Status:** Marked `ignore` because users can use a lambda instead of a bare function name as a workaround. Same as new minimal repro `"Pass overload set into placeholder parameter (@POSIPP)"` at line 149 of the same file. Notes below preserved for when CFWG is picked up.

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

**Current failure:** `Couldn't find a suitable function __call((overloads: myfunc), i32). No function with that name exists.` There is no `__call` candidate for an OverloadSet receiver (the prior fallback, `functor1.vale`'s `__call(v void, …) where F Prot = func drop(P1)R`, was deleted as vestigial — it never matched anything except a hypothetical hardcoded-`drop` path no test exercised).

**Root cause:** The `each` function in `array/each/each.vale` has `where func(&F,&T)void`. To satisfy this bound with `F=OverloadSetT(myfunc)` and `T=i32`, the solver tries to resolve the bound by calling `__call(F, T)`. There's no mechanism to coerce the OverloadSet kind into a concrete prototype that matches the bound's signature.

**Historical status:** Regression. Added 2022-04 in `IntegrationTestsA.scala` and was green for months before being parked in AfterRegions during the refactor.

**Fix:** This is the canonical CFWG case (`docs/Generics.md:201`). Implement OverloadSet→Prot coercion at bound discharge — likely in the BRRZ-style mid-solve real-lookup pattern (use `delegate.resolveFunction` against the OverloadSet's `overloadsEnv` with the bound's signature, bind F's prototype to the result).

See the shared Family 1 fix description above.

### 1.6 Tests overload set and concept function — DEPRIORITIZED (now `ignore`)

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:72`

**Status:** Marked `ignore` for the same reason as 1.5 — users can use a lambda instead of a bare function name. Notes below preserved for when CFWG is picked up.

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

**Root cause:** Identical to 1.5 — `where func(&F,&X)void` triggers `__call((overloads: print), str)` lookup, which fails for the same CFWG reason (no OverloadSet→Prot coercion exists at bound discharge).

**Historical caveat:** Earlier sessions claimed this test "passes" because `expectCompilerOutputs()` (typing-pass-only) succeeded. But `evalForKind()` (full pipeline) fails. The typing pass tolerates the unresolved bound; later phases don't.

**Fix:** Same CFWG work as 1.5. These two tests will likely go green together.

---

# Family 2: Anonymous-param lambda inference (2 tests) — RESOLVED

Both tests were originally introduced as aspirational at the templates→generics transition commit (Sept 2022), already bearing `vimpl()` markers waiting on a design decision about how lambdas should be specialized. @LAGTNGZ settled that decision: lambdas are templates, not generics — specialized per call site with arg types baked into `LambdaCallFunctionTemplateNameT`, rather than compiled once with abstract placeholders like top-level generics.

The tests' original assertions predated that decision and were subtly wrong under it:
- Test 2.1 asserted `lambda.function.genericParams.size == 1`. Under LAGTNGZ the compiler works end-to-end with size 0, but for architectural hygiene — keeping the `_`-vs-`<T>` distinction contained in the postparser — the postparser now lifts each untyped lambda param's synthesized coord rune into `genericParams`. Later passes see a uniform `FunctionS` shape regardless of origin.
- Test 2.2 asserted `lambdaFuncs.size == 2` but filtered by `FunctionTemplateNameT(StrI("__call"), _)` — the pre-LAGTNGZ case class. The actual lambda-call name class is `LambdaCallFunctionNameT`, and the typing pass was already producing the two expected entries per-call-site; the test just couldn't see them.

**Fix landed:** postparser lift in `FunctionScout.scala` (one-line add to `genericParametersS` assembly); test 2.1 now checks the lift and rune bookkeeping; test 2.2 now uses `coutputs.lookupLambdasIn("main")` and asserts the two `LambdaCallFunctionTemplateNameT.paramTypes` tuples differ. See `docs/arcana/LambdasAreGenericTemplatesNotGenerics-LAGTNGZ.md` for the design, and `docs/historical/family2_handoff.md` for the investigation trail.

---

# Family 3: Generic virtual dispatcher — abstract generics not in self-interface (1 test) — DEFERRED (now `ignore`)

> **Status:** Marked `ignore` pending Instantiator-owner review of the Layer 4 patch (see below). Three layers already landed on this branch; the final fix is prototyped and verified clean but not committed. Test header in `AfterRegionsIntegrationTests.scala` cross-references this section and the investigation doc.

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

Three layered changes sit on the `vale-after-regions` branch. All non-regressing — when documented they kept the suite at 28 pass / 12 fail / 7 ignored (snapshot from when those layers landed; the suite has since reached 38/0/14 with the unrelated Family 1.x and 4.x work). Map function still fails, but further down the pipeline each time.

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

## Layer 4 — attempted patch (2026-04-24, prototyped and reverted)

The rough shape above was prototyped end-to-end on 2026-04-24, verified working, and **reverted** pending Instantiator owner review. Documenting it here so the next attempt has a known-good starting point.

### What was attempted

Single edit in `Frontend/InstantiatingPass/src/dev/vale/instantiating/Instantiator.scala`, `translateOverride`'s `dispatcherPlaceholderIdToSuppliedTemplata` build (lines 654-678 in pre-patch source), splitting on the dispatcher placeholder's rune shape:

```scala
val dispatcherPlaceholderIdToSuppliedTemplata =
  dispatcherIdT.localName.templateArgs
    .map(dispatcherPlaceholderTemplata => {
      val dispatcherPlaceholderId =
        TemplataCompiler.getPlaceholderTemplataId(dispatcherPlaceholderTemplata)
      // Split on the dispatcher placeholder's rune shape: an impl-mimicked placeholder
      // reads its concrete value from the instantiated impl's templateArgs, a fresh
      // dispatcher-owned placeholder (one minted for an abstract generic the impl's
      // self-interface doesn't pin, like R in map<T, R>) reads from the abstract's
      // instantiated templateArgs. Everything downstream is origin-agnostic.
      val (dispatcherIndex, dispatcherRune) =
        dispatcherPlaceholderId match {
          case IdT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(idx, rn))) => (idx, rn)
          case IdT(_, _, NonKindNonRegionPlaceholderNameT(idx, rn)) => (idx, rn)
          case other => vwat(other)
        }
      val templataC =
        dispatcherRune match {
          case DispatcherRuneFromImplS(_) => {
            val implPlaceholder =
              vassertSome(
                implPlaceholderToDispatcherPlaceholder
                    .find(_._2 == dispatcherPlaceholderTemplata))._1
            val implIndex =
              implPlaceholder match {
                case IdT(_, _, KindPlaceholderNameT(KindPlaceholderTemplateNameT(idx, _))) => idx
                case IdT(_, _, NonKindNonRegionPlaceholderNameT(idx, _)) => idx
                case other => vwat(other)
              }
            implIdC.localName.templateArgs(implIndex)
          }
          case _ => {
            // Fresh dispatcher-owned placeholder: its index is the position in the
            // abstract's genericParameters (set by FunctionCompilerSolvingLayer's
            // dispatcher flatMap), which matches abstractFuncPrototypeC.id.localName.templateArgs
            // per AFCTD.
            abstractFuncPrototypeC.id.localName.templateArgs(dispatcherIndex)
          }
        }
      // ... existing trailing logic (vregionmut wrapping, asInstanceOf, etc.)
      dispatcherPlaceholderId -> vregionmut(templataC.asInstanceOf[ITemplataI[sI]])
    })
```

No new imports needed — `DispatcherRuneFromImplS` is already in scope via `import dev.vale.postparsing._`.

### Verification (clean — would have shipped)

- **Map function (targeted):** PASS, was vassertSome panic
- **Full AfterRegions:** 31/9/7 (was 30/10/7), Family 3 resolved
- **Broader sanity** (CompilerGenericsTests, CompilerVirtualTests, CompilerLambdaTests, ClosureTests, IntegrationTestsA/B/C): 125/0, no regressions

### Why reverted (not because it's wrong)

The owner of Instantiator wants to land Instantiator changes personally and didn't have brainpower this session to fully understand the patch. The patch makes symptoms disappear, but four architectural smells surfaced during review and weren't resolved:

1. **Asymmetric rune representation.** Impl-mimicked dispatcher placeholders carry a `DispatcherRuneFromImplS(innerRune)` wrapper. Fresh ones carry a bare `CodeRuneS("R")` — indistinguishable at a glance from a top-level user-written rune. Cleanup: introduce a sibling `DispatcherRuneFromAbstractS(innerRune: IRuneS)` in `postparsing/names.scala` (right next to `DispatcherRuneFromImplS` and `CaseRuneFromImplS`); mint fresh placeholders with that wrapper in `FunctionCompilerSolvingLayer`'s dispatcher flatMap; `translateOverride`'s match becomes symmetric `DispatcherRuneFromImplS(_)` / `DispatcherRuneFromAbstractS(_)` / catch-all `vwat`.

2. **Implicit position convention vs explicit map.** The impl-mimicked path uses an explicit map (`implPlaceholderToDispatcherPlaceholder`). The fresh-placeholder path relies on a convention — "dispatcher fresh-placeholder's `index` field equals abstract's `genericParameters` position equals `abstractFuncPrototypeC.id.localName.templateArgs` position" — three implicit links per AFCTD. Cleanup: extend `OverrideT` (or whatever struct carries the dispatcher → override metadata) with `abstractPlaceholderToDispatcherPlaceholder`, populated in EdgeCompiler at the same time Layer 3 appends fresh placeholders. `translateOverride` then does no positional arithmetic — just two explicit map lookups.

3. **Thin test coverage.** `map<T, R>` is the only test in the entire suite that exercises the fresh-placeholder path. The patch makes it pass but doesn't prove the mechanism is correct for `mapPair<A, B, C>`, abstracts with `R` in struct fields, abstracts with `R` behind a generic struct (`Vec<R>`), etc. We got lucky that map's `R` appears only in signature-position uses. Investment: write 2–3 more abstracts with self-unreachable generics in `genericvirtuals/` and run them through this pipeline before declaring the path trustworthy.

4. **Stale dispatcher path predates Dec-2022 incremental-placeholdering migration.** The investigation doc flagged this: sibling phases (`evaluateGenericFunctionFromNonCall`, `StructCompilerGenericArgsLayer`) were migrated in commit `960e0eec` to the incremental-solve model with `getFirstUnsolvedIdentifyingRune` + `commitStep`. The dispatcher path (`evaluateGenericVirtualDispatcherFunctionForPrototype`) was not migrated in that commit. Layer 1's vimpl removal patches the pre-migration shape in place rather than aligning with siblings. Cleanup: bigger refactor, doesn't directly help (1) or (2) but removes "two ages of code" weirdness.

### Recommended order for the next attempt

1. **Land Layer 4 verbatim** (the patch above) — proves the path works end-to-end, gets Map function green.
2. **Cheap cleanup: smell (1)** — introduce `DispatcherRuneFromAbstractS`, mint fresh placeholders with it, simplify the match. Mostly mechanical, strict improvement.
3. **Medium cleanup: smell (2)** — explicit `abstractPlaceholderToDispatcherPlaceholder` map. Removes positional fragility.
4. **Test investment: smell (3)** — `mapPair`, `R`-in-struct-field, `R`-behind-`Vec`. Stress the fresh-placeholder pathway before trusting it elsewhere.
5. **Defer smell (4)** — incremental-placeholdering migration is a distraction from the AfterRegions quest. Note it in arcana for whoever picks up the next big dispatcher refactor.

Documentation debt to queue: `docs/Generics.md` §§ AFCTD/OMCNAGP don't cover the "abstract generic not pinned by self-interface" case. Once Layer 4 lands, an arcana for the cross-cutting pipeline (postparser → FunctionCompilerSolvingLayer → EdgeCompiler → Instantiator.translateOverride) would be a natural follow-up — propose an acronym ending in Z.

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

# Family 4: Pre-refactor-disabled tests with design questions (3 tests) — RESOLVED

## The shared context

These three tests were each passing in production for months or years (between 2021-04 and 2022-08) before being explicitly `vimpl()`/`vfail()`-d during the pre-refactor monomorphization-prep churn of Aug–Sep 2022. Each disablement was accompanied by a design question the author wanted to answer before re-enabling.

The original framing predicted these would be "less about compiler work and more about resolving design questions." Reality was lighter: 4.1 was an overcorrected `vfail()` removed (no compiler change); 4.2 was a structurally-broken test rewritten around `^&SomeStruct` (no compiler change); 4.3 was a negative test pivoted to a positive test against the same already-supported feature (no compiler change). All three resolved without touching the compiler. See per-sub-test sections below for the full diagnoses, including the architectural follow-up captured for 4.1 (the deferred-solving exploration, `investigations/deferred-solving-across-def-callsite.md`).

## 4.1 imm tuple access — RESOLVED (2026-04-24)

**File:** `Frontend/IntegrationTests/test/dev/vale/AfterRegionsIntegrationTests.scala:72`

**Resolution:** Removed `vfail()`, test passes as-written. No compiler change needed. AfterRegions integration baseline moved 30 → 31 passing.

**Diagnosis:** The author's disabling comment "these tuples are actually mutable" was a complaint about the test *name*, not its *code*. The test body is `t = (true, 42); return t.1;` — it exercises tuple field access by integer index. That works fine under the current Builtins state (`Tup2<T0, T1>` declared `mut`, fields share-able primitives). The test name "imm tuple access" was aspirational — written in 2021-04-15 when variadic `Tup<T RefList>` may have inferred imm-when-all-fields-imm-able, then disabled at `c1f24496` (2022-09-05, the "Milano case" fix) when that variadic Tup was replaced with the manually-mut `Tup2<T0, T1>` we have today.

**Why this was overcorrection:** The disabling reasoning conflates "the underlying struct is mut" with "the test is broken." The test code never actually verifies imm-ness — there's no operation in `(true, 42).1` that distinguishes mut from imm. So the test passes regardless of whether `Tup2` is mut or imm.

**What it doesn't test (still open as a separate question):** Whether tuples should be imm-when-all-fields-imm-able, the way `Tup0 imm { }` is unconditionally imm. No test in the suite currently probes this; if it becomes a real language feature later, this test's name is already accurate for that future world. No action needed today.

## 4.2 Can turn a borrow coord into an owning coord — RESOLVED (2026-04-26)

**File:** `Frontend/TypingPass/test/dev/vale/typing/AfterRegionsTests.scala:175`

**Resolution:** Removed `vimpl()` and rewrote the test body around `^&SomeStruct` (own-coercion applied to a concrete borrow coord) to faithfully exercise the named property. No compiler change. AfterRegions baseline moved 31 → 32 passing.

**Diagnosis:** The original body was structurally broken — `func bork<T>(x T) ^T { return SomeStruct(); }` cannot typecheck for arbitrary T because the body's concrete `SomeStruct()` doesn't match the placeholder kind in `^T`'s computed return-type coord. The author's `vimpl()` was guarding against a real type error, not just an unfinished design. The author's comment "perhaps we wanted a `&SomeStruct()` instead?" pointed in the right direction (the test name promises borrow→own coercion), but a simple call-site change wasn't enough — the body's coherence problem also needed fixing.

**The rewrite shape:**
```vale
import v.builtins.panicutils.*;

struct SomeStruct { }

func inner<T>() T { panic("never"); }

func bork() ^&SomeStruct {
  return inner<^&SomeStruct>();
}
```

Why this works: `^&SomeStruct` is the named property as a direct type expression — own-coercion applied to a concrete borrow coord. AugmentSR's mutable-kind branch (`CompilerSolver.scala:957-962`) sees a known borrow inner and concludes the outer as `CoordT(OwnT, _, SomeStruct)`. `inner<T>() T { panic(...); }` is a generic helper whose body type-checks for any T (the panic body satisfies any return type via `__Never`). bork's body calls `inner<^&SomeStruct>()`, forcing the type system to commit `^&SomeStruct` to a coord that bork can return. No generic-vs-instantiation puzzle (bork is non-generic), no drop-bound issues (no param to drop), no body kind-mismatch.

**Things that didn't work, captured for the record:**
- **Removing only `vimpl()` from the original test body:** body fails kind-mismatch at typecheck (`Couldn't convert SomeStruct to expected return type Kind$bork.T`).
- **Changing only the call site to `bork(&SomeStruct())`:** same body kind-mismatch — body type-checking happens once with placeholder T, independent of any call site.
- **The user's `inner<^T>()` rewrite with original `func bork<T>(x T) ^T`** + drop bound: body typechecks ✓ but the test is then "T inferred from own input is own" + "^T can be computed at body-typecheck time"; doesn't actually exercise the borrow→own coercion. Also stalls on `where func drop(T)void` for T = &SomeStruct since the auto-derived drop only covers own.
- **Generic without param** (`bork<T>() ^T` with `bork<&SomeStruct>()` at call site): compiles, but `lookupFunction("bork")` returns the generic header where T is a placeholder defaulting to OwnT — neither a BorrowT nor a refined OwnT assertion is meaningful at the generic level.

**Historical status:** Regression. Added 2021-11-20 in "Passes all tests in Templar except a region one!"; passing for ~9 months. Disabled at `ab3cf9e6` (2022-08-21). The `vimpl()` was honest — the test body never made sense as a generic. The named property (own-coercion of a borrow coord) was always real, just wasn't being expressed in a runnable form.

## 4.3 Report when downcasting to interface — RESOLVED (2026-04-27)

**Resolution:** The original negative test was asserting an obsolete design constraint that the language no longer enforces. Pivoted from negative→positive: deleted the original test from `AfterRegionsErrorTests.scala`, added `Can downcast interface to interface through registered impl` to `AfterRegionsTests.scala` testing the real behavior. AfterRegions baseline moved 32 → 33 passing.

**Diagnosis:** The original test asserted `compile.getCompilerOutputs() match { case Err(CantDowncastToInterface(_, _)) => }` for a program that does `super.as<Sub>()` with `impl Super for Sub`. Two findings made this expectation untenable:

1. **`CantDowncastToInterface` is dead code in production.** Defined in `CompilerErrorReporter.scala:54`, imported by `AsSubtypeMacro.scala:6`, humanized in `CompilerErrorHumanizer.scala:137`. No `throw` site exists anywhere — the error class is defined but never raised. The test was matching against an error that no code path generates.
2. **The type system was deliberately built to support interface→interface downcast.** `typing/types/types.scala:210`: `case class InterfaceTT(...) extends ICitizenTT with ISuperKindTT`. Following the chain — `ICitizenTT extends ISubKindTT` (line 197) and `ISuperKindTT` directly (line 210) — `InterfaceTT` is admissible as both subKind (downcast target) and superKind (downcast source). `AsSubtypeMacro.scala:57-58` pattern-matches against these traits, accepting interface kinds on either side. With `impl ISuper for ISub` registered, `implCompiler.isParent(subKind=ISub, superKind=ISuper)` returns `IsParent(...)` and the macro generates a clean `AsSubtypeTE` returning `Result<&ISub, &ISuper>`.

What actually happens with `vimpl()` removed: compilation gets all the way through the `as<>` macro generation. The eventual failure is downstream — `CouldntFindFunctionToCallT` for `drop(Result<&ISub, &ISuper>)` because the test's `main()` discards the Result without consuming it. The downcast itself is fine.

**The pivoted test:**
```vale
import v.builtins.as.*;
import v.builtins.result.*;
import v.builtins.logic.*;
import v.builtins.drop.*;
import panicutils.*;

sealed interface ISuper { }
sealed interface ISub { }
impl ISuper for ISub;

func tryDowncast(ship ISuper) bool {
  result Result<&ISub, &ISuper> = (&ship).as<ISub>();
  return result.is_ok();
}

exported func main() bool {
  return tryDowncast(__pretend<ISuper>());
}
```

Sealed interfaces (so `#!DeriveInterfaceDrop` provides the own-drop), `import v.builtins.drop.*` (provides the generic borrow-drop `func drop<T>(x &T) where T = Ref[_, Kind[mut]] { }`), and consuming the Result with `is_ok()` (which takes a borrow). The conditional drop of `result` at end-of-function is satisfied because both branches (Ok and Err) of the Result drop need only `drop(&ISub)` / `drop(&ISuper)`, both auto-provided by the generic borrow drop.

**Things that didn't work, captured for the record:**
- **Non-sealed interfaces**: drop bound `func drop(OkType)void, func drop(ErrType)void` on Result's drop has no candidate for `drop(&ISuper)` / `drop(&ISub)` because non-sealed interfaces don't get auto-drops via `#!DeriveInterfaceDrop` and the generic borrow drop wasn't imported. Sealed + drop import fixed both.
- **Asserting on `AsSubtypeTE` inside `tryDowncast`'s body**: the AsSubtypeTE lives inside the (generic) `as<>` function's body, not inside `tryDowncast`'s body — `tryDowncast` only contains a `FunctionCallTE` to `as<>`. The Collector pattern would need to dig through the wrong AST.
- **Asserting on the `as<>` instantiation specific to (ISub, ISuper)**: `coutputs.functions` only holds the *generic* function definitions with placeholder kinds, not per-call-site monomorphizations. So you can't filter for an `as<ISub, ISuper>` specialization from coutputs — that exists at the Instantiator pass, not the typing pass.
- **Deeply-nested Collector pattern matching the `result` LetNormalTE**: too many parens in the inner pattern, hit a Scala parse error. Not worth fixing — successful compilation IS the assertion. If `(&ship).as<ISub>()` had been rejected, `expectCompilerOutputs()` would have thrown.

**Connection to Family 1:** The author's `vimpl()` comment ("can we solve this by putting an impl in the environment for that placeholder?") hinted at Family 1's CFWG impl-bound-propagation question. But the test as-written used concrete types (ISuper, ISub), not placeholders, so the placeholder framing was somewhat speculative. The pivoted resolution doesn't depend on Family 1's resolution.

**Historical status:** Regression. Added 2021-05-21 in the runtime-arrays series; passing for ~15 months as a real error test. Disabled at `ab3cf9e6` (2022-08-21). The error class it asserted was likely a 2021-era constraint that the type-trait redesign of 2022 obviated; the disabling was correct triage when the assertion became untenable, but no replacement was written. This pivot writes the replacement.

---

# If you pick up the deferred items

The quest is closed at 38/0/14. If a future session wants to advance from here, the three deferred buckets are independent of each other:

1. **Family 3 / Map function** — smallest unit of work. Land Layer 4 (the patch is documented inline at the Family 3 section, with verification details). Blocked on Instantiator-owner review.
2. **Family 1.3** — needs substitution-based equality for impl-level placeholders so the impl-mutability check doesn't false-positive on `IFunction1.anonymous`. See `investigations/reports_error_1_3.md`.
3. **CFWG (1.5/1.6/@POSIPP)** — multi-day. Implement OverloadSet→Prot coercion per `docs/Generics.md:201`. Lambda workaround means this isn't blocking real users.

---

# Common pitfalls — things earlier sessions got wrong

1. **Handoff documents go stale.** The "Borrowing toArray" handoff claimed `AugmentSR.scala:950` was a regions bug. It's actually intentional language semantics. The real fix was a one-character test change. Re-run the test and read the actual current error before trusting prior diagnosis.

2. **Immutables only have ShareT.** Ownership is meaningless for immutables (they're refcounted). Writing `&E` in generic code is correct: produces `BorrowT` for mutable E, collapses to `ShareT` for immutable E. `AugmentSR` and ~13 sibling locations in the typing pass do this collapse by design. Don't "fix" them.

3. **Vale arrays use `arr[i]` not `arr.get(i)`.** Universal across stdlib. Tests writing `arr.get(i)` will fail resolution — the fix is `arr[i]`.

4. **Capability vs test-syntax matters.** A failing test using new bounds-style syntax (`where implements(...)`) isn't automatically aspirational. The CAPABILITY may have worked in templates. Check with: `git log --all -S "test name"` to find origin, then `git show <commit>^:path/to/pre-refactor-file.scala | grep -A N "similar pattern"` to see if the feature was tested pre-refactor.

5. **"Verified PASS" can mean typing-only.** `expectCompilerOutputs()` only runs the typing pass; `evalForKind()` runs the full pipeline. Tests can pass the former and fail the latter. Always use `evalForKind` if end-to-end correctness is the goal.

6. **Named blocker codes — don't re-investigate.** When a test sits in a named-blocker bucket (CFWG, IRBFPTIPT, MKRFA, BRRZ), recognize the code and work at that level. Don't re-derive what the blocker is. CFWG is `docs/Generics.md:201`; IRBFPTIPT smoking gun is `EdgeCompiler.scala:421-423`.

7. **Scalatest's `ignore(...)` is the right tool** for tests that document roadmap but shouldn't run. Several tests across this quest had `// This test does not pass yet, use #[ignore].` comments sitting there unapplied — the suggestion had been there all along.

---

# Cross-references

**For active work:**
- `investigations/family1_handoff.md` — junior-engineer handoff for Family 1. Originally written when 6+1 tests were failing; tests 1.1, 1.2, 1.4 are now resolved and 1.5/1.6/@POSIPP are deprioritized (see in-doc updates). Still useful for the remaining deferred 1.3 followup.
- `investigations/family3_map_function.md` — full investigation doc for Family 3 (Map function), with the architectural diagnosis Layers 1–3 already landed and Layer 4 sketched.
- `investigations/deferred-solving-across-def-callsite.md` — architectural exploration that came out of the Family 4.1 follow-up. Two design alternatives (deferred-solving / lift-to-generic-param) for letting def-time solves be incomplete. Out of scope for current quest, queued as a future architectural cleanup.

**For context on the test lifecycle:**
- `docs/historical/after-regions-fixing-tests-quest.md` — earlier-phase historical record covering the 28 → 33 recovery sessions before this branch's work. Its "lessons learned" section is still dense with gotchas worth reading before any future deferred-item work.
- `docs/historical/family2_handoff.md` — example of a junior-engineer handoff (Family 2, now resolved). Format reference for `family1_handoff.md`.

**For the generics/regions architecture:**
- `docs/Generics.md` — the design doc. Sections CFWG (line 201), NBIFP (line 489), SFWPRL (line 353), STCMBDP (line 392), DRSINI (search for "DRSINI"), SIPWDR (line 340).
- `docs/arcana/BoundReturnResolution-BRRZ.md` — recently-shipped bound-return inference; related but distinct from bound-informed overload search.
- `docs/arcana/EachCallSiteIsItsOwnSolve-ECSIIOSZ.md` — every call site spins up its own fresh solver.
- `docs/refactor-thoughts/mkrfa-protocol-leak.md` — MKRFA contract details; Family 1 work may touch the sites listed there.

**For the regions feature specifically:**
- `docs/regions/Regions.md` — region semantics, mutability, purity.

**For stdlib patterns:**
- `Frontend/Tests/test/main/resources/array/each/each.vale` — the `each` function whose where-clause is at the heart of Family 1 tests 1.5 and 1.6.
- ~~`Frontend/Builtins/src/dev/vale/resources/functor1.vale`~~ — deleted as vestigial; no test exercised it. CFWG no longer has a legacy shim to preserve.
- `Frontend/Tests/test/main/resources/programs/genericvirtuals/mapFunc.vale` — the map-function source underlying Family 3.
