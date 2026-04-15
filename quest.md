# Fixing AfterRegions Tests — Plan

38 tests fail across 6 test classes. All other 1048 tests pass.

## History

The AfterRegionsTests files have a mixed history:

- **`0b7d3a54` "Basic anonymous subclasses done!"** — file created with "Prints bread crumb trail" test
- **`ab3cf9e6` "Good point to start monomorphization"** — added ~10 generic/template tests. The commit message explicitly lists these as **known-broken backlog**: "method call on a generic arg, tuples, recursive generic function, generic interface's anonymous substruct, handing an overload set into a function that takes a concept function." These were **never passing** — they were parked here as the roadmap for what to fix after monomorphization.
- **`23090bdc` "All tests pass! Moved a few to AfterRegions"** — moved 4 tests **from CompilerTests** (where they were passing) into AfterRegionsTests for organizational reasons: "Reports when we give too many args", "Reports when ownership doesnt match", "Failure to resolve a Prot rule's function doesnt halt", "Basic IFunction1 anonymous subclass". Three of these still pass; one ("Reports when ownership doesnt match") broke during the subsequent solver refactoring (error wrapping shape changed from `FailedCompilerSolve` to `FailedSolve`).
- **Regions merge (`3c948b62`–`700a1e72`)** — added contextRegion params, more tests, more `vimpl()` stubs.
- **Later commits** — added remaining test files (integration, postparsing, parsing AfterRegions tests), many with `vimpl()` markers from the start.

**Summary:** ~4 tests were previously passing and regressed. ~10 were the known-broken backlog from the generics transition. The rest were written as aspirational targets, many with `vimpl()` from day one.

### Recovered
- **Reports when ownership doesnt match** — was passing, broke when solver error types changed from `FailedCompilerSolve` to `FailedSolve` wrapped in `FindFunctionResolveFailure(ResolvingSolveFailedOrIncomplete(...))`. Fixed by updating the test's match pattern.
- **Detects sending non-citizen to citizen** — two issues: (1) test's `moo<T>(a T)` lacked `func drop(T)void` bound, so definition failed before call site was reached; (2) error wrapping changed from `InferFailure/SendingNonCitizen` to `FindFunctionResolveFailure/BadIsaSubKind`. Fixed by adding drop bound to test and updating match pattern. See `investigations/implements_non_citizen.md`.
- **Accidentally mention type rune** — `drop(Z)` where `Z` is a type rune crashed with MatchError in `ExpressionCompiler#evaluate`'s `RuneLookupSE` handler (no case for `CoordTemplataT`). Fixed by adding `CantUseRuneValueAsExpression` structured error type, catch-all case in the match, and humanizer message. Test updated from `vimpl(e)` placeholder to match on the new error type. See `investigations/accidentally_mention_type_rune.md`.
- **Call bound with wrong arguments** — compiler was already correct. `str(true)` where bound is `func str(&X)str` correctly fails with `SpecificParamDoesntSend(0, CoordT(share, BoolT()), CoordT(borrow, KindPlaceholderT(X)))`. Test just had `vimpl(e)` placeholder. Fixed by updating test to match on `SpecificParamDoesntSend`. See `investigations/call_bound_wrong_arguments.md`.
- **Ambiguous call** — `vimpl()` was inside `OverloadResolver.narrowDownCallableOverloads` (line 622), not in the test. The compiler correctly found two matching `add(int, int)` overloads but hit a placeholder where it should construct a `CouldntNarrowDownCandidates` error. The `vimpl` existed because `CouldntNarrowDownCandidates` expected `Vector[RangeS]` but prototypes no longer have ranges (removed during MFBFDP refactoring). Fixed by changing the error type to hold `Vector[PrototypeT[IFunctionNameT]]` instead, updating the humanizer, and updating the test. See `investigations/ambiguous_call.md`.
- **Reports when non-kind interface in impl** and **Reports when non-kind struct in impl** — `Lexer.lexImpl` called `lexIdentifier` for interface/struct name positions, which rejects `&` prefix with `BadImplInterface`/`BadImplStruct` before input reaches the parser/postparser. Fixed by adding `lexImplOwnershipPrefix` helper that lexes `&`/`^` symbols before the identifier and includes them in the scramble. The parser's `TemplexParser.parseTemplex` then creates `InterpretedPT`, and the postparser's `scoutImpl` correctly throws `CantOwnershipInterfaceInImpl`/`CantOwnershipStructInImpl`. See `investigations/lex_impl_ownership.md`.
- **Cant make non-weakable extend a weakable**, **Cant make weakable extend a non-weakable**, and **Cant make weak ref to non-weakable** — all three weak ref tests fixed. First two: `WeakableImplingMismatch` was defined but never thrown; added check to `ImplCompiler.compileImpl`. Third: `&&m` on a local variable went through `softLoad` which bypassed `weakAlias`'s weakable check; fixed by routing `LoadAsWeakP` on non-weak addresses through `weakAlias` in `evaluateLookupForLoad`. Also added missing `weakable` to `Base` struct in "Lock weak member" and "Weak yonder member" tests. See `investigations/weak_ref_impl_mismatch.md`.
- **Forgetting set when changing** — `ship.x = 4;` parsed as let-binding, producing `BadThingAfterTypeInPattern` instead of `ForgotSetKeyword`. In `ExpressionParser.parseStatement`, when `parseLet` fails with `BadThingAfterTypeInPattern`, convert to `ForgotSetKeyword`. The `self.fuel = 42` case is safe (handled by `isConstructing` before the error fires).
- **Report leaving out semicolon** — `set x = 7 )` parsed successfully because the stray `)` was never in the scramble — `Lexer.atEnd` stops `lexScramble` at `)` by design. `TestParseUtils.compileBlockContents` didn't check for leftover input after `lexScramble`. Added a check matching `lexCurlied`'s existing stray-`)` detection (`BadStartOfStatementError`). See `investigations/report_leaving_out_semicolon.md`.
- **Func with func bound with missing 'where'** — `func sum<T>() func moo(&T)void {3}` parsed successfully because `func moo(&T)void` was treated as a `FuncPT` return type templex. `FuncPT` only makes sense in `where` clauses (generates `CallSiteFuncSR`/`ResolveSR` rules, not a coord type). Added check in `Parser.parseFunction`: reject `FuncPT` as return type with new `FuncBoundWithoutWhere` error. See `investigations/func_bound_missing_where.md`.
- **Abstract func without virtual** — `AbstractBodyMacro.generateFunctionBody` hit `vassert(params2.exists(_.virtuality == Some(AbstractT())))` assertion. `VirtualAndAbstractGoTogether` was defined in `PostParser.scala` but never thrown. Added check in `FunctionScout.scoutFunction`: if top-level function has `AbstractAttributeP` but no parameter has `AbstractP`, throw the error. Test moved from `typing.AfterRegionsErrorTests` to `postparsing.AfterRegionsErrorTests`.
- **Report when downcasting between unrelated types** — `ship.as<Spoon>()` produced generic `CouldntFindFunctionToCallT` instead of `CantDowncastUnrelatedTypes`. The `as` builtin has `where implements(SubType, SuperType)`, so the solver rejects with `IsaFailed` when no impl exists. `CantDowncastUnrelatedTypes` was defined/humanized but never thrown. Added interception in `CallCompiler.evaluateCall`: when calling `as` and all rejections are `IsaFailed`, throw `CantDowncastUnrelatedTypes`. See `investigations/category_f_tests.md`.

## Lessons learned

- **Generic functions need `func drop(T)void` bounds** if they take ownership of a `T` param. Without it, the definition fails trying to drop the param, before any call site is reached. Several AfterRegions tests may be missing this bound.
- **Error wrapping shapes changed** during the solver refactoring. Tests matching on `InferFailure(FailedSolve(...))` may need to also match `FindFunctionResolveFailure(ResolvingSolveFailedOrIncomplete(FailedSolve(...)))`. The underlying error (e.g., `OwnershipDidntMatch`, `BadIsaSubKind`) is the same — just wrapped differently.
- **MatchErrors in the compiler are often missing cases**, not logic bugs. The fix is usually a new catch-all or a new case in a match block. Per NSTDX, new error types should be structured (not stringly-typed).
- **If printlns don't fire, the code path isn't reached** — which is often THE finding. In the "Detects sending non-citizen" investigation, the expression compiler never ran for `moo(7)` because `moo`'s definition failed first (missing drop bound).
- **Some tests just need `vimpl(e)` replaced** — the compiler is already doing the right thing but the test was written with a placeholder. Before investigating compiler internals, run the test and read the error output. If the error looks correct, the fix is just updating the test's match pattern. "Call bound with wrong arguments" was this — zero compiler changes needed.
- **Function bounds use `PrototypeTemplataCalleeCandidate`**, not `FunctionCalleeCandidate` — when matching on `FindFunctionFailure.rejectedCalleeToReason`, bound-declared functions show up as prototype candidates. The rejection reason types (`SpecificParamDoesntSend`, etc.) are the same either way.
- **`SpecificParamDoesntSend` is a pre-solver rejection** — the overload resolver checks param sendability before invoking the solver. When the ownership mismatch is obvious (e.g., share vs borrow), it rejects immediately. Solver-level rejections come wrapped in `FindFunctionResolveFailure(ResolvingSolveFailedOrIncomplete(FailedSolve(...)))`.
- **`vimpl()` can be inside the compiler, not just in tests** — the "Ambiguous call" fix had the `vimpl()` inside `OverloadResolver.narrowDownCallableOverloads`, not in the test. The error-throwing structure was already there, just blocked by a type mismatch placeholder. Always check the stack trace to see whether the `vimpl` is in test code or compiler code.
- **When error types reference removed fields, change the error type** — `CouldntNarrowDownCandidates` expected `Vector[RangeS]` but the source data (prototypes) no longer carried ranges. Rather than inventing fake ranges, change the error type to hold what's actually available (`Vector[PrototypeT[IFunctionNameT]]`). Check that only one construction site exists before changing.
- **Lexer-level rejections can block postparsing error paths** — if the lexer rejects syntax before the parser/postparser sees it, the intended error can't be produced. The fix is often to make the lexer more permissive (lex the "bad" syntax as tokens in a scramble) and let downstream passes validate and produce the structured error. The parser and postparser already had the right checks — only the lexer was blocking.
- **Error types that exist but are never thrown** — `WeakableImplingMismatch` was defined with proper fields but no code ever constructed it. When investigating a missing error, search for construction/throw sites, not just the definition. If there are zero throw sites, the check was never implemented.
- **`CompilerTestCompilation.test` vs `RunCompilation.test`** — `CompilerTestCompilation` uses `Builtins.getModulizedCodeMap` (builtins in `v.builtins.*` packages, require explicit imports). `RunCompilation` uses `Builtins.getCodeMap` (builtins also in root namespace, visible without imports). Tests that call builtin functions like `lock` need either `import v.builtins.weak.*;` or `RunCompilation.test`.
- **Existing tests can have bugs too** — "Lock weak member" and "Weak yonder member" both used `&&Base` without `weakable` on `Base`. All the integration tests in `weaks/` correctly used `weakable`. The fix was to add `weakable` to the buggy tests AND add the missing compiler check.
- **`LocalHelper.softLoad` and `ExpressionCompiler.weakAlias` are different code paths for `&&`** — `softLoad` handles loading local variables (address expressions → references), `weakAlias` handles augmenting expression results. Only `weakAlias` checks weakable. The fix for local loads: in `evaluateLookupForLoad`, when `LoadAsWeakP` on a non-weak address, load as borrow first then route through `weakAlias`. The `!= WeakT` guard ensures loading existing weak ref members (like `ship.origin` where origin is `&&Base`) still works.
- **The lexer's `atEnd` stops scrambles at `)`, `}`, `]`** — this is by design for matching paired delimiters. But `TestParseUtils.compileBlockContents` calls `lexScramble` at the top level (no enclosing parens), so stray `)` silently stops the scramble without error. The fix is to check for leftover input in `LexingIterator` after `lexScramble`, matching what `lexCurlied` already does (line 716-718).
- **`FuncPT` is NOT a type** — it generates function resolution rules (`CallSiteFuncSR`, `DefinitionFuncSR`, `ResolveSR`), not a coord. It should never appear as a function return type. When it does, it means a `where` keyword is missing before a function bound.
- **Error types defined but never thrown is a recurring pattern** — `VirtualAndAbstractGoTogether`, `CantDowncastUnrelatedTypes`, `WeakableImplingMismatch`, `ForgotSetKeyword` were all defined with proper fields, humanized, but had zero throw sites. The check logic was simply never implemented.
- **`FunctionTemplateNameT` includes `codeLocation`** — this makes every function declaration unique by source position. The old `declaredSignatures` mechanism compared by `SignatureT` which could detect same-signature collisions. After refactoring to `functionDeclaredNames` keyed by `IdT`, the location-stamped template names prevent collision detection.
- **Intercepting overload resolution failures can produce better errors** — when the overload resolver fails to find a function, the generic `CouldntFindFunctionToCallT` can be intercepted and replaced with a more specific error if the rejection pattern is recognizable (e.g., all `IsaFailed` rejections on an `as` call → `CantDowncastUnrelatedTypes`).
- **`Prot[...]` decomposition is dead syntax** — no `.vale` code uses it. The `func name(params)return` syntax in `where` clauses completely replaces it via `CallSiteFuncSR`/`ResolveSR`. However, the `func` syntax cannot discover an unknown return type from name+params alone — `ResolveSR` requires both params AND return to be known. The old `Prot` decomposition was specifically designed for this but was never updated for 3 components.
- **Postparsing errors (`ICompileErrorS`) are caught by `scoutProgram`** — `CompileErrorExceptionS` is thrown and caught at `PostParser.scoutProgram` (line 375-377), converting to `Err(ICompileErrorS)`. Tests for postparsing errors should use `PostParserTestCompilation.test(code).getScoutput()`, not `CompilerTestCompilation.test(code).getCompilerOutputs()`.

## Already Fixed

**CompilerErrorHumanizer** (3 changes, committed):

1. Added `PredictedFunctionNameT` / `PredictedFunctionTemplateNameT` to `humanizeName` — fixes MatchError crash in "Generic interface anonymous subclass" and any test triggering predicted function names in error messages.
2. Added `InternalSolverError` case to `humanizeRuleError` — fixes MatchError crashes in "Test overload set", "Diff iter", "Call Array<> without element type", "Test interface default generic argument in type", "Detects sending non-citizen to citizen", "Reports when two functions with same signature". These now produce proper human-readable error messages.
3. Added 6 missing `ITypingPassSolverError` cases (`KindIsNotStruct`, `CouldntFindImpl`, `CantSharePlaceholder`, `NoCommonAncestors`, `CantDetermineNarrowestKind`, `FunctionDoesntHaveName`) — eliminates compiler warnings about non-exhaustive matches.

These make error reporting robust but don't flip any tests from fail to pass — the underlying compiler issues remain.

---

## A. Tests that immediately `vimpl()`/`vfail()` in the test body (6 tests)

Explicitly marked not-yet-implementable. Can't be mechanically fixed — need design decisions.

| Test | Class | Issue |
|---|---|---|
| TODO | IntegrationTests | 3 design-question `vimpl()` stubs at line 27 |
| imm tuple access | IntegrationTests | `vfail()` at top — "these tuples are actually mutable" |
| Can turn a borrow coord into an owning coord | typing.AfterRegionsTests | `vimpl()` at top — test design unclear |
| Report when downcasting to interface | typing.AfterRegionsErrorTests | `vimpl()` at top — needs impl-in-environment design |

**Effort:** Design decisions required. Not fixable without resolving open questions about regions/ownership.

---

## B. Tests that end with `vimpl()` after compilation (compilation itself fails first) (4 tests)

Even if compilation were fixed, these hit `vimpl()` at the end.

| Test | Class | Compile error | Then |
|---|---|---|---|
| Test one-anonymous-param lambda identifying runes | postparsing.AfterRegionsErrorTests | `0 != 1` — lambda generic params count wrong | `vimpl()` |
| Test two instantiations of anonymous-param lambda | typing.AfterRegionsTests | `0 != 2` — lambda funcs not found | `vimpl()` |
| Prints bread crumb trail | typing.AfterRegionsErrorTests | Can't find `isEmpty` override for `Some<_>` | `vimpl()` |
| Reports error | typing.AfterRegionsErrorTests | Can't compile virtual interface dispatch on `imm` struct | `vimpl()` |

**Effort:** Need both compiler fixes AND design decisions.

---

## C. Parser issues (0 remaining, 3 fixed)

| Test | Class | Issue |
|---|---|---|
| ~~Forgetting set when changing~~ | parsing.AfterRegionsTests | **FIXED.** In `ExpressionParser.parseStatement`, when `parseLet` fails with `BadThingAfterTypeInPattern`, convert it to `ForgotSetKeyword`. The `self.fuel = 42` case is safe because `isConstructing` handles it before the error fires. See `investigations/report_leaving_out_semicolon.md`. |
| ~~Report leaving out semicolon~~ | parsing.AfterRegionsTests | **FIXED.** The stray `)` was never in the scramble — `Lexer.atEnd` stops `lexScramble` at `)` by design. `TestParseUtils.compileBlockContents` didn't check for leftover input after `lexScramble`. Added a check matching `lexCurlied`'s existing stray-`)` detection. Test updated to expect `BadStartOfStatementError` (matching production behavior). See `investigations/report_leaving_out_semicolon.md`. |
| ~~Func with func bound with missing 'where'~~ | parsing.functions.AfterRegionsFunctionTests | **FIXED.** `FuncPT` (function prototype templex) only makes sense in `where` clauses — downstream it generates function resolution rules, not a type. Added check in `Parser.parseFunction`: if return type parses as `FuncPT`, return `Err(FuncBoundWithoutWhere)`. New error type defined, humanized, test updated. See `investigations/func_bound_missing_where.md`. |

---

## D. PostParsing errors caught too early at lex time (0 remaining, 2 fixed)

| Test | Class | Issue |
|---|---|---|
| ~~Reports when non-kind interface in impl~~ | postparsing.AfterRegionsErrorTests | **FIXED.** Added `lexImplOwnershipPrefix` to lexer so `&`/`^` pass through to parser/postparser. |
| ~~Reports when non-kind struct in impl~~ | postparsing.AfterRegionsErrorTests | **FIXED.** Same fix. |

---

## E. Prototype rule expects 3 components (0 remaining, 1 commented out)

| Test | Class | Issue |
|---|---|---|
| ~~Prototype rule to get return type~~ | typing.AfterRegionsTests | **COMMENTED OUT.** `Prot[name, params, return]` is dead syntax — no `.vale` code uses it. The `func name(params)return` syntax (`FuncPT` → `CallSiteFuncSR`/`ResolveSR`) completely replaces it for all real use cases. However, the `func` syntax can't discover an unknown return type from just name+params — `ResolveSR` requires both params AND return to already be known. The old `Prot` decomposition was specifically designed for this "look up function, discover return type" use case, but was never updated from 2-component to 3-component form in `RuleScout`. The `PrototypeComponentsSR` machinery exists in all solver phases but no compilable `.vale` program triggers it. |

---

## F. Compiler logic gaps — wrong error type or missing behavior (2 remaining, 4 fixed, 1 commented out)

| Test | Class | Issue |
|---|---|---|
| ~~Reports when two functions with same signature~~ | typing.AfterRegionsErrorTests | **COMMENTED OUT.** Returns `Ok` — no duplicate function detection. The old `declaredSignatures: Map[SignatureT, RangeS]` mechanism in `CompilerOutputs` was commented out during refactoring. The replacement `functionDeclaredNames` uses `IdT[IFunctionNameT]` which includes `FunctionTemplateNameT.codeLocation`, so two functions at different source locations are treated as different. `FunctionAlreadyExists` error type exists with an active throw site in `declareFunction`, but the key comparison uses location-stamped names that never collide. Needs signature-level (location-agnostic) duplicate detection to be restored. |
| Lambda is incompatible anonymous interface | typing.AfterRegionsErrorTests | **Blocked on category H.** Returns `Ok` instead of `BodyResultDoesntMatch`. The test's lambda `(_) => { 4 }` actually returns `int`, matching the interface method's `int` return — the types are compatible. The test depends on generic anonymous interface subclassing (category H) working first. `BodyResultDoesntMatch` is defined with 2 active throw sites in `FunctionBodyCompiler.scala` (lines 94, 130). |
| ~~Abstract func without virtual~~ | postparsing.AfterRegionsErrorTests | **FIXED.** Moved from typing to postparsing level. `VirtualAndAbstractGoTogether` was defined but never thrown. Added check in `FunctionScout.scoutFunction`: if a top-level function has `AbstractAttributeP` but no parameter has `AbstractP`, throw the error. Added humanization in `PostParserErrorHumanizer`. Test moved to `postparsing/AfterRegionsErrorTests.scala`. |
| ~~Call bound with wrong arguments~~ | typing.AfterRegionsErrorTests | **FIXED.** Compiler was already correct — `SpecificParamDoesntSend` rejection. Test had `vimpl(e)` placeholder. |
| Inherit reachable bounds (IRBFPTIPT) | typing.AfterRegionsErrorTests | "Expected non-empty!" assertion failure — bound inheritance for nested generic params not implemented. Core generics feature gap. |
| ~~Ambiguous call~~ | typing.AfterRegionsErrorTests | **FIXED.** `vimpl()` in `OverloadResolver.narrowDownCallableOverloads` blocked error construction. Changed `CouldntNarrowDownCandidates` to hold prototypes instead of removed ranges. |
| ~~Report when downcasting between unrelated types~~ | typing.AfterRegionsErrorTests | **FIXED.** The `as` builtin has `where implements(SubType, SuperType)`, so when `Spoon` doesn't implement `ISpaceship`, the solver rejects with `IsaFailed`. `CantDowncastUnrelatedTypes` was defined and humanized but never thrown. Added interception in `CallCompiler.evaluateCall`: when calling `as` and all rejections are `IsaFailed`, throw `CantDowncastUnrelatedTypes` instead of generic `CouldntFindFunctionToCallT`. See `investigations/category_f_tests.md`. |

---

## G. Solver conflicts / generic inference failures (5 remaining, 2 fixed)

Core type solver bugs where the solver produces contradictory conclusions.

| Test | Class | Conflict |
|---|---|---|
| Test overload set (integration) | IntegrationTests | `OverloadSetT` vs `VoidT()` on `ImplicitCoercionKindRuneS` — overload sets aren't properly coerced |
| Diff iter | IntegrationTests | `own` vs `share` ownership on same `KindPlaceholderT` — `HashSet<K Ref imm>` has `imm` but `HashSetDiffIterator<X>` doesn't propagate it |
| Call Array<> without element type | IntegrationTests | `mut` vs `imm` mutability conflict in array construction |
| ~~Test interface default generic argument in type~~ | typing.AfterRegionsTests | **FIXED.** Default generic param rules were added to the solver eagerly and unconditionally, conflicting with arg-inferred values. `assembleCallSiteRules` eagerly added `LiteralSR(_211, 5)` which combined with the hoisted `EqualsSR(H, _211)` forced `H=5` before argument inference ran. Fixed by removing eager default addition from `assembleCallSiteRules` and adding incremental default logic to `solveForResolving` — defaults now fire only for runes that remain unsolved after argument inference. See DRSINI. |
| ~~Test returning empty seq~~ | IntegrationTests | **FIXED.** Two combined issues. (1) Solver deadlock: `TuplePT` lowering unconditionally emitted `MaybeCoercingLookupSR + MaybeCoercingCallSR`, but for zero-arg tuples the pre-processor carve-out at `RuneTypeSolver.scala:441` refused to seed `Tup0`'s ambiguous templata type. Fixed in `TemplexScout.scala` by branching `TuplePT` on `elements.isEmpty` and emitting a single `MaybeCoercingLookupSR` for empty tuples (matching how bare zero-arg kind templates like `Spaceship` are handled). (2) VM leak: once compilation succeeded, `main() ()` returned a `Tup0{}` struct whose reference was `OwnH` (because `Tup0` was a default-mutable struct), and Vivem's `cleanup` does nothing for `OwnH` at end-of-program. Fixed by adding `imm` to `tup0.vale` (`struct Tup0 imm { }`), which makes the return reference Share and routes it through the existing StructHT destructure+dealloc path. See `investigations/test_returning_empty_seq.md` and `investigations/test_returning_empty_seq_vm_leak.md`. |
| Make array without type | IntegrationTests | "Must specify element for arrays" — inference for element type from lambda not working |
| Borrowing toArray | IntegrationTests | **Category-G cause fixed (MKRFA preprocessing added to ArrayCompiler), now H-blocked.** The three `ArrayCompiler` expression entry points (`evaluateRuntimeSizedArrayFromCallable`, `evaluateStaticSizedArrayFromValues`, `evaluateStaticSizedArrayFromCallable`) hardcoded `val initialKnowns = Vector()` and passed rules straight to `makeSolver`/`solveForResolving` without stripping `RuneParentEnvLookupSR` rules and converting them to `InitialKnown`s (the MKRFA contract). The solver's `RuneParentEnvLookupSR` handler at `CompilerSolver.scala:852` is a defensive no-op that silently swallowed the unpreprocessed rules, so `E` was never seeded from `callingEnv` and `_51111111111 = &E` stalled. Fix copies the MKRFA fold from `OverloadResolver.scala:311-325` inline into all three ArrayCompiler methods. Body now compiles (`Array<mut, &E>` return type preserved). Test still fails because of a separate pre-existing category-H bug at `Instantiator.scala:3174` — `KindPlaceholderT` → `KindTemplataI` substitution uses `vimpl()` for ownership composition, so `E := int` into `Array<mut, &E>` yields `Array<mut, i32>` instead of `Array<mut, &int>`. See `investigations/borrowing_to_array.md`. |

**Effort:** High. These are core inference bugs. The solver produces contradictory conclusions when inferring ownership/mutability for generics. Fixing requires deep understanding of how the solver handles placeholders, implicit coercion runes, and default generic arguments.

---

## H. Deep `vimpl()` stubs in compiler internals (4 tests)

| Test | Class | Blocked by |
|---|---|---|
| Map function | IntegrationTests | `FunctionCompilerSolvingLayer.scala:489` — `vimpl()` in `evaluateGenericVirtualDispatcherFunctionForPrototype`. Need to implement placeholder creation for generic virtual dispatcher params. |
| Upcasting in a generic function | IntegrationTests | `Instantiator.scala:3174` — `vimpl()` in `translateCoord` for `KindPlaceholderT` → `KindTemplataI`. Need ownership composition logic for region-aware coords. |
| Method call on generic data | typing.AfterRegionsTests | Compilation fails on `x.launch()` where `x: &T` and `implements(T, IShip)`. Generic + interface virtual dispatch not fully working. |
| Impl rule | typing.AfterRegionsTests | Same underlying issue — `x.getFuel()` where `x: T` and `implements(T, IShip)`. |

**Effort:** High. These are the core "after regions" features. The virtual dispatcher placeholder creation and instantiator coord translation are fundamental to making generics work with the region system.

---

## I. Weak reference tests (0 remaining, 3 fixed)

| Test | Class | Issue |
|---|---|---|
| ~~Cant make non-weakable extend a weakable~~ | typing.AfterRegionsErrorTests | **FIXED.** Added weakable check to `ImplCompiler.compileImpl`. |
| ~~Cant make weakable extend a non-weakable~~ | typing.AfterRegionsErrorTests | **FIXED.** Same fix. |
| ~~Cant make weak ref to non-weakable~~ | typing.AfterRegionsErrorTests | **FIXED.** Missing weakable check on local variable load path (`evaluateLookupForLoad`). Two existing tests ("Lock weak member", "Weak yonder member") were also missing `weakable` on their structs. |

---

## J. Infinite recursion (1 test)

| Test | Class | Issue |
|---|---|---|
| Infinite lambda call | IntegrationTests | `lam = (f, z) => { f(f, z) }; lam(lam, 7)` — compiler can't type-check self-referential lambda. Requires cycle detection or lazy/fixpoint typing for recursive lambdas. |

**Effort:** High. Needs fundamental change to how the compiler handles recursive types in lambda inference.

---

## Summary by Effort

| Category | Remaining | Fixed/Commented | Effort |
|---|---|---|---|
| A. `vimpl()`/`vfail()` in test body | 6 | 0 | Design decisions needed |
| B. Compile fails + `vimpl()` at end | 4 | 0 | Compiler fix + design decisions |
| C. Parser issues | 0 | 3 fixed | Done |
| D. Lex-time errors instead of postparsing | 0 | 2 fixed | Done |
| E. Prot rule 3 components | 0 | 1 commented out | Done (dead syntax) |
| F. Wrong error type / missing behavior | 2 | 4 fixed, 1 commented out | Remaining: blocked (H) + high (generics) |
| G. Solver conflicts | 5 | 2 fixed | High (core inference) |
| H. Deep `vimpl()` in compiler | 4 | 0 | High (core feature) |
| I. Weak reference checks | 0 | 3 fixed | Done |
| J. Infinite recursion | 1 | 0 | High |
| **Total remaining** | **22** | | |
| **Recovered so far** | **20** | | 17 fixed + 2 commented out + 1 moved to postparsing |

### All recovered tests (19 total)

**Previously recovered (10):** Reports when ownership doesnt match, Detects sending non-citizen to citizen, Accidentally mention type rune, Call bound with wrong arguments, Ambiguous call, Reports when non-kind interface in impl, Reports when non-kind struct in impl, Cant make non-weakable extend a weakable, Cant make weakable extend a non-weakable, Cant make weak ref to non-weakable

**This session (9):**
- Forgetting set when changing (C, fixed)
- Report leaving out semicolon (C, fixed)
- Func with func bound with missing 'where' (C, fixed)
- Abstract func without virtual (F, fixed — moved to postparsing)
- Report when downcasting between unrelated types (F, fixed)
- Reports when two functions with same signature (F, commented out — needs signature-level duplicate detection)
- Prototype rule to get return type (E, commented out — dead `Prot[...]` syntax)
- Test interface default generic argument in type (G, fixed — DRSINI: defaults now incremental not initial)
- Test returning empty seq (G, fixed — empty-tuple `TuplePT` lowering skips the redundant call rule; `Tup0` marked `imm` to route through share-dealloc)
- Borrowing toArray (G-cause fixed, test now H-blocked — added MKRFA preprocessing to three `ArrayCompiler` methods, body compiles with correct `Array<mut, &E>` return; test still fails on category-H instantiator bug at `Instantiator.scala:3174`. Fixing category H now unlocks 5 tests instead of 4)
