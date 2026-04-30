# Fix Rust Interop Branch for Master Merge

## Status

**Starting point:** 113 non-AfterRegions test failures  
**After completed work:** 55 remaining (58 fixed, committed as f4dfbf64)

### What Was Wrong and How We Fixed It

The `rustinterop-merged` branch introduced several new features (`extern struct` syntax, `GenericParameterS.inherited` field, `FunctionS.lift` field, `RegionT(IRegionT)`, ValeRuster refactor) but left many placeholders and premature assertions in the codebase. Here's each root cause and its fix:

---

**Root Cause 1: Premature ZXCVCXZ assertion in PrototypeT (32 tests)**

The `lift` feature was being developed to change how struct methods are namespaced — moving them from nested under `StructTemplateNameT` to the outer scope. A `vwat()` assertion was added to `PrototypeT`'s constructor to catch any prototype whose `initSteps` contained a bare `StructTemplateNameT` — the idea being that once `lift` was complete, this should never happen. But the actual `assembleName` logic that would make this invariant hold was still commented out in `FunctionCompilerMiddleLayer.scala`. So the assertion fired on every struct method (drop functions, constructors, etc.), killing 32 tests.

*Fix:* Removed the `id.initSteps.foreach({ case StructTemplateNameT(_) => vwat() })` block from `PrototypeT` in `ast.scala:471-476`. The `IdT` constructor already had explicit `vpass()` allowances for Vec methods with `StructTemplateNameT`, confirming this was a known-valid pattern.

---

**Root Cause 2: `GenericParameterS.inherited` field broke vasserts (21 tests)**

`GenericParameterS` gained a new `inherited: Boolean` field (3rd param) that Sylvan doesn't have. It marks whether a generic param was inherited from a parent citizen (interface/struct) vs declared by the function itself. When `PostParser.scala:678` constructs interface internal methods, it copies the interface's generic params with `.copy(inherited = true)`. But the `InterfaceS` constructor (postparsing AST) and `InterfaceA` constructor (higher-typing AST) both had `vassert(genericParams == internalMethod.genericParams)` — structural equality on case classes. Since the interface's own params had `inherited = false` and the method copies had `inherited = true`, the assertion always failed.

*Fix:* Changed both vasserts to `vassert(genericParams.map(_.copy(inherited = true)) == internalMethod.genericParams)` — normalizing the interface's params to `inherited = true` before comparing. This is correct because interface methods can't declare their own generic params (enforced at `FunctionScout.scala:135`) and always use `AbstractBodyS` (no body-inferred params), so the method's params are always exactly the interface's params with `inherited` flipped.

---

**Root Cause 3: `vimpl()` placeholders for `inherited` param (17 tests)**

Two call sites construct `GenericParameterS` but had `vimpl()` (a panic placeholder) where the new `inherited` boolean should go:
- `PostParser.scoutExportAs` at line 523 — constructs a region generic param for export declarations
- `AnonymousInterfaceMacro.makeStruct` at line 222 — constructs generic params for anonymous substruct member runes

*Fix:* Replaced both `vimpl()` calls with `false`. In the export case, this is a default region param (not inherited). In the macro case, `memberRunes` are the anonymous substruct's own closure-field type parameters (not inherited from the interface). Verified that these never flow through `ParentCitizen` (which has a `vassert(x.inherited)` guard).

**Important gotcha discovered during this fix:** The plan originally included changing `ContextRegionRune(rune)` back to `rune` on the next line of `scoutExportAs`. A verification agent caught that this would be a **compile error** — ValeRustInterop refactored `translateTemplex` to take `IContextRegion` instead of `IRuneS`, so the `ContextRegionRune(...)` wrapper is required.

---

**Root Cause 4: `inherited` filter in FunctionCompilerSolvingLayer stripped all template args from interface methods (15 tests)**

This was the most surprising find. `FunctionCompilerSolvingLayer.scala:660` had `function.genericParameters.filter(!_.inherited).map(_.rune.rune)` — filtering out inherited runes when adding them to the near environment. Sylvan has no `inherited` field, so no filter. For interface methods where ALL generic params are inherited (they come from the parent interface), this filter stripped ALL runes. This cascaded:

1. `runedEnv.templateArgs` became empty
2. `assembleName` built a `FunctionNameT` with zero `templateArgs`
3. `FunctionHeaderT`'s constructor tried to verify placeholders exist in `templateArgs` by index
4. `templateArgs(0)` threw `IndexOutOfBoundsException: 0`

The initial investigation categorized these 15 tests as "InterfaceA vassert failures" because fixing the InterfaceA vassert just unmasked this deeper bug — the tests got past the vassert but crashed at `FunctionHeaderT` instead.

*Fix:* Removed the `filter(!_.inherited)` — now all generic params (inherited or not) are added to the near env, matching Sylvan's behavior. The `inherited` filter in `FunctionCompilerCore:284` (the struct-method branch) is intentional and was kept — it strips inherited template args from the function name because the struct's type args come from `selfStructId` instead.

---

**Root Cause 5: FunctionCompilerCore's StructTemplateNameT branch fired for non-method functions (related to Root Cause 4)**

`FunctionCompilerCore.makePrototype` had a `case Some(StructTemplateNameT(_))` branch that assumed any function with a struct template in its `initSteps` has a `self` parameter. Anonymous substruct forwarder methods (which implement interface methods) are inside a struct's environment but don't have a `self` param — they have params like `this` or generated names.

*Fix:* Added a `hasSelfParam` guard: `case Some(StructTemplateNameT(_)) if hasSelfParam =>`. Functions without `self` fall through to the default path `PrototypeT(fullEnv.id, returnCoord)`.

---

**Root Cause 6: Stale test API for extern maps (2 tests)**

`PackageH`'s constructor changed: fields 8 and 9 went from `externNameToFunction: Map[StrI, PrototypeH]` / `externNameToKind: Map[StrI, KindHT]` to `prototypeToExtern: Map[PrototypeH, HamutsFunctionExtern]` / `kindToExtern: Map[OpaqueHT, HamutsKindExtern]`. Two tests still used the old API:
- `VivemTests.scala:88` had `vimpl()` as a placeholder for the extern map
- `IntegrationTestsC.scala:178` had a commented-out `externNameToFunction.get("sqrt")` assertion replaced with `vimpl()`

*Fix:* VivemTests now constructs a real `HamutsFunctionExtern` with a `SimpleId` for `__vbi_addI32`. IntegrationTestsC now uses `prototypeToExtern.values.find(_.maybeExternName == "sqrt")`.

---

**Root Cause 7: CompilerErrorHumanizer missing match cases (0 tests fixed, but prevents MatchError crashes)**

Three name/error types were missing from `CompilerErrorHumanizer`'s match statements:
- `InternalSolverError` — wraps `SolverConflict` when the solver has conflicting conclusions for a rune. Missing from `humanizeRuleError` match.
- `PredictedFunctionNameT` / `PredictedFunctionTemplateNameT` — used for predicted drop function names before actual compilation. Missing from `humanizeName` match.

These caused `scala.MatchError` crashes during error formatting. The underlying tests still fail (they have legitimate compilation failures), but now they fail with readable error messages instead of unhelpful `MatchError` stack traces.

*Fix:* Added all three match cases. `InternalSolverError` humanizes its inner `SolverConflict`/`RuleError`/`SolveIncomplete`. `PredictedFunctionNameT` formats identically to `FunctionNameT`.

---

**Root Cause 8: Debug printlns left in Instantiator**

Two `println` calls at `Instantiator.scala:1477,1482` (inside a `sanityCheck` guard) printed "Looking for: ..." and "Candidate: ..." to stdout during test runs, polluting test output.

*Fix:* Deleted both lines.

---

### Lessons Learned

1. **The `inherited` field had deeper tentacles than investigation found.** Three read sites were identified, but the SolvingLayer filter (Root Cause 4) was the most impactful and hardest to find — it caused crashes at a totally different location (`FunctionHeaderT`) than where the filter was applied.

2. **Fixing one assertion can unmask a deeper bug.** The InterfaceA vassert fix let tests proceed past the assertion, only to crash at `FunctionHeaderT` due to empty template args. The categorization "15 InterfaceA vassert failures" was wrong — it was actually "15 SolvingLayer inherited-filter failures" that happened to be blocked by the vassert.

3. **"Match Sylvan" isn't always correct.** The `ContextRegionRune` wrapper was an intentional ValeRustInterop divergence. A verification agent caught this before it became a debugging detour.

4. **The extern pipeline works bottom-up.** The commented-out top-down `addFunctionExtern` in TypingPass is NOT needed — the Instantiator collects externs by discovering `ExternFunctionCallTE` nodes. But `ensureDeepExports` validation depends on the top-down path, so it can't be re-enabled without also re-enabling `addFunctionExtern`.

---

## Remaining Work

### Tier 2: Fix `lift` feature (5 tests)

**2a. Fix StructDropMacro drop function scoping (3 tests)**

File: `Frontend/TypingPass/src/dev/vale/typing/macros/citizen/StructDropMacro.scala:114`

Problem: Line 114 uses `structName.copy(localName = dropFunctionName)` which makes the drop function a *sibling* of the struct (flat namespace). Sylvan uses `structName.addStep(dropFunctionName)` which makes it a *child* (nested under the struct). Three tests pattern-match on the old nested structure:
- "Automatically drops struct" — expects `IdT(_, Vector(StructTemplateNameT("MyStruct")), FunctionNameT("drop", ...))`
- "Test return from inside if destroys locals" — same pattern
- "Prints bread crumb trail" — override resolution fails because drop is in wrong namespace

Fix: Change `structName.copy(localName = ...)` back to `structName.addStep(...)`.

**2b. Fix lift detection in FunctionScout (1 test)**

File: `Frontend/PostParsingPass/src/dev/vale/postparsing/FunctionScout.scala:529`

Problem: Lift detection checks for the `self` keyword (`LocalNameDeclarationP(NameP(_, self)) if self == keywords.self`). The test "Lift methods correctly" expects `capacity(v Vec<T>) i64` to have `lift = true`, but the param is named `v`, not `self`.

Fix: Either change the detection to check for any struct-typed param, or update the test expectation to match the `self`-keyword rule. Need to check what the intended design is.

**2c. Possible: StructCompiler lift filter (1 test)**

File: `Frontend/TypingPass/src/dev/vale/typing/citizen/StructCompiler.scala:131`

The `.filter(_.lift)` on internal methods may be filtering out methods that should be visible. "Test UMSEIR" might be affected. Investigate after 2a/2b.

---

### Tier 3: Mark pre-existing failures as `ignore` (~24 tests)

These tests have "does not pass yet" comments in Sylvan, or call `vimpl()`/`vfail()` directly in the test body. They should be annotated with `ignore` in both codebases, but this branch only needs them ignored to get a clean merge.

**Test-level stubs (vimpl/vfail in test body):**
- "TODO" (`AfterRegionsIntegrationTests.scala:27`)
- "imm tuple access" (`AfterRegionsIntegrationTests.scala:73`)
- "Can turn a borrow coord into an owning coord" (`AfterRegionsTests.scala:140`)
- "Report when downcasting to interface" (`AfterRegionsErrorTests.scala:144`)
- "Report when downcasting between unrelated types" (`AfterRegionsErrorTests.scala:181`)

**"Does not pass yet" tests (pre-existing in Sylvan):**
- "Reports when two functions with same signature" (`AfterRegionsErrorTests.scala:129`)
- "Forgetting set when changing" (`AfterRegionsTests.scala:11`)
- "Report leaving out semicolon or ending body after expression, for paren" (`AfterRegionsTests.scala:21`)
- "Abstract func without virtual" (`AfterRegionsErrorTests.scala:246`)
- "Detects sending non-citizen to citizen" (`AfterRegionsErrorTests.scala:218`)
- "Accidentally mention type rune" (`AfterRegionsErrorTests.scala:265`)
- "Lambda is incompatible anonymous interface" (`AfterRegionsErrorTests.scala:213`)
- "Reports error" (`AfterRegionsErrorTests.scala` — depends on other pre-existing)
- "Reports when ownership doesnt match" (`AfterRegionsTests.scala:304`)

**Compiler vimpl stubs (hit vimpl in compiler, not test):**
- "Map function" — `vimpl()` at `FunctionCompilerSolvingLayer.scala:531`
- "Upcasting in a generic function" — `vimpl()` at `Instantiator.scala:3210`
- "Impl rule" — depends on "Method call on generic data" (pre-existing)

**Solver/resolution limitations (pre-existing):**
- "Test overload set" — solver conflict, overload set as kind
- "Diff iter" — solver ownership conflict
- "Call Array<> without element type" — internal solver error
- "Make array without type" — "Must specify element for arrays"
- "Test interface default generic argument in type" — solver conflict
- "Borrowing toArray" — solver can't resolve runes
- "Infinite lambda call" — "Function already exists"
- "Test returning empty seq" — solver can't resolve `Tup0` rune
- "Cant make non-weakable extend a weakable" (+ 2 similar weakable tests)
- "Method call on generic data" — method resolution on generic types
- "Tests overload set and concept function" — can't resolve `__call`
- "Func with func bound with missing 'where'" — pre-existing
- "Call bound with wrong arguments" — pre-existing
- "Inherit reachable bounds for params and things inside params too" — pre-existing
- "Ambiguous call" — pre-existing

**Extern validation (needs full pipeline re-enablement):**
- "Reports when extern function depends on non-exported param"
- "Reports when extern function depends on non-exported return"

**Impl error type mismatch (lexer design issue):**
- "Reports when non-kind interface in impl"
- "Reports when non-kind struct in impl"

---

### Tier 4: Mark WIP new-feature tests as `ignore` (7 tests)

These test incomplete features. Mark `ignore` with TODO comments.

**Rust interop lexer (3 tests):**
- "Call rust builtin" — needs `LexAndExplore.scala` module-skip for `rust`/`frust` packages
- "Call rust free function" — same
- "Import rust object" — same

**TransmigratePE / iso regions (2 tests):**
- "Create iso object" — `ExpressionScout.scala` has no case for `TransmigratePE`
- "Reference iso object" — same

**Extern struct Vec (2 tests):**
- "Extern rust Vec capacity" — extern struct method resolution not complete
- "Extern rust Vec len" — same

---

### Tier 5: Cleanup

- Remove remaining DO NOT SUBMIT markers (63 total across Frontend)
- Fix `VonHammer.scala:130` VonMember key name
- Remove `FunctionS.lift` DO NOT SUBMIT comment (keep the field)
- Clean up `FunctionScout.scala` DO NOT SUBMIT comments

---

## Key Architectural Notes

**`inherited` field on `GenericParameterS`:** Marks whether a generic param was inherited from a parent citizen vs declared by the function itself. Used in `FunctionCompilerCore:284` to strip inherited template args from struct-method function names (struct args come from `selfStructId` instead). The SolvingLayer filter was removed — all runes are added to the near env regardless of `inherited`.

**`lift` field on `FunctionS`:** Controls whether struct internal methods are placed in the struct's outer env (lifted, callable via UFCS) or inner env (not lifted, inherits struct's rune context). Currently: methods with `self` param are lifted, static methods are not. The `assembleName` lift logic in `FunctionCompilerMiddleLayer` is still commented out.

**Extern pipeline:** Works bottom-up via the Instantiator (`ExternFunctionCallTE` discovery + `ExternI` struct detection). The top-down TypingPass `addFunctionExtern` path is commented out and NOT needed for the pipeline to function. However, `ensureDeepExports` extern validation depends on the top-down path's `getFunctionExterns`, so it can't be re-enabled without also re-enabling `addFunctionExtern`.

**`ContextRegionRune` wrapper:** ValeRustInterop refactored `translateTemplex` to take `IContextRegion` instead of `IRuneS`. Do NOT remove `ContextRegionRune(rune)` wrappers — they're required for type safety.
