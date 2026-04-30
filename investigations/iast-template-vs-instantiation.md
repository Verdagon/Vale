# I-AST template-vs-instantiation mismatch on lifted struct methods (non-generic structs)
## Symptom

`Instantiator.scala#assemblePlaceholderMap` panics with `vwat()` when called as
`assemblePlaceholderMap(funcT.header.id, desiredPrototypeS.id)` from
`translateFunction`.

DEBUG capture shows the leaf-recursion mismatch:

```
idT = IdT(PackageCoordinate(StrI(vtest),Vector()),Vector(),
        StructNameT(StructTemplateNameT(StrI(IntRangeIter)),Vector()))
idS = IdI(PackageCoordinate(StrI(vtest),Vector()),Vector(),
        StructTemplateNameI(StrI(IntRangeIter)))
  localNameT class = dev.vale.typing.names.StructNameT
  localNameUncastedI class = dev.vale.instantiating.ast.StructTemplateNameI
```

So at the same path position:
- typed AST: `StructNameT(StructTemplateNameT, Vector())` — **instantiation** form, zero args
- I-AST:    `StructTemplateNameI`                          — **template** form

This is the lift-prefix step (per @SMLRZ) for a lifted method (`drop`/`clone`)
on a non-generic struct (`IntRangeIter`, `Carrier`, `Thing`, …).

The two ASTs are supposed to be shape-parallel for `assemblePlaceholderMap`'s
zip-walk, but the I-AST emitted the wrong sealed-trait branch for this prefix
step.

## What sI/nI/cI mean (background)

These are not "instantiation phases." From
`InstantiatingPass/.../types.scala#IRegionsModeI`:

```scala
sealed trait IRegionsModeI
class sI() extends IRegionsModeI         // subjective
class nI() extends sI                     // "new" — start of a new instantiation
class cI() extends IRegionsModeI         // collapsed
```

The `I`-suffixed types (`IdI`, `StructNameI`, `StructTemplateNameI`, …) are the
**Instantiating-pass** parallel hierarchy to the typing pass's `T`-suffixed
types. The type parameter just tags region mode. Both T-AST and I-AST contain
both template and instantiation name forms — emitting the wrong one is a
consistency bug, not a phase confusion.

## Data flow into the failure

1. `translatePrototype` (Instantiator.scala) builds `desiredPrototypeS: PrototypeI[sI]`
   by calling `translateFunctionId(...)` (line 3002–3019).
2. `translateFunctionId`:
   ```scala
   val IdT(module, steps, last) = fullNameT
   IdI(
     module,
     steps.map(translateName(...)),     // initSteps
     translateFunctionName(...))         // last
   ```
3. `desiredPrototypeS` → consistent-collapsed via
   `RegionCollapserConsistent.collapsePrototype(...)` → `desiredPrototypeN: PrototypeI[nI]`.
4. `monouts.newFunctions.enqueue((desiredPrototypeT, desiredPrototypeN, …))`
   (only one enqueue site, line 1465).
5. `translate()` dequeues and calls `translateFunction(funcT, newFuncIdN, …)`.
6. In `translateFunction`: `val desiredPrototypeS: PrototypeI[sI] = desiredPrototypeN`
   — pure type cast (CCFCTS: sI/nI/cI are zero-member, see types.scala).
7. `assemblePlaceholderMap(funcT.header.id, desiredPrototypeS.id)`.

## Hypothesis: which producer emits the wrong form

The two relevant producers, both in `Instantiator.scala`:

- `translateName` (line 3801)
  ```scala
  case StructNameT(StructTemplateNameT(humanName), templateArgs) =>
    StructNameI(StructTemplateNameI(humanName), templateArgs.map(translateTemplata(...)))
  // ...
  case StructTemplateNameT(humanName) => StructTemplateNameI(humanName)
  ```
  Should emit `StructNameI(template, [])` for an instantiation with empty args.
  Only emits a bare `StructTemplateNameI` when the input is bare `StructTemplateNameT`.

- `RegionCollapserConsistent.collapseName` (line 116–137)
  ```scala
  case s: IStructTemplateNameI[_] => collapseStructTemplateName(s.asInstanceOf[...])
  // ...
  case s @ StructNameI(_, _) => collapseStructName(map, s)
  ```
  Dispatches by sealed-trait branch. Doesn't downgrade `StructNameI` →
  `StructTemplateNameI`.

So neither producer **looks** like it would emit the wrong form, given the
typed-AST input `StructNameT(StructTemplateNameT, Vector())`. Yet the symptom
shows the I-AST has `StructTemplateNameI` at that position.

Possibilities:
- (A) The typed-AST function id's initSteps actually contains a bare
  `StructTemplateNameT` (not wrapped in `StructNameT(_, [])`). The DEBUG showed
  the *recursion-leaf* `idT.localName` is `StructNameT(...)` — but that's
  whatever step recursion landed on, which depends on the original initSteps
  path. Need to print the **original** typed-AST id (before recursion strips).
- (B) Some intermediate code path replaces the prefix step. e.g. the
  `translateFunctionName` call inside `translateFunctionId` might construct
  the I-AST function name from the function's own `IFunctionNameT` (which has
  a `template: IFunctionTemplateNameT` and nests its container), and somewhere
  in there the container step is rebuilt from a template name only.
- (C) `RegionCollapserConsistent.collapseFunctionName` (line 39–103) operates
  on the function name itself, not the prefix. But maybe a function-name
  variant carries a "container template" reference that gets emitted as a
  prefix step? Unlikely given the code shape.

## Plan

1. Instrument `translatePrototype` to print:
   - `desiredPrototypeT.id` (typed-AST, the real source)
   - `desiredPrototypeS.id` immediately after `translateFunctionId` (sI form)
   - `desiredPrototypeN.id` after the consistent collapse (nI form)
   For each, print full repr so we can see exact sealed-trait branches at every
   position.
2. Run the failing test, tee to `/tmp/trace.txt`, grep for the
   `IntRangeIter` / `Carrier` / `Thing` cases.
3. Decide between hypotheses A/B/C based on which form first appears in which
   prototype.

## Findings

### Round 1 — instrumented `translatePrototype` to print `T.id`, `S.id`, `N.id`

Captured the contrast between two functions on the same non-generic struct
`IntRangeIter`:

**Function `next` (regular, hand-written method):**
```
T.id.initSteps = Vector(StructNameT(StructTemplateNameT(IntRangeIter), Vector()))
S.id.initSteps = Vector(StructNameI(StructTemplateNameI(IntRangeIter), Vector()))
N.id.initSteps = Vector(StructNameI(StructTemplateNameI(IntRangeIter), Vector()))
```
✓ Instantiation form throughout. `assemblePlaceholderMap` walks happily.

**Function `drop` (auto-generated by `StructDropMacro`):**
```
T.id.initSteps = Vector(StructTemplateNameT(IntRangeIter))
S.id.initSteps = Vector(StructTemplateNameI(IntRangeIter))
N.id.initSteps = Vector(StructTemplateNameI(IntRangeIter))
```
✗ Bare template form throughout. `assemblePlaceholderMap` panics.

**Conclusion: the bug is in the typed AST.** The simplifier (`translateName`)
and consistent collapser are innocent — they faithfully translated
`StructTemplateNameT` → `StructTemplateNameI`. The typed AST's drop function
id had the wrong shape from the start.

### Root cause

For regular methods like `next`, the function id is built by
`FunctionCompilerMiddleLayer.assembleName` (per @SMLRZ). With `function.lift = true`
and a `self` parameter, it does:
```scala
selfStructId.addStep(...funcName...)     // FunctionCompilerMiddleLayer.scala#assembleName
```
Here `selfStructId` is the self parameter's `StructTT` id, whose localName is
`StructNameT(template, args)` — **instantiation form**. So `addStep` puts an
instantiation step into initSteps. ✓

For drop, `StructDropMacro.getStructSiblingEntries` does:
```scala
val dropNameT = structName.addStep(...funcName...)   // StructDropMacro.scala
```
Here `structName` comes from `Compiler.preprocessStruct`, which is built as:
```scala
val structNameT = packageName.addStep(nameTranslator.translateNameStep(structA.name))
                                       // → StructTemplateNameT (template form)
```
So `structName.localName = StructTemplateNameT(IntRangeIter)` — **template form**.
`addStep` puts the bare template name into initSteps. ✗

The recent commit `9653a59a` ("Fix `StructDropMacro` to use `addStep` instead of
`copy(localName = ...)` for drop function naming") changed `StructDropMacro`'s
naming to `addStep`, but the parent it appends to is the template-form
`structName`. The shape produced is inconsistent with what `assembleName`
produces for hand-written methods on the same struct.

### Fix options

**Option 1 — Reshape the prefix in `StructDropMacro` (targeted):** Replace the
template-form parent step with an instantiation-form one before `addStep`:
```scala
val structInstanceName =
  structName.localName match {
    case t: IStructTemplateNameT =>
      StructNameT(t, structA.genericParameters.map(/* placeholder for each */))
    case other => vwat(other)
  }
val structInstantiatedId =
  IdT(structName.packageCoord, structName.initSteps, structInstanceName)
val dropNameT = structInstantiatedId.addStep(...funcName...)
```
For non-generic structs, the placeholder vector is empty. For generic structs,
each generic param needs a `KindPlaceholderT`/`PlaceholderTemplataT` created
the same way the rest of the typing pass creates them (probably from the
struct's `TemplateTemplataType` and the runes already in scope).

**Option 2 — Make drop go through `assembleName` like regular methods:** Set
`function.lift = true` on the FunctionA the macro produces, ensure drop has a
recognizable `self` parameter (it does — `keywords.thiss` capture), and let
`FunctionCompilerMiddleLayer.assembleName` rebuild the id when the function is
typed. The macro then only registers the function template, not the final id.
Risk: there may be other code that consumes the macro's pre-baked id (via
`Compiler.preprocessStruct`'s `Vector[(IdT[INameT], IEnvEntry)]` return type).

Option 1 is local and matches the existing fix style. Option 2 unifies the
two code paths but is broader.

### Round 2 — instrumented `StructDropMacro` itself; checked which structs trigger it

Added a `println` inside `StructDropMacro.getStructSiblingEntries` printing
`structName`, `structName.localName.class`, `structA.genericParameters.size`,
`dropNameT`, and `dropNameT.initSteps` for every macro fire.

Across multiple test runs:
```
structName.localName.class = StructTemplateNameT
genericParameters.size = 0
dropNameT.initSteps = Vector(StructTemplateNameT(<name>))
```
For Tup0, IntRange, IntRangeIter, and every other auto-derived drop. **All
non-generic.**

### Why generic structs aren't observed hitting this

Surveyed all generic structs in `Frontend/Builtins/src/dev/vale/resources/`:

| File | Struct | Has `#!DeriveStructDrop`? |
|---|---|---|
| `tup0.vale` | `Tup0` | no (non-generic) |
| `tup1.vale` | `Tup1<T0>` | yes — user drop |
| `tup2.vale` | `Tup2<T0, T1>` | yes — user drop |
| `tupN.vale` | `Tup3<T0, T1, T2>` | yes — user drop |
| `opt.vale` | `Some<T>`, `None<T>` | yes — user drop |
| `result.vale` | `Ok<O,E>`, `Err<O,E>` | yes — user drop |

`#!` suppresses the macro, so for every generic builtin struct the drop is
hand-written and goes through `FunctionCompilerMiddleLayer.assembleName` (the
correct-shape path).

For test programs (e.g. `getOr.vale`, `templatedoption.vale`), generic structs
defined there also use either `#!DeriveStructDrop` or `where func drop(T)void`
clauses paired with explicit drop functions.

**So in practice the macro only fires for non-generic structs**, and the bug
only manifests there. Generic-struct macro-fire is theoretically broken too,
but no test exercises it.

### Conclusion: scope of the fix

The fix needs to handle both cases for safety:
- **Non-generic struct** (every observed case): wrap `structName.localName`
  in `StructNameT(template, Vector())` — empty templateArgs.
- **Generic struct** (theoretical, not observed): would need placeholders
  for each generic param (`KindPlaceholderT`/`PlaceholderTemplataT` — same
  way `assembleName` produces them via the self-param's `StructTT`).

Since no test currently triggers the generic case, the safest move is:
1. Wrap with empty args for non-generic structs (covers all observed
   failures).
2. For generic structs, panic/`vimpl` with a clear message — leave it as
   not-yet-implemented.
3. Whoever writes the first generic-struct-without-`#!DeriveStructDrop` test
   will hit the panic and know to extend the macro.

This matches @TUCMPX (panic for unimplemented branches) rather than silently
producing the wrong shape.

### Where the fix goes

Single site: `StructDropMacro.getStructSiblingEntries` at
`Frontend/TypingPass/src/dev/vale/typing/macros/citizen/StructDropMacro.scala`,
the `addStep` line.

Alternative: also check the parallel macros (`InterfaceDropMacro`,
`StructConstructorMacro`) for the same shape bug — but the captures show
interface drops live at package level (no struct prefix), so
`InterfaceDropMacro` likely doesn't have this issue.
`StructConstructorMacro` may need the same fix.

### Round 3 — traced WHERE the broken shape actually originates

The `dropNameT` returned from the macro is just an env-registration key. The
shape that ends up in `funcT.header.id` is set by `FunctionHeaderT(env.id, …)`
in `StructDropMacro.generateFunctionBody`, where `env.id` is computed by
`makeNamedEnv` → `assembleName` (FunctionCompilerMiddleLayer).

Inspecting `assembleName`:
```scala
val maybeSelfStructId =
  if (!function.lift) None
  else {
    function.params.zip(paramTypes).collectFirst {
      case (paramS, CoordT(_, _, StructTT(structId)))
        if paramS.pattern.name.exists(c => c.name match {
          case CodeVarNameS(name) if name == keywords.self => true
          case _ => false
        }) => structId
    }
  }
```

The self-struct path requires **both** `function.lift = true` **and** a
parameter literally named `keywords.self` (= `"self"`).

Looking at the macro (StructDropMacro.scala line 91–111), the FunctionA is
constructed with:
- `lift = true` (position 110 — passes the first check)
- parameter named `keywords.thiss` (= `"this"`) — **fails the name check**

So `maybeSelfStructId = None`, and assembleName falls to:
```scala
case None => parentId.addStep(...)
```
where `parentId = runedEnv.parentEnv.id` — the struct's parent-env id, whose
localName is `StructTemplateNameT(name)` (bare template form). `addStep`
produces the broken-shape `dropNameT.initSteps = Vector(StructTemplateNameT)`.

User-written drops (e.g. `func drop<T>(self XSome<T>) void`) take the param
literally named `self`, so they hit `Some(selfStructId)` and use the self
param's `StructTT.id` as the parent. That id is in instantiation form
(`StructNameT(template, [placeholders])` for both generic and non-generic
structs — non-generic just has empty placeholders). Hence user-written drops
have the correct shape.

### Why @SMLRZ documents this differently

`Frontend/docs/arcana/StructMethodLiftRules-SMLRZ.md` says:
- (line 80–83) "Generated drop functions are not lifted. They use `addStep`
  to nest under the struct's namespace. ... Drop functions don't have a
  `self` parameter (they use `thiss`)"
- (line 87) "`NameHammer.simplifyName` only handles `StructNameI`. ... It
  does NOT handle bare `StructTemplateNameI` — that would hit `vimpl(other)`
  and crash. This is why the function ID must contain `StructNameI`."

The two statements contradict each other in practice: doc-says-drop-is-not-
lifted-and-uses-thiss + doc-says-the-id-must-be-StructNameI ⇒ doc-says-do-the-
broken-thing. The current code follows the first half of the doc and produces
exactly the shape the second half says will crash.

### Generalized fix (no special-casing of non-generic)

Per the user's principle ("non-generic things are generic things with zero
generic params; never special-case"), the fix should make every drop go
through the same id-construction logic. There are two equivalent ways:

**Option A — make the macro use `self`:** Change
`StructDropMacro.scala#getStructSiblingEntries` to construct the param with
`keywords.self` instead of `keywords.thiss`. The drop body uses
`ArgLookupTE(0, …)` (positional, not by name — see `generateFunctionBody`
line 226), so the rename is body-safe. With `lift = true` already set, the
drop now flows through `assembleName`'s self-struct path uniformly with
user-written struct methods. For non-generic structs the self struct's
`StructTT.id` is `StructNameT(template, Vector())` (empty args); for generic
structs it's `StructNameT(template, [placeholders])`. Same code, both work.

**Option B — make `assembleName` independent of param name:** Drop the
`self`-name check; detect "is this a struct method" purely by whether `lift =
true` and there's a `StructTT` self-param at index 0. Same effect as A,
broader change. Risk: changes a general rule the rest of the typing pass
might rely on.

A is the smaller, more targeted change that matches @SMLRZ's intent (drop
should still register where it currently does — only the id shape changes,
which the doc itself says is required).

The macro's `dropNameT` (the env-registration key returned by the macro) is
a separate concern. It's currently `structName.addStep(funcName)`, which
also has the broken shape. Either: (i) leave that broken-shape key for env
registration since `funcT.header.id` is what the rest of the pipeline cares
about, or (ii) fix it the same way (also requires either the rename or the
self-param-by-position approach).

### Followups to verify before fixing

- Sanity-check that `StructConstructorMacro` doesn't have the same issue.
  Constructors take an output struct, not a self-param, so the path is
  likely different — but worth a glance.
- The doc @SMLRZ will need updating to reflect "macro-generated drops use
  `self` and flow through assembleName's lifted path uniformly with
  user-written struct methods" once the fix lands.

### Round 4 — context from existing docs/investigations

User asked to look at `docs/` and `investigations/` for prior writings on
this topic before fixing. Relevant findings:

**`Frontend/docs/arcana/StructMethodLiftRules-SMLRZ.md`** is the primary
arcana doc. It already documents the contradiction we hit:
- (line 14) lists `StructDropMacro.scala:113 — drop function generation
  (bypasses FunctionScout, uses addStep)` as one of the @SMLRZ sites.
- (line 80–83) "Generated drop functions are not lifted. They use `addStep`
  to nest under the struct's namespace. ... Drop functions don't have a
  `self` parameter (they use `thiss`)."
- (line 87) "`NameHammer.simplifyName` only handles `StructNameI`. ... It
  does NOT handle bare `StructTemplateNameI` — that would hit `vimpl(other)`
  and crash. **This is why the function ID must contain `StructNameI`** ..."

The two prescriptions contradict in practice. The current code follows the
"use addStep / use thiss / not lifted" half and produces the
`StructTemplateNameI` shape the doc itself says will crash.

**`Frontend/docs/historical/2026-04-16-regression-fixes-and-rust-interop-discovery.md`**
documents the Group G work that introduced the current `lift` mechanism.
"Bug 3: Header and prototype IDs diverged for lifted struct methods" was
fixed by moving struct-extraction + arg-stripping into `assembleName`
(Option 4 of 6). That fix addressed **user-defined** lifted struct methods
(those with `self` param) — bringing their header.id and prototype.id into
agreement. **It did not touch the macro-derived drop path.** The macro path
was left producing the inconsistent shape that we're now hitting through a
different downstream (the Instantiator's `assemblePlaceholderMap`).

**`docs/arcana/PlaceholdersNamedByDenizenTemplate-PNBDTZ.md`** (newer)
documents the placeholder-naming convention specifically for **user-defined**
lifted drop. It treats user-defined `func drop(self Ok<T,E>)` as a free
function whose root denizen is `drop` itself; the function's runtime ID
becomes `[Ok<T,E>, drop(args)]`. This confirms the lifted path produces
instantiation-form prefixes for both generic and non-generic structs.

**`investigations/vec-capacity-integration.md`** is the prior investigation
that led to Bug 3's fix. Same shape concerns, different downstream.

**`investigations/anonymous-substruct-cluster.md`** addressed an unrelated
`inherited` filter bug for interface anon substructs.

### Reconciling the doc with the fix

@SMLRZ as written prescribes contradictory things; the current macro
follows the broken half. Fixing this requires updating @SMLRZ alongside the
code. Concretely:

- Lines 80–83: rephrase to say drops uniformly flow through `assembleName`'s
  lifted path, with `self` as the param name. The "not lifted" wording is a
  vestige from before Group G — at that point `lift=true` had a different
  meaning, and "drop is not lifted" was meaningful. Today, post-Group G,
  `lift=true` + `self` param is what the path is named, and drop should use
  it.
- Line 14: still correct (the file still touches drop generation), but the
  fix is the param rename (Option A in Round 3) plus a corresponding
  rewording of the surrounding text.

### Recommendation (unchanged from Round 3)

**Option A — rename the macro's param from `thiss` to `self`** is the
right fix:
1. The macro already sets `lift = true`. Renaming just the param name
   makes `assembleName`'s self-detection succeed.
2. The drop body reads the param positionally (`ArgLookupTE(0, …)` in
   `generateFunctionBody`), so the rename doesn't affect codegen.
3. `assembleName`'s self-struct path uses the self param's `StructTT.id` —
   which is `StructNameT(template, [placeholders])` for both generic and
   non-generic structs (non-generic has empty placeholders). General code,
   no special-case for arity.
4. Brings macro-derived drop into the same path that Group G fixed for
   user-defined drops, eliminating the shape divergence the @SMLRZ doc
   itself flagged as required.

Doc updates needed alongside the code change:
- @SMLRZ lines 80–83 ("Generated drop functions are not lifted. ... they
  use `thiss`") — invert.
- @SMLRZ line 14 — keep, but its "uses `addStep`" wording now refers to
  the same `selfStructId.addStep(...)` path that user-defined methods use.
- A short note in `docs/historical/` capturing this finding alongside the
  Group G doc.

### Cleanup

Debug printouts to remove when the fix lands:
- `Instantiator.scala#translatePrototype` (the `DBG translatePrototype` and
  `DBG translatePrototype-enqueue DROP` prints)
- `StructDropMacro.scala#getStructSiblingEntries` (the `DBG StructDropMacro`
  prints)

(Stopping here per skill rules. No fix code applied.)

