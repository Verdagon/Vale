# Cluster #1: vassertSome at Instantiator translateCoord

Failure: `Assertion failed! Expected non-empty!` at `Instantiator.scala:3195` —
the double `vassertSome(vassertSome(substitutions.get(placeholderId.initId)).get(placeholderId))`
inside `translateCoord`'s `KindPlaceholderT` arm.

## Triggering tests

Out of 14 occurrences in `tmp/trace.txt`, distribution by test:

- `downcastBorrowFailed` × 12 (4 stacks × naive-rc, resilient-v3, unsafe-fast)
- `stdlib_list` × 2

`downcastBorrowFailed.vale` is the simplest repro:

```vale
sealed interface IShip  {}
struct Serenity {}
impl IShip for Serenity;
struct Raza { fuel int; }
impl IShip for Raza;

func moo(ship IShip) int { ... maybeRaza = ship.as<Raza>() ... }
exported func main() int { moo(Serenity()) }
```

The crash fires while compiling `vtest / drop` (and also `vtest / constructor<Serenity>`).
Stack from trace.txt:

```
translateCoord(Instantiator.scala:3195)
translateTemplata(Instantiator.scala:3550)         // PlaceholderTemplataT or CoordTemplataT case
translateName$anonfun$1(Instantiator.scala:3815)    // templateArgs.map(translateTemplata(...))
translateName(Instantiator.scala:3815)              // StructNameT(template, templateArgs) arm
translateFunctionId$anonfun$1(Instantiator.scala:3013)  // steps.map(translateName(...))
translateFunctionId(Instantiator.scala:3013)
translateCollapsedFunction(Instantiator.scala:1795)
translateFunction(Instantiator.scala:937)
```

So we're translating one of the *parent steps* of the function id (the step
prefix above the function name), and that parent step is a `StructNameT` whose
`templateArgs` contain a `KindPlaceholderT`. That placeholder's
`placeholderId.initId(interner)` is not present in `substitutions`.

## Open questions

- Which `StructNameT` is the parent step?
- What's the placeholder's `initId`?
- What key(s) ARE in `substitutions`?
- Was `translateFunction` called on the right `topLevelDenizenTemplateId`?

Per the `LHPCTLD` comment in Instantiator (lines 415, 532, 916, 3193): "only the
top level denizen has placeholders". So if a parent step has placeholders, its
`initId` should equal the top-level denizen template id. Either:
1. The parent step is itself the top-level denizen and the lookup *should*
   work — but a different (bug-introducing) denizen got chosen as top level.
2. The placeholder's `initId` is foreign (came from a different denizen) — a
   stale/cross-contaminated placeholder.

## Plan

1. Instrument `translateCoord` and `translateFunction` (single-test scope is fine
   since downcastBorrowFailed is the dominant trigger).
2. Run via Tester invocation directly:
   `java -cp Frontend/Frontend.jar dev.vale.passmanager.PassManager build --output_dir tmp/out --sanity_check true vtest=Frontend/Tests/test/main/resources/programs/downcast/downcastBorrowFailed.vale`
3. Capture and analyze.

## Findings (after first instrumentation run)

### What's being translated

A `drop` function id of shape:

```
desiredPrototypeT.id =
  IdT(_, Vector(StructNameT(StructTemplateNameT(Ok), [<placeholders>])),
         FunctionNameT(FunctionTemplateNameT(drop@result.vale:558), [<placeholders>], [<params>]))
```

I.e. a method `drop` lifted into the `Ok` struct's id-prefix per @SMLRZ. The
placeholders inside both the `StructNameT` templateArgs and the `FunctionNameT`
templateArgs are all rooted (`placeholderId.initId`) at:

```
IdT(_, Vector(), FunctionTemplateNameT(drop@result.vale:558))
```

— i.e. the function's own template id.

### What `substitutions` looks like

`translateFunction` builds `substitutions` as:

```scala
val topLevelDenizenId = getTopLevelDenizenId(desiredPrototypeT.id)        // first IInstantiationNameT step
val topLevelDenizenTemplateId = TemplataCompiler.getTemplate(topLevelDenizenId)
val substitutions = Map(
  topLevelDenizenTemplateId -> assemblePlaceholderMap(funcT.header.id, desiredPrototypeS.id))
```

Per `getTopLevelDenizenId`, the *first* IInstantiationNameT step in
`desiredPrototypeT.id` is `StructNameT(Ok, …)`. Therefore:

```
topLevelDenizenTemplateId = IdT(_, Vector(), StructTemplateNameT(Ok))
```

But the inner map (built by `assemblePlaceholderMap`) contains placeholders
keyed by their actual `IdT[IPlaceholderNameT]`, all rooted at
`FunctionTemplateNameT(drop@558)`. From the trace:

```
substitutions inner keys for top =
  Set(IdT(_, Vector(FunctionTemplateNameT(drop@558)), KPNT(0, OkType)),
      IdT(_, Vector(FunctionTemplateNameT(drop@558)), KPNT(1, ErrType)))
```

### The mismatch

`translateCoord`'s lookup is two-level:

```scala
substitutions.get(placeholderId.initId(interner)).get(placeholderId)
```

For our placeholder, `placeholderId.initId = IdT(_, Vector(), FunctionTemplateNameT(drop@558))`.
But `substitutions` only has the key `IdT(_, Vector(), StructTemplateNameT(Ok))`.
The outer `get` returns `None` → outer `vassertSome` throws "Expected non-empty".

The inner map is *correct* — it contains exactly the placeholder we want to
look up. Only the OUTER key is wrong.

### Root cause

Interaction of @SMLRZ ("lifted" methods) with @LHPCTLD ("only top-level denizen
has placeholders") and `getTopLevelDenizenId`:

- `getTopLevelDenizenId` picks the *first* `IInstantiationNameT` step in the id.
- For a lifted method, the first such step is the **lifting parent struct**
  (`StructNameT(Ok, …)`), because @SMLRZ's `assembleName` synthesizes that
  prefix.
- But the placeholders themselves were created during typing under the
  *function's* template id (`drop@558`), not the parent struct's template id —
  the lifting prefix only *copies* placeholders, it doesn't *own* them.
- So the outer key derived from `getTopLevelDenizenId` (the struct's template)
  is not what `placeholderId.initId` returns (the function's template) for any
  placeholder in the function's id.

For non-lifted, non-method functions (e.g. `func foo<T>(...)`), the function id
has no struct prefix; `getTopLevelDenizenId` returns the function itself; so
the outer key matches the placeholders' `initId`. The bug only fires when
@SMLRZ-lift kicks in (struct methods) AND the method has its own generic
parameters whose placeholders show up in translated coords (here: `Ok`'s
`drop` is `func drop<OkType, ErrType>(self Ok<OkType, ErrType>) void` — a
generic standalone function lifted into Ok).

### Why this didn't fire before Fixes #1–#3

The previous Fix #2 (`StructDropMacro` parameter rename `thiss` → `self`)
caused the macro-generated `drop` to start being routed through @SMLRZ's lift
path uniformly, increasing the volume of lifted-method ids reaching
`translateCoord`. Fix #2 made the StructNameT prefix arrive in instantiation
form (`StructNameT(template, [placeholders])`) — which previously crashed
earlier in `assemblePlaceholderMap` with `vwat()`. Now that `assemblePlaceholderMap`
runs to completion for these ids, the *consumer-side* mismatch surfaces.

`downcastBorrowFailed.vale` triggers it because `IShip.as<Raza>` desugars to a
Result-returning chain that eventually drops `Ok<&Raza, &IShip>`, hitting the
generic `drop@result.vale:558` defined for `Ok<OkType, ErrType>` — a
double-generic standalone function lifted into Ok. `stdlib_list` triggers it
similarly via stdlib's Result usage.

### Collapsed call tree

```
- Instantiator#translateFunction(desiredPrototypeT.id =
    [StructNameT(Ok, [phOk, phErr]), FunctionNameT(drop@558, ...)]):
    Builds substitutions with outer key = getTopLevelDenizenId(...)
    = StructTemplateNameT(Ok). Inner map is correct (keyed by placeholder ids
    rooted at drop@558).
  - translateCollapsedFunction ... translateFunctionId ...
    translateName(StructNameT(Ok, [phOk, phErr])):
    Calls translateTemplata on each templateArg.
    - translateTemplata(CoordTemplataT(CoordT(_, _, KindPlaceholderT(phOk)))):
      Routes to translateCoord.
      - translateCoord(KindPlaceholderT(phOk)):
        FAIL: substitutions.get(phOk.initId = drop@558_template) = None.
        Outer key in substitutions is StructTemplateNameT(Ok), not drop@558.
```

### Candidate fixes

A. **`getTopLevelDenizenId` should look past lifted-method prefixes.** Detect
   when the first IInstantiationNameT step is a struct-prefix-of-a-lifted-method
   and skip past it. This is the "fix the model of top-level denizen" approach.

B. **Key `substitutions` by the function's own template** in `translateFunction`,
   not by `getTopLevelDenizenId(desiredPrototypeT.id)`. The placeholders rooted
   at the function are the only ones that need looking up here. This is the
   surgical fix: change the outer key in `translateFunction` (line 920–923) to
   `TemplataCompiler.getTemplate(funcT.header.id)`-equivalent (taking the
   function as the denizen), or compute it from the placeholders themselves.

C. **Multi-key the substitutions map**: have `assemblePlaceholderMap` partition
   by `initId` and produce multiple outer entries — one per distinct `initId`
   it discovers. Most general; biggest blast radius.

(B) seems most surgical: per @LHPCTLD, all placeholders share *one* `initId`
(the true top-level denizen), and that `initId` is the placeholders'
`initId`, not whatever `getTopLevelDenizenId` returns when @SMLRZ-lift adds a
struct prefix. We should ask the placeholders, not the prefix.

Sibling translateStruct/translateInterface code paths (lines 405–419 and
522–537) likely have the same latent issue for any struct/interface whose
prefix is itself a lifted-method shape — but I haven't verified that those
shapes actually arise. They MAY be fine because struct/interface ids don't
get lifting prefixes (lift only applies to functions per @SMLRZ).

### Recommendation

Apply candidate (B) in `translateFunction` only, smallest-possible change.
Verify that struct/interface translate paths don't also need it (likely not —
@SMLRZ is function-only). Then re-run full Backend test suite.

**Stopping here for user review before any fix.**

