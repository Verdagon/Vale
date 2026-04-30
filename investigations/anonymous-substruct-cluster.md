# Anonymous Substruct Cluster Investigation

## Background

The `GenericParameterS` case class gained an `inherited: Boolean` field in ValeRustInterop (not present in Sylvan). This field marks whether a generic parameter was inherited from a parent citizen (struct/interface) rather than declared by the function itself. Three code paths used `.inherited` at the time of investigation:

1. `FunctionScout.scala:34` — `ParentCitizen` constructor: `vassert(x.inherited)` — validates all params passed to parent are marked inherited.
2. `FunctionCompilerCore.scala:284` — struct-method branch: `filter(!_._1.inherited)` — strips inherited template args from the function name (struct args come from `selfStructId`).
3. `FunctionCompilerSolvingLayer.scala:660` — `addRunedDataToNearEnv`: was `filter(!_.inherited)`, now removed.

## The core fix that resolved the cluster

**`FunctionCompilerSolvingLayer.scala:660`**: Removed `filter(!_.inherited)` from the rune list passed to `addRunedDataToNearEnv`. This filter was new (Sylvan has no `inherited` field, so no filter). For interface methods where ALL generic params are inherited (they come from the parent interface), this filter stripped ALL runes, causing:

- `runedEnv.templateArgs` to be empty
- `FunctionNameT` to be built with zero `templateArgs`
- `FunctionHeaderT` constructor assertion to crash with `IndexOutOfBoundsException: 0` at `ast.scala:369`, because it tried to verify placeholders exist in `templateArgs` by index, but the vector was empty

Removing the filter restores Sylvan's behavior: all generic params (inherited or not) are added to the near env.

## Tests in this cluster — status after all fixes

| Test | Status | Error | Root Cause |
|------|--------|-------|------------|
| Generic interface anonymous subclass | **FAILING** | `MatchError: PredictedFunctionNameT(...)` at `CompilerErrorHumanizer.scala:693` | Missing match case in `humanizeName` (see below) |
| Can turn a borrow coord into an owning coord | **FAILING** | `vimpl()` at `AfterRegionsTests.scala:140` | Pre-existing test stub — identical `vimpl()` at same line in Sylvan |
| Impl rule | **FAILING** | `FindFunctionResolveFailure: Couldn't find getFuel(&Kind$T)` | Pre-existing — Sylvan comment says "Depends on Method call on generic data" (also pre-existing) |
| Integer is compatible with interface anonymous substruct | **PASSING** | — | Fixed by SolvingLayer inherited-filter removal |
| Lambda is compatible with interface anonymous substruct | **PASSING** | — | Fixed by SolvingLayer inherited-filter removal |
| Basic IFunction1 anonymous subclass | **PASSING** | — | Fixed by SolvingLayer inherited-filter removal |
| Reports error | **FAILING** | `Couldn't find an override: foo(&B)` | Pre-existing — Sylvan marks this "does not pass yet, use #[ignore]". Depends on "Basic interface anonymous subclass" |
| Report when downcasting to interface | **FAILING** | `vimpl()` at `AfterRegionsErrorTests.scala:144` | Pre-existing test stub — identical in Sylvan with "does not pass yet" comment |
| Report when downcasting between unrelated types | **FAILING** | `Couldn't find as(&ISpaceship)` then `vimpl()` at line 181 | Pre-existing — Sylvan marks "does not pass yet" with `vimpl()` at line 181 |
| Lambda is incompatible anonymous interface | **FAILING** | `vwat: HinputsT#()` at `AfterRegionsErrorTests.scala:213` | Pre-existing — Sylvan marks "does not pass yet" at line 217. Test expects compilation to fail but it succeeds |
| Lambda is compatible anonymous interface | **PASSING** | — | Fixed earlier (InterfaceA vassert fix) |
| Reports when ownership doesnt match | **FAILING** | `FindFunctionResolveFailure` then `vfail` at `AfterRegionsTests.scala:304` | Pre-existing — Sylvan has `vimpl()` at line 227 on the same result handling |

## Detailed error analysis

### "Generic interface anonymous subclass" — CompilerErrorHumanizer MatchError

**Stack trace:**
```
scala.MatchError: PredictedFunctionNameT(
  PredictedFunctionTemplateNameT(StrI(drop)),
  Vector(),
  Vector(CoordT(share, RegionT(DefaultRegionT), StructTT(...LambdaCitizenNameT...))))
  at CompilerErrorHumanizer$.humanizeName(CompilerErrorHumanizer.scala:693)
```

**What happens:** The test compiles `Bork((x) => { 7 })` — an anonymous substruct of interface `Bork<T>` constructed from a lambda. During compilation, a drop function prototype with `PredictedFunctionNameT` is encountered. When the compiler produces an error message (for some solver failure during drop resolution), `CompilerErrorHumanizer.humanizeName` hits a `MatchError` because `PredictedFunctionNameT` is not in its match (line 693-779).

**`PredictedFunctionNameT` definition** (`names.scala:415-430`):
```scala
// PredictedFunctionNameT and PredictedFunctionTemplateNameT are special names similar to
// FunctionNameT/FunctionTemplateNameT but used when the compiler predicts what a function's
// name will be before it's actually compiled.
case class PredictedFunctionTemplateNameT(humanName: StrI) extends IFunctionTemplateNameT {
  override def makeFunctionName(...): PredictedFunctionNameT = { ... }
}
case class PredictedFunctionNameT(
  template: PredictedFunctionTemplateNameT,
  templateArgs: Vector[ITemplataT[ITemplataType]],
  parameters: Vector[CoordT]) extends IFunctionNameT { ... }
```

**Same bug in Sylvan:** `PredictedFunctionNameT` exists in Sylvan (`names.scala`) but is also missing from Sylvan's `humanizeName`. The bug is pre-existing but only triggered when compilation reaches a code path that uses predicted function names in error messages. ValeRustInterop reaches this path more often due to the `lift` and `inherited` changes.

**Fix:** Add to `CompilerErrorHumanizer.humanizeName` at line 693:
```scala
case PredictedFunctionNameT(template, templateArgs, parameters) => {
  humanizeName(codeMap, template) +
    humanizeGenericArgs(codeMap, templateArgs, None) +
    "(" + parameters.map(CoordTemplataT).map(humanizeTemplata(codeMap, _)).mkString(", ") + ")"
}
case PredictedFunctionTemplateNameT(humanName) => humanName.str
```

### "Impl rule" — Method resolution on generic type

**Vale source:**
```vale
interface IShip { func getFuel(virtual self &IShip) int; }
struct Firefly {}
func getFuel(self &Firefly) int { return 7; }
impl IShip for Firefly;

func genericGetFuel<T>(x T) int where implements(T, IShip) {
  return x.getFuel();
}
```

**Error:** `Couldn't find a suitable function getFuel(&Kind$genericGetFuel.T)` — the method call on a generic-bounded type `T` can't find `getFuel` even though `T implements IShip` is declared.

**Pre-existing in Sylvan:** The comment above the test in Sylvan says "Depends on Method call on generic data", which is another test that fails in both codebases. This is a known limitation of method resolution on abstract/generic types.

### "Reports error" — Override resolution failure

**Vale source:**
```vale
interface A { func foo(virtual a &A) int; }
struct B imm { val int; }
impl A for B;
func foo(b &B) int { return b.val; }
```

**Error:** `Couldn't find an override: Couldn't find a suitable function foo(&B)`

**Pre-existing in Sylvan:** Marked "does not pass yet, use #[ignore]". Depends on "Basic interface anonymous subclass". The impl override resolution for `B`'s `foo` can't match against the interface's virtual `foo` because of some type-checking nuance with immutable structs.

## Summary

| Category | Count | Action Needed |
|----------|-------|---------------|
| Fixed by SolvingLayer change | 3 | None — now passing |
| Fixed earlier (InterfaceA vassert) | 1 | None — now passing |
| Pre-existing test stubs (vimpl/vfail in test body) | 5 | Mark as `ignore` |
| Pre-existing "does not pass yet" | 2 | Mark as `ignore` |
| Missing CompilerErrorHumanizer match case | 1 | Add `PredictedFunctionNameT` case |

**Net result:** 4 tests fixed, 7 pre-existing (should be `ignore`d), 1 fixable regression (humanizer match case).
