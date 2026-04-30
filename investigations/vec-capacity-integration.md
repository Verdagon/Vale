# "Extern rust Vec capacity" IntegrationTestsA — Instantiator prototype mismatch

## Error

```
vfail() at Instantiator.translateCollapsedFunction
newHeader.toPrototype != desiredPrototypeC
```

## Collapsed call tree

```
- Instantiator#translateFunction() ... translateCollapsedFunction():
  desiredPrototypeC has FunctionNameIX(capacity, templateArgs=Vector(), params=[Vec<int>])
  newHeader.toPrototype has FunctionNameIX(capacity, templateArgs=Vector(CoordTemplataI(int)), params=[Vec<int>])
  
  The difference: desiredPrototypeC has EMPTY templateArgs (inherited T was stripped by
  FunctionCompilerCore#makePrototype per @SMLRZ). But translateFunctionHeader translates
  from header.fullName which still has the unstripped FunctionNameT with T in templateArgs.
```

## Root cause

Two different IDs for the same function:

1. **Prototype ID** (from `FunctionCompilerCore#makePrototype`): Strips inherited template args per @SMLRZ, producing `FunctionNameT(capacity, Vector(), [Vec<int>])`. This is `desiredPrototypeC`.

2. **Header fullName** (from `FunctionHeaderT.fullName`): The environment-level ID that still has all template args, including inherited ones: `FunctionNameT(capacity, Vector(int), [Vec<int>])`. This is what `translateFunctionHeader` uses to build `newHeader`.

The Instantiator translates from the header's fullName, not the prototype's ID. For lifted struct methods, the prototype has stripped inherited args but the header hasn't — they diverge.

## Detailed mechanism

Two methods in `FunctionCompilerCore` produce different IDs for the same function:

1. **`getFunctionPrototypeForCall` (line 244)** — called from `FunctionCompilerMiddleLayer` (line 381). Detects `self` param + `StructTemplateNameT` in initSteps, strips inherited template args per @SMLRZ, builds prototype with stripped `FunctionNameT`. Returns `PrototypeT(strippedId, returnCoord)`.

2. **`finalizeHeader` (line 308)** — called from `finishCompilingFunctionMaybeDeferred` (line 346). Sets `FunctionHeaderT(fullEnv.id, ...)` using the **raw environment ID** with all template args, including inherited ones.

These two are called for the same function but produce divergent IDs. The prototype says `capacity(templateArgs=[], params=[Vec<int>])` while the header says `capacity(templateArgs=[int], params=[Vec<int>])`.

The Instantiator's `translateCollapsedFunction` translates from `headerT` (unstripped) and compares against `desiredPrototypeC` (stripped). They don't match → `vfail()`.

## Possible fixes

1. **`finalizeHeader` should use the prototype's ID** — pass the already-computed prototype to `finalizeHeader` and use its ID instead of `fullEnv.id`
2. **Strip inherited args in `finalizeHeader` too** — duplicate the stripping logic
3. **Strip at the environment level** — make the function environment's ID already stripped before either method sees it
