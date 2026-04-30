# "Extern rust Vec len" — Forgot to intern FunctionNameT

## Error

```
Forgot to intern: FunctionNameT(FunctionTemplateNameT(StrI(capacity),tvl:73),Vector(),Vector(CoordT(share,...,StructTT(...Vec<int>...))))
```

Crash at `IInterning.equals` during HashMap lookup — an unintern'd `FunctionNameT` is being compared.

## Collapsed call tree

```
- FunctionCompilerCore#makePrototype():
  Hits the StructTemplateNameT + hasSelfParam branch (line 270).
  Strips inherited template args per @SMLRZ (line 292-293).
  Creates new FunctionNameT at line 294: FunctionNameT(template, newTemplateArgs, parameters)
  BUG: This is a raw constructor call, NOT interner.intern(FunctionNameT(...)).
  The unintern'd FunctionNameT goes into newId → PrototypeT → stored in CompilerOutputs.

- Later: HashMap lookup triggers IdT.equals → FunctionNameT.equals → IInterning.equals:
  Checks uniqueId == 0 → vfail("Forgot to intern")
```

## Root cause

`FunctionCompilerCore.scala` line 294 creates `FunctionNameT(template, newTemplateArgs, parameters)` without interning. Should be `interner.intern(FunctionNameT(template, newTemplateArgs, parameters))`.

## Why "Vec capacity" passes but "Vec len" fails

"Vec capacity" calls `capacity` only via `v.capacity()`. The prototype is created once and used directly — never compared via `equals`.

"Vec len" calls `capacity` three ways: `Vec<int>.capacity(v)`, `capacity(v)`, `v.capacity()`. The second/third call looks up the already-compiled function in a HashMap, triggering `equals` on the unintern'd `FunctionNameT`.

## Fix

Line 294 of FunctionCompilerCore.scala: wrap in `interner.intern(...)`. But note: `interner` may not be in scope at this point — need to check.
