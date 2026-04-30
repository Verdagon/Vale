# Group G: Identifiability check fails for non-lifted struct methods

## Collapsed call tree

```
- CompilerTestCompilation.test("extern struct Vec<T> imm { extern func with_capacity(c i64) Vec<T>; ... }")
  - FunctionScout#scoutFunction(with_capacity):
    lift=false (no `self` param)
    genericParametersS=Vector() (empty — parent T excluded because lift=false)
    rulesArray after RuneParentEnvLookupSR filter:
      - MaybeCoercingLookupSR(i64)
      - MaybeCoercingLookupSR(Vec)  
      - MaybeCoercingCallSR(Vec, [CodeRuneS(T)])  ← references T from return type Vec<T>
    identifyingRunes=Vector() (empty)
    - PostParser#checkIdentifiability(identifyingRunes=[], rules=[...referencing T]):
      - IdentifiabilitySolver: T appears in rules but not in identifying runes → FAIL
```

## Root cause

For non-lifted struct methods (`lift=false`), the parent's generic params (like `T`) are NOT in `genericParametersS` — by design, because the method lives in the parent's inner environment and inherits `T` from there. But `T` still appears in the rules (from `Vec<T>` in the return type via `MaybeCoercingCallSR`). The identifiability solver sees `T` referenced in rules but not in the identifying runes and fails.

The `RuneParentEnvLookupSR` filter we added helps (removes the explicit "T must be identifying" rule), but `T` still gets pulled in via the type rules for `Vec<T>`.

## Fix

Skip `checkIdentifiability` for non-lifted citizen methods. They don't need standalone identifiability — their parent runes are resolved from the parent environment at compile time (OverloadResolver preprocesses `RuneParentEnvLookupSR` rules into initial knowns).

Lifted methods DO need the check because they're compiled in the outer environment with their own explicit generic params.
