# Identifiability Checks Include Parent Citizen Runes (ICIPCRZ)

The `checkIdentifiability` call in `FunctionScout` must include the parent citizen's generic parameters as known identifying runes, even for non-lifted methods where those parameters are not in `genericParametersS`. Parent runes are always provided at the call site via struct type args (e.g. `Vec<int>.with_capacity(42i64)` provides `T=int`), making them a valid input avenue for identifiability purposes.

## Where

- `PostParsingPass/.../FunctionScout.scala` — the `checkIdentifiability` call (~line 620) passes `genericParametersS ++ extraGenericParamsFromParentS` as identifying runes
- `PostParsingPass/.../FunctionScout.scala` — the `rulesArray` filtering (~line 596) removes `RuneParentEnvLookupSR` rules before identifiability checking
- `PostParsingPass/.../IdentifiabilitySolver.scala` — `RuneParentEnvLookupSR.getPuzzles` returns empty Vector (unsolvable), and `solveRule` hits `vimpl()` (unimplemented)
- `PostParsingPass/.../rules/TemplexScout.scala:38` — creates `RuneParentEnvLookupSR` rules when a rune is looked up from the parent environment
- `TypingPass/.../OverloadResolver.scala:314` — preprocesses `RuneParentEnvLookupSR` rules into initial knowns at call resolution time (the runtime equivalent of what identifiability models statically)

## Cross-cutting effect

Three things must stay in sync:

1. **Parent runes as identifying inputs.** `checkIdentifiability` receives `extraGenericParamsFromParentS` runes so the solver knows they'll be available. Without this, non-lifted methods that reference parent generics (like `with_capacity(c i64) Vec<T>` inside `extern struct Vec<T>`) fail identifiability because `T` appears in rules but not in identifying runes.

2. **RuneParentEnvLookupSR rules filtered out.** These rules must be removed from `rulesArray` before the identifiability check because `IdentifiabilitySolver.solveRule` for `RuneParentEnvLookupSR` hits `vimpl()`. This applies to both struct and interface citizen methods.

3. **Lift doesn't affect identifiability of parent runes.** Whether a method is lifted or not is about compilation environment and naming (see @SMLRZ). Identifiability is about whether runes can be determined at the call site. Parent runes are always determinable because `Vec<int>.method()` always provides `T=int`, regardless of lift.

## Why it exists

Identifiability checks verify that every rune in a function's signature can be determined from inputs at the call site. For standalone functions, these inputs are the function's own generic params. For citizen methods, there's an additional input avenue: the parent's type args provided through the qualified call syntax (`Vec<int>.method()`). The identifiability solver has no built-in concept of "parent environment" — it only knows about identifying runes and rules. So parent runes must be explicitly added to the identifying runes set to model this input avenue.

The `RuneParentEnvLookupSR` filtering is a separate concern: these rules exist for the TypingPass solver (which preprocesses them via environment lookup in OverloadResolver), but the IdentifiabilitySolver has no environment to look up from and its `solveRule` implementation is `vimpl()`. Filtering them is safe because their only purpose — declaring that a rune comes from the parent — is already captured by including the parent runes in the identifying set.

## See also

- @SMLRZ — Struct Method Lift Rules (controls compilation environment and naming, orthogonal to identifiability)
