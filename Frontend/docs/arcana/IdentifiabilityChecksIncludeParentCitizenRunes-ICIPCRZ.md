# Identifiability Checks Include Parent Citizen Runes (ICIPCRZ)

The `checkIdentifiability` call in `FunctionScout` must include the parent citizen's generic parameters as known identifying runes. Parent runes are always provided at the call site via the qualified container syntax (e.g. `Vec<int>.with_capacity(42i64)` provides `T=int`), making them a valid input avenue for identifiability purposes.

## Where

- `PostParsingPass/.../FunctionScout.scala` — the `checkIdentifiability` call passes `genericParametersS ++ extraGenericParamsFromParentS` as identifying runes (`extraGenericParamsFromParentS` are also prepended to the function's own `genericParameters` per `extraGenericParamsFromParentS ++ functionUserSpecifiedGenericParametersS ++ extraGenericParamsFromBodyS`).
- `PostParsingPass/.../FunctionScout.scala` — the `rulesArray` filtering removes `RuneParentEnvLookupSR` rules before identifiability checking when the function is a citizen method.
- `PostParsingPass/.../IdentifiabilitySolver.scala` — `RuneParentEnvLookupSR.getPuzzles` returns empty Vector (unsolvable), and `solveRule` hits `vimpl()` (unimplemented).
- `PostParsingPass/.../rules/TemplexScout.scala` — creates `RuneParentEnvLookupSR` rules when a rune is looked up from the parent environment.
- `TypingPass/.../OverloadResolver.scala` — preprocesses `RuneParentEnvLookupSR` rules into initial knowns at call resolution time (used by paths that don't go through the explicit-callsite-template-args channel below).
- `TypingPass/.../expression/ExpressionCompiler.scala` — for qualified-container calls (`Vec<int>.foo(...)`), constructs `containerInitialKnowns` from the resolved container's template args and passes them via `extraInitialKnowns` through the call chain. This is the runtime mechanism that justifies the parent runes' identifiability.

## Cross-cutting effect

Three things stay in sync:

1. **Parent runes as identifying inputs.** `checkIdentifiability` receives `extraGenericParamsFromParentS` runes so the solver knows they'll be available. Without this, internal methods that reference parent generics (like `with_capacity(c i64) Vec<T>` inside `extern struct Vec<T>`) fail identifiability because `T` appears in rules but not in identifying runes.

2. **`RuneParentEnvLookupSR` rules filtered out for citizen methods.** These rules must be removed from `rulesArray` before the identifiability check because `IdentifiabilitySolver.solveRule` for `RuneParentEnvLookupSR` hits `vimpl()`. The runtime side instead delivers parent runes via `extraInitialKnowns` from `ExpressionCompiler` (see below) — no `RuneParentEnvLookupSR` rule is needed at the identifiability layer.

3. **The runtime mechanism: explicit callsite container template args.** For a qualified call `Vec<int>.foo(...)`, the postparser's `OutsideLoadSE` carries an `explicitArgsByTemplate: Map[IImpreciseNameS, Vector[RuneUsage]]` containing the user-written args per container in the syntactic chain. `ExpressionCompiler` resolves the container, then zips `structDef.instantiatedCitizen.id.localName.templateArgs` (placeholder form, with rune embedded) positionally with the callsite arg-runes to build `containerInitialKnowns: Vector[InitialKnown]`. These flow through to `assembleKnownTemplatas` and seed the parent runes (`T = int` etc.) directly. This is the channel that makes parent runes identifiable — they're supplied by the callsite's syntactic prefix.

## Why it exists

Identifiability checks verify that every rune in a function's signature can be determined from inputs at the call site. For standalone functions, these inputs are the function's own generic params. For citizen methods, there's an additional input avenue: the parent's type args provided through the qualified call syntax (`Vec<int>.method()`). The identifiability solver has no built-in concept of "parent environment" — it only knows about identifying runes and rules. So parent runes must be explicitly added to the identifying runes set to model this input avenue.

The justification has shifted across the SMLRZ rollback:

- **Pre-rollback**: parent runes were identifiable because they were "inherited" into the function's `genericParameters` (and the `outerEnv.id.initId` walk-up in `assembleKnownTemplatas` smuggled them into the runtime solver via the function's defining env).
- **Post-rollback** (current): parent runes are identifiable because the **callsite** must syntactically supply them via the container prefix (`Vec<int>.foo`), and `ExpressionCompiler` extracts them into `containerInitialKnowns` for the solver. The function's `genericParameters` still includes them (for solver visibility within the function), but the runtime binding flows from the callsite, not from any inheritance/walk-up mechanism.

The input set the identifiability solver checks is unchanged (still `extraGenericParamsFromParentS` runes); only the runtime mechanism for actually delivering those runes' values changed. The identifiability check models the same predicate — "can the callsite supply this rune?" — under both regimes.

The `RuneParentEnvLookupSR` filtering is a separate concern: these rules exist for some non-citizen-method paths in TypingPass (which preprocesses them via environment lookup in OverloadResolver), but the IdentifiabilitySolver has no environment to look up from and its `solveRule` implementation is `vimpl()`. Filtering them for citizen methods is safe because their identifiability is already captured by including parent runes in the identifying set, and the runtime binding flows through the explicit-callsite channel rather than through env lookup.

## See also

- `expressions.scala`'s `OutsideLoadSE` — the postparser carrier with `containerLookups` (rules + result rune per container) and `explicitArgsByTemplate` (positional args per template).
- `ExpressionCompiler.scala`'s qualified-container resolution path — builds `containerInitialKnowns` from the resolved structDef's template args.
- `FunctionCompilerSolvingLayer.assembleKnownTemplatas` — master-form one-liner that zips `function.genericParameters` with `explicitTemplateArgs`; appends `extraInitialKnowns` from the callsite.
