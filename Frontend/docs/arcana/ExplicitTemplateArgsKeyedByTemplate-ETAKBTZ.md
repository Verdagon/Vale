# Explicit Template Args Keyed By Template (ETAKBTZ)

When a callsite supplies explicit template args — whether on the function itself (`f<i32>()`), on a container prefix (`Vec<int>.foo()`), or on multiple steps in a syntactic chain (`Outer<i32>.Inner<bool>.spork<str>()`) — the carrier is a **map keyed by template**:

```
PostParser stage:  Map[IImpreciseNameS, Vector[RuneUsage]]
TypingPass stage:  Map[IdT[ITemplateNameT], Map[IRuneS, ITemplata]]
```

Outer key: which template the args belong to (the function template, or any citizen template along the syntactic chain). Inner: the args themselves (positional at postparser, rune-keyed after typing-pass resolution).

It is **not** a flat `Vector[ITemplata]`.

## The reason: default arguments

Generic templates can declare default values for trailing template parameters:

```vale
struct Outer<Q, P = List<Q>> { ... }
```

A callsite like `Outer<i32>.Inner<bool>.spork()` supplies `Q = i32` for `Outer` but elides `P`. Conceptually:

```
{Outer: {Q: i32}, Inner: {T: bool}}
```

`P` is absent from `Outer`'s inner map; the default mechanism fills it in during solving. The same holds for any rune that has a default: just leave it out of the inner map and the solver supplies the default.

A flat vector can't represent this without out-of-band metadata. `[i32, bool]` for a chain expecting `[Q, P, T]` either drops a value (which one?) or needs a sentinel like `[i32, _, bool]` plus a separate "which positions are user-supplied" mask. Either workaround is a layer of representation the map structure makes unnecessary.

## Composition

The same shape supports two adjacent features without redesign:

**Overload-set values** (future). Today `zork(Vec<i32>.foo)` (passing a method as a function reference) isn't supported. When it is, the function reference's type carries a partial map — `{Vec: {T: i32}}` — with `foo`'s own runes still free to be supplied at the eventual call. Same shape as a call-site map, just with a missing leaf entry.

**Typed aliases** (future). `alias Bytes: Something = Vec<u8>` introduces an imprecise name `Bytes` that resolves through the alias to `Vec<u8>`. At the callsite `Bytes.foo()`, the postparser carries `Bytes` as the outer-map key; typing-pass alias resolution maps it through to `Vec`'s template id. Aliases are not a special case — they're just one more kind of `IImpreciseNameS` that the map's outer key can hold.

The Map composes with both because its outer key is "which template" — agnostic to whether that template is a function, a struct, an alias, or part of a function reference's partial application. A flat Vector commits to "args belong to one specific known template" up front, and would need a redesign for either feature.

## Stages

**At the postparser** (`OutsideLoadSE.explicitArgsByTemplate` — see `expressions.scala`), the outer key is `IImpreciseNameS`. We don't know yet which concrete template a name resolves to (or whether it's an alias). The inner is positional: `Vector[RuneUsage]`. The map only contains entries for templates the user wrote args on — absences are normal.

**At the typing pass** (after container resolution in `ExpressionCompiler` — see `containerInitialKnowns` construction), the outer key has been resolved to `IdT[ITemplateNameT]`. The inner becomes rune-keyed: `Map[IRuneS, ITemplata]`. The runes are the *target* template's own runes (e.g. `Outer`'s `Q`), not the callsite's local runes. Resolving the positional vector into a rune-keyed map is what `ExpressionCompiler` does by zipping `structDef.instantiatedCitizen.id.localName.templateArgs` (placeholder form, with rune embedded) with the callsite arg-runes.

## What goes through this channel

- Function-level template args (`f<i32>()` — keyed by `f`'s name).
- Container template args at the callsite prefix (`Vec<int>.foo()` — keyed by `Vec`'s name).
- Multi-step nested chains (`Outer<i32>.Inner<bool>.spork<str>()` — three entries, one per step).

Each entry contributes `InitialKnown`s to the function compile's solver (via the `extraInitialKnowns` parameter on the `FunctionCompiler` call chain), seeding parent-citizen runes that the function inherited via `extraGenericParamsFromParentS`. This is the runtime delivery of the parent runes whose identifiability is asserted by @ICIPCRZ.

## See also

- @ICIPCRZ — Identifiability Checks Include Parent Citizen Runes (the predicate this map's runtime mechanism satisfies; the map is *how* the callsite supplies parent runes).
- `OutsideLoadSE` in `PostParsingPass/.../expressions.scala` — the postparser carrier with `containerLookups` (rules + result rune per container) and `explicitArgsByTemplate`.
- `ExpressionCompiler.scala`'s qualified-container resolution path — builds `containerInitialKnowns` by zipping the resolved template's placeholder structure with the callsite's positional arg-runes.
- `FunctionCompilerSolvingLayer.assembleKnownTemplatas` — the master-form one-liner that zips `function.genericParameters` with `explicitTemplateArgs`; the call-site path appends `extraInitialKnowns` derived from this map.
