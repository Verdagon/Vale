# Placeholders Are Self-Disambiguating (PASDZ)

Per @PNBDTZ, every placeholder is named with a path prefix derived from its owning denizen's template id. That makes a placeholder id (`IdT[IPlaceholderNameT]`) globally unique on its own — `placeholderId.initId(interner)` recovers the owner, and structural equality on the full id distinguishes placeholders across denizens. The Instantiator's substitution maps rely on this in two distinct contexts:

**Lambda bodies — placeholders point at the parent, not the lambda.** Per @LHPCTLD, lambdas don't own their own placeholders; they inherit from the enclosing function.

```vale
func apply<T>(t T) T {
  inner = (x T) => x;
  inner(t)
}
```

When the Instantiator translates `inner`'s body, the lambda is the denizen being compiled, but `T` referenced in the body has prefix `[apply]`, not the lambda's prefix. The id encoding faithfully tracks *ownership* independent of *which denizen we're currently translating*, so a single substitutions map keyed by full id Just Works for nested-scope translation.

**Override dispatchers — multiple denizens' placeholders coexist in one map.** An override dispatcher is a synthetic function the Instantiator pretends exists: it forwards an abstract-function call to the right impl based on the receiver's concrete type. Conceptually it's an inter-impl `match` statement:

```vale
interface ISpaceship<A, B, C> { }
abstract func launch<A, B, C>(self &ISpaceship<A, B, C>, bork int);
struct Raza<Y, Z> { }
impl<Y, Z> ISpaceship<Y, Z, bool> for Raza<Y, Z>;
struct Milano<W, Q> { }
impl<W, Q> ISpaceship<int, W, Q> for Milano<W, Q>;
```

Pretend-form:

```vale
func dispatcher<I, J, K>(self &ISpaceship<I, J, K>, bork int) {
  match self {
    raza &Raza<Y, Z>   => launch(raza, bork)        // case 1: Raza's impl, brings Y, Z
    milano &Milano<W, Q> => launch(milano, bork)    // case 2: Milano's impl, brings W, Q
  }
}
```

Each case is conceptually a small function in its own right. The dispatcher's own generics `I, J, K` are determined by the caller's call signature, but each case can bring in **independent runes** that the dispatcher can't see — `Y, Z` for the Raza case, `W, Q` for the Milano case — because the dispatcher's signature doesn't constrain them (e.g. `Raza<Y, Z>` matches `ISpaceship<I, J, bool>` for *any* `Y, Z` such that `Y=I`, `Z=J`). Those case-specific runes belong conceptually to the *impl*'s scope, not the dispatcher's, so they're named under the impl's template denizen — a different prefix.

When the Instantiator translates one case, the substitutions map has to bind both sets of placeholders at once: the dispatcher's (e.g. `[OverrideDispatcherTemplate(launch, ...)] :: KindPlaceholderName(0, I)`) and the impl's (e.g. `[ImplTemplate(...)] :: KindPlaceholderName(0, Y)`). Local rune indices like `0` would collide across the two; the path-encoded prefixes don't. `translateOverride` builds the case substitution map by `++`-merging the two sub-maps, with an explicit disjointness assertion at the merge as a runtime check.

## See also

- @PNBDTZ — Placeholders Named By Denizen Template (the underlying naming convention this arcana relies on)
- @LHPCTLD — Lambdas inherit placeholders from their enclosing function (referenced in the lambda example)
- @SMLRZ — Struct Method Lift Rules (lift can add a struct prefix to a function id; the path-encoded placeholder ids stay anchored at the function denizen regardless)
