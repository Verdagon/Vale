# Handoff: Category G Solver Conflicts

Six tests fail because the type solver produces contradictory or incomplete conclusions
when resolving generic types. All tests are in
`Frontend/IntegrationTests/test/dev/vale/AfterRegionsIntegrationTests.scala`.

## Background you need

### What the solver does

The compiler has a constraint solver that figures out types for generic code. When you
write `HashSet<int>`, the solver determines that the generic param `K` equals `int`. It
does this by processing "rules" — small constraints like "rune X equals int" or "rune Y
is the kind part of coord Z."

Each rule involves "runes" (type variables). When a rule fires, it produces "conclusions"
(resolved values for runes). If a rune gets two different conclusions, the solver reports
a **conflict**.

### Key files

| File | What it does |
|------|-------------|
| `Frontend/Solver/src/dev/vale/solver/SimpleSolverState.scala` | Core solver state. `commitStep` (line 113) detects conflicts by checking if a rune already has a different conclusion. `getNextSolvable` (line 155) picks the next rule to fire. |
| `Frontend/TypingPass/src/dev/vale/typing/infer/CompilerSolver.scala` | Compiler-specific solver logic. `advanceInfer` (line 309) is the main loop. `solveCallRule` (line 1000) handles template instantiation rules. |
| `Frontend/TypingPass/src/dev/vale/typing/InferCompiler.scala` | High-level solve functions: `solveForDefining` (compiling a definition), `solveForResolving` (resolving a call/instantiation), `incrementallySolve` (solve with fallback callbacks). |
| `Frontend/TypingPass/src/dev/vale/typing/TemplataCompiler.scala` | `assembleCallSiteRules` and `assemblePredictRules` — assemble rule sets for different solve contexts. |

### How to investigate a solver conflict

1. **Run the test** and read the error output. It shows:
   - Which rune has the conflict
   - The two conflicting values
   - The step-by-step solver trace (which rules fired, what they concluded)

2. **Trace the rune names.** Rune names like `_41111.kind` are auto-generated. The suffix
   `.kind` means "the kind component of coordinate rune `_41111`." The prefix `Kind$` or
   `Int$` in placeholder names tells you the rune's type.

3. **Find where the conflicting values come from.** Look at the solver steps to see which
   rule concluded each value. The step trace shows rules in firing order.

4. **Understand why both values are being concluded.** Usually one comes from the
   function/struct definition and one from the call site arguments. The fix is either:
   - A missing rule that should propagate information
   - A rule that incorrectly forces a value
   - A placeholder that should have been resolved earlier

### Recent example: DRSINI

We just fixed a category G test ("Test interface default generic argument in type") where
default generic param rules were added to the solver eagerly, conflicting with
argument-inferred values. The fix moved defaults to incremental callbacks so they only fire
for unsolved runes. See `docs/arcana/DefaultRulesShouldBeIncrementalNotInitial-DRSINI.md`
for the full writeup — it's a good model for how to investigate and document solver bugs.

### Lessons from quest.md

Read the "Lessons learned" section in `quest.md` (starts around line 33). Key points:

- **If printlns don't fire, the code path isn't reached.** This is often THE finding.
- **Some tests just need `vimpl(e)` replaced** — run the test first and read the error.
  If it looks correct, the fix might be updating the test, not the compiler.
- **`vimpl()` can be inside the compiler, not just in tests.** Check the stack trace.
- **Error types defined but never thrown** is a recurring pattern.

---

## The six failing tests

### 1. Test returning empty seq

**Line:** 51. **Error:** "Couldn't solve some runes: _2112"

```vale
export () as Tup0;
exported func main() () {
  return ();
}
```

**What happens:** The return type `()` is an empty tuple (`Tup0`). The solver creates a
lookup rule `_2112 = "Tup0"` and a call rule `_2111 = _2112<>` (instantiate Tup0 with no
args). Neither rule fires — `_2112` stays unsolved.

**Where to look:** The `LookupSR` rule for `"Tup0"` is not resolving. This could be because
`Tup0` isn't in the environment, or the solver can't find it via the lookup delegate. Check
`CompilerSolver.scala`'s handling of `LookupSR` and `MaybeCoercingLookupSR` to see how
type names are resolved. Also check whether `export () as Tup0` correctly registers the
name.

**Likely difficulty:** Medium. Probably a missing name registration or lookup path.

---

### 2. Make array without type

**Line:** 186. **Error:** "Internal error: Must specify element for arrays."

```vale
exported func main() int {
  a = #[](10, {_});
  return a.3;
}
```

**What happens:** `#[]` is a static-sized array with no explicit element type. The compiler
should infer the element type from the lambda `{_}` (which returns `int`). Instead it hits
an internal assertion.

**Where to look:** Search for "Must specify element for arrays" in the codebase to find the
assertion. The issue is that element type inference from lambdas isn't implemented — the
compiler requires explicit element types for `#[]` syntax.

**Likely difficulty:** Medium-high. Needs the solver to infer the element type from the
generator lambda's return type, which may require feeding the lambda's return type back as
an initial known.

---

### 3. Call Array<> without element type

**Line:** 172. **Error:** "Couldn't solve some runes: _1215, _7111, E, _7111.kind"

```vale
exported func main() int {
  a = Array<imm>(3, {13 + _});
  sum = 0;
  drop_into(a, &(e) => { set sum = sum + e; });
  return sum;
}
```

**What happens:** `Array<imm>` provides the mutability but not the element type. The solver
should infer `E = int` from the lambda `{13 + _}` but can't solve E. The runes `_1215`,
`_7111`, `_7111.kind` all depend on E.

**Where to look:** Similar to "Make array without type" — the solver needs to infer the
array element type from the generator lambda. Check how the `Array` constructor's generic
params are resolved. The solver has M (mutability) from the explicit `<imm>` but E (element
type) is unsolved.

**Likely difficulty:** Medium-high. Same root cause as "Make array without type."

---

### 4. Borrowing toArray

**Line:** 199. **Error:** "Couldn't solve some runes: _51111111111, E"

```vale
func toArray<E>(list &List<E>) []<mut>&E {
  return []&E(list.len(), { list.get(_) });
}
```

**What happens:** Inside `toArray`, the expression `[]&E(list.len(), { list.get(_) })` tries
to create a runtime-sized array of `&E`. The solver can't determine E. The rule
`_51111111111 = &E` and `inherit E` are present but neither fires.

**Where to look:** The `inherit E` rule should bring E from the function's outer scope
(where it's determined by the `list &List<E>` parameter). Check how `RuneParentEnvLookupSR`
(the "inherit" rule) works in `CompilerSolver.scala`. Also check whether `&E` as an array
element type creates the right rules.

**Likely difficulty:** Medium. The `inherit` mechanism might not be working for this case,
or the rules for `[]&E(...)` might not be set up correctly.

---

### 5. Test overload set

**Line:** 78. **Error:** Solver conflict on `_41111.kind`: was `(overloads: myfunc)` but now
concluding `void`.

```vale
import array.each.*;
func myfunc(i int) { }
exported func main() int {
  mylist = [#](1, 3, 3, 7);
  mylist.each(myfunc);
  42
}
```

**What happens:** `myfunc` is passed as an overload set to `each`. The solver first
concludes that the kind rune is `OverloadSetT(myfunc)` (the overload set itself), then
later tries to conclude it's `VoidT` (the return type of myfunc). These are different
kinds, so the solver conflicts.

**Where to look:** The issue is how overload sets are coerced into callable values. When
`myfunc` is passed to `each`, it needs to be resolved to a specific function prototype,
not left as an `OverloadSetT`. Check how `OverloadSetT` interacts with `CoerceToCoordSR`
and function call resolution. The `ImplicitCoercionKindRuneS` rune is involved — it's
created when the compiler decomposes a coord into ownership + kind for coercion.

**Likely difficulty:** High. Overload set coercion is a complex feature involving the
interaction between the solver, overload resolver, and function call compilation.

---

### 6. Diff iter

**Line:** 128. **Error:** Solver conflict on rune X: "was Kind$diff_iter.K but now
concluding Kind$diff_iter.K" (they look the same but differ in ownership).

```vale
struct HashSet<K Ref imm> {
  table! Array<mut, Opt<K>>;
  size! int;
}
struct HashSetDiffIterator<X> {
  table &[]Opt<X>;
  otherTable &HashSet<X>;
  pos! int;
}
func diff_iter<K>(a &HashSet<K>, b &HashSet<K>) HashSetDiffIterator<K> {
  HashSetDiffIterator<K>(a.table, b, 0)
}
```

**What happens:** `HashSet<K Ref imm>` constrains K to be immutable. `HashSetDiffIterator<X>`
has no such constraint. When `diff_iter` calls `HashSetDiffIterator<K>(a.table, b, 0)`, the
solver tries to unify K (with `imm` ownership) against X (with default `own` ownership).
The kind placeholders look identical in the error message but have different ownership,
causing a conflict.

**Where to look:** The test's own comment (line 139-141) explains the issue well. The solver
conflict is between `share` and `own` ownership on what looks like the same
`KindPlaceholderT`. Check how ownership modifiers on generic params (like `K Ref imm`)
propagate through struct member types and constructor calls. The `imm` modifier on
`HashSet<K Ref imm>` creates an ownership constraint that `HashSetDiffIterator<X>` doesn't
have.

**Likely difficulty:** High. Ownership propagation through generic params is a core
inference feature. The solver needs to handle the case where the same kind appears with
different ownership in different contexts.

---

## Suggested investigation order

1. **Test returning empty seq** — simplest test code, clear error, likely a missing lookup
2. **Borrowing toArray** — clear "inherit" rule issue, self-contained function
3. **Make array without type** / **Call Array<> without element type** — related issues,
   tackle together
4. **Diff iter** — ownership propagation, more complex
5. **Test overload set** — overload set coercion, most complex

## How to run a single test

```bash
cd Frontend
sbt 'testOnly dev.vale.AfterRegionsIntegrationTests -- -t "Test returning empty seq"'
```

For faster iteration, use `compile` first to check for build errors:
```bash
sbt compile
```

## How to add solver debug output

The solver trace in the error message is already quite detailed. If you need more, you can
add `println` statements in:

- `SimpleSolverState.commitStep` (line 113) — see every conclusion as it's made
- `CompilerSolver.advanceInfer` (line 309) — see every rule as it fires
- `CompilerSolver.solveCallRule` (line 1000) — see template instantiation details
