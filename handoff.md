# Handoff: Category G Solver Conflicts

Five tests fail because the type solver produces contradictory or incomplete conclusions
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

### Recent example: Borrowing toArray (partial — MKRFA fixed, two other blockers remain)

Worth studying for methodology. Handoff originally pointed at `RuneParentEnvLookupSR`
semantics as the suspect. That direction was right, but the root cause was structural rather than
semantic: three `ArrayCompiler` expression entry points
(`evaluateRuntimeSizedArrayFromCallable`, `evaluateStaticSizedArrayFromValues`,
`evaluateStaticSizedArrayFromCallable`) each hardcoded `val initialKnowns = Vector()` and handed
rules straight to the solver without running the **MKRFA preprocessing** (look up each
`RuneParentEnvLookupSR` rune in `callingEnv`, emit an `InitialKnown`, strip the rule). The
solver's handler for that rule at `CompilerSolver.scala:852` is a defensive no-op, so
unpreprocessed rules fired silently with empty conclusions. `E` was never seeded; `_51111111111 =
&E` stalled.

Fix: copy the MKRFA fold from `OverloadResolver.scala:311-325` inline into all three
`ArrayCompiler` methods. `toArray`'s body now compiles with the correct `Array<mut, &E>` return
type. The test still fails, but for **two other, independent** reasons:

1. **`AugmentSR` collapses `&T` on immutable `T`** at `CompilerSolver.scala:891-917` (inner-known
   path). When `l.toArray()` resolves with `E:=int`, the handler hits
   `case MutabilityTemplataT(ImmutableT) => innerCoord.ownership` at line 900 — it deliberately
   drops the `&` augment because int is immutable, collapsing `&int` to `share int`. Pre-regions
   Vale semantics; the "regions" feature is supposed to preserve the borrow on immutable elements.

2. **No `Array.get` function exists.** `l.toArray().get(1)` is UFCS sugar for `get(arr, 1)`, and
   the stdlib has no such function (canonical array access is `arr[i]`; no `get` file in
   `Frontend/Tests/test/main/resources/array/`). Even if (1) were fixed, this would still block.

The test is effectively dual-blocked on a regions-semantics change to `AugmentSR` *and* either a
stdlib addition or a test change. Full writeup in `investigations/borrowing_to_array.md`.

**Methodology lessons:**
- The structural clue came from the trace: `inherit E` appeared in the fired-rules list but had
  no conclusion indented beneath it (compare to other rules that do). That told us the rule
  "fired" but produced no value — pointing at the no-op handler.
- The handler's comment *"This rule does nothing, it was actually preprocessed"* made the missing
  preprocessing obvious once the clue was followed.
- A single-round audit of other nested-solver call sites showed the vulnerable pattern is narrow:
  only expression-level rule sources that invoke the solver directly. Declaration-scoped solvers
  (struct/interface/function/impl) can't hit this because their rule sources never emit
  `RuneParentEnvLookupSR`.
- **Don't guess categorization before verifying.** My first attempt categorized the residual
  failure as category-H (instantiator) based on the error type looking like a coord-ownership
  issue and quest.md having a known `Instantiator.scala:3174` vimpl. Instrumenting the
  instantiator's `translateCoord` produced zero prints — the error happens in the typing pass,
  before the instantiator runs. Adding a println to `getMaybeReturnType` showed the borrow was
  already gone in the typing pass. Always confirm which pass owns the failure before naming it.
- Future improvement queued: extract the MKRFA fold into an `InferCompiler` helper (now 4 inline
  copies); tighten the no-op handler to `vwat()` so future violations fail loudly.

### Recent example: "Test returning empty seq"

Another category G fix, worth reading for methodology. This handoff originally pointed at
`CompilerSolver.scala`'s `LookupSR` handling ("probably a missing name registration or
lookup path"). That guess was wrong: the failure was actually in HigherTypingPass's
`RuneTypeSolver` pre-processing carve-out (`RuneTypeSolver.scala:441`), which refused to
seed the templata type for `Tup0` because its shape
(`TemplateTemplataType(Vector(), KindTemplataType())`) is ambiguous between
"use as a kind" and "call with zero args." The fix was to simplify the `TuplePT` lowering
so empty tuples emit a single `MaybeCoercingLookupSR` (matching how every other zero-arg
kind template is already handled) instead of a lookup + explicit call pair that
deadlocked on the carve-out.

Once compilation succeeded, a second, separate bug surfaced: Vivem's end-of-program
`cleanup` does nothing for `OwnH` references, and `main()` returned a default-mutable
`Tup0{}` whose ref was `OwnH`, so the allocation leaked. Fixed by marking `Tup0` as
`imm` in `tup0.vale`. Full writeups in `investigations/test_returning_empty_seq.md`
and `investigations/test_returning_empty_seq_vm_leak.md`.

**Methodology lesson:** read the stack trace before trusting this handoff's pointer. The
top of the error may originate in a different compiler pass than the one the handoff
guessed. In this case, `expectAstrouts` (HigherTypingPass) was on the stack, not any
typing-pass solver function.

### Lessons from quest.md

Read the "Lessons learned" section in `quest.md` (starts around line 33). Key points:

- **If printlns don't fire, the code path isn't reached.** This is often THE finding.
- **Some tests just need `vimpl(e)` replaced** — run the test first and read the error.
  If it looks correct, the fix might be updating the test, not the compiler.
- **`vimpl()` can be inside the compiler, not just in tests.** Check the stack trace.
- **Error types defined but never thrown** is a recurring pattern.

---

## The five failing tests

### 1. Make array without type

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

### 2. Call Array<> without element type

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

### 3. Borrowing toArray (G-cause fixed, now H-blocked)

**Line:** 199. **Original error (pre-fix):** "Couldn't solve some runes: _51111111111, E"

```vale
func toArray<E>(list &List<E>) []<mut>&E {
  return []&E(list.len(), { list.get(_) });
}
```

**Status:** The category-G MKRFA cause (preprocessing missing in `ArrayCompiler`) has
been fixed — see `investigations/borrowing_to_array.md`. `toArray`'s body now compiles with
the correct return type `Array<mut, &E>`. The test still fails, but for two other blockers
that are NOT category-H (instantiator is not involved — verified by instrumentation).

**Remaining blockers:**

1. **`AugmentSR` drops the `&` on immutable elements in the typing pass.** At
   `CompilerSolver.scala:891-917` (inner-known path), line 900 has:
   ```scala
   case MutabilityTemplataT(ImmutableT) => innerCoord.ownership
   ```
   When `l.toArray()` resolves with `E:=int`, the solver fires `AugmentSR(outer, BorrowP, E)`.
   Inner is `CoordT(own, ..., IntT)`; `int` is immutable; the handler returns `own` (the
   inner's ownership) instead of applying `BorrowP`. So `&int` collapses to `share int` at
   solve-time in the typing pass, and `getMaybeReturnType` then returns
   `Array<mut, share int>` — the `&` is lost before the instantiator or monomorphization
   ever runs. Pre-regions Vale design; the "regions" feature is supposed to preserve the
   borrow. **This is a semantic design decision**, not a missing implementation — fixing it
   means deciding that `&T` where T is immutable stays as a borrow (potentially gated on a
   region flag), then reworking line 900 and downstream consumers that currently assume
   "immutable implies share".

2. **No `Array.get` function exists.** `l.toArray().get(1)` is UFCS sugar for `get(arr, 1)`.
   The stdlib has `func get(&List<E>, int) &E` and `Some.get()`, but nothing for
   `Array<...>`. Vale's canonical array access is `arr[i]`. The error's 7 candidates are all
   `List.get` / `Opt.get`. Even if (1) above were resolved, the test would still fail here.
   Either the stdlib needs an `Array.get`, or the test needs to use `[1]` indexing.

**Where to look:**
- For (1): `CompilerSolver.scala:891-917` (the `AugmentSR` inner-known path) and how
  callers expect the "immutable implies share" behavior. Search for similar immutable-coercion
  logic to understand the blast radius of changing it.
- For (2): `Frontend/Tests/test/main/resources/array/` — potential place to add a `get`
  function file, modeled on existing files in that directory. Or change the test.

#### Background: what this test is actually testing

Vale generic functions have generic parameters (here, `E`) that are scoped to the
function's template. When code *inside* the function body references `E` (for example,
the array literal type `[]<mut>&E`), the compiler sets up a nested solve context for
that expression. The inner context doesn't automatically see `E`'s value from the
outer context. Instead, the rule generator emits a `RuneParentEnvLookupSR` (the
"inherit" rule) that says: "look up rune `E` by name in the parent environment when
solving this inner context."

So two things need to work correctly here:

1. The **inherit mechanism**: `RuneParentEnvLookupSR(_, E)` fires, finds the outer `E`
   (which is a `KindPlaceholderT` produced by the outer function's generic-param
   setup), and commits it to the inner solver's conclusions for `E`.
2. The **`&E` lowering**: the templex `&E` produces rules like
   `AugmentSR(_5111, BorrowP, _1234)` where `_1234` is an inner rune equated with
   `E` via some other rule. Once `E` is inherited, those chain.

If `E` never gets a conclusion, step 1 is broken. If `E` *does* get a conclusion but
`_51111111111` (the `&E` composite rune) doesn't, step 2 is broken.

#### Files and line numbers worth reading first

- `Frontend/PostParsingPass/src/dev/vale/postparsing/rules/rules.scala` — search for
  `case class RuneParentEnvLookupSR`. Find the case class definition to understand the
  rule's fields.
- `Frontend/PostParsingPass/src/dev/vale/postparsing/RuneTypeSolver.scala:141-149` and
  `289-299` — puzzle and solveRule for `RuneParentEnvLookupSR` at the rune-type-solver
  level (only types, not values).
- `Frontend/TypingPass/src/dev/vale/typing/infer/CompilerSolver.scala` — search for
  `RuneParentEnvLookupSR`. This is the typing-pass handler — where the rule actually
  resolves the inherited rune's *value* (not just its type). Read both the case in
  `advanceInfer` (around line 852) and the preprocessing at
  `InferCompiler.scala`'s initial-rule-registration path.
- `Frontend/TypingPass/src/dev/vale/typing/InferCompiler.scala` — search for
  `RuneParentEnvLookupSR` and `parentEnv`. This file orchestrates nested solves.
- `Frontend/TypingPass/src/dev/vale/typing/ArrayCompiler.scala` — search for `[]` array
  construction expression handling. `[]<mut>&E(...)` compilation goes through here.

#### Suggested first hour

**Step 1: Establish a baseline trace.** Run the test and capture the full solver
output:

```bash
cd Frontend
sbt 'testOnly dev.vale.AfterRegionsIntegrationTests -- -t "Borrowing toArray"' 2>&1 \
  | tee /tmp/borrowing-toarray.txt
```

Then in a *separate* step, inspect the file:

```bash
grep -nE "(error|FAILED|Couldn't|Unsolved|Steps:|added rule)" /tmp/borrowing-toarray.txt \
  | head -80
```

Read the printed solver trace. It lists every rule that was added and every rule that
fired, in order. The unsolved rules and runes at the bottom are your target.

**Step 2: Confirm which solver is failing.** The stack trace in the error output
tells you which compiler pass threw. If it says `HigherTypingPass.expectAstrouts`,
you're dealing with the rune-*type* solver (only figures out shapes). If it says
anything from `typing/` or `InferCompiler`, you're in the typing-pass solver (figures
out values). These are different solvers with different rule handlers — don't mix
them up. For "Borrowing toArray" the error is likely in the typing-pass solver (the
rune names like `_51111111111` and the composite `.kind` suffix pattern are
typing-pass-style), but verify from the stack trace before assuming.

**Step 3: Add the minimum set of printlns.**

- In `CompilerSolver.scala`'s `RuneParentEnvLookupSR` handler (search for it), print
  the rune name, what it found in the parent env (or `None`), and the committed
  conclusion.
- In `SimpleSolverState.commitStep` (`Frontend/Solver/src/dev/vale/solver/SimpleSolverState.scala:113`),
  print every `(rune, conclusion)` as it's added.
- In `SimpleSolverState.getNextSolvable` (line 155), print the rule it picks next.

Re-run the test. From the prints, you'll see which rules fire, which don't, and
specifically: does the `RuneParentEnvLookupSR` for `E` fire? If no, the rule
isn't being scheduled — check its puzzle. If yes but it returns nothing from the
parent env, the parent-env lookup mechanism is broken for this shape. If yes and it
returns `E`'s value but `_51111111111` still doesn't solve, the `&E` rule chain is
the bug.

**Step 4: Read a past investigation as a model.** The session before this one fixed
"Test returning empty seq," which was a similar "a rule that should fire doesn't
because its inputs aren't known" bug. Read:

- `investigations/test_returning_empty_seq.md`
- `investigations/test_returning_empty_seq_vm_leak.md`

Especially the first one, because it walks through: observed failure → puzzle
deadlock analysis → pre-processing carve-out → the specific reason the rule didn't
fire → chosen fix. Mirror that structure in your own writeup at
`investigations/borrowing_to_array.md` as you go.

#### Vocabulary cheat sheet

- **Rune** — a named type-variable slot. `_5111`, `E`, `K`, `_1234.kind`.
- **Conclusion** — the concrete value (or type) the solver figures out for a rune.
- **Rule** — a constraint relating runes. Examples: `LookupSR` (look up a name),
  `EqualsSR` (two runes are equal), `AugmentSR` (coord with ownership modifier),
  `RuneParentEnvLookupSR` (inherit from outer env).
- **Puzzle** — what inputs a rule needs before it can fire. Each rule declares one
  or more puzzles; the solver schedules a rule as soon as any of its puzzles is
  satisfied.
- **`MaybeCoercing*` rules** — rules that preserve Kind↔Coord ambiguity until the
  expected type is known. They get *explicified* to concrete `LookupSR`/`CallSR`
  plus `CoerceToCoordSR` by `HigherTypingPass.explicifyLookups`.
- **`KindPlaceholderT`** — the typing-pass representation of an unknown generic
  param (like `E` inside `toArray`'s body).
- **Placeholder names (`Kind$foo.E`, `Int$foo.N`)** — a placeholder whose kind is
  `E`, scoped to the denizen `foo`. The prefix (`Kind$`, `Int$`) tells you the
  placeholder's templata type.
- **Inherit rule** — colloquial name for `RuneParentEnvLookupSR`.
- **Denizen** — a top-level nameable thing (struct, interface, function). Runes and
  placeholders are scoped to a denizen.

#### How to know you're on the right track

- You can describe, in one sentence, which rule failed to fire and why.
- You can point to a specific line where the bug manifests (e.g., "the puzzle for
  `RuneParentEnvLookupSR` requires X known, but X is never supplied by anyone in this
  rule set").
- Your proposed fix, explained out loud, lists which other tests it could affect
  and why it won't break them.

#### How to know you're off-track

- You're inventing new rule types or new pre-processing phases before you've
  confirmed that existing rules aren't firing due to scheduling, not semantics.
  (The empty-seq fix taught us: architecture changes are usually overkill; the bug
  is usually one carve-out or one asymmetric lowering.)
- You've made a change but you don't know *why* the failing test was failing. If
  the test now passes but you can't explain mechanistically what was wrong before,
  your change is probably masking rather than fixing.
- You've deleted or bypassed a check (a `vassert`, a carve-out) to make the rule
  fire. Those checks usually exist for a reason — understand it first.

#### What to hand back

When you're done, produce:

1. A file `investigations/borrowing_to_array.md` mirroring
   `investigations/test_returning_empty_seq.md`'s structure: reproduction, root
   cause, alternatives considered, chosen fix, verification.
2. Updated `quest.md` row for "Borrowing toArray" — strike through, add a short
   explanation, bump the Category G count ("4 remaining, 3 fixed").
3. Updated `quest.md` Summary table and session-recovered list.
4. Updated `handoff.md` — remove the "Borrowing toArray" section, renumber,
   update the suggested investigation order, and add a new "Recent example"
   paragraph alongside DRSINI and the empty-seq example.
5. The code change itself, with a commit message that references the investigation
   file.

---

### 4. Test overload set

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

### 5. Diff iter

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

1. **Make array without type** / **Call Array<> without element type** — related issues,
   tackle together. May now benefit from the MKRFA fix landed for "Borrowing toArray" but
   likely still need generator-lambda element-type inference.
2. **Diff iter** — ownership propagation, more complex
3. **Test overload set** — overload set coercion, most complex
4. **Borrowing toArray** — category-G cause fixed; now blocked by category-H
   instantiator bug (`Instantiator.scala:3174`). Fix category H and this passes.

## How to run a single test

```bash
cd Frontend
sbt 'testOnly dev.vale.AfterRegionsIntegrationTests -- -t "Borrowing toArray"'
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
