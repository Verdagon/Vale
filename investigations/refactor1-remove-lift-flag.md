# Refactor #1: Remove the `lift` Flag (Attempted, Stalled)

(Id-shorthand notation per `docs/background/id-shorthand-notation.md`.)

**Status**: stalled mid-session. The refactor turns out to be entangled with `OWPFRD` and `rootCompilingDenizenEnv` semantics in ways that aren't separately landable from Refactor #2 (placeholder-ownership change). Working tree contains a partial implementation that builds but crashes the minimum repro at `Compiler.sanityCheckConclusion`'s OWPFRD assertion.

## What we set out to do

Replace the `lift: Boolean` flag on `FunctionS`/`FunctionA` with a structural derivation. Per the user's articulated rule:

> A function is "lifted" iff it's declared inside a `struct Foo { ... }` or `interface Foo { ... }` block. That's the entire decision. No `self`-keyword sniffing, no first-struct-typed-param heuristic.

The flag's only live consumer in the typing pass was `FunctionCompilerMiddleLayer.assembleName`, where it gated the @SMLRZ id-shape transform. Replacing the gate with a structural check on `templateId.initSteps.lastOption` (does it match a citizen-template-name?) was supposed to:

- Remove `lift` from `FunctionS`/`FunctionA` AST.
- Remove `LiftableAttributeP` detection and self-keyword sniffing in `FunctionScout`.
- Remove `if (functionA.lift)` partitioning in `StructCompiler` / `StructCompilerGenericArgsLayer` (universal lift: all citizen methods go to outerEnv).
- Update `assembleName` to gate on templateId structure, reconstruct instantiated citizen id from `templateId.initSteps.last` + inherited templateArgs.
- Keep `inherited` flag on `GenericParameterS` for now (user agreed it's a separate refactor).

## Plumbing introduced

To make `templateId` carry the citizen prefix even when `parentEnv` is `declaringEnv` (per Fix #9), threaded a new parameter through:

- `IStructCompilerDelegate.evaluateGenericFunctionFromNonCallForHeader` — added `maybeOwningCitizenId: Option[IdT[ICitizenTemplateNameT]]`.
- `FunctionCompiler.evaluateGenericFunctionFromNonCall` — added the same parameter (default `None`).
- `FunctionCompilerClosureOrLightLayer.evaluateGenericLightFunctionFromNonCall` — added the parameter; uses `maybeOwningCitizenId.getOrElse(parentEnv.id)` as the prefix for `outerEnvId`.
- `StructCompilerCore.compileStruct` — takes `citizenTemplateId: IdT[ICitizenTemplateNameT]` and passes it through the first deferred-compile loop's pairing.
- `StructCompilerGenericArgsLayer.compileStruct` (struct path) — passes its in-scope `structTemplateId` through.

Result: drop:0's `templateId` becomes `[anon:I template, drop_template]` (citizen-prefixed) while `parentEnv` stays as `declaringEnv` (the package). Templated id ≠ parent chain shape.

## Where it broke

### Round 1: assembleName gate fired for interface methods, breaking EdgeCompiler

Initial gate: `templateId.initSteps.lastOption is ICitizenTemplateNameT`. This includes `IInterfaceTemplateNameT`. For interface methods like `moo` in our minimum repro, the transform now fired and reconstructed the id as `[InterfaceName(I, []), moo_inst]` (instantiated form). But pre-refactor, interface methods went through the default path and produced `[InterfaceTemplate(I), moo_inst]` (template-form initSteps).

`EdgeCompiler.lookForOverride` does `getFunctionTemplate(prototype.id)` → reduces leaf to template, keeps initSteps. So it looked up `[InterfaceName(I, []), moo_template]` against a registered key of `[InterfaceTemplate(I), moo_template]`. Mismatch → `getOuterEnvForFunction` returned None → `vassertSome` crashed.

**Mitigation**: narrowed the gate to `IStructTemplateNameT` (excluding interfaces). Interfaces now keep their template-form citizen step. This compiled but unmasked the next problem.

### Round 2: OWPFRD asserted on substruct-rooted placeholders

With the gate properly firing for `anon:I.drop:0` and `assembleKnownTemplatas` walking up the citizen-prefixed templateId, the substruct's placeholders (`anon:I$functor:moo`) flowed into drop:0's solver context.

`Compiler.sanityCheckConclusion` (OWPFRD) checks:

```scala
val rootDenizenEnv = env.originalCallingEnv.rootCompilingDenizenEnv
val originalCallingEnvTemplateName = rootDenizenEnv.denizenTemplateId
accum.elementsReversed.foreach(placeholderName => {
  vassert(placeholderName.steps.startsWith(originalCallingEnvTemplateName.steps))
})
```

drop:0 is `isRootCompilingDenizen = true` (per the existing pattern match on `parentEnv.id`, since parentEnv is `packageEnv`). So `rootDenizenEnv = drop:0_env`, `denizenTemplateId = [anon:I template, drop_template]`. Placeholder steps are `[anon:I template, KindPlaceholder(0, $functor:moo)]`. `startsWith` fails at step 1 (`KindPlaceholder ≠ drop_template`).

The substruct's placeholder is at depth 2 (`[anon:I, $X]`). drop:0's denizenTemplateId is at depth 2 (`[anon:I, drop]`). The placeholder isn't *deeper* than the root; it's *sibling*. `startsWith` requires placeholder to extend the root path, but the placeholder lives one level shallower than where it would need to be.

### Round 3: tried changing `isRootCompilingDenizen` to false for citizen methods

If drop:0 isn't a root, `rootCompilingDenizenEnv` walks parentEnv to find one. Walked → packageEnv → `vwat()`. The walk-up assumes a non-root has an in-denizen parent that's *the* root; drop:0's parentEnv is package (per Fix #9), not in-denizen. No walk path.

### Round 4: tried deriving denizenTemplateId structurally from templateId

`derive(funcTemplateId)` returned `IdT(_, initSteps.dropRight(1), citizenTemplate)` when initSteps' last is a citizen template, else templateId itself. For drop:0 → `[anon:I template]`. OWPFRD now compares against `[anon:I template]`; substruct's placeholders satisfy `startsWith([anon:I])`.

But the minimum repro still failed. The reason: not all placeholders flowing through drop:0's solver are substruct-rooted. Some are drop:0-rooted (drop:0's *own* runes, of which inherited ones become drop:0-rooted via FunctionScout's `extraGenericParamsFromParentS` mechanism). With denizenTemplateId now `[anon:I]`, those drop:0-rooted placeholders fail `startsWith([anon:I])` because their steps include `drop:0` after `anon:I` plus the placeholder name.

Reverted.

## The deeper conclusion

The OWPFRD assertion as written encodes a **strict invariant**: "every placeholder in this compilation context shares one prefix — the rootCompilingDenizenEnv's denizenTemplateId." That invariant fits a **flat-identity** world (master's pre-@SMLRZ shape) where every function is its own root and inherits nothing from siblings/parents at the placeholder level.

@PNBDTZ + universal-lift breaks this invariant: drop:0's body legitimately sees placeholders from multiple denizens — the substruct's `$functor:moo` (rooted at `[anon:I]`) AND drop:0's own runes (rooted at `[anon:I, drop]` after env construction with citizen-prefix). Two prefixes coexist. No single "the root."

Three architectural responses exist:

1. **Loosen OWPFRD** to "each placeholder is rooted at *some ancestor* of the current denizen" rather than "all share one prefix." Matches what @PNBDTZ permits. Per-placeholder check; no single denizenTemplateId answers for all.

2. **Make placeholders flat under the using denizen** (Refactor #2). When drop:0 is constructed, its inherited `T` placeholder gets rooted at `[anon:I, drop, $T]` instead of `[anon:I, $T]`. Then strict OWPFRD holds — all placeholders share drop:0's prefix. This requires changing `createPlaceholder` to use the using-denizen's templateId as the namePrefix even for inherited runes, and changing how the substruct's bound-args reference them so substitution still flows. Conceptually: copy parent runes into the using denizen's namespace at compile time.

3. **Keep `lift` flag, accept structural-detection isn't independently landable** in the current architecture. Defer Refactor #1 until Refactor #2 enables it, or co-package them.

(2) is the user's articulated end-state vision (`MyStruct<str>.foo<bool>(...)` with each arg living at exactly one structural level). (1) is a less invasive intermediate step that loosens the assertion without changing placeholder ownership. (3) is the conservative path that keeps yesterday's gains and defers structural cleanup.

## Why we couldn't see this earlier

The agent's earlier survey ("can rune inheritance be removed entirely?") concluded inheritance was redundant scaffolding because `assembleKnownTemplatas` walks up the citizen and binds parent runes from there. Strictly true — but Fix #9 (yesterday) had severed the parent walk-up for lifted methods, so `assembleKnownTemplatas` *wasn't* finding parent runes that way. The function-level overload of `assembleKnownTemplatas` (zip with explicitTemplateArgs) was the actual binding source post-Fix-#9, and that path requires inherited entries in `function.genericParameters`.

So the survey's "redundant scaffolding" claim was true *in principle* but not *on this branch's current state*. Refactor #1's universalization restored the walk-up path (templateId now citizen-prefixed → walk-up reaches anon:I), making inheritance redundant again — but in doing so it surfaced placeholders that OWPFRD then objected to.

Net: Refactor #1 makes inheritance redundant *and* surfaces placeholders into solver context that need OWPFRD's strict invariant relaxed. The two effects are inseparable; the assertion's design and the universal-lift design are at odds.

## Working tree state (at session end)

Partial Refactor #1 changes are still in the tree, broken at OWPFRD. The list:

**ASTs (lift field removed)**:
- `Frontend/PostParsingPass/src/dev/vale/postparsing/ast.scala` — `FunctionS` no longer carries `lift`.
- `Frontend/HigherTypingPass/src/dev/vale/highertyping/ast.scala` — `FunctionA` no longer carries `lift`.

**Producer-side**:
- `Frontend/PostParsingPass/src/dev/vale/postparsing/FunctionScout.scala` — lift detection block removed; `extraGenericParamsFromParentS` now unconditionally added to `genericParametersS`.
- `Frontend/HigherTypingPass/src/dev/vale/highertyping/HigherTypingPass.scala` — destructuring/reconstruction no longer references lift.
- `Frontend/TypingPass/src/dev/vale/typing/macros/AnonymousInterfaceMacro.scala` — same.
- `Frontend/TypingPass/src/dev/vale/typing/macros/StructConstructorMacro.scala` — `FunctionA(...)` no longer passes `true` for lift.
- `Frontend/TypingPass/src/dev/vale/typing/macros/citizen/InterfaceDropMacro.scala` — same.
- `Frontend/TypingPass/src/dev/vale/typing/macros/citizen/StructDropMacro.scala` — same (both `FunctionA(...)` constructions).
- `Frontend/TypingPass/src/dev/vale/typing/expression/ExpressionCompiler.scala` — closure FunctionA destructuring/reconstruction.

**Partition removal (universal lift)**:
- `Frontend/TypingPass/src/dev/vale/typing/citizen/StructCompiler.scala` — removed `internalMethods.filter(_.lift)`; all internal methods go to outerEnv.
- `Frontend/TypingPass/src/dev/vale/typing/citizen/StructCompilerGenericArgsLayer.scala` — removed `internalMethods.filter(!_.lift)` from both runesEnv constructions; passes `structTemplateId` through to compileStruct.

**Plumbing**:
- `Frontend/TypingPass/src/dev/vale/typing/citizen/StructCompiler.scala` — `IStructCompilerDelegate.evaluateGenericFunctionFromNonCallForHeader` takes `maybeOwningCitizenId`.
- `Frontend/TypingPass/src/dev/vale/typing/Compiler.scala` — delegate impl propagates the param.
- `Frontend/TypingPass/src/dev/vale/typing/function/FunctionCompiler.scala` — `evaluateGenericFunctionFromNonCall` takes `maybeOwningCitizenId`.
- `Frontend/TypingPass/src/dev/vale/typing/function/FunctionCompilerClosureOrLightLayer.scala` — `evaluateGenericLightFunctionFromNonCall` takes `maybeOwningCitizenId` and uses it for `outerEnvId` construction.
- `Frontend/TypingPass/src/dev/vale/typing/citizen/StructCompilerCore.scala` — `compileStruct` takes `citizenTemplateId`; first deferred-compile loop passes `Some(citizenTemplateId)`. Second loop has `vfail` tripwire (no FunctionEnvEntries expected). Two other delegate call sites pass `None`.

**Gate change**:
- `Frontend/TypingPass/src/dev/vale/typing/function/FunctionCompilerMiddleLayer.scala` — `assembleName` no longer reads `function.lift`. Gate is `templateId.initSteps.lastOption is IStructTemplateNameT`. Reconstructs instantiated citizen id from inherited templateArgs.

**Tests**:
- `Frontend/PostParsingPass/test/dev/vale/postparsing/PostParserTests.scala` — "Lift methods correctly" test removed.

**Verification status**: builds clean (`sbt assembly` succeeds). Minimum repro (`interface I { func moo(virtual this &I) int; } exported func main() int { return 0; }`) crashes at `Compiler.sanityCheckConclusion`'s OWPFRD vassert with `Assertion failed!`. Backend suite not run (would also crash on equivalent paths).

## How to recover

**Option A — revert the entire Refactor #1**: undo all the file changes listed above. Working tree returns to the post-Fix-#9 state (180/198 passing). `lift` flag stays. Quest.md's Refactor #1 description should be updated to "blocked on Refactor #2 / OWPFRD loosening."

**Option B — keep the changes as a checkpoint**: commit them with a clear "WIP: blocked on OWPFRD" message so the work isn't lost. Don't merge to master. Continue from this state in a future session that also tackles Refactor #2 or OWPFRD loosening.

**Option C — push through to Refactor #2**: in the same session that lands Refactor #1, also do Refactor #2 (per-using-denizen placeholders). Strict OWPFRD then holds because all placeholders flatten under the using denizen. Bigger atomic change but unblocks the architectural target in one shot.

User chose to wind down before deciding. Recommend a fresh-session decision once the current state is fully digested.

## Connection to broader architecture

Refactor #1's failure mode reveals that the @SMLRZ + @PNBDTZ + Fix-#9 stack has accumulated architectural mismatches:

- @SMLRZ wants methods to look like Rust paths (`Vec<T>.push()`).
- @PNBDTZ wants placeholders rooted at the declaring denizen (so the substruct owns its `where exists` runes).
- Fix #9 wants drop's parentEnv to skip the citizen (so lookup walk-up doesn't surface duplicate __call).
- OWPFRD wants all placeholders to share one prefix.

These four are individually defensible but collectively over-constrained. Any two can hold; all four can't simultaneously without a placeholder-ownership change (Refactor #2) or an OWPFRD weakening.

The user's UFCS-philosophy north star — "function placement only affects ID shape, nothing else; templateArgs storable at any structural level; `MyStruct<str>.foo<bool>(...)` end shape" — is consistent with Refactor #2's per-using-denizen placeholder model. Once placeholders flatten under the using denizen, all four constraints hold.

So Refactor #1 alone is *not* the natural unit of work. Refactor #1 + Refactor #2 together are. The session attempted #1 alone and discovered the entanglement.

## Recommendations for the next session

1. **Before touching code**, audit `createPlaceholder` and the placeholder-ownership rules. Specifically: when StructDropMacro generates drop and FunctionScout adds `extraGenericParamsFromParentS` to drop's `genericParametersS` with `inherited=true`, are the resulting placeholders rooted at drop's templateId or at the substruct's templateId? The answer determines whether OWPFRD's strict invariant holds today (and how it broke today's session).

2. **Decide between Option A/B/C above** for the working tree.

3. **If pursuing Refactor #1+#2 together**: scope is bigger (~3-5 days). Need a plan that covers `createPlaceholder` callers, `assembleKnownTemplatas`'s walk-up, the substitutions Map's content (it currently keys by globally-unique placeholder ids — would per-using-denizen ownership change those keys?), and the InstantiatedAST monomorphizer's expectations.

4. **If pursuing OWPFRD loosening alone**: smaller scope. Per-placeholder check ("each placeholder is rooted at some ancestor of the current denizen") instead of single-prefix check. Audit places that pass `originalCallingTemplateId` to `addInstantiationBounds` — same pattern, same loosening needed.

5. **Quest.md is updated** to reflect status as of Fix #9. Refactor #1 is described in quest.md as the next refactor; should be amended to note this session's findings (entanglement with OWPFRD/Refactor #2).
