# Handoff: Make `rustinterop-merged` Pass 198/198 Backend Tests

Hi! You're picking up a debugging quest mid-stream. This doc has way more context than you'll think you need, because the codebase is unfamiliar and the failure modes are subtle. Skim what's obvious to you and read carefully where things look unfamiliar.

## TL;DR — what to do

The Backend test suite is currently **0/198 passing** on the `rustinterop-merged` branch. We've fixed the dominant failure cluster; three smaller ones remain. Tackle them in this order, and tests should go green:

1. **Dominant remaining cluster (~14 occurrences)**: `vassertSome "Expected non-empty"` at `Instantiator.scala:3195` in `translateCoord`. Likely a missing entry in the substitutions map.
2. **Anonymous-substruct cluster (~5 occurrences)**: `vimpl()` at `FunctionCompilerClosureOrLightLayer.scala:231`. Probably an unhandled match arm for `AnonymousSubstructMemberRuneS`.
3. **`Assertion failed! IdT(...)` (~5 occurrences)**: Same site as #2 — likely the same root cause; will probably fall when #2 falls.

After all three, expect most or all 198 tests to pass.

There's also a separate `FrontendRust` (the in-progress Rust port of the Frontend) that doesn't compile because `OptimizedSolverState` is missing a `new` method. That's a separate quest — see "After tests pass" below.

## What this codebase is

This repo is the Vale compiler. There's an in-progress migration from a Scala implementation (mature, mostly working) to a Rust port (mid-migration). The `rustinterop-merged` branch contains a recent merge that added a Rust-interop pipeline (Vale code can `import rust.std.vec.Vec` and call into Rust crates) plus a wave of refactors. The merge introduced regressions; this quest is to fix them.

**Key directories:**
- `Frontend/` — the Scala compiler (the working one). This is where you'll be doing most of your work.
  - `Frontend/PostParsingPass/` — desugaring and name resolution
  - `Frontend/TypingPass/` — type inference, generic resolution, monomorphization-prep
  - `Frontend/InstantiatingPass/` — actual monomorphization (turns `Vec<T>` into `Vec<int>`)
  - `Frontend/SimplifyingPass/` — collapse to LLVM-friendly form
  - `Frontend/docs/arcana/` — short docs explaining specific tribal-knowledge facts (file naming convention: `<TitleCase>-<UPPERCASE_ID>.md`, referenced by `@<UPPERCASE_ID>` annotations in the code)
  - `Frontend/docs/historical/` — session-summary docs from major debugging sessions
- `FrontendRust/` — the in-progress Rust port. Compiles separately. Mid-migration; expect lots of `panic!()` placeholders and commented-out Scala. Don't touch unless explicitly working on it.
- `Backend/` — LLVM-based code generator. C++. Test-runner-driven; don't usually need to touch this.
- `Tester/` — drives the test suite. Built from Vale source. You'll need to build it once.
- `investigations/` — write-ups of past debugging sprints. Read these for context on past root causes.
- `todo/` — old task lists.
- `quest.md` — current quest tracker (updated as fixes land).
- `tmp/` — local scratch (gitignored). Use this for piping test output through grep instead of `/tmp/`.

## Compiler pipeline you need to understand

The Scala Frontend has multiple passes. For your bugs, the relevant ones (in order) are:

1. **PostParsingPass** — turns parser output into a typed-name AST (`*S` types). Resolves names, validates rules.
2. **TypingPass** — type-checks against generics, produces typed AST (`*T` types). Where most of the action lives. Outputs include `FunctionHeaderT` (the function's signature) and a `CompilerOutputs` collection of all compiled functions.
3. **InstantiatingPass** — monomorphizes. Turns `Vec<T>` into `Vec<int>` etc. Produces `*I` types in three flavors (more on this below).
4. **SimplifyingPass / Hammer** — flatten for codegen.
5. **VonHammer** — emit `.vast` (Vale AST) files for the Backend.

When a test compiles, the trace you'll see shows the typing pass output (`Compiling function: foo`) and then an exception at some pass. The exception's stack tells you which pass crashed.

### The `*S` / `*T` / `*I` naming convention

The compiler has three parallel AST hierarchies, named by suffix:
- `*S` = post-parsing (e.g. `IRuneS`, `INameS`)
- `*T` = typing pass (e.g. `IRuneT`, `INameT`, `StructNameT`)
- `*I` = instantiating pass (e.g. `IRuneI`, `INameI`, `StructNameI`)

Each pass has its own complete set of name/type AST classes, even when they look almost identical. The `T → I` translation happens in `Instantiator.scala`'s `translateName`/`translateStructName`/`translateFunctionId`/etc.

**Important non-intuition**: The `*I` types take a region-mode type parameter (`sI`, `nI`, `cI`). These are NOT instantiation phases — they're region-tracking tags:
- `sI` = "subjective" — what the typing pass produced; an instantiation request
- `nI` = "new" — starting point for a fresh monomorphization
- `cI` = "collapsed" — finalized after region collapsing

So `IdI[sI, IFunctionNameI[sI]]` is "an instantiating-pass function ID in subjective region mode", not "an ID that's been instantiated by sI." The casts between `sI` and `nI`/`cI` are zero-cost because these classes have no runtime fields (see `CCFCTS` annotation in `types.scala`).

This caught me out — I assumed `idS` (the `s` here is for `sI`) would never contain template-only names, since "we're past instantiation now." Wrong. The `*I` AST has `StructTemplateNameI` AND `StructNameI` — the same template/instantiation distinction the `*T` AST has. Whether you have one or the other is a matter of *which form was emitted by the producer*, not what region mode you're in.

### The `lift` flag and `assembleName`

Struct methods can be "lifted" or "not lifted". This is documented in `Frontend/docs/arcana/StructMethodLiftRules-SMLRZ.md` (referred to as @SMLRZ). Read this. Short version:

- A struct method `func capacity(self Vec<T>) i64` defined inside or alongside `Vec<T>` produces a function ID like `[Vec<i32>, capacity()]` — i.e., the struct goes in `initSteps` and the function name is the leaf. This shape is *required* by NameHammer (the simplifier) and downstream Rust-interop code.
- The mechanism: `function.lift = true` + parameter literally named `self` causes `FunctionCompilerMiddleLayer.assembleName` to use the self param's `StructTT.id` as the prefix.
- Without both flags, `assembleName` falls to `parentEnv.id.addStep(funcName)` — which produces the *bare template* form `[StructTemplate, capacity()]`, which crashes NameHammer.

This is exactly the bug we just fixed in commit `76cd7e0c`: the `StructDropMacro` was setting `lift = true` but naming its parameter `thiss` (= "this"), so the self-detection failed and it fell to the broken branch. We renamed it to `self`.

### Placeholders

When the typing pass encounters a generic like `T` in `func foo<T>(...)`, it doesn't know what `T` will resolve to yet. So it creates a **placeholder** — a typed-AST stand-in for "T, to be filled in later". Placeholders have a name like `KindPlaceholderNameT(KindPlaceholderTemplateNameT(0, CodeRuneS("T")))` — index `0` (which generic param slot) plus the original rune (`T`).

Placeholders come in two flavors:
- `KindPlaceholderT` (wrapped in `CoordTemplataT(CoordT(_, _, KindPlaceholderT(...)))`) for kind/coord generic params.
- `PlaceholderTemplataT` (with payload type `IntegerTemplataType` / `MutabilityTemplataType` / etc.) for non-coord generic params (e.g. `<N Int>` for `StaticSizedArrayIter`).

When the instantiator monomorphizes `Vec<T>` to `Vec<int>`, it builds a **substitutions** map: `placeholder_T → int`. Then it walks the typed AST and substitutes every placeholder it finds.

Important docs: `@PNBDTZ` (`Frontend/docs/arcana/PlaceholdersNamedByDenizenTemplate-PNBDTZ.md`) explains placeholder naming. Read this if you hit cluster #1's bug.

## What's been done in recent commits

You should read these commit messages in detail. They explain not just what changed but the diagnostic process.

```
0d365a79  Add a NonKindNonRegionPlaceholderNameT arm to assembleKnownTemplatas
76cd7e0c  Route macro-generated drop through assembleName's self-struct path
43e631c6  Loosen the index match in assembleKnownTemplatas (literal 0 → wildcard)
6a76dd29  Track previously-untracked notes (gitignore tmp/ etc)
```

Three of these (43e631c6, 76cd7e0c, 0d365a79) are real bug fixes. They all originate from a single WIP commit `79805fad "Extern struct methods work!"` (from June 2024) that introduced multiple "DO NOT SUBMIT" markers and `vimpl()` placeholders, some of which never got finished. Expect more of these to surface as you fix.

Read `investigations/iast-template-vs-instantiation.md` for the full investigation that led to commit `76cd7e0c` (the trickiest of the three). It uses the **collapsed-call-tree** technique, which I recommend for your fixes too.

## How to reproduce / build / test

```bash
# One-time: build the Tester (depends on a bootstrapping valec)
cd Tester && bash build.sh ~/BootstrappingValeCompiler

# Iterate: rebuild Frontend.jar (~30-90s)
cd Frontend && sbt 'set test in assembly := {}' assembly

# Run backend tests (~5-10 minutes, runs all 198)
cd Backend && bash test.sh > ../tmp/trace.txt 2>&1
tail -5 ../tmp/trace.txt   # Look for "Done! Passed N/198"

# Tally exception types
grep -oE "Assertion failed!?\s*\w*" tmp/trace.txt | sort | uniq -c

# Find the first failing test's full stack trace
grep -B 2 "Exception in thread" tmp/trace.txt | head -40
```

**Single-test reproduction (much faster iteration):**
The test runner's frontend invocation is:
```
java -cp ../Frontend/Frontend.jar dev.vale.passmanager.PassManager build \
  --output_dir <out> --sanity_check true \
  vtest=/path/to/test.vale
```
Just running this on a single failing test is much faster than the whole suite. Find a failing test in `tmp/trace.txt` (search for `Error 1 building test <name>`) and run that one invocation.

**Some test programs are directories** (e.g. `programs/externs/rsaimmparamexport/`). Pass the whole directory to `vtest=` for those — the runner expects a directory or file. The test runner output shows you the exact path it used.

## How to investigate

### Tools

- **`/collapsed-call-tree` skill**: A repo-local skill at `.claude/skills/collapsed-call-tree/`. It's a debugging methodology for compiler bugs: instrument key functions with `println`, run a failing test, build a collapsed call tree in `investigations/`, iterate. Use this for the larger investigations. Past examples in `investigations/` show what the output looks like.
- **`Frontend/docs/arcana/`**: short docs (`@<TitleCase>-<UPPERCASE_ID>.md`) explaining tribal-knowledge facts. Annotated with `@<ID>` in source. When code references `@SMLRZ`, that's `StructMethodLiftRules-SMLRZ.md`. Always read these before changing related code.
- **`Frontend/docs/historical/`**: post-mortems of past debugging sessions. The `2026-04-16-...` doc covers the previous round of regression fixes (Groups A-G) and is genuinely useful background.

### Methodology that worked for me

1. **Find the smallest failing test you can reproduce.** Single-test reproduction is much faster.
2. **Read the full stack trace.** Don't just look at the top frame — the call site (one or two frames up) tells you the entry point. Find which function is calling the crashing one.
3. **Read the source around the crash.** Look for unfinished branches: `vimpl()`, `vwat()`, `// DO NOT SUBMIT`, commented-out fallbacks that don't typecheck. These are signposts.
4. **Look at git blame for the crash site.** Often the crash is in code that was recently changed and not finished. Search the commit message — sometimes the author flagged it.
5. **Add `println` instrumentation** to print the inputs at the crash site and the inputs at the function that produced them. Rebuild Frontend.jar, run the single failing test, capture to `tmp/trace.txt`.
6. **Compare working vs failing.** If a function works for some inputs and not others, instrument it to print all inputs. Compare the working pattern to the failing one. The difference is your bug.
7. **Read related arcana docs.** Often the doc already flags the issue you're seeing.
8. **Fix the producer, not the consumer.** When two functions disagree (e.g. one produces a shape, another consumes it and crashes), fix the side that's wrong relative to the rest of the codebase. Usually that's the producer of the wrong shape, but check what other consumers expect.

### Hard-won lessons (tribal knowledge)

- **`sbt` may not detect file changes via timestamps.** If `sbt assembly` says "no changes" after editing, run `sbt clean assembly` or `touch` the modified files first.
- **Hook config in `.claude/settings.json`** can block file edits in this repo. If you can't edit a `.scala` file, check the hooks. The previous session disabled them; they may or may not be re-enabled.
- **Don't background long-running test commands.** This project's CLAUDE.md says no background tasks. The test suite takes 5-10 minutes; just wait for it.
- **Bash tool runtime auto-backgrounds long commands** sometimes despite no `run_in_background` flag. If you see "Command running in background", that's the tool's choice; you can poll its output file or use `TaskOutput` to wait.
- **Don't make temporary programs to debug.** Add a test case in the project instead. The CLAUDE.md is explicit about this.
- **Don't use `cd ... && cargo` or `cd ... && sbt` patterns.** Use `--manifest-path` or run from the project's directory directly.
- **Don't use `git checkout` to revert changes.** Use `git diff` and apply changes manually.
- **Use `tmp/` (in this repo) instead of `/tmp/`** for scratch logs. The user explicitly asked for this. `tmp/` is gitignored.
- **Don't run `awk`** to slice logs. The user explicitly disallowed this. Use `grep`/`sed`/`head`/`tail` and the Read tool.
- **The `lift` flag is `true` for the macro-derived drop already** — don't be misled into thinking we changed `lift`. We changed only the parameter name, and that was enough for `assembleName`'s self-detection check (which AND's `lift==true` with `param.name == keywords.self`).

## Cluster #1 in detail (your first target)

**The failure:** `vassertSome "Expected non-empty"` at `Instantiator.scala:3195`.

**The code at line 3195** (in `translateCoord`):
```scala
case KindPlaceholderT(placeholderId) => {
  vassertSome(vassertSome(substitutions.get(placeholderId.initId(interner))).get(placeholderId)) match {
    case CoordTemplataI(...) => ...
    case KindTemplataI(...) => ...
  }
}
```

This double-`vassertSome`:
1. Outer: looks up `placeholderId.initId(interner)` (the *denizen template ID* — the key for this denizen's substitutions table) in the global substitutions map.
2. Inner: looks up the specific placeholder in that table.

If either is `None`, the assertion fires. The error message "Expected non-empty" doesn't tell you which one.

**Investigation plan:**
1. Find a failing test that triggers this. Probably something downstream of one of our earlier fixes — look in `tmp/trace.txt` for `translateCoord(Instantiator.scala:3195)` and look at which test it's compiling.
2. Add `println`s before line 3195 to print:
   - `placeholderId` (the placeholder being looked up)
   - `placeholderId.initId(interner)` (the denizen template ID it's keyed under)
   - `substitutions.keys` (what keys are actually in the map)
   - Whether the first or second `vassertSome` is failing
3. From the output, decide whether:
   - **Upstream bug**: The substitution wasn't populated. Find who built `substitutions` (probably `assemblePlaceholderMap` — the same function we wrestled with — or the caller of `translateCoord`). The map might be missing an entry it should have.
   - **Wrong-lookup bug**: The wrong placeholder is being looked up. Trace back to find what placeholder appears in the AST being walked.

**Where `substitutions` comes from:** Most likely `Instantiator.translateFunction` line 920-923 (which we already touched). It calls `assemblePlaceholderMap(funcT.header.id, desiredPrototypeS.id)` to build the map. With our fix, this should now produce the right map for non-generic struct drops. But maybe the map is correct yet the consumer is looking up the wrong key.

**Hypothesis to start with:** This bug existed all along and was masked by the earlier crashes (which fired before reaching `translateCoord`). The clue would be in the cases that *recently* started failing here — what changed about which functions reach the Instantiator after our fix. Look at the failing test's typed-AST function id (compare to `funcT.header.id` shape) and which placeholder is being resolved.

**Don't be afraid to use the `/collapsed-call-tree` skill.** That's literally what it's for, and the methodology produced the previous fix cleanly.

## Cluster #2 in detail

**The failure:** `vimpl()` at `FunctionCompilerClosureOrLightLayer.scala:231`. The error message includes a full IdT dump showing `AnonymousSubstructNameT(_, [..., KindPlaceholderTemplateNameT(0, AnonymousSubstructMemberRuneS(...))])` — note the `0`, which suggests the same kind of half-finished pattern as Fix #1. There's also a second variant where the assertion message is the entire IdT — that's `vimpl(other)` printing the unhandled value.

**Where to start:**
1. Open `FunctionCompilerClosureOrLightLayer.scala` and look at line 231. It's likely a `match` with `case other => vimpl(other)`. Look at the existing arms; the missing one is for whatever shape the failure shows.
2. Compare to similar match statements elsewhere — there might be a parallel function that handles all the cases correctly, and you can mirror it.
3. Triggering tests: `upcastif.vale`, `interfacemutreturnexport/test.vale`. These exercise interface anonymous substructs (lambdas implementing interfaces).

There's a prior investigation at `investigations/anonymous-substruct-cluster.md` that covers a different but adjacent area. Read it for context on anonymous substructs.

## When you're done

For each fix:
1. Apply the fix.
2. Rebuild Frontend.jar.
3. Run the test suite. Confirm `Passed N/198` increases meaningfully.
4. Update `quest.md` to mark that fix done and capture the next target.
5. Write up the investigation (if non-trivial) in `investigations/<name>.md`. Reference relevant arcana.
6. Update `Frontend/docs/arcana/` if the fix changed the prescribed behavior of a documented invariant.
7. Commit with a verbose message — explain what was wrong, why it was wrong, what the fix is, and why it's right. Include `Co-Authored-By: ...` if a tool generated meaningful content. See recent commits for the style.

**One commit per fix.** If a single investigation produces multiple independent fixes, commit them separately even if you discovered them together. The user asked us to split a previous mixed commit into 4 single-purpose ones.

When all 198 tests pass:
- Update `quest.md` Step 1 status: **DONE**.
- Move to Step 2: `FrontendRust` build error. The trait `ISolverState` requires a `new` method that `OptimizedSolverState` doesn't implement. Look at the recent Scala-side commit `74371d69` ("eliminate canonical-rune indirection") which **commented out** `OptimizedSolverState` entirely on the Scala side. Most likely the Rust port should match — retire `OptimizedSolverState` and use only `SimpleSolverState`. Check Scala-Rust parity rules in `.claude/CLAUDE.md` (project) and the shields under `Luz/shields/`.
- Step 3: Run `scripts/test-rust-interop.sh` (with `DIVINATION_PATH` and `RUST_CARGO_TOML` env vars) to confirm the Rust-interop pipeline still works post-fixes.
- Add at least one Rust-interop test to the regular Backend test suite so this pipeline doesn't silently regress.

## Resources

- `quest.md` — current state, updated as you go.
- `investigations/iast-template-vs-instantiation.md` — the most recent investigation; good template for your own writeups.
- `investigations/anonymous-substruct-cluster.md` — older cluster; may have relevant context for cluster #2.
- `Frontend/docs/arcana/StructMethodLiftRules-SMLRZ.md` — read for the lift mechanism and how function IDs are built.
- `Frontend/docs/arcana/PlaceholdersNamedByDenizenTemplate-PNBDTZ.md` — how placeholders are named; matters for cluster #1.
- `Frontend/docs/arcana/IdentifiabilityChecksIncludeParentCitizenRunes-ICIPCRZ.md` — non-lifted method identifiability rules; mostly background.
- `Frontend/docs/historical/2026-04-16-regression-fixes-and-rust-interop-discovery.md` — previous big debugging session covering Groups A-G regressions and rust-interop pipeline discovery.
- `.claude/skills/collapsed-call-tree/` — the debugging skill. `/collapsed-call-tree` invokes it.
- `.claude/CLAUDE.md` — project-level Claude instructions (migration philosophy, lifetime invariants for the Rust port, etc.).
- `~/.claude/CLAUDE.md` — user-level instructions (don't run in background, no temporary programs, no `cd && cargo`, etc.).

Good luck. Reach out if anything in the existing work doesn't make sense — the user can clarify, and the recent commit messages and investigation docs should answer most "what was the author thinking" questions.
