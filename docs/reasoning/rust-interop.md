# Rust Interop — Reasoning

Design decisions that warrant explanation. "Why this approach over the alternatives we considered."

## Why hook ValeRuster at the Frontend's resolver level, not in HigherTypingPass

**Alternative considered:** invoke ValeRuster from `HigherTypingPass` after encountering an `ImportS` node (an earlier design sketched in commented-out code at `HigherTypingPass.scala:756-768`).

**Why rejected:** HigherTypingPass runs *after* lex+parse. If ValeRuster generates new `.vale` files at this stage, they'd need to be re-parsed — meaning the compiler has to restart the parser mid-pipeline, or the generated structures need to be synthesized directly as higher-typed AST (bypassing the parser), losing consistency with everything else.

**Chosen approach:** plug into the resolver chain between lexer import discovery and file resolution. The generated `.vale` files enter the pipeline at the parsing stage, identical to on-disk sources. See @RRPGRZ for the chain ordering detail.

## Why pre-scan all imports and invoke ValeRuster eagerly, not lazily per-resolve

**Alternative considered:** invoke ValeRuster on-demand inside `resolveRustPackageContents`, the first time a given rust package is requested.

**Why rejected:** ValeRuster's startup cost is dominated by reading hundreds of MB of rustdoc JSON (`std.json`, `alloc.json`, `core.json`, plus cargo-rustdoc'd user crates). Paying that per-package would mean one JSON parse per `import rust.*` line in user code — 10 imports = 10 startups. Eager pre-scan pays it once.

**Chosen approach:** `invokeValeRusterIfNeeded` in `PassManager.build()` reads every input source before compilation starts, regex-extracts `import rust.<path>` lines, and invokes ValeRuster once per unique type path. The resolver then just reads files from disk.

**Trade-off:** ValeRuster's per-invocation cost is currently a full rustdoc JSON reload (no shared cache across calls). A future improvement is to invoke ValeRuster once with a *list* of types, or to keep a daemon process. The current approach is correct and simple; optimization can come later.

## Why redirect Divination's stdout to a file instead of fixing the Vale stdlib

**Alternative considered:** fix `stdlib/src/command/native/subprocess.c::read_into_buffer` to use block-sized `fread` instead of character-by-character `fgetc`. That would drain pipes fast enough to avoid the deadlock described in @DSLFNPZ.

**Why rejected (for now):** the stdlib `Subprocess` API has multiple consumers (tests, the Coordinator itself, any Vale program shelling out). Changing read semantics risks subtle behavioral changes — partial-read boundaries, blocking behavior, EOF signaling. Fixing it is the right long-term move but not on the critical path for Rust interop, and the workaround is one line of shell redirection.

**Chosen approach:** Backend appends `> divination.log 2>&1` to the shell command it passes to `std::system`. Divination's output lands in a file, the Backend's own stdout stays small, and no pipe buffer can fill up.

**When to revisit:** any other component in this pipeline that wants to stream output back through the Subprocess machinery will hit the same bug. If that happens twice more, fix the stdlib.

## Why derive the Rust target triple from a path instead of passing it as an argument

**Alternative considered:** thread the triple through `build.vale` → `invoke_clang` as an explicit `Opt<str>` or `str` parameter.

**Why rejected:** the bootstrap `valec` compiler has a type-checker bug (already fixed in current in-repo Frontend, but not in the bootstrap) where `Opt<str>` combined with `set` reassignment inside a closure triggers `scala.MatchError: StrT()`. See `docs/migration/rust-interop.md` for details. Until the bootstrap is rebuilt, we can't use that pattern in Coordinator source that has to compile with bootstrap.

**Chosen approach:** `invoke_clang` receives `rust_include_dir` (which it already needs for `-I`) and derives the triple inline as `Path(rust_include_dir).parent().name()`. Divination's output directory structure (`target/<triple>/release/`) makes this trivially recoverable. No separate `str` variable needs to flow through the call boundary.

**When to revisit:** when the bootstrap compiler is rebuilt from a newer Frontend that has the `Opt<str>` fix. The cleaner signature (`rust_target_triple str` as a sibling of `rust_include_dir`) becomes available then. See @CADFRTZ for the current arrangement.
