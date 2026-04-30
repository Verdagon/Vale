# Reducing the Rustc Fork in the Facade Architecture

## Context

The `rustc-lang-facade` architecture currently requires a 4-patch fork of
rustc plus a fifth patch for the partitioner visibility hook. I'm exploring
whether we can achieve the same capabilities with fewer (or zero) patches,
to make the architecture viable for Vale's deployment story once Vale-in-Rust
lands.

Vale's current interop (ValeRuster + Divination, opaque bytes + cbindgen +
cargo-c) has structural dealbreakers — no closures, no `dyn Trait`, no
Vale-defined types inside Rust generics, because all three need rustc to
accept a type or vtable it didn't compile. The facade architecture handles
all three natively. So we're moving to a facade approach for Vale's next
interop generation.

The holdup is the fork. Vale ships as a precompiled binary to users; a
four-patch fork of a pinned nightly is expensive to maintain across rustc
versions, and it makes "just install valec" meaningfully harder. I want
your read on whether we can shrink the fork — ideally to zero — without
giving up the capability wins.

You designed the current facade architecture; if some of these
alternatives were considered and rejected for reasons I'm not seeing,
that's what I most want to hear.

---

## Current fork patches

Per §2.2 and §10.6.4 of `rust-interop-guide.md`:

1. **`rustc_middle/src/queries.rs`** — defines the new `per_instance_mir` query
2. **`rustc_monomorphize/src/collector.rs`** — checks `per_instance_mir` before falling through to `instance_mir`
3. **`rustc_codegen_ssa/src/mono_item.rs`** — skips `codegen_instance` when `per_instance_mir` returned `Some`
4. **`rustc_mir_transform/src/shim.rs`** — default provider returning `None`
5. **`rustc_monomorphize/src/partitioning.rs`** — `VISIBILITY_OVERRIDE_HOOK` for linkage override on `__lang_stubs` wrappers

Patches 1–4 are one logical feature ("new Instance-keyed MIR query"). Patch 5
is a separate feature ("partitioner linkage hook").

---

## Alternatives per patch

### Patches 1–4: can we eliminate the new query?

The core need is **per-Instance MIR** rather than per-DefId MIR. The fork
adds a new query because existing queries are DefId-keyed.

#### Alternative A: override `optimized_mir` with a trampoline body

Rustc's consumers (collector + codegen) call `optimized_mir(def_id)`, then
substitute with the instance's generic args. If we `override_queries` for
`optimized_mir` on `__lang_stubs` DefIds and return a generic MIR body whose
only content is a `ReifyFnPointer` cast of `extern "C" fn __toylang_impl_<name>`,
then:

- Substitution is a no-op on the call target (extern C functions are
  monomorphization-invariant in their signature).
- The collector walks the `ReifyFnPointer` reference and registers the
  extern symbol for linking, exactly as today.
- Per-Instance symbol naming moves to the `symbol_name` override, which is
  already Instance-keyed.
- Codegen sees an `unreachable!()`-equivalent body and emits nothing (or we
  use a `#[inline(never)]` empty body that gets dead-code-eliminated).

The Instance distinction matters in two places today. Here's where it could
move:

- **`monomorphize_fn` symbol naming** — already in `symbol_name`, stays there.
- **Per-Instance dep walking** — move to a pre-pass from
  `Callbacks::after_analysis`, walking `tcx.reachable_set` or similar and
  populating `ToylangState.toylang_instances` before codegen. The collector
  then doesn't drive our dep walk; we drive it.

`override_queries` lives in `rustc_interface::Config::override_queries`. It's
`rustc_private` / nightly, but it's a stable-within-nightly extension point
used by rust-analyzer, clippy, miri, and both alternative codegen backends.
No fork.

**Question for you:** was this considered when the new query was added?
What specifically pushed toward "new query" vs "override existing query
with a trampoline body"? I can see an argument that the trampoline is
awkward and `per_instance_mir` is cleaner — but cleaner-at-fork-cost vs
awkward-at-zero-fork is a different tradeoff for Vale's distribution story
than for toylang's.

#### Alternative B: be a `CodegenBackend`

`rustc_codegen_cranelift` and `rustc_codegen_gcc` plug in via
`-Zcodegen-backend=valec`. They receive CGUs after monomorphization and
emit `.o` files directly. Zero fork patches.

**Why this might fit:**

- Sanctioned extension point, explicitly supported by the rustc team.
- Our real work happens at codegen time (`generate_and_compile`); being the
  backend is a natural home for it.
- The "internal toylang functions never reaching rustc's MonoItems"
  optimization falls out naturally: we just don't emit them from the stubs
  walk and they never enter rustc's partitioner.

**Why it might not:**

- We'd be nominally responsible for emitting Rust deps' IR too. But we can
  delegate — `rustc_codegen_llvm` is available as a library; we wrap it and
  only intercept consumer DefIds.
- The backend receives CGUs *after* partitioning. If we need to influence
  partitioning (patch 5's issue), we're back to that problem — but see
  below for linkage alternatives.
- Accessing MIR from a backend context is slightly more awkward than from a
  query provider — we'd call `tcx.optimized_mir(def_id)` and do our own
  substitution. Not much more awkward in practice.

**Question for you:** was the `CodegenBackend` route evaluated? The guide
lists `per_instance_mir` as central, but I'm not seeing why it's structurally
incompatible with being a backend — only that the backend path moves some
mechanics around.

#### Alternative C: pre-built stub bodies + `Callbacks::after_analysis` walk

The dumbest version: generated stub bodies already have real content
(`extern "C"` forward declarations + a call through them). No MIR override
at all. The `Callbacks::after_analysis` hook runs our dep walk using
`tcx.mir_built` + explicit substitution, populating `toylang_instances`.
Codegen compiles the stub bodies normally, which emit the extern decls; our
`.o` provides the definitions.

This loses the "unreachable!() in stubs, replaced at monomorphization" pattern
but keeps the capability. Possibly the simplest zero-fork design. Not sure
if it has holes I haven't thought of.

### Patch 5: eliminate the partitioner hook

The hook forces `External/Default` linkage on `__lang_stubs` wrappers so
they're not internalized during CGU partitioning (which would break external
`.o` linking).

#### Alternative A: `#[linkage = "external"]` under `#![feature(linkage)]`

The guide's §10.6.5 rejects this with "requires `#![feature(linkage)]` at the
crate root, which propagates a nightly feature flag into user-controlled
territory."

The feature flag lives in `__lang_stubs.rs`, which we generate — it's never
user-authored. The feature is per-crate and isolated to the stub crate. The
"propagation" concern seems to be about a nightly requirement leaking to
users, but we're already requiring a pinned nightly for the whole facade
architecture; one more feature flag inside a generated file feels like a
small incremental cost.

**Question for you:** was there a concrete failure mode with `#[linkage]`
beyond the feature-flag-propagation concern? E.g. did it interact badly with
something in the partitioner even with the right annotation, or was it
rejected on principle?

#### Alternative B: separate crate for wrappers

Cross-crate linkage defaults differ from intra-crate. A `pub
#[inline(never)]` item in a dependency crate is less likely to be
internalized, because the linker can't assume all callers are visible.

If `__lang_stubs` became its own compile-separately crate (e.g., a build
dependency that produces an rlib), its wrappers would be treated as
externally-visible by partitioning of *its* CGUs, and the main crate
references them across a crate boundary where internalization doesn't apply.

Untested, and there's probably a Cargo-orchestration wrinkle, but worth a
sanity check.

#### Alternative C: upstream RFC

"Alternative language backends need to prevent internalization of their
runtime symbols" is articulable. Rustc already carries similar hooks for
`-Zprint-mono-items`, `-Zshare-generics`, `-Cno-prepopulate-passes`, etc.
Some precedent exists. Slow but not implausible. Good long-term move
regardless of the short-term choice.

---

## Stable-MIR

Orthogonal to the fork question, but worth flagging: `rustc_smir` /
`stable_mir` exposes a stable subset of MIR and types. It doesn't cover
every rustc internal we poke at (`fn_abi_of_instance` isn't there last I
checked), but it covers most of the type/instance walk. Using it reduces
our churn exposure to `rustc_middle::ty` internals over time.

**Question for you:** have you been tracking stable_mir? Does the current
surface cover what our dep walk needs?

---

## Proposed target architecture

If the alternatives above check out:

- `-Zcodegen-backend=valec` as the entry point — zero rustc patches
- `rustc_interface::Config::override_queries` for `optimized_mir`,
  `symbol_name`, `layout_of`, `mir_shims` — public nightly API
- `stable_mir` for MIR / type inspection during the dep walk — reduces
  churn surface
- `#[linkage = "external"]` in generated `__lang_stubs.rs` under
  `#![feature(linkage)]` — eliminates the partitioner patch
- Pinned nightly, bumped on our schedule (quarterly), not tracking tip

Fork patches required: **zero.** Nightly features required: `rustc_private`,
`linkage`. Same nightly-pin cost we already pay, but no rebase-the-fork
cost on every bump.

---

## What I want from you

1. Was `override_queries` on `optimized_mir` considered? What pushed toward
   adding `per_instance_mir` as a new query instead of a trampoline-body
   workaround?
2. What did the `CodegenBackend` route look like when evaluated? The guide
   doesn't mention it explicitly; I want to understand if it was ruled out
   for a specific reason or just not chosen.
3. `#[linkage = "external"]` — was there a concrete failure beyond feature-flag
   propagation? Did you test it?
4. Is there rustc-internal context I'm missing about why per-Instance MIR
   as a new query felt necessary rather than a workaround?
5. What's your read on `stable_mir`'s readiness? Would adopting it be worth
   the rewrite cost even if we don't eliminate the fork immediately?
6. Am I underestimating the cost of the trampoline-body / backend-plugin
   approaches? What failure modes have you seen or anticipated?

Goal is a shared understanding of what's actually blocking a zero-fork
facade, so I can make a call for Vale with eyes open. If the fork is truly
load-bearing, I want to know that directly from someone who hit the walls
rather than inferring it from the guide.
