# Rust Interop (Background)

Vale programs can use Rust types directly via `import rust.<crate>.<path>.<Type>`.
The Coordinator (`valec`) invokes ValeRuster (generates Vale bindings from rustdoc JSON) during the Frontend pass, and Divination (generates a C-API Rust crate + static library) during the Backend pass. See `docs/usage/rust-interop.md` for how to use the feature, `docs/architecture/rust-interop.md` for the complete end-to-end flow, and `docs/migration/rust-interop.md` for known gaps.
