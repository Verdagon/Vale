# RustResolverPrecedesGenericResolver-RRPGRZ

The rust-package resolver fallback must be chained *before* `resolvePackageContents` in the `FullCompilation` resolver `.or` chain, not after. The Frontend's `resolvePackageContents` unconditionally returns `Some(Map.empty)` for packages it can't find (rather than `None`), so any fallback placed after it in `.or` would be short-circuited for rust packages.

In `Frontend/PassManager/src/dev/vale/passmanager/PassManager.scala::build`:

```scala
Builtins.getCodeMap(interner, keywords)
  .or(packageCoord => resolveRustPackageContents(rustBindingsDir, packageCoord))
  .or(packageCoord => resolvePackageContents(interner, allInputs, packageCoord))
```

If the rust fallback were listed second, it would never run for `rust.*` packages: `resolvePackageContents` would filter inputs by `module == "rust"`, find nothing matching (user inputs aren't in the rust module), build an empty `sourceInputs`, and return `Some(filepathToSource)` with an empty map. `IPackageResolver.or` treats `Some` as a hit and skips the fallback — so `resolveRustPackageContents` would never get a chance to read the ValeRuster-generated `.vale` files. The lexer would then process an empty rust package and the user's `import rust.std.vec.Vec` would surface later as "Couldn't find anything with the name 'Vec'".

Placed first, the rust resolver returns `None` for non-rust packages (letting `resolvePackageContents` handle them normally) and `Some(Map[...])` with the generated bindings for rust packages (short-circuiting the generic resolver, which is what we want).

**How this affects maintenance:** anyone refactoring the resolver chain, or changing `resolvePackageContents` to return `None` for empty results, must check that the rust-interop smoke test still passes. If a future cleanup makes `resolvePackageContents` return `None` on miss, this ordering constraint can be relaxed, but it must be verified — the scripts/test-rust-interop.sh run is the ground truth.
