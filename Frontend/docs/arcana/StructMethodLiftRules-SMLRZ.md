# Struct Method Lift Rules (SMLRZ)

The `lift: Boolean` field on `FunctionS`/`FunctionA` controls whether a struct's internal method is compiled in the struct's outer environment (lifted) or inner environment (not lifted, inherits struct's generic placeholders). This field exists to support Rust interop, where the downstream naming pipeline requires struct type args on the struct step of the path (e.g. `Vec<i32>::capacity`), not on the function step (e.g. `Vec::capacity<i32>`).

## Where

- `PostParsingPass/.../FunctionScout.scala:514-531` — lift detection logic
- `PostParsingPass/.../ast.scala` — `FunctionS.lift` field definition
- `TypingPass/.../function/FunctionCompilerCore.scala:267-297` — `makePrototype` struct-method branch: extracts instantiated struct from `self` param, filters inherited template args
- `TypingPass/.../function/FunctionCompilerMiddleLayer.scala:449-469` — `assembleName`: builds function ID using parent env's instantiated ID
- `TypingPass/.../citizen/StructCompilerCore.scala:178` — environment selection: `if (functionA.lift) outerEnv else structInnerEnv`
- `TypingPass/.../citizen/StructCompilerGenericArgsLayer.scala:118-119` — non-lifted methods registered in resolved env
- `TypingPass/.../citizen/StructCompiler.scala:130-134` — lifted methods registered in outer env
- `TypingPass/.../macros/citizen/StructDropMacro.scala:113` — drop function generation: bypasses FunctionScout, sets `lift = true` and names the parameter `self` so `assembleName` routes it through the same lifted struct-method path as user-written drops
- `SimplifyingPass/.../NameHammer.scala:79-89` — `simplifyName` handles `StructNameI` (instantiated), crashes on `StructTemplateNameI`
- `Backend/src/utils/rustify.cpp:25-44` — `rustifySimpleIdStep` emits type args as `<i32>` on each path step

## Why Rust interop requires this

Rust organizes methods inside `impl<T> Vec<T>` blocks. The Rust path for a method is `Vec<i32>::capacity` — type args belong to the type, not the method. When a method has its own generics (e.g. `fn map<U>`), the Rust path is `Vec<i32>::map<String>` — struct args on the struct step, method args on the method step.

ValeRuster generates Vale bindings that place methods inside `extern struct` blocks:

```vale
extern struct Vec<T> imm {
  extern func capacity(self Vec<T>) i64;
}
```

The downstream pipeline (NameHammer → rustify → ValeRuster) produces Rust paths directly from Vale's ID structure with **no rearrangement**:
1. `NameHammer.simplifyName` converts `StructNameI("Vec", [int])` → `SimpleIdStep("Vec", [int])`
2. `rustifySimpleIdStep` converts that → `"Vec<i32>"`
3. Path steps joined with `::` → `"Vec<i32>::capacity"`
4. ValeRuster matches this directly against Rust's `impl` blocks

If Vale produced the old-style `Vec::capacity<i32>` (type args on the function), ValeRuster would look for a bare `Vec` type and a `capacity<i32>` method inside it — which doesn't match Rust's structure. There is no rearrangement code anywhere in ValeRuster to move type args from the function to the struct.

## How the ID structure is built

### The `inherited` field on `GenericParameterS`

When a method is declared inside an `extern struct Vec<T>` block, `T` is an inherited generic param — it comes from the struct, not the method. The `inherited: Boolean` field on `GenericParameterS` marks this. In `FunctionCompilerCore.scala:284-290`, inherited template args are stripped from the function's `templateArgs`:

```scala
val newTemplateArgs = genericParametersS.zip(templateArgs).filter(!_._1.inherited).map(_._2)
```

For `capacity(self Vec<T>)` where T is inherited: `templateArgs` becomes empty → `Vec<int>.capacity()`.
For a hypothetical `map<U>(self Vec<T>, f Func<T,U>)` where T is inherited but U is own: `templateArgs` keeps only U → `Vec<int>.map<String>()`.

### `makePrototype` in `FunctionCompilerCore`

For struct methods with a `self` parameter (detected by the `StructTemplateNameT` + `hasSelfParam` guard at line 267-268):
1. Extracts the instantiated struct ID from the `self` param's type (`selfStructId`)
2. Strips inherited template args from the function name
3. Builds the prototype as `selfStructId.addStep(newFuncName)` — putting `StructNameT("Vec", [Placeholder_T])` in `initSteps`

For all other functions: uses the default `PrototypeT(fullEnv.id, returnCoord)`.

### `assembleName` in `FunctionCompilerMiddleLayer`

Uses `runedEnv.parentEnv.id.addStep(makeFunctionName(...))`. For struct internal methods, `parentEnv.id` has the instantiated struct, so the call-site ID shape matches the definition-site ID shape. This is necessary because functions are cached in `CompilerOutputs` by exact `SignatureT` equality.

## Lift detection rules

`FunctionScout.scala:514-549` determines `lift`:

- **Free functions**: always lifted
- **Closures**: always lifted
- **Interface methods**: always lifted
- **Struct methods**: lifted only if a parameter is named `self` (matching `keywords.self`)
- **`LiftableAttributeP` attribute**: forces lift regardless

The `self` keyword rule was chosen over a type-based rule (checking if any param has the parent struct's type) for simplicity. An earlier type-parameter-based approach is preserved as commented-out code in `FunctionScout.scala:533-546`.

## Macro-generated functions bypass FunctionScout

Functions created by macros (e.g. `StructDropMacro`) construct `FunctionS`/`FunctionA` directly and set `lift` themselves. They do NOT go through `FunctionScout`'s lift detection logic.

**Generated drop functions ARE lifted, just like user-written drops.** They set `lift = true` and name their first parameter `self`, so `assembleName` takes the same self-struct path it takes for user-written `func drop(self Vec<T>)`. That path uses the self param's `StructTT.id` as the prefix — `StructNameT(template, [placeholders])` for both generic and non-generic structs (non-generic just has empty placeholders) — and `addStep`s the function name onto it. This produces the `StructNameI` prefix that downstream passes require (see "`NameHammer.simplifyName` only handles `StructNameI`" below).

This wasn't always the case. Pre-Group-G the macro used `thiss` as the param name and the comment claimed drops were "not lifted" — but that produced bare `StructTemplateNameT` in the function ID's initSteps, which was inconsistent with the user-written-drop shape and crashed the Instantiator's `assemblePlaceholderMap` (typed-AST has `StructNameT(template, [])` for non-generic struct prefixes everywhere else, so the simplified-AST mismatch was lethal).

For extern structs, ValeRuster generates `extern func drop(self Vec<T>)` as a separate extern method — the macro-generated drop is for Vale-side structs only.

## `NameHammer.simplifyName` only handles `StructNameI`

`NameHammer.simplifyName` matches `StructNameI(StructTemplateNameI(humanName), templateArgs)` and produces `SimpleIdStep(humanName, templateArgs)`. It does NOT handle bare `StructTemplateNameI` — that would hit `vimpl(other)` and crash. This is why the function ID must contain `StructNameI` (instantiated with type args), not `StructTemplateNameI` (bare template).
